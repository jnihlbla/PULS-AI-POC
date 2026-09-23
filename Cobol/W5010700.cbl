000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5010700.                                                
000300*AUTHOR.         MARIE-ANN EVERBÄCK/SUSANNE ENEGARD.                      
000400*DATE-WRITTEN.   FEBRUARI 1980/SEPTEMBER 1986.                            
000500*    FUNKTION.   TP-PROGRAM FÖR FRÅGA PÅ HISTORIKREGISTRET (WDL2).        
000600*                TP-PROGRAM FÖR FRÅGA PÅ HISTORIKREGISTRET (WDL6).        
000700*                INLEVERANSINFORMATION.                                   
000800*    INDATA.                                                              
000900*        TRANSAKTION: W5T107                                              
001000*        MID:         W5I10701                                            
001100*    UTDATA.                                                              
001200*        MOD:         W5O10701                                            
001300*                                                                         
001400*   ÄNDRINGAR:                                                            
001500*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
001600*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
001700*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
001800*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
001900*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP3                                                                
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                     PIC X(8)    VALUE 'W5010700'.              
002900 77  JA                        PIC X(1)    VALUE 'J'.                     
003000 77  NEJ                       PIC X(1)    VALUE 'N'.                     
003100 77  DEL-RAPP-FINNS            PIC X(1)    VALUE 'N'.                     
003200 77  FOERSTA-GAANG             PIC X(1)    VALUE 'N'.                     
003300 77  FORTS-FINNS               PIC X(1)    VALUE 'N'.                     
003400 77  SPRAAK-IX                 PIC S9(9)   VALUE ZERO  COMP SYNC.         
003500 77  INDX                      PIC S9(9)   VALUE ZERO  COMP SYNC.         
003600 77  MAX-MOD-LAENGD            PIC S9(4)   VALUE +1048 COMP SYNC.         
003700 77  MAX-ANT-RADER-1           PIC S9(9)   VALUE +15   COMP SYNC.         
003800 77  MAX-INDX                  PIC S9(4)   VALUE +14   COMP SYNC.         
003900                                                                          
004000                                                                          
004100 77  WS-IDLEVNR-8        PIC X(8)               VALUE SPACE.              
004200                                                                          
004300 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
004400     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
004500     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
004600                                                                          
004700 77  BYT-SW                    PIC X       VALUE 'N'.                     
004800     88 BYT-BILD                           VALUE 'J'.                     
004900     88 BYT-EJ-BILD                        VALUE 'N'.                     
005000 77  OK-SW                     PIC X       VALUE 'J'.                     
005100     88 ALLT-OK                            VALUE 'J'.                     
005200     88 ALLT-NOT-OK                        VALUE 'N'.                     
005300 77  INDATA-SW                 PIC X       VALUE 'J'.                     
005400     88 INDATA-OK                          VALUE 'J'.                     
005500     88 INDATA-NOT-OK                      VALUE 'N'.                     
005600 77  WS-INLE-ART-SW            PIC X       VALUE 'J'.                     
005700     88 WS-INLE-ART-FOUND                  VALUE 'J'.                     
005800     88 WS-INLE-ART-SAKNAS                 VALUE 'N'.                     
005900 77  WS-INLE-INL-SW            PIC X       VALUE 'J'.                     
006000     88 WS-INLE-INL-FOUND                  VALUE 'J'.                     
006100     88 WS-INLE-INL-SAKNAS                 VALUE 'N'.                     
006200 77  WS-INLC-ART-SW            PIC X       VALUE 'J'.                     
006300     88 WS-INLC-ART-FOUND                  VALUE 'J'.                     
006400     88 WS-INLC-ART-SAKNAS                 VALUE 'N'.                     
006500 77  WS-INLC-INL-SW            PIC X       VALUE 'J'.                     
006600     88 WS-INLC-INL-FOUND                  VALUE 'J'.                     
006700     88 WS-INLC-INL-SAKNAS                 VALUE 'N'.                     
006800                                                                          
006900 77  W-POSTTYP                 PIC X(3)    VALUE SPACE.                   
007000     88 POSTTYP-OK                         VALUE '310' 'R31'              
007100                                           'R32' 'R33' 'R34'              
007200                                           'R40' ' '.                     
007300                                                                          
007400 77  W-IDTRANS                 PIC X(4)    VALUE SPACE.                   
007500     88 EGEN-MID                           VALUE '5107'.                  
007600     88 HELP-MID                           VALUE '0551'.                  
007700     88 GODK-MID                           VALUE '5107' '5197'.           
007800                                                                          
007900 01  W-INDX                    PIC 99      VALUE ZERO.                    
008000 01  W-IDDC                    PIC X(2)    VALUE SPACE.                   
008100 01  W-FAELT                   PIC X       VALUE SPACE.                   
008200 01  W-FORSTA                  PIC X       VALUE 'J'.                     
008300 01  W-DEL-KVRAPP              PIC S9(7)   VALUE ZERO  COMP-3.            
008400 01  W-C2-FORDEL               PIC S9(7)   VALUE ZERO  COMP-3.            
008500 01  WS-TIUPPDAT               PIC S9(7)   VALUE ZERO  COMP-3.            
008600 01  WS-DEL-TIAAVVD            PIC S9(7)   VALUE ZERO  COMP-3.            
008700 01  W-IDLEVNR                 PIC  X(5)   VALUE SPACE.                   
008800 01  WS-FLDC                   PIC X       VALUE SPACE.                   
008900 01  WS-DAINLEV                PIC 9(16)   VALUE ZERO.                    
009000 01  WS-INLE-DAINLEV           PIC 9(16)   VALUE ZERO.                    
009100 01  WS-INLC-DAINLEV           PIC 9(16)   VALUE ZERO.                    
009200 01  WS-IDAVINR                PIC Z(6)9   VALUE ZERO.                    
009300 01  WS-IDAVINR-NUM            PIC 9(7)    VALUE ZERO.                    
009400 01  WS-IDINLEV-REGDAT         PIC 9(6)    VALUE ZERO.                    
009500 01  WS-TIREGDAT               PIC 9(6)    VALUE ZERO.                    
009600                                                                          
009700***** FIELDS USED IN PATTERN MATCHING  *********                          
009800 01  WS-PATTERN                PIC X(8)    VALUE SPACES.                  
009900 01  WS-TEXT                   PIC X(8)    VALUE ZEROES.                  
010000 01  FILLER REDEFINES WS-TEXT.                                            
010100     03  WS-TEXT-NUM           PIC 9(8).                                  
010200                                                                          
010300 01  WS-PATTERN-LEN            PIC 99      VALUE ZERO.                    
010400 01  WS-TEXT-LEN               PIC 99      VALUE ZERO.                    
010500 01  WS-TRAIL-SPACE            PIC 99      VALUE ZERO.                    
010600 01  IX-P-MAX                  PIC 99      VALUE ZERO.                    
010700 01  IX-T-MAX                  PIC 99      VALUE ZERO.                    
010800 01  IX-T                      PIC 99      VALUE ZERO.                    
010900 01  IX-P                      PIC 99      VALUE ZERO.                    
011000                                                                          
011100 01  WS-NUM                    PIC 99      VALUE ZERO.                    
011200                                                                          
011300 01 SW-MATCH                   PIC X  VALUE 'N'.                          
011400     88  MATCH                        VALUE 'J'.                          
011500     88  NO-MATCH                     VALUE 'N'.                          
011600                                                                          
011700 01  RESULT-TAB.                                                          
011800     03 FILLER OCCURS 20 TIMES.                                           
011900         05 FILLER OCCURS 20 TIMES.                                       
012000            07 WS-RESULT     PIC X  VALUE 'N'.                            
012100                                                                          
012200***** END OF FIELDS USED IN PATTERN MATCHING  ******                      
012300                                                                          
012400 01  X                         PIC 9       VALUE ZERO.                    
012500 01  AX                        PIC 9       VALUE ZERO.                    
012600 01  BX                        PIC 9       VALUE ZERO.                    
012700 01  W-IDPTYP-SOK              PIC X(3)    VALUE SPACE.                   
012800 01  W-DATUM-SOK               PIC 9(8)    VALUE ZERO.                    
012900 01  W-IDLEVNR-NUM             PIC X(5)    VALUE SPACE.                   
013000 01  W-IDLEVNR-SOK             PIC X(5)    VALUE SPACE.                   
013100 01  W-KDRT-SOK                PIC 9(2)    VALUE ZERO.                    
013200 01  W-FLKDRT                  PIC X       VALUE 'N'.                     
013300                                                                          
013400 01  W-IDAVINR-SOK             PIC S9(7) COMP-3 VALUE ZERO.               
013500                                                                          
013600 01  W-DATUM-AAAAMMDD-SOK      PIC 9(8) VALUE ZERO.                       
013700 01  FILLER REDEFINES W-DATUM-AAAAMMDD-SOK.                               
013800       05  W-DATUM-AA-SOK      PIC 9(2).                                  
013900       05  W-DATUM-AAMMDD-SOK  PIC 9(6).                                  
014000                                                                          
014100 01  FILLER                  PIC X(16) VALUE 'TABELL'.                    
014200 01  SPAR-TABELL.                                                         
014300   03 TABELL OCCURS 14.                                                   
014400     05 IDPTYP-TAB           PIC X(3)        VALUE SPACE.                 
014500     05 IDLOPNRM-TAB         PIC Z(7)9       VALUE ZERO.                  
014600     05 TIAAVVD-TAB          PIC S9(5)       VALUE ZERO.                  
014700     05 IDLEVNR-TAB          PIC X(5)        VALUE SPACE.                 
014800     05 KDRT-TAB             PIC Z9          VALUE ZERO.                  
014900     05 IDFS-TAB             PIC X(8)        VALUE SPACE.                 
015000     05 FILLER REDEFINES IDFS-TAB.                                        
015100       10 IDAVINR-TAB        PIC 9(8).                                    
015200     05 TIAVSDAT-TAB         PIC 9(6)        VALUE ZERO.                  
015300     05 IDANALYS-TAB         PIC X(12)       VALUE SPACE.                 
015400     05 IDKST-TAB            PIC X(10)       VALUE SPACE.                 
015500     05 IDKONTO-TAB          PIC Z(9)9       VALUE ZERO.                  
015600     05 IDDISTR-TAB          PIC 9(5)        VALUE ZERO.                  
015700     05 IDKUNDNR-TAB         PIC 9(7)        VALUE ZERO.                  
015800     05 IDORDER-TAB          PIC 9(7)        VALUE ZERO.                  
015900     05 IDPRODNR-TAB         PIC 9(7)        VALUE ZERO.                  
016000     05 IDFAKT-TAB           PIC 9(7)        VALUE ZERO.                  
016100     05 IDSHIPM-TAB          PIC 9(7)        VALUE ZERO.                  
016200                                                                          
016300 01  DYNAMISKA-SUBPROGRAM.                                                
016400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
016500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
016600   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
016700   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
016800   03  W009CIA                   PIC X(8)    VALUE 'W009CIA '.            
016900                                                                          
017000*01 -COPY WMEDAREA                                                        
017100    SKIP3                                                                 
017200 01  MESSAGE-CODES.                                                       
017300     03 PARTNO-MISSING           PIC X(3)    VALUE '017'.                 
017400     03 FYLL-I-CMD               PIC X(3)    VALUE '048'.                 
017500     03 INF-MORE-INFO-EXISTS     PIC X(3)    VALUE '105'.                 
017600     03 LAST-PAGE                PIC X(3)    VALUE '106'.                 
017700     03 BYTE-AV-BILD             PIC X(3)    VALUE '127'.                 
017800     03 KEY-WRONG                PIC X(3)    VALUE '401'.                 
017900     03 ERR-NOT-AUTHORIZED       PIC X(3)    VALUE '405'.                 
018000                                                                          
018100 01  W-PROG-TO-PROG-SW1.                                                  
018200     03  M-SW-LL-5197            PIC S9(4)   VALUE +240 COMP SYNC.        
018300     03  M-SW-Z1-Z2-5197         PIC X(2)    VALUE LOW-VALUE.             
018400     03  M-SW-KDTRANS-5197       PIC X(8)    VALUE 'W5T197  '.            
018500     03  M-SW-IDTRANS-5197       PIC X(4)    VALUE '5107'.                
018600     03  M-SW-KDMFSTYP-5197      PIC X(1)    VALUE '2'.                   
018700                                                                          
018800*    03  MID -COPY W5I19701 -PRE 5197-                                    
018900     EJECT                                                                
019000                                                                          
019100 01  FILLER                      PIC X(16) VALUE 'SPAR-AREA '.            
019200 01  SPAR-AREA.                                                           
019300     03  SPAR-IDARTNR            PIC X(9)  VALUE SPACE.                   
019400     03  SPAR-IDDC               PIC X(2)  VALUE SPACE.                   
019500     03  SPAR-DAINLEV-NEXT       PIC 9(16) VALUE ZERO.                    
019600     03  SPAR-DAINLEV-ENTER      PIC 9(16) VALUE ZERO.                    
019700     03  SPAR-IDPTYP             PIC X(3)  VALUE SPACE.                   
019800     03  SPAR-IDLOPNRM           PIC Z(7)9 VALUE ZERO.                    
019900     03  SPAR-TIAAVVD            PIC S9(5) VALUE ZERO.                    
020000     03  SPAR-IDLEVNR            PIC X(5)  VALUE SPACE.                   
020100     03  SPAR-KDRT               PIC Z9    VALUE ZERO.                    
020200     03  SPAR-IDFS               PIC X(8)  VALUE SPACE.                   
020300     03 FILLER REDEFINES SPAR-IDFS.                                       
020400       05 SPAR-IDAVINR           PIC 9(8).                                
020500     03  SPAR-TIAVSDAT           PIC 9(6)  VALUE ZERO.                    
020600     03  SPAR-IDKONTO            PIC Z(9)9 VALUE ZERO.                    
020700     03  SPAR-IDANALYS           PIC X(12) VALUE SPACE.                   
020800     03  SPAR-IDKST              PIC X(10) VALUE SPACE.                   
020900     03  SPAR-IDDISTR            PIC 9(5)  VALUE ZERO.                    
021000     03  SPAR-IDKUNDNR           PIC 9(7)  VALUE ZERO.                    
021100     03  SPAR-IDORDER            PIC 9(7)  VALUE ZERO.                    
021200     03  SPAR-IDPRODNR           PIC 9(7)  VALUE ZERO.                    
021300     03  SPAR-IDFAKT             PIC 9(7)  VALUE ZERO.                    
021400     03  SPAR-DATUM-SOK          PIC 9(6)  VALUE ZERO.                    
021500     03  SPAR-IDPTYP-SOK         PIC X(3)  VALUE SPACE.                   
021600     03  SPAR-IDLEVNR-SOK        PIC X(5)  VALUE SPACE.                   
021700     03  SPAR-KDRT-SOK           PIC X(2)  VALUE LOW-VALUE.               
021800     03  SPAR-IDAVINR-SOK        PIC X(8)  VALUE SPACE.                   
021900     03  SPAR-BILD               PIC X(4)  VALUE SPACE.                   
022000     03  SPAR-IDSHIPM            PIC 9(7)  VALUE ZERO.                    
022100     EJECT                                                                
022200                                                                          
022300                                                                          
022400 01  SPLIT-DAINLEV             PIC 9(16).                                 
022500 01  FILLER    REDEFINES SPLIT-DAINLEV.                                   
022600     03  SPLIT-TISEKEL         PIC 9(2).                                  
022700     03  SPLIT-TIAAMMDD        PIC 9(6).                                  
022800     03  FILLER                PIC 9(8).                                  
022900                                                                          
023000 01  IDFS-WS                   PIC X(8).                                  
023100 01  IDDC-WS                   PIC X(2).                                  
023110 01  IDDC-WS-CLEAR             PIC X(2).                                  
023200 01  IDARTNR-WS                PIC X(9).                                  
023300 01  FILLER    REDEFINES IDARTNR-WS.                                      
023400     03  KEY-IDARTNR           PIC 9(9).                                  
023500     EJECT                                                                
023600*- - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                      
023700 01  FILLER                    PIC X(16)  VALUE 'NYCKLAR-T-DLI'.          
023800 01  NYCKLAR-TILL-DLI.                                                    
023900     03  W-IDARTNR-X.                                                     
024000         05  W-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.            
024100     03  W-DAINLEV-X.                                                     
024200         05  W-DAINLEV         PIC 9(16)  VALUE ZERO.                     
024300     EJECT                                                                
024400*- - - - - - - - - - - - - - - - -  DYNAMISKA SUB-PROGRAM                 
024500 01  FILLER                    PIC X(16)  VALUE 'DYN-SUB-PGM'.            
024600 01  DYN-SUB-PGM.                                                         
024700     03  WDATKONV              PIC X(8)   VALUE 'WDATKONV'.               
024800     SKIP2                                                                
024900 01  FILLER                    PIC X(16)  VALUE 'WDATAREA'.               
025000*01  -COPY WDATAREA                                                       
025100     EJECT                                                                
025200*                    ****   PARAMETRAR TILL W005INIT                      
025300 01  FILLER                    PIC X(16)  VALUE 'WMSGINIT'.               
025400*01  -COPY WMSGINIT                                                       
025500     EJECT                                                                
025600 01  FILLER                    PIC X(16)  VALUE 'W009CIA '.               
025700*01  -COPY W009CIA                                                        
025800     EJECT                                                                
025900*                    ****   VALID IDDC CODES                              
026000 01  FILLER                    PIC X(16)  VALUE 'IDDC CODES'.             
026100*01  -COPY WWDCKONS                                                       
026200*01  -COPY WWDC99                                                         
026300     EJECT                                                                
026400*01  -COPY WWLEV04                                                        
026500     EJECT                                                                
026600*- - - - - - - - - - - - - - - - -  MID-AREA                              
026700 01  FILLER                    PIC X(16)  VALUE 'MID-AREA   '.            
026800*01  MID -COPY W5I10701.                                                  
026900     EJECT                                                                
027000*- - - - - - - - - - - - - - - - -  MSG-AREA                              
027100 01  FILLER                    PIC X(16)  VALUE 'MSG-AREA   '.            
027200*    -COPY WMSGAREA                                                       
027300     EJECT                                                                
027400     03 MOD REDEFINES  MSG-AREA.                                          
027500*       05  -COPY W5O10701                                                
027600     EJECT                                                                
027700*- - - - - - - - - - - - - - - - -  MFS-AREA                              
027800 01  FILLER                    PIC X(16)  VALUE 'MFS-AREA   '.            
027900*    -COPY WMFSAREA                                                       
028000     EJECT                                                                
028100*- - - - - - - - - - - - - - - - -  IMS-WS                                
028200 01  FILLER                    PIC X(16)  VALUE 'IMS-WS     '.            
028300 01      IMS-WS.                                                          
028400*                        **** STATUS-KOD FRÅN IMS                         
028500     03  STATUS-WS             PIC XX.                                    
028600         88  SEGMENT-FINNS       VALUE '  '.                              
028700         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
028800         88  BASEN-SLUT          VALUE 'GB'.                              
028900     SKIP3                                                                
029000     03  GODK-STATUSKODER.                                                
029100         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
029200     SKIP3                                                                
029300 01  SSA1                      PIC X(64).                                 
029400 01  SSA2                      PIC X(64).                                 
029500 01  SSA3                      PIC X(64).                                 
029600     EJECT                                                                
029700*                            IMS FUNKTIONSKODER                           
029800*01      -COPY W0003                                                      
029900     EJECT                                                                
030000*- - - - - - - - - - - - - - - - -  DLI-IO-AREA                           
030100 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-AREA'.            
030200 01  DLI-IO-AREA               PIC X(200)  VALUE SPACE.                   
030300     SKIP3                                                                
030400*01  WLINLE01 -COPY WDL201 -RED DLI-IO-AREA.                              
030500     EJECT                                                                
030600*01  WLINLE11 -COPY WDL211 -RED DLI-IO-AREA.                              
030700     EJECT                                                                
030800*01  WLINLE21 -COPY WDL221 -RED DLI-IO-AREA.                              
030900     EJECT                                                                
031000*01  WLINLE22 -COPY WDL222 -RED DLI-IO-AREA.                              
031100     EJECT                                                                
031200*01  WLINLE23 -COPY WDL223 -RED DLI-IO-AREA.                              
031300     EJECT                                                                
031400*01  WLINLE31 -COPY WDL231 -RED DLI-IO-AREA.                              
031500     EJECT                                                                
031600 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDK601'.                  
031700 01  DLI-IO-WDK601.                                                       
031800*    03  WDK601   -COPY WDK601                                            
031900     EJECT                                                                
032000 01  FILLER             PIC X(16) VALUE 'DLI-IO-WLINLC01'.                
032100 01  DLI-IO-WLINLC01.                                                     
032200*    03  -COPY WDL601  -PRE INLC-                                         
032300 01  FILLER             PIC X(16) VALUE 'DLI-IO-WLINLC11'.                
032400 01  DLI-IO-WLINLC11.                                                     
032500*    03  -COPY WDL611  -PRE INLC-                                         
032600     EJECT                                                                
032700 LINKAGE SECTION.                                                         
032800*01  -COPY W0009     -PRE MSG-                                            
032900     SKIP2                                                                
033000*01  -COPY W0009     -PRE ALT1-                                           
033100     SKIP2                                                                
033200*    -COPY W0008     -PRE USEA-                                           
033300         05  FILLER       PIC X(1).                                       
033400     SKIP2                                                                
033500*    -COPY W0008     -PRE INLE-                                           
033600         05  FILLER       PIC X(1).                                       
033700*    -COPY W0008     -PRE INLC-                                           
033800         05  FILLER       PIC X(1).                                       
033900*    -COPY W0008     -PRE WDK6-                                           
034000         05  FILLER       PIC X(1).                                       
034100                                                                          
034200     EJECT                                                                
034300 PROCEDURE DIVISION USING MSG-PCB ALT1-PCB USEA-PCB INLE-PCB              
034400                          INLC-PCB WDK6-PCB.                              
034500     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB USEA-PCB INLE-PCB             
034600                           INLC-PCB WDK6-PCB.                             
034700*                                                                         
034800     PERFORM IMS-GET-MSG                                                  
034900     IF SEGMENT-FINNS                                                     
035020       PERFORM A-INIT-SPARA-INPUT                                         
035100       IF ALLT-NOT-OK                                                     
035200         MOVE KEY-WRONG       TO MED-IDMFSFEL                             
035300         CALL WMEDKONV USING MED-WMEDAREA                                 
035400         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
035500         PERFORM X-RENSA-BILD                                             
035600       ELSE                                                               
035700         IF INDATA-OK                                                     
035800           MOVE KEY-IDARTNR TO W-IDARTNR                                  
035900           IF W-IDARTNR = ZERO                                            
036000              MOVE PARTNO-MISSING   TO MED-IDMFSFEL                       
036100              CALL WMEDKONV USING MED-WMEDAREA                            
036200              MOVE MED-MFSFEL       TO MOD-TEMFSFEL                       
036300              PERFORM X-RENSA-BILD                                        
036400           ELSE                                                           
036500              PERFORM S1-SECURITY-CHECK-PARTNO-IDLEV                      
036600              IF PASSED-SECURITY-CHECK                                    
036700              AND SEGMENT-FINNS                                           
036800                 IF W-IDDC = WC-NDC-JP-6A OR WC-NDC-JP-61 OR              
036810                             WC-NDC-AU                                    
036900                    PERFORM F-JAPAN-AUS                                   
037000                 ELSE                                                     
037100                    PERFORM B-REDIGERA-RAD                                
037200                 END-IF                                                   
037300                                                                          
037400                 IF MFS-SPLIT AND BYT-BILD                                
037500                    PERFORM E-BYT-BILD                                    
037600                 END-IF                                                   
037700              ELSE                                                        
037800                 IF MED-IDMFSFEL = PARTNO-MISSING                         
037900*                  PARTNO NOT REGISTERED                                  
038000                   CONTINUE                                               
038100                 ELSE                                                     
038200*                  USER NOT AUTHORIZED                                    
038300                   CONTINUE                                               
038400                 END-IF                                                   
038500              END-IF                                                      
038600           END-IF                                                         
038700         END-IF                                                           
038800       END-IF                                                             
038900       IF MFS-SPLIT AND BYT-BILD AND PASSED-SECURITY-CHECK                
039000         CONTINUE                                                         
039100       ELSE                                                               
039200           COMPUTE MSG-KVLL = LENGTH OF MOD-W5O10701 + 4                  
039300           PERFORM IMS-INSERT-MSG                                         
039400       END-IF                                                             
039500     END-IF                                                               
039600     MOVE ZERO TO RETURN-CODE                                             
039700     GOBACK                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 A-INIT-SPARA-INPUT SECTION.                                              
040100     IF MSG-DUBBLA-TRANSKODER                                             
040200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10701                 
040300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
040400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
040500     ELSE                                                                 
040600       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W5I10701                   
040700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
040800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
040900     END-IF                                                               
041000                                                                          
041100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
041200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
041300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
041400     MOVE 0           TO W-IDAVINR-SOK                                    
041500     MOVE LOW-VALUE TO MSG-AREA                                           
041600     MOVE 'W5O107N1' TO MFS-IDMOD                                         
041700     MOVE '5107' TO MOD-IDTRANS                                           
041800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
041900                                                                          
042000                                                                          
042100     IF EGEN-MID OR HELP-MID OR GODK-MID                                  
042200       CONTINUE                                                           
042300     ELSE                                                                 
042400       MOVE MFS-RENSA-FAELT TO MID-IDDC-IN                                
042500                               MID-DATUM-IN                               
042600                               MID-IDFS-IN                                
042700                               MID-IDPTYP-IN                              
042800                               MID-IDLEVNR-IN                             
042900                               MID-KDRT-IN                                
043000                               MID-IDDC-UT                                
043100                               MID-DATUM-UT                               
043200                               MID-IDFS-UT                                
043300                               MID-IDPTYP-UT                              
043400                               MID-IDLEVNR-UT                             
043500                               MID-KDRT-UT                                
043600       MOVE SPACE TO MFS-KDTRTYP                                          
043700       MOVE '7' TO MFS-IDPFK                                              
043800     END-IF                                                               
043900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
044000     MOVE '001'                  TO MSGI-KDCALL                           
044100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
044200     MOVE '5107'                 TO MSGI-IDTRANS                          
044300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
044400                                                                          
044500     IF EGEN-MID AND MID-IDARTNR-IN NOT = ALL '+'                         
044600       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
044700     ELSE                                                                 
044800       IF MID-IDARTNR-IN NUMERIC                                          
044900         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
045000       END-IF                                                             
045100     END-IF                                                               
045200     IF  EGEN-MID OR GODK-MID                                             
045300       IF MID-IDFS-IN NOT = ALL '+'                                       
045400         MOVE MID-IDFS-IN        TO MSGI-IDFS                             
045500       ELSE                                                               
045600          IF MID-IDFS-UT NOT = SPACE                                      
045700            MOVE MID-IDFS-UT     TO MSGI-IDFS                             
045800          END-IF                                                          
045900       END-IF                                                             
046000     ELSE                                                                 
046100       MOVE SPACE                TO MSGI-IDFS                             
046200     END-IF                                                               
046300                                                                          
046400     IF  EGEN-MID OR  GODK-MID                                            
046500       IF MID-IDDC-IN NOT = ALL '+'                                       
046600         IF MID-IDDC-IN NOT > ZERO                                        
046700            MOVE SPACE           TO MSGI-IDDC-KEY                         
046800         ELSE                                                             
046900             MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                         
047000         END-IF                                                           
047100       ELSE                                                               
047102          IF MID-IDDC-UT = WC-DC-ZERO                                     
047104             MOVE MSGI-IDDC      TO IDDC-WS-CLEAR                         
047105          ELSE IF MID-IDDC-UT = SPACE                                     
047107             MOVE SPACE          TO MSGI-IDDC-KEY                         
047108          ELSE                                                            
047300             MOVE MID-IDDC-UT    TO MSGI-IDDC-KEY                         
047400          END-IF                                                          
047420          END-IF                                                          
047500       END-IF                                                             
047800     END-IF                                                               
047900                                                                          
048200                                                                          
048201     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048202                                                                          
048300     MOVE MSGI-IDARTNR   TO IDARTNR-WS                                    
048400     MOVE MSGI-IDFS      TO IDFS-WS                                       
048500     MOVE MSGI-IDDC-KEY  TO IDDC-WS                                       
048600     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
048700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
048800     MOVE SPACE          TO SPAR-BILD                                     
048900                                                                          
049000     IF MID-IDARTNR-IN NOT = ALL '+' AND W-IDTRANS = '5107'               
049100       MOVE ZERO           TO SPAR-DAINLEV-ENTER                          
049200                              SPAR-DAINLEV-NEXT                           
049300     ELSE                                                                 
049400       IF MID-IDARTNR-IN NOT NUMERIC AND NOT GODK-MID                     
049500         MOVE ZERO         TO SPAR-DAINLEV-ENTER                          
049600                              SPAR-DAINLEV-NEXT                           
049700       END-IF                                                             
049800     END-IF                                                               
049900                                                                          
050000     IF MID-IDDC-IN NOT = ALL '+' AND W-IDTRANS = '5107'                  
050100       IF MID-DATUM-UT NUMERIC                                            
050200         MOVE ZERO       TO MID-DATUM-UT                                  
050300       END-IF                                                             
050400       MOVE ZERO          TO SPAR-DAINLEV-ENTER                           
050500                             SPAR-DAINLEV-NEXT                            
050600     END-IF                                                               
050700                                                                          
050800     IF GODK-MID                                                          
050900       IF MID-DATUM-IN NOT = ALL '+'                                      
051000         IF MID-DATUM-IN NUMERIC                                          
051100           MOVE MID-DATUM-IN       TO W-DATUM-AAMMDD-SOK                  
051200           IF MID-DATUM-IN(1:2) > 50                                      
051300             MOVE 19               TO W-DATUM-AA-SOK                      
051400           ELSE                                                           
051500             MOVE 20               TO W-DATUM-AA-SOK                      
051600           END-IF                                                         
051700         ELSE                                                             
051800           MOVE ZERO               TO W-DATUM-AAAAMMDD-SOK                
051900*          MOVE NEJ                TO OK-SW                               
052000         END-IF                                                           
052100       ELSE                                                               
052200         IF MID-DATUM-UT NUMERIC                                          
052300           MOVE MID-DATUM-UT       TO W-DATUM-AAMMDD-SOK                  
052400           IF MID-DATUM-UT(1:2) > 50                                      
052500             MOVE 19               TO W-DATUM-AA-SOK                      
052600           ELSE                                                           
052700             MOVE 20               TO W-DATUM-AA-SOK                      
052800           END-IF                                                         
052900         END-IF                                                           
053000       END-IF                                                             
053100       IF MID-IDPTYP-IN NOT = ALL '+'                                     
053200         MOVE MID-IDPTYP-IN        TO W-POSTTYP                           
053300         IF POSTTYP-OK                                                    
053400           MOVE MID-IDPTYP-IN      TO W-IDPTYP-SOK                        
053500                                      MOD-IDPTYP-UT                       
053600         ELSE                                                             
053700*          MOVE NEJ                TO OK-SW                               
053800           MOVE MFS-RENSA-FAELT    TO MOD-IDPTYP-IN                       
053900         END-IF                                                           
054000       ELSE                                                               
054100         IF MID-IDPTYP-UT NOT = SPACE                                     
054200           MOVE MID-IDPTYP-UT      TO W-IDPTYP-SOK                        
054300                                      MOD-IDPTYP-UT                       
054400         END-IF                                                           
054500       END-IF                                                             
054600                                                                          
054700       IF MID-IDLEVNR-IN NOT = ALL '+'                                    
054800           MOVE MID-IDLEVNR-IN     TO W-IDLEVNR-SOK                       
054900                                      MOD-IDLEVNR-UT                      
055000       ELSE                                                               
055100           IF MID-IDLEVNR-UT NOT = SPACE                                  
055200             MOVE MID-IDLEVNR-UT   TO W-IDLEVNR-SOK                       
055300                                      MOD-IDLEVNR-UT                      
055400           ELSE                                                           
055500             MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-UT                      
055600           END-IF                                                         
055700       END-IF                                                             
055800                                                                          
055900       IF MID-KDRT-IN NOT = ALL '+'                                       
056000         IF MID-KDRT-IN NUMERIC                                           
056100           MOVE MID-KDRT-IN          TO W-KDRT-SOK                        
056200                                        MOD-KDRT-UT                       
056300           MOVE 'J'                  TO W-FLKDRT                          
056400         ELSE                                                             
056500*          MOVE 'N'                   TO OK-SW                            
056600           MOVE MFS-RENSA-FAELT      TO MOD-KDRT-IN                       
056700         END-IF                                                           
056800       ELSE                                                               
056900        IF MID-KDRT-UT(1:1) = ' '                                         
057000          MOVE ZERO TO MID-KDRT-UT(1:1)                                   
057100        END-IF                                                            
057200         IF MID-KDRT-UT NUMERIC                                           
057300           IF MID-KDRT-UT >= 0                                            
057400             MOVE MID-KDRT-UT        TO W-KDRT-SOK                        
057500                                        MOD-KDRT-UT                       
057600             MOVE 'J'                TO W-FLKDRT                          
057700           ELSE                                                           
057800             MOVE MFS-RENSA-FAELT    TO MOD-KDRT-UT                       
057900           END-IF                                                         
058000         END-IF                                                           
058100       END-IF                                                             
058200                                                                          
058300*CHECK IDDC                                                               
058322       IF IDDC-WS-CLEAR > SPACE                                           
058323          MOVE MSGI-IDDC            TO W-IDDC                             
058324                                       IDDC-WS                            
058325          MOVE SPACE                TO IDDC-WS-CLEAR                      
058330       ELSE                                                               
058400         IF IDDC-WS = SPACE OR ZERO                                       
058500           MOVE SPACE               TO W-IDDC                             
058600                                        IDDC-WS                           
058700         ELSE                                                             
059200             MOVE IDDC-WS           TO W-IDDC                             
059700         END-IF                                                           
059800       END-IF                                                             
059830                                                                          
059900       MOVE +1 TO INDX                                                    
060000       PERFORM UNTIL INDX > MAX-INDX                                      
060100         IF MID-CMD (INDX) NOT = '+' AND ' '                              
060200           IF MID-CMD (INDX) = 'S'                                        
060300              MOVE INDX TO W-INDX                                         
060400              MOVE 14 TO INDX                                             
060500              MOVE JA TO BYT-SW                                           
060600           END-IF                                                         
060700         END-IF                                                           
060800         ADD 1 TO INDX                                                    
060900       END-PERFORM                                                        
061000     END-IF                                                               
061100                                                                          
061200     IF IDARTNR-WS NUMERIC                                                
061300       CONTINUE                                                           
061400     ELSE                                                                 
061500       MOVE NEJ TO OK-SW                                                  
061600     END-IF                                                               
061700                                                                          
061800     IF GODK-MID AND ALLT-OK                                              
061900      IF POSTTYP-OK                                                       
062000        CONTINUE                                                          
062100      ELSE                                                                
062200        MOVE NEJ                TO OK-SW                                  
062300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
062400        MOVE MFS-RENSA-FAELT    TO MOD-IDPTYP-UT                          
062500      END-IF                                                              
062600     ELSE                                                                 
062700* NOT GODK-MID BUT ALLT-OK **                                             
062710       IF ALLT-OK                                                         
062730          MOVE MSGI-IDDC       TO W-IDDC                                  
062740                                  IDDC-WS                                 
062762       END-IF                                                             
062763     END-IF                                                               
062770                                                                          
062800     IF MSGI-IDLAND-SPR = 'GB'                                            
062900       MOVE 2     TO SPRAAK-IX                                            
063000       MOVE 'GB ' TO MED-IDSKYLT                                          
063100     ELSE                                                                 
063200       MOVE 1     TO SPRAAK-IX                                            
063300       MOVE 'S'   TO MED-IDSKYLT                                          
063400     END-IF                                                               
063500     MOVE IDFS-WS      TO MOD-IDFS-UT                                     
063600                          WS-PATTERN                                      
063700                                                                          
063800     MOVE IDDC-WS      TO MOD-IDDC-UT                                     
063900     MOVE IDARTNR-WS   TO MOD-IDARTNR-UT                                  
064000                          SPAR-IDARTNR                                    
064100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
064200     INSPECT SPAR-IDARTNR REPLACING LEADING ZERO BY SPACE                 
064300                                                                          
064360     MOVE MOD-IDDC-UT TO SPAR-IDDC                                        
064370     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
064600     IF W-DATUM-AAMMDD-SOK > 0                                            
064700        MOVE W-DATUM-AAMMDD-SOK  TO MOD-DATUM-UT                          
064800     ELSE                                                                 
064900        MOVE MFS-RENSA-FAELT     TO MOD-DATUM-UT                          
065000     END-IF                                                               
065100                                                                          
065200*      INSPECT MOD-DATUM-UT REPLACING LEADING ZERO BY SPACE               
065300*      INSPECT MOD-KDRT-UT REPLACING LEADING ZERO BY SPACE                
065400     INSPECT MOD-IDPTYP-UT REPLACING LEADING ZERO BY SPACE                
065500                                                                          
065600                                                                          
065700     IF ALLT-OK                                                           
065800*---   KONTROLL AV SELECT-RAD---------                                    
065900       IF MFS-SPLIT AND BYT-BILD                                          
066000         CONTINUE                                                         
066100       ELSE                                                               
066200         IF MFS-SPLIT AND BYT-EJ-BILD                                     
066300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
066400           MOVE FYLL-I-CMD      TO MED-IDMFSFEL                           
066500           CALL WMEDKONV USING MED-WMEDAREA                               
066600           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
066700           MOVE NEJ TO INDATA-SW                                          
066800         ELSE                                                             
066900           IF BYT-BILD AND NOT MFS-SPLIT                                  
067000             PERFORM MFS-ROER-EJ-FAELT-UT                                 
067100             PERFORM MFS-LAES-IN-FAELT                                    
067200             MOVE BYTE-AV-BILD    TO MED-IDMFSFEL                         
067300             CALL WMEDKONV USING MED-WMEDAREA                             
067400             MOVE MED-MFSFEL      TO MOD-TEMFSFEL                         
067500             MOVE NEJ TO INDATA-SW                                        
067600           END-IF                                                         
067700         END-IF                                                           
067800       END-IF                                                             
067900*----  SLUT KONTROLL AV SELECT-RAD------                                  
068000     END-IF                                                               
068100                                                                          
068200*    IF ALLT-OK AND INDATA-OK                                             
068300       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                             
068400                               MOD-IDDC-IN                                
068500                               MOD-DATUM-IN                               
068600                               MOD-IDFS-IN                                
068700                               MOD-IDPTYP-IN                              
068800                               MOD-IDLEVNR-IN                             
068900                               MOD-KDRT-IN                                
069000                               MOD-TEMFSFEL                               
069100                               MOD-TEMFSINF                               
069200*    END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 B-REDIGERA-RAD SECTION.                                                  
069600     SKIP2                                                                
069700     MOVE +1                     TO INDX                                  
069800     MOVE NEJ                    TO DEL-RAPP-FINNS                        
069900     MOVE JA                     TO WS-INLC-ART-SW                        
070000                                    WS-INLE-ART-SW                        
070100                                    WS-INLC-INL-SW                        
070200                                    WS-INLE-INL-SW                        
070300                                                                          
070400     MOVE ZERO                   TO W-DAINLEV                             
070500     MOVE 'JA'                   TO W-FORSTA                              
070600                                                                          
070700     IF MFS-IDPFK = '8'                                                   
070710       IF SPAR-DAINLEV-NEXT NOT NUMERIC                                   
070720          MOVE ZERO TO SPAR-DAINLEV-NEXT                                  
070730       END-IF                                                             
070800       IF SPAR-DAINLEV-NEXT > ZERO                                        
070900         MOVE SPAR-DAINLEV-NEXT  TO W-DAINLEV                             
071000         MOVE ZERO               TO SPAR-DAINLEV-ENTER                    
071100                                    W-DATUM-SOK                           
071200       END-IF                                                             
071300     END-IF                                                               
071400                                                                          
071500     IF MFS-ENTER OR MFS-SPLIT                                            
071510       IF SPAR-DAINLEV-ENTER NOT NUMERIC                                  
071520          MOVE ZERO TO SPAR-DAINLEV-ENTER                                 
071530       END-IF                                                             
071600       IF SPAR-DAINLEV-ENTER > ZERO                                       
071700         MOVE SPAR-DAINLEV-ENTER TO W-DAINLEV                             
071800         MOVE ZERO               TO SPAR-DAINLEV-ENTER                    
071900       END-IF                                                             
072000     END-IF                                                               
072100                                                                          
072200     IF MFS-IDPFK = '7' OR MID-DATUM-IN NOT = ALL '+'                     
072300       OR MID-IDPTYP-IN NOT = ALL '+' OR MID-KDRT-IN NOT = ALL '+'        
072400       OR MID-IDLEVNR-IN NOT = ALL '+'                                    
072500       OR MID-IDFS-IN NOT = ALL '+'                                       
072600       IF W-DATUM-AAMMDD-SOK > ZERO                                       
072700         MOVE W-DATUM-AAAAMMDD-SOK  TO W-DATUM-SOK                        
072800         COMPUTE W-DAINLEV   = 9999999999999999 -                         
072900                               (W-DATUM-SOK * 100000000)                  
073000         MOVE W-DAINLEV             TO SPAR-DAINLEV-ENTER                 
073100                                                                          
073200       ELSE                                                               
073300         MOVE ZERO                  TO W-DAINLEV                          
073400       END-IF                                                             
073500     END-IF                                                               
073600                                                                          
073700     IF W-IDTRANS = '5197' AND SPAR-DAINLEV-ENTER > 0                     
073800       MOVE SPAR-DAINLEV-ENTER (1:1) TO W-FAELT                           
073900       IF SPAR-DAINLEV-ENTER (1:1) = 0                                    
074000         MOVE 8                  TO SPAR-DAINLEV-ENTER(1:1)               
074100       ELSE                                                               
074200         MOVE 7                  TO SPAR-DAINLEV-ENTER(1:1)               
074300       END-IF                                                             
074400       MOVE SPAR-DAINLEV-ENTER   TO W-DAINLEV                             
074500     END-IF                                                               
074600                                                                          
074700     PERFORM IMS-GET-ART                                                  
074800     IF SEGMENT-SAKNAS                                                    
074900       MOVE NEJ                  TO WS-INLE-ART-SW                        
075000     END-IF                                                               
075100                                                                          
075200     PERFORM IMS-GET-INLC-ART                                             
075300     IF SEGMENT-SAKNAS                                                    
075400       MOVE NEJ                  TO WS-INLC-ART-SW                        
075500     END-IF                                                               
075600                                                                          
075700     IF WS-INLE-ART-FOUND OR WS-INLC-ART-FOUND                            
075800       IF WS-INLE-ART-FOUND                                               
075900         PERFORM IMS-GET-INLEV                                            
076000         IF SEGMENT-SAKNAS                                                
076100           MOVE NEJ              TO WS-INLE-INL-SW                        
076200           MOVE ZERO             TO INL-DAINLEV                           
076300         END-IF                                                           
076400       ELSE                                                               
076500         MOVE NEJ                TO WS-INLE-INL-SW                        
076600         MOVE ZERO               TO INL-DAINLEV                           
076700       END-IF                                                             
076800                                                                          
076900       IF WS-INLC-ART-FOUND                                               
077000         PERFORM IMS-GET-INLC-INL                                         
077100         IF SEGMENT-SAKNAS                                                
077200           MOVE NEJ              TO WS-INLC-INL-SW                        
077300           MOVE ZERO             TO INLC-INL-DAINLEV                      
077400         END-IF                                                           
077500       ELSE                                                               
077600         MOVE NEJ                TO WS-INLC-INL-SW                        
077700                                    WS-INLC-ART-SW                        
077800         MOVE ZERO               TO INLC-INL-DAINLEV                      
077900       END-IF                                                             
078000                                                                          
078100       PERFORM                                                            
078200         UNTIL (WS-INLE-INL-SAKNAS AND WS-INLC-INL-SAKNAS) OR             
078300               (INDX >= MAX-ANT-RADER-1)                                  
078400         EVALUATE TRUE                                                    
078500           WHEN WS-INLE-INL-FOUND AND                                     
078600                WS-INLC-INL-SAKNAS                                        
078700             PERFORM BA-PROCESS-INLE                                      
078800           WHEN WS-INLE-INL-SAKNAS AND                                    
078900                WS-INLC-INL-FOUND                                         
079000             PERFORM BB-PROCESS-INLC                                      
079100           WHEN WS-INLE-INL-FOUND AND                                     
079200                WS-INLC-INL-FOUND                                         
079300             IF INL-DAINLEV < INLC-INL-DAINLEV                            
079400               PERFORM BA-PROCESS-INLE                                    
079500             ELSE                                                         
079600               PERFORM BB-PROCESS-INLC                                    
079700             END-IF                                                       
079800         END-EVALUATE                                                     
079900                                                                          
080000       END-PERFORM                                                        
080100                                                                          
080200       IF DEL-RAPP-FINNS = JA                                             
080300         CONTINUE                                                         
080400       ELSE                                                               
080500         IF WS-INLE-INL-SAKNAS AND WS-INLC-INL-SAKNAS                     
080600           MOVE SPAR-DAINLEV-ENTER                                        
080700                                 TO SPAR-DAINLEV-NEXT                     
080800           PERFORM D-BLANKA-RADER                                         
080900           MOVE LAST-PAGE        TO MED-IDMFSINF                          
081000           CALL WMEDKONV      USING MED-WMEDAREA                          
081100           MOVE MED-TEMFSINF     TO MOD-TEMFSINF                          
081200         ELSE                                                             
081300           PERFORM C-KOLLA-FORTS                                          
081400         END-IF                                                           
081500       END-IF                                                             
081600     ELSE                                                                 
081700       MOVE PARTNO-MISSING   TO MED-IDMFSFEL                              
081800       CALL WMEDKONV USING MED-WMEDAREA                                   
081900       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
082000       PERFORM X-RENSA-BILD                                               
082100     END-IF                                                               
082200     MOVE '002'                  TO MSGI-KDCALL                           
082300     MOVE '5107'                 TO MSGI-IDTRANS                          
082400     MOVE SPAR-AREA              TO MSGI-SPAR-AREA                        
082500     CALL W005INIT            USING MSGI-WMSGINIT                         
082600                                    USEA-PCB                              
082700     .                                                                    
082800     EJECT                                                                
082900 BA-PROCESS-INLE SECTION.                                                 
083000     SKIP2                                                                
083100     MOVE INL-DAINLEV            TO W-DAINLEV                             
083200                                    SPLIT-DAINLEV                         
083300     IF W-FORSTA = 'J'                                                    
083400       MOVE INL-DAINLEV          TO SPAR-DAINLEV-ENTER                    
083500       MOVE 'NEJ'                TO W-FORSTA                              
083600     END-IF                                                               
083700                                                                          
083800     IF W-IDPTYP-SOK = ' ' OR '310' OR 'R31' OR 'R32' OR 'R30'            
083900       PERFORM IMS-GET-31-32-310                                          
084000       IF SEGMENT-FINNS                                                   
084100         IF W-KDRT-SOK = 0 OR W-KDRT-SOK = MOT-KDRT                       
084200           IF W-IDLEVNR-SOK = SPACE                                       
084300              OR W-IDLEVNR-SOK = MOT-IDLEVNR                              
084400             IF W-IDPTYP-SOK = ' ' OR W-IDPTYP-SOK = MOT-IDPTYP           
084500               IF MOT-IDFS > SPACE                                        
084600                 MOVE MOT-IDFS             TO WS-TEXT                     
084700               ELSE                                                       
084800                 MOVE MOT-IDAVINR          TO WS-TEXT-NUM                 
084900               END-IF                                                     
085000                                                                          
085100               PERFORM S04-CHECK-PATTERN                                  
085200               IF MATCH                                                   
085300                 IF MOT-IDDC = W-IDDC OR W-IDDC = SPACE                   
085400                   IF W-FLKDRT = 'N'                                      
085500                     IF MOT-IDPTYP = 'R32'                                
085600                       PERFORM BAA-REDIGERA-R32                           
085700                       ADD +1 TO INDX                                     
085800                     ELSE                                                 
085900                       PERFORM BAB-REDIGERA-R30-R31-310                   
086000                     END-IF                                               
086100                   ELSE                                                   
086200                     IF W-KDRT-SOK = MOT-KDRT                             
086300                       IF MOT-IDPTYP = 'R32'                              
086400                         PERFORM BAA-REDIGERA-R32                         
086500                         ADD +1 TO INDX                                   
086600                       ELSE                                               
086700                         PERFORM BAB-REDIGERA-R30-R31-310                 
086800                       END-IF                                             
086900                     END-IF                                               
087000                   END-IF                                                 
087100                 END-IF                                                   
087200               END-IF                                                     
087300             END-IF                                                       
087400           END-IF                                                         
087500         END-IF                                                           
087600       END-IF                                                             
087700     END-IF                                                               
087800     IF W-IDPTYP-SOK = ' ' OR 'R33' OR 'R34'                              
087900       PERFORM IMS-GET-33-34                                              
088000       IF SEGMENT-FINNS                                                   
088100         IF W-KDRT-SOK = 0 OR W-KDRT-SOK = DIR-KDRT                       
088200           IF W-IDLEVNR-SOK = SPACE                                       
088300              OR W-IDLEVNR-SOK = DIR-IDLEVNR                              
088400             IF W-IDPTYP-SOK = ' ' OR W-IDPTYP-SOK = DIR-IDPTYP           
088500               MOVE DIR-IDAVINR       TO  WS-TEXT-NUM                     
088600               MOVE DIR-IDLEVNR       TO  LEV04-IDLEVNR                   
088700               IF LEV04-REFNR                                             
088800                 IF DIR-IDSUPREF NOT = SPACE                              
088900                   MOVE DIR-IDSUPREF (4:7)                                
089000                                      TO  WS-TEXT                         
089100                 END-IF                                                   
089200               END-IF                                                     
089300               PERFORM S04-CHECK-PATTERN                                  
089400               IF MATCH                                                   
089500                   MOVE DIR-IDDC TO WS-IDDC                               
089600                   IF DDC-CN                                              
089700                   OR DDC-US                                              
089800                   OR DIR-IDDC = W-IDDC                                   
089900                   OR W-IDDC   = SPACE                                    
090000                     IF W-FLKDRT = 'N'                                    
090100                       PERFORM BAC-REDIGERA-R33-R34                       
090200                       ADD +1 TO INDX                                     
090300                     ELSE                                                 
090400                       IF W-KDRT-SOK = DIR-KDRT                           
090500                         PERFORM BAC-REDIGERA-R33-R34                     
090600                         ADD +1 TO INDX                                   
090700                       END-IF                                             
090800                     END-IF                                               
090900                   END-IF                                                 
091000               END-IF                                                     
091100             END-IF                                                       
091200           END-IF                                                         
091300         END-IF                                                           
091400       END-IF                                                             
091500     END-IF                                                               
091600     IF W-IDPTYP-SOK = ' ' OR 'R40'                                       
091700       PERFORM IMS-GET-40                                                 
091800       IF SEGMENT-FINNS                                                   
091900         IF W-KDRT-SOK = 0                                                
092000           IF W-IDLEVNR-SOK = SPACE                                       
092100              OR W-IDLEVNR-SOK = RET-IDLEVNR                              
092200             IF W-IDPTYP-SOK = ' ' OR W-IDPTYP-SOK = RET-IDPTYP           
092300               IF RET-IDDC = W-IDDC                                       
092400               OR W-IDDC = SPACE                                          
092500                 MOVE RET-IDORDNR          TO WS-TEXT-NUM                 
092600                 PERFORM S04-CHECK-PATTERN                                
092700                 IF MATCH                                                 
092800                   PERFORM BAD-REDIGERA-R40                               
092900                   ADD +1 TO INDX                                         
093000                 END-IF                                                   
093100               END-IF                                                     
093200             END-IF                                                       
093300           END-IF                                                         
093400         END-IF                                                           
093500       END-IF                                                             
093600     END-IF                                                               
093700                                                                          
093800     IF DEL-RAPP-FINNS = JA                                               
093900       CONTINUE                                                           
094000     ELSE                                                                 
094100       PERFORM IMS-GET-INLEV                                              
094200       IF SEGMENT-SAKNAS                                                  
094300         MOVE NEJ                TO WS-INLE-INL-SW                        
094400       END-IF                                                             
094500     END-IF                                                               
094600     .                                                                    
094700     EJECT                                                                
094800 BAA-REDIGERA-R32 SECTION.                                                
094900     SKIP2                                                                
095000     MOVE MOT-IDPTYP        TO MOD-IDPTYP(INDX)                           
095100                               IDPTYP-TAB(INDX)                           
095200     MOVE MOT-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
095300                               IDLOPNRM-TAB(INDX)                         
095400                                                                          
095500     MOVE MOT-TIUPPDAT      TO WS-TIUPPDAT                                
095600     PERFORM Y-KONV-AAVVD                                                 
095700     IF DAT-KDSVAR-OK                                                     
095800       MOVE DAT-TIAAVVD     TO MOD-TIAAVVD(INDX)                          
095900                              TIAAVVD-TAB(INDX)                           
096000     ELSE                                                                 
096100       MOVE ZERO            TO MOD-TIAAVVD(INDX)                          
096200                              TIAAVVD-TAB(INDX)                           
096300     END-IF                                                               
096400     MOVE MOT-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
096500     MOVE MOT-KDRT          TO MOD-KDRT(INDX)                             
096600     MOVE MOT-TIAVIDAT      TO MOD-TIAVSDAT(INDX)                         
096700     IF MOT-IDFS > SPACE                                                  
096800        MOVE MOT-IDFS       TO MOD-IDFS(INDX)                             
096900     ELSE                                                                 
097000        MOVE MOT-IDAVINR    TO MOD-IDAVINR(INDX)                          
097100     END-IF                                                               
097200     MOVE MOT-KVANTMOT      TO MOD-KVANTMOT(INDX)                         
097300     MOVE MOT-KVAVIS        TO MOD-KVAVIS(INDX)                           
097400*    MOVE MOT-KVFORDEL      TO MOD-KVFORDEL(INDX)                         
097500     MOVE MOT-KVRETUR       TO MOD-KVRETUR(INDX)                          
097600     MOVE MOT-KDAVVANT      TO MOD-KDAVVANT(INDX)                         
097700     MOVE MOT-IDLEVNR       TO IDLEVNR-TAB(INDX)                          
097800     MOVE MOT-IDFS          TO IDFS-TAB(INDX)                             
097900     IF MOT-IDFS > SPACE                                                  
098000       MOVE MOT-IDFS        TO IDFS-TAB(INDX)                             
098100     ELSE                                                                 
098200       MOVE MOT-IDAVINR     TO IDAVINR-TAB(INDX)                          
098300     END-IF                                                               
098400     MOVE MOT-KDRT          TO KDRT-TAB(INDX)                             
098500     MOVE MOT-TIAVIDAT      TO TIAVSDAT-TAB(INDX)                         
098600     MOVE MOT-IDKONTO       TO IDKONTO-TAB(INDX)                          
098700     MOVE MOT-IDANALYS      TO IDANALYS-TAB(INDX)                         
098800     MOVE MOT-IDKST         TO IDKST-TAB(INDX)                            
098900     MOVE MOT-IDSHIPM       TO IDSHIPM-TAB(INDX)                          
099000     .                                                                    
099100     EJECT                                                                
099200 BAB-REDIGERA-R30-R31-310 SECTION.                                        
099300     SKIP2                                                                
099400     MOVE JA   TO FOERSTA-GAANG                                           
099500     MOVE ZERO TO W-C2-FORDEL                                             
099600                                                                          
099700     MOVE MOT-IDPTYP        TO MOD-IDPTYP(INDX)                           
099800                               IDPTYP-TAB(INDX)                           
099900     MOVE MOT-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
100000                               IDLOPNRM-TAB(INDX)                         
100100                                                                          
100200     PERFORM S01-KONV-IDINLEV                                             
100300     IF DAT-KDSVAR-OK                                                     
100400       MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                           
100500                              TIAAVVD-TAB(INDX)                           
100600                              WS-DEL-TIAAVVD                              
100700     ELSE                                                                 
100800       MOVE ZERO           TO MOD-TIAAVVD(INDX)                           
100900                              TIAAVVD-TAB(INDX)                           
101000                              WS-DEL-TIAAVVD                              
101100     END-IF                                                               
101200     MOVE MOT-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
101300     MOVE MOT-KDRT          TO MOD-KDRT(INDX)                             
101400     MOVE MOT-TIAVIDAT      TO MOD-TIAVSDAT(INDX)                         
101500     IF MOT-IDFS > SPACE                                                  
101600       MOVE MOT-IDFS        TO MOD-IDFS(INDX)                             
101700     ELSE                                                                 
101800       MOVE MOT-IDAVINR     TO MOD-IDAVINR(INDX)                          
101900     END-IF                                                               
102000     MOVE MOT-KVAVIS        TO MOD-KVAVIS(INDX)                           
102100     MOVE MOT-KVANTMOT      TO MOD-KVANTMOT(INDX)                         
102200     MOVE MOT-KVRETUR       TO MOD-KVRETUR(INDX)                          
102300     MOVE MOT-KDAVVANT      TO MOD-KDAVVANT(INDX)                         
102400     MOVE MOT-IDLEVNR       TO IDLEVNR-TAB(INDX)                          
102500     IF MOT-IDFS > SPACE                                                  
102600        MOVE MOT-IDFS       TO IDFS-TAB(INDX)                             
102700     ELSE                                                                 
102800        MOVE MOT-IDAVINR    TO IDAVINR-TAB(INDX)                          
102900     END-IF                                                               
103000     MOVE MOT-KDRT          TO KDRT-TAB(INDX)                             
103100     MOVE MOT-TIAVIDAT      TO TIAVSDAT-TAB(INDX)                         
103200     MOVE MOT-IDKONTO       TO IDKONTO-TAB(INDX)                          
103300     MOVE MOT-IDANALYS      TO IDANALYS-TAB(INDX)                         
103400     MOVE MOT-IDKST         TO IDKST-TAB(INDX)                            
103500     MOVE MOT-IDSHIPM       TO IDSHIPM-TAB(INDX)                          
103600     ADD +1 TO INDX                                                       
103700                                                                          
103800     PERFORM IMS-GET-DEL                                                  
103900     MOVE ZERO TO W-C2-FORDEL                                             
104000     PERFORM UNTIL                                                        
104100      NOT ( SEGMENT-FINNS AND INDX < MAX-ANT-RADER-1 )                    
104200       IF FOERSTA-GAANG = JA                                              
104300         MOVE SPACES TO WS-TEXT                                           
104400         PERFORM S04-CHECK-PATTERN                                        
104500         IF MATCH                                                         
104600           MOVE ZERO TO W-DEL-KVRAPP                                      
104700           PERFORM BABA-FLYTTA-SPAR                                       
104800           MOVE NEJ TO FOERSTA-GAANG                                      
104900         END-IF                                                           
105000       END-IF                                                             
105100       COMPUTE W-DEL-KVRAPP = W-DEL-KVRAPP + DEL-KVRAPP                   
105200       MOVE W-DEL-KVRAPP TO MOD-KVAVIS(INDX)                              
105300                                                                          
105400       PERFORM IMS-GET-DEL                                                
105500     END-PERFORM                                                          
105600     IF FOERSTA-GAANG = JA                                                
105700       CONTINUE                                                           
105800     ELSE                                                                 
105900       ADD +1 TO INDX                                                     
106000     END-IF                                                               
106100     IF SEGMENT-FINNS                                                     
106200       MOVE JA TO DEL-RAPP-FINNS                                          
106300       MOVE W-DAINLEV        TO SPAR-DAINLEV-NEXT                         
106400     ELSE                                                                 
106500       MOVE NEJ TO DEL-RAPP-FINNS                                         
106600     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900 BABA-FLYTTA-SPAR SECTION.                                                
107000     SKIP2                                                                
107100     MOVE 'P32'             TO MOD-IDPTYP(INDX)                           
107200                               IDPTYP-TAB(INDX)                           
107300     MOVE MFS-RENSA-FAELT   TO MOD-IDLOPNRM(INDX)                         
107400                               MOD-IDLEVNR(INDX)                          
107500                               MOD-KDRT(INDX)                             
107600                               MOD-TIAVSDAT(INDX)                         
107700*                              MOD-IDKONTO(INDX)                          
107800                               MOD-IDAVINR(INDX)                          
107900                               MOD-KVANTMOT(INDX)                         
108000*                              MOD-KVFORDEL(INDX)                         
108100                               MOD-KVRETUR(INDX)                          
108200                               MOD-KDAVVANT(INDX)                         
108300                                                                          
108400     MOVE DEL-TIREGDAT      TO WS-TIUPPDAT                                
108500     PERFORM Y-KONV-AAVVD                                                 
108600     IF DAT-KDSVAR-OK                                                     
108700       MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                           
108800                              TIAAVVD-TAB(INDX)                           
108900     ELSE                                                                 
109000       MOVE ZERO           TO MOD-TIAAVVD(INDX)                           
109100                              TIAAVVD-TAB(INDX)                           
109200     END-IF                                                               
109300     .                                                                    
109400     EJECT                                                                
109500 BAC-REDIGERA-R33-R34 SECTION.                                            
109600     SKIP2                                                                
109700     MOVE DIR-IDPTYP        TO MOD-IDPTYP(INDX)                           
109800     MOVE DIR-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
109900     MOVE DIR-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
110000     MOVE DIR-KDRT          TO MOD-KDRT(INDX)                             
110100     MOVE DIR-TIAVSDAT      TO MOD-TIAVSDAT(INDX)                         
110200*    MOVE DIR-KVAVIS        TO MOD-KVAVIS(INDX)                           
110300     MOVE DIR-KVAVIS        TO MOD-KVANTMOT(INDX)                         
110400     MOVE DIR-IDAVINR       TO MOD-IDAVINR(INDX)                          
110500                               IDAVINR-TAB(INDX)                          
110600     MOVE DIR-IDLEVNR       TO LEV04-IDLEVNR                              
110700     IF LEV04-REFNR                                                       
110800       IF DIR-IDSUPREF NOT = SPACE                                        
110900         MOVE DIR-IDSUPREF (4:7)                                          
111000                            TO MOD-IDAVINR(INDX)                          
111100                               IDAVINR-TAB(INDX)                          
111200       END-IF                                                             
111300     END-IF                                                               
111400*                                                                         
111500     MOVE DIR-IDPTYP        TO IDPTYP-TAB(INDX)                           
111600     MOVE DIR-IDLOPNRM      TO IDLOPNRM-TAB(INDX)                         
111700     MOVE DIR-KDRT          TO KDRT-TAB(INDX)                             
111800     MOVE DIR-TIAVSDAT      TO TIAVSDAT-TAB(INDX)                         
111900     MOVE DIR-IDKONTO       TO IDKONTO-TAB(INDX)                          
112000     MOVE DIR-IDANALYS      TO IDANALYS-TAB(INDX)                         
112100     MOVE DIR-IDKST         TO IDKST-TAB(INDX)                            
112200     MOVE DIR-IDLEVNR       TO IDLEVNR-TAB(INDX)                          
112300     MOVE DIR-IDDISTR       TO IDDISTR-TAB(INDX)                          
112400     MOVE DIR-IDKUNDNR      TO IDKUNDNR-TAB(INDX)                         
112500     MOVE DIR-IDORDNR5      TO IDORDER-TAB(INDX)                          
112600     MOVE DIR-IDPRODNR      TO IDPRODNR-TAB(INDX)                         
112700     MOVE DIR-IDFAKT        TO IDFAKT-TAB(INDX)                           
112800     MOVE ZERO              TO IDSHIPM-TAB(INDX)                          
112900                                                                          
113000     PERFORM S01-KONV-IDINLEV                                             
113100     IF DAT-KDSVAR-OK                                                     
113200       MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                           
113300                              TIAAVVD-TAB(INDX)                           
113400                              WS-DEL-TIAAVVD                              
113500     ELSE                                                                 
113600       MOVE ZERO           TO MOD-TIAAVVD(INDX)                           
113700                              TIAAVVD-TAB(INDX)                           
113800                              WS-DEL-TIAAVVD                              
113900     END-IF                                                               
114000*    MOVE MFS-RENSA-FAELT   TO MOD-KVANTMOT(INDX)                         
114100     MOVE MFS-RENSA-FAELT   TO MOD-KVAVIS(INDX)                           
114200                               MOD-KVRETUR(INDX)                          
114300                               MOD-KDAVVANT(INDX)                         
114400     .                                                                    
114500     EJECT                                                                
114600 BAD-REDIGERA-R40 SECTION.                                                
114700     SKIP2                                                                
114800     MOVE RET-IDPTYP        TO MOD-IDPTYP(INDX)                           
114900     MOVE RET-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
115000     MOVE RET-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
115100     MOVE RET-IDORDNR       TO MOD-IDAVINR(INDX)                          
115200                               IDAVINR-TAB(INDX)                          
115300     MOVE RET-KVRETUR       TO MOD-KVRETUR(INDX)                          
115400     MOVE RET-IDPTYP        TO IDPTYP-TAB(INDX)                           
115500     MOVE RET-IDLOPNRM      TO IDLOPNRM-TAB(INDX)                         
115600     MOVE RET-IDLEVNR       TO IDLEVNR-TAB(INDX)                          
115700     MOVE ZERO              TO IDSHIPM-TAB(INDX)                          
115800                                                                          
115900     PERFORM S01-KONV-IDINLEV                                             
116000     IF DAT-KDSVAR-OK                                                     
116100       MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                           
116200                              TIAAVVD-TAB(INDX)                           
116300                              WS-DEL-TIAAVVD                              
116400     ELSE                                                                 
116500       MOVE ZERO           TO MOD-TIAAVVD(INDX)                           
116600                              TIAAVVD-TAB(INDX)                           
116700                              WS-DEL-TIAAVVD                              
116800     END-IF                                                               
116900                                                                          
117000     MOVE MFS-RENSA-FAELT   TO MOD-KDRT(INDX)                             
117100                               MOD-TIAVSDAT(INDX)                         
117200*                              MOD-IDKONTO(INDX)                          
117300                               MOD-KVAVIS(INDX)                           
117400                               MOD-KVANTMOT(INDX)                         
117500                               MOD-KDAVVANT(INDX)                         
117600                               MOD-CMD(INDX)                              
117700     .                                                                    
117800     EJECT                                                                
117900                                                                          
118000 S04-CHECK-PATTERN SECTION.                                               
118100                                                                          
118200     INITIALIZE RESULT-TAB ALL   TO VALUE                                 
118300                                                                          
118400     SET NO-MATCH                TO TRUE                                  
118500                                                                          
118600     IF WS-TEXT NUMERIC                                                   
118700       IF WS-TEXT-NUM = ZERO                                              
118800         MOVE '0'                TO WS-TEXT                               
118900       ELSE                                                               
119000         MOVE ZEROES             TO WS-NUM                                
119100         INSPECT WS-TEXT TALLYING WS-NUM FOR LEADING ZEROES               
119200         COMPUTE WS-TEXT-LEN = LENGTH OF WS-TEXT - WS-NUM                 
119300         MOVE WS-TEXT (1 + WS-NUM : WS-TEXT-LEN)                          
119400                                 TO WS-TEXT                               
119500       END-IF                                                             
119600     END-IF                                                               
119700                                                                          
119800     MOVE ZERO                   TO WS-TRAIL-SPACE                        
119900     INSPECT FUNCTION REVERSE (WS-PATTERN)                                
120000     TALLYING WS-TRAIL-SPACE FOR LEADING SPACES                           
120100                                                                          
120200     COMPUTE WS-PATTERN-LEN = LENGTH OF WS-PATTERN -                      
120300                              WS-TRAIL-SPACE                              
120400                                                                          
120500     MOVE ZERO                   TO WS-TRAIL-SPACE                        
120600     INSPECT FUNCTION REVERSE (WS-TEXT)                                   
120700     TALLYING WS-TRAIL-SPACE FOR LEADING SPACES                           
120800                                                                          
120900     COMPUTE WS-TEXT-LEN = LENGTH OF WS-TEXT -                            
121000                              WS-TRAIL-SPACE                              
121100                                                                          
121200     IF WS-PATTERN-LEN = 0                                                
121300       SET MATCH                 TO TRUE                                  
121400     ELSE                                                                 
121500                                                                          
121600       COMPUTE IX-P-MAX = WS-PATTERN-LEN + 1                              
121700       COMPUTE IX-T-MAX = WS-TEXT-LEN + 1                                 
121800                                                                          
121900       MOVE JA                   TO WS-RESULT (1, 1)                      
122000                                                                          
122100       PERFORM                                                            
122200       VARYING IX-P FROM 1 BY 1                                           
122300         UNTIL IX-P > WS-PATTERN-LEN                                      
122400         IF WS-PATTERN (IX-P : 1) = '*'                                   
122500           MOVE WS-RESULT (1, IX-P )                                      
122600                                 TO WS-RESULT (1, IX-P + 1)               
122700         END-IF                                                           
122800       END-PERFORM                                                        
122900                                                                          
123000       PERFORM                                                            
123100       VARYING IX-T FROM 1 BY 1                                           
123200         UNTIL IX-T > WS-TEXT-LEN                                         
123300         PERFORM                                                          
123400         VARYING IX-P FROM 1 BY 1                                         
123500           UNTIL IX-P > WS-PATTERN-LEN                                    
123600           IF WS-PATTERN (IX-P : 1) = '*'                                 
123700             IF WS-RESULT (IX-T + 1, IX-P) = JA OR                        
123800                WS-RESULT (IX-T, IX-P + 1) = JA                           
123900               MOVE JA           TO WS-RESULT (IX-T + 1, IX-P + 1)        
124000             ELSE                                                         
124100               MOVE NEJ          TO WS-RESULT (IX-T + 1, IX-P + 1)        
124200             END-IF                                                       
124300           ELSE                                                           
124400             IF WS-TEXT (IX-T:1) = WS-PATTERN (IX-P:1)                    
124500               MOVE WS-RESULT (IX-T, IX-P )                               
124600                                 TO WS-RESULT (IX-T + 1, IX-P + 1)        
124700             ELSE                                                         
124800               MOVE NEJ          TO WS-RESULT (IX-T + 1, IX-P + 1)        
124900             END-IF                                                       
125000           END-IF                                                         
125100         END-PERFORM                                                      
125200       END-PERFORM                                                        
125300       IF WS-RESULT (IX-T-MAX, IX-P-MAX) = JA                             
125400         SET MATCH               TO TRUE                                  
125500       ELSE                                                               
125600         SET NO-MATCH            TO TRUE                                  
125700       END-IF                                                             
125800     END-IF                                                               
125900     .                                                                    
126000     EJECT                                                                
126100                                                                          
126200 BB-PROCESS-INLC SECTION.                                                 
126300     SKIP2                                                                
126400     IF W-FORSTA = 'J'                                                    
126500       MOVE INLC-INL-DAINLEV     TO SPAR-DAINLEV-ENTER                    
126600       MOVE 'NEJ'                TO W-FORSTA                              
126700     END-IF                                                               
126800                                                                          
126900     MOVE INLC-INL-IDLEVNR       TO W-IDLEVNR                             
127000                                                                          
127100     IF W-IDLEVNR = '1441 ' OR 'BP2TW'                                    
127300       CONTINUE                                                           
127400     ELSE                                                                 
127500       IF W-IDDC = INLC-INL-IDDC OR W-IDDC = SPACE                        
127600         IF W-IDPTYP-SOK = ' ' OR                                         
127700            W-IDPTYP-SOK = INLC-INL-IDPTYP                                
127800           IF W-KDRT-SOK = 0 OR                                           
127900              W-KDRT-SOK = INLC-INL-KDRT                                  
128000             IF W-IDLEVNR-SOK = SPACE OR                                  
128100                W-IDLEVNR-SOK = INLC-INL-IDLEVNR                          
128200               IF W-FLKDRT   = 'N'                                        
128300                 PERFORM BBAA-FLYTTA-IDAVINR                              
128400                 MOVE WS-IDAVINR-NUM TO WS-TEXT-NUM                       
128500                                                                          
128600                 PERFORM S04-CHECK-PATTERN                                
128700                 IF MATCH                                                 
128800                   PERFORM BBA-WRITE-INLC-DATA                            
128900                 END-IF                                                   
129000               ELSE                                                       
129100                 IF W-KDRT-SOK = INLC-INL-KDRT                            
129200                   PERFORM BBAA-FLYTTA-IDAVINR                            
129300                   MOVE WS-IDAVINR-NUM TO WS-TEXT-NUM                     
129400                   PERFORM S04-CHECK-PATTERN                              
129500                   IF MATCH                                               
129600                     PERFORM BBA-WRITE-INLC-DATA                          
129700                   END-IF                                                 
129800                 END-IF                                                   
129900               END-IF                                                     
130000             END-IF                                                       
130100           END-IF                                                         
130200         END-IF                                                           
130300       END-IF                                                             
130400     END-IF                                                               
130500                                                                          
130600     PERFORM IMS-GET-INLC-INL                                             
130700     IF SEGMENT-SAKNAS                                                    
130800       MOVE NEJ                  TO WS-INLC-INL-SW                        
130900     END-IF                                                               
131000     .                                                                    
131100     EJECT                                                                
131200 BBA-WRITE-INLC-DATA SECTION.                                             
131300     SKIP2                                                                
131400     MOVE INLC-INL-IDPTYP        TO MOD-IDPTYP      (INDX)                
131500     MOVE INLC-INL-IDLEVNR       TO MOD-IDLEVNR     (INDX)                
131600     MOVE INLC-INL-KDRT          TO MOD-KDRT        (INDX)                
131700     MOVE INLC-INL-IDPTYP        TO IDPTYP-TAB      (INDX)                
131800     MOVE INLC-INL-IDLEVNR       TO IDLEVNR-TAB     (INDX)                
131900     MOVE INLC-INL-KDRT          TO KDRT-TAB        (INDX)                
132000     MOVE WS-IDAVINR             TO MOD-IDAVINR     (INDX)                
132100                                    IDAVINR-TAB     (INDX)                
132200     IF INLC-INL-IDANALYS NUMERIC                                         
132300       MOVE INLC-INL-IDANALYS    TO IDANALYS-TAB    (INDX)                
132400     ELSE                                                                 
132500       MOVE SPACE                TO IDANALYS-TAB    (INDX)                
132600     END-IF                                                               
132700                                                                          
132800     IF INLC-INL-IDKONTO NUMERIC                                          
132900       MOVE INLC-INL-IDKONTO     TO IDKONTO-TAB     (INDX)                
133000     ELSE                                                                 
133100       MOVE ZERO                 TO IDKONTO-TAB     (INDX)                
133200     END-IF                                                               
133300                                                                          
133400       MOVE INLC-INL-IDKST       TO IDKST-TAB       (INDX)                
133500                                                                          
133600     IF INLC-INL-IDPTYP = 'R30' OR '310'                                  
133700       IF W-IDPTYP-SOK = ' ' OR INLC-INL-IDPTYP                           
133800         MOVE INLC-INL-IDFAKT    TO MOD-IDLOPNRM    (INDX)                
133900         MOVE INLC-INL-IDFAKT    TO IDLOPNRM-TAB    (INDX)                
134000         MOVE INLC-INL-KVAVIS    TO MOD-KVAVIS      (INDX)                
134100       END-IF                                                             
134200     ELSE                                                                 
134300       IF INLC-INL-IDPTYP = 'R31'                                         
134400         IF W-IDPTYP-SOK = ' ' OR INLC-INL-IDPTYP                         
134500           MOVE INLC-INL-IDLOPNRM                                         
134600                                 TO MOD-IDLOPNRM    (INDX)                
134700           MOVE INLC-INL-IDLOPNRM                                         
134800                                 TO IDLOPNRM-TAB    (INDX)                
134900           MOVE INLC-INL-KVAVIS  TO MOD-KVAVIS      (INDX)                
135000           MOVE INLC-INL-KVANTMOT                                         
135100                                 TO MOD-KVANTMOT    (INDX)                
135200         END-IF                                                           
135300       ELSE                                                               
135400         IF INLC-INL-IDPTYP = 'R32'                                       
135500           IF W-IDPTYP-SOK = ' ' OR INLC-INL-IDPTYP                       
135600             MOVE INLC-INL-KVANTMOT                                       
135700                                 TO MOD-KVANTMOT    (INDX)                
135800             MOVE INLC-INL-KVAVIS                                         
135900                                 TO MOD-KVAVIS      (INDX)                
136000             IF INLC-INL-IDLOPNRM = 0                                     
136100               MOVE INLC-INL-IDFAKT                                       
136200                                 TO MOD-IDLOPNRM    (INDX)                
136300                                    IDLOPNRM-TAB    (INDX)                
136400             ELSE                                                         
136500               MOVE INLC-INL-IDLOPNRM                                     
136600                                 TO MOD-IDLOPNRM    (INDX)                
136700                                    IDLOPNRM-TAB    (INDX)                
136800             END-IF                                                       
136900                                                                          
137000             IF INLC-INL-FLMAKUL = 'J' OR 'Y'                             
137100               MOVE '2'          TO MOD-KDAVVANT    (INDX)                
137200             ELSE                                                         
137300               IF INLC-INL-KVANTMOT = INLC-INL-KVAVIS OR                  
137400                 (INLC-INL-KVANTMOT = INLC-INL-KVAVIS * -1)               
137500                 MOVE '0'        TO MOD-KDAVVANT    (INDX)                
137600               ELSE                                                       
137700                 MOVE '1'        TO MOD-KDAVVANT    (INDX)                
137800               END-IF                                                     
137900             END-IF                                                       
138000                                                                          
138100           END-IF                                                         
138200         ELSE                                                             
138300           MOVE INLC-INL-KVANTMOT                                         
138400                                 TO MOD-KVANTMOT    (INDX)                
138500           MOVE INLC-INL-KVAVIS  TO MOD-KVAVIS      (INDX)                
138600           MOVE INLC-INL-IDLOPNRM                                         
138700                                 TO MOD-IDLOPNRM    (INDX)                
138800                                    IDLOPNRM-TAB    (INDX)                
138900         END-IF                                                           
139000       END-IF                                                             
139100     END-IF                                                               
139200     MOVE INLC-INL-DAINLEV       TO WS-DAINLEV                            
139300     MOVE WS-DAINLEV (3:6)       TO WS-IDINLEV-REGDAT                     
139400     COMPUTE WS-TIREGDAT = 999999 - WS-IDINLEV-REGDAT                     
139500     MOVE WS-TIREGDAT            TO MOD-TIAVSDAT    (INDX)                
139600                                    TIAVSDAT-TAB    (INDX)                
139700                                                                          
139800     MOVE INLC-INL-TIINLINL      TO WS-TIUPPDAT                           
139900     PERFORM Y-KONV-AAVVD                                                 
140000     IF DAT-KDSVAR-OK                                                     
140100       MOVE DAT-TIAAVVD          TO MOD-TIAAVVD     (INDX)                
140200                                    TIAAVVD-TAB     (INDX)                
140300     ELSE                                                                 
140400       MOVE ZERO                 TO MOD-TIAAVVD     (INDX)                
140500                                    TIAAVVD-TAB     (INDX)                
140600     END-IF                                                               
140700     MOVE INLC-INL-KVART-SKROT   TO MOD-KVRETUR     (INDX)                
140800                                                                          
140900     ADD +1                      TO INDX                                  
141000     .                                                                    
141100     EJECT                                                                
141200 BBAA-FLYTTA-IDAVINR SECTION.                                             
141300     SKIP2                                                                
141400     MOVE +7                     TO AX                                    
141500     MOVE +7                     TO BX                                    
141600     MOVE +1                     TO X                                     
141700     MOVE ZERO                   TO WS-IDAVINR                            
141800                                    WS-IDAVINR-NUM                        
141900     PERFORM                                                              
142000       UNTIL X > 7                                                        
142100       IF INLC-INL-IDKUNDRF(AX:1) >= 0                                    
142200         MOVE INLC-INL-IDKUNDRF(AX:1)                                     
142300                                 TO WS-IDAVINR(BX:1)                      
142400                                    WS-IDAVINR-NUM(BX:1)                  
142500         SUBTRACT 1            FROM AX                                    
142600         SUBTRACT 1            FROM BX                                    
142700         ADD 1                   TO X                                     
142800       ELSE                                                               
142900         SUBTRACT 1            FROM AX                                    
143000         ADD 1                   TO X                                     
143100       END-IF                                                             
143200     END-PERFORM                                                          
143300     .                                                                    
143400     EJECT                                                                
143500 C-KOLLA-FORTS SECTION.                                                   
143600     SKIP2                                                                
143700     MOVE NEJ TO FORTS-FINNS                                              
143800                                                                          
143900     IF WS-INLE-INL-FOUND                                                 
144000       PERFORM                                                            
144100         UNTIL WS-INLE-INL-SAKNAS OR                                      
144200               FORTS-FINNS = JA                                           
144300         MOVE INL-DAINLEV        TO W-DAINLEV                             
144400                                    WS-INLE-DAINLEV                       
144500         PERFORM IMS-GET-31-32-310                                        
144600         IF SEGMENT-FINNS                                                 
144700           IF MOT-IDDC = W-IDDC OR W-IDDC = SPACE                         
144800             MOVE JA             TO FORTS-FINNS                           
144900           END-IF                                                         
145000         ELSE                                                             
145100           PERFORM IMS-GET-33-34                                          
145200           IF SEGMENT-FINNS                                               
145300             MOVE DIR-IDDC TO WS-IDDC                                     
145400             IF DDC-CN                                                    
145500             OR DDC-US                                                    
145600             OR DIR-IDDC = W-IDDC OR W-IDDC = SPACE                       
145700               MOVE JA           TO FORTS-FINNS                           
145800             END-IF                                                       
145900           ELSE                                                           
146000             PERFORM IMS-GET-40                                           
146100             IF SEGMENT-FINNS                                             
146200               IF RET-IDDC = W-IDDC OR W-IDDC = SPACE                     
146300                 MOVE JA         TO FORTS-FINNS                           
146400               END-IF                                                     
146500             END-IF                                                       
146600           END-IF                                                         
146700         END-IF                                                           
146800         IF FORTS-FINNS = JA                                              
146900           CONTINUE                                                       
147000         ELSE                                                             
147100           PERFORM IMS-GET-INLEV                                          
147200           IF SEGMENT-SAKNAS                                              
147300             MOVE NEJ            TO WS-INLE-INL-SW                        
147400           END-IF                                                         
147500         END-IF                                                           
147600       END-PERFORM                                                        
147700     END-IF                                                               
147800                                                                          
147900     MOVE NEJ TO FORTS-FINNS                                              
148000                                                                          
148100     IF WS-INLC-INL-FOUND                                                 
148200       PERFORM                                                            
148300         UNTIL WS-INLC-INL-SAKNAS OR                                      
148400               FORTS-FINNS = JA                                           
148500         MOVE INLC-INL-DAINLEV   TO W-DAINLEV                             
148600                                    WS-INLC-DAINLEV                       
148700         IF W-IDDC = INLC-INL-IDDC  OR W-IDDC = SPACE                     
148800           MOVE JA               TO FORTS-FINNS                           
148900         END-IF                                                           
149000         IF FORTS-FINNS = JA                                              
149100           CONTINUE                                                       
149200         ELSE                                                             
149300           PERFORM IMS-GET-INLC-INL                                       
149400           IF SEGMENT-SAKNAS                                              
149500             MOVE NEJ            TO WS-INLC-INL-SW                        
149600           END-IF                                                         
149700         END-IF                                                           
149800       END-PERFORM                                                        
149900     END-IF                                                               
150000                                                                          
150100     EVALUATE TRUE                                                        
150200       WHEN WS-INLE-DAINLEV > ZERO AND                                    
150300            WS-INLC-DAINLEV > ZERO                                        
150400         IF WS-INLE-DAINLEV < WS-INLC-DAINLEV                             
150500           MOVE WS-INLE-DAINLEV  TO SPAR-DAINLEV-NEXT                     
150600         ELSE                                                             
150700           MOVE WS-INLC-DAINLEV  TO SPAR-DAINLEV-NEXT                     
150800         END-IF                                                           
150900         MOVE INF-MORE-INFO-EXISTS                                        
151000                                 TO MED-IDMFSINF                          
151100         CALL WMEDKONV           USING MED-WMEDAREA                       
151200         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
151300       WHEN WS-INLE-DAINLEV = ZERO AND                                    
151400            WS-INLC-DAINLEV > ZERO                                        
151500         MOVE WS-INLC-DAINLEV    TO SPAR-DAINLEV-NEXT                     
151600         MOVE INF-MORE-INFO-EXISTS                                        
151700                                 TO MED-IDMFSINF                          
151800         CALL WMEDKONV           USING MED-WMEDAREA                       
151900         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
152000       WHEN WS-INLE-DAINLEV > ZERO AND                                    
152100            WS-INLC-DAINLEV = ZERO                                        
152200         MOVE WS-INLE-DAINLEV    TO SPAR-DAINLEV-NEXT                     
152300         MOVE INF-MORE-INFO-EXISTS                                        
152400                                 TO MED-IDMFSINF                          
152500         CALL WMEDKONV           USING MED-WMEDAREA                       
152600         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
152700       WHEN OTHER                                                         
152800         MOVE SPAR-DAINLEV-ENTER TO SPAR-DAINLEV-NEXT                     
152900         MOVE LAST-PAGE          TO MED-IDMFSINF                          
153000         CALL WMEDKONV USING MED-WMEDAREA                                 
153100         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
153200     END-EVALUATE                                                         
153300                                                                          
153400     .                                                                    
153500     EJECT                                                                
153600 D-BLANKA-RADER SECTION.                                                  
153700     PERFORM UNTIL                                                        
153800      NOT ( INDX < MAX-ANT-RADER-1 )                                      
153900       MOVE MFS-RENSA-FAELT TO MOD-IDPTYP(INDX)                           
154000                               MOD-IDLOPNRM(INDX)                         
154100                               MOD-TIAAVVD(INDX)                          
154200                               MOD-IDLEVNR(INDX)                          
154300                               MOD-KDRT(INDX)                             
154400                               MOD-TIAVSDAT(INDX)                         
154500*                              MOD-IDKONTO(INDX)                          
154600                               MOD-IDAVINR(INDX)                          
154700                               MOD-KVAVIS(INDX)                           
154800                               MOD-KVANTMOT(INDX)                         
154900*                              MOD-KVFORDEL(INDX)                         
155000                               MOD-KVRETUR(INDX)                          
155100                               MOD-KDAVVANT(INDX)                         
155200                               MOD-CMD(INDX)                              
155300       ADD +1 TO INDX                                                     
155400     END-PERFORM                                                          
155500     .                                                                    
155600     EJECT                                                                
155700 E-BYT-BILD SECTION.                                                      
155800     SKIP2                                                                
155900*    MOVE JA TO BYT-SW                                                    
156000*    MOVE NEJ TO ALLT-SW                                                  
156100                                                                          
156200* ---HÄMTAR RÄTT RAD-VÄRDE TILL 5197-BILDEN                               
156300                                                                          
156400     MOVE W-INDX                   TO INDX                                
156500*                                                                         
156600     IF IDPTYP-TAB(INDX) NOT = SPACE                                      
156700       MOVE IDPTYP-TAB(INDX)         TO SPAR-IDPTYP                       
156800       MOVE IDLOPNRM-TAB(INDX)       TO SPAR-IDLOPNRM                     
156900       MOVE TIAAVVD-TAB(INDX)        TO SPAR-TIAAVVD                      
157000       MOVE IDLEVNR-TAB(INDX)        TO SPAR-IDLEVNR                      
157100       IF (IDFS-TAB(INDX) > SPACE)                                        
157200         MOVE IDFS-TAB(INDX)         TO SPAR-IDFS                         
157300       ELSE                                                               
157400         MOVE IDAVINR-TAB(INDX)      TO SPAR-IDAVINR                      
157500       END-IF                                                             
157600       MOVE KDRT-TAB(INDX)           TO SPAR-KDRT                         
157700       MOVE TIAVSDAT-TAB(INDX)       TO SPAR-TIAVSDAT                     
157800       MOVE IDANALYS-TAB(INDX)       TO SPAR-IDANALYS                     
157900       MOVE IDKST-TAB(INDX)          TO SPAR-IDKST                        
158000       MOVE IDKONTO-TAB(INDX)        TO SPAR-IDKONTO                      
158100       MOVE IDDISTR-TAB(INDX)        TO SPAR-IDDISTR                      
158200       MOVE IDKUNDNR-TAB(INDX)       TO SPAR-IDKUNDNR                     
158300       MOVE IDPRODNR-TAB(INDX)       TO SPAR-IDPRODNR                     
158400       MOVE IDORDER-TAB(INDX)        TO SPAR-IDORDER                      
158500       MOVE IDFAKT-TAB (INDX)        TO SPAR-IDFAKT                       
158600       MOVE IDSHIPM-TAB (INDX)       TO SPAR-IDSHIPM                      
158700       MOVE W-DATUM-AAMMDD-SOK       TO SPAR-DATUM-SOK                    
158800       MOVE W-IDPTYP-SOK             TO SPAR-IDPTYP-SOK                   
158900       MOVE W-IDLEVNR-SOK            TO SPAR-IDLEVNR-SOK                  
159000       MOVE IDFS-WS                  TO SPAR-IDAVINR-SOK                  
159100       IF W-FLKDRT = 'J'                                                  
159200         MOVE W-KDRT-SOK             TO SPAR-KDRT-SOK                     
159300       ELSE                                                               
159400         MOVE LOW-VALUE              TO SPAR-KDRT-SOK                     
159500       END-IF                                                             
159600       MOVE '5197'                   TO SPAR-BILD                         
159700                                                                          
159800       MOVE '002'               TO MSGI-KDCALL                            
159900       MOVE '5107'              TO MSGI-IDTRANS                           
160000       MOVE SPAR-AREA           TO MSGI-SPAR-AREA                         
160100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
160200                                                                          
160300* ---SKICKAR VÄRDE TILL 5197-MID FÖR ATT SEDAN                            
160400* ---STARTA UPP DETTA PROGRAM                                             
160500                                                                          
160600       MOVE LOW-VALUE              TO 5197-MID-W5I19701                   
160700       MOVE SPAR-IDPTYP            TO 5197-MID-IDPTYP                     
160800       MOVE SPAR-IDLOPNRM          TO 5197-MID-IDLOPNRM                   
160900       MOVE SPAR-TIAAVVD           TO 5197-MID-TIAAVVD                    
161000       MOVE SPAR-IDLEVNR           TO 5197-MID-IDLEVNR                    
161100       MOVE SPAR-KDRT              TO 5197-MID-KDRT                       
161200       MOVE SPAR-TIAVSDAT          TO 5197-MID-TIAVSDAT                   
161300       MOVE SPAR-IDANALYS          TO 5197-MID-IDANALYS                   
161400       MOVE SPAR-IDKST             TO 5197-MID-IDKST                      
161500       MOVE SPAR-IDKONTO           TO 5197-MID-IDKONTO                    
161600       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O10701 + 17                     
161700       PERFORM IMS-INSERT-ALT1-MSG                                        
161800     END-IF                                                               
161900     .                                                                    
162000     EJECT                                                                
162100 F-JAPAN-AUS SECTION.                                                     
162200     PERFORM FA-LAES-GRUNDDATA                                            
162300                                                                          
162400     IF SEGMENT-SAKNAS                                                    
162500        MOVE PARTNO-MISSING   TO MED-IDMFSFEL                             
162600        CALL WMEDKONV USING MED-WMEDAREA                                  
162700        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
162800        PERFORM X-RENSA-BILD                                              
162900     ELSE                                                                 
163000     IF MFS-IDPFK = '8'                                                   
163100      MOVE SPAR-DAINLEV-NEXT  TO W-DAINLEV                                
163200                                                                          
163300      MOVE ZERO               TO SPAR-DAINLEV-ENTER                       
163400     END-IF                                                               
163500     IF MFS-SPLIT OR MFS-ENTER                                            
163600       IF SPAR-DAINLEV-ENTER > 0                                          
163700         MOVE SPAR-DAINLEV-ENTER TO W-DAINLEV                             
163800         MOVE ZERO               TO SPAR-DAINLEV-ENTER                    
163900       END-IF                                                             
164000     END-IF                                                               
164100     IF MFS-SPLIT AND W-IDTRANS = '5107'                                  
164200      MOVE SPAR-DAINLEV-ENTER TO W-DAINLEV                                
164300                                                                          
164400      MOVE ZERO               TO SPAR-DAINLEV-ENTER                       
164500     END-IF                                                               
164600     IF W-IDTRANS = '5197' AND SPAR-DAINLEV-ENTER > 0                     
164700       MOVE SPAR-DAINLEV-ENTER          TO W-DAINLEV                      
164800     END-IF                                                               
164900     IF MFS-IDPFK = '7' OR MID-DATUM-IN NOT = ALL '+'                     
165000       OR MID-IDPTYP-IN NOT = ALL '+' OR MID-KDRT-IN NOT = ALL '+'        
165100       OR MID-IDLEVNR-IN NOT = ALL '+'                                    
165200       OR MID-IDFS-IN NOT = ALL '+'                                       
165300       IF W-DATUM-AAMMDD-SOK > ZERO                                       
165400         MOVE W-DATUM-AAAAMMDD-SOK      TO W-DATUM-SOK                    
165500         COMPUTE W-DAINLEV       = 9999999999999999 -                     
165600                               (W-DATUM-SOK * 100000000)                  
165700         MOVE W-DAINLEV                 TO SPAR-DAINLEV-ENTER             
165800                                                                          
165900       ELSE                                                               
166000         MOVE ZERO                      TO W-DAINLEV                      
166100       END-IF                                                             
166200     END-IF                                                               
166300       PERFORM FB-LAES-RADDATA                                            
166400                                                                          
166500       IF SEGMENT-FINNS                                                   
166600        MOVE INLC-INL-IDLEVNR TO W-IDLEVNR                                
166700        IF W-IDLEVNR = '1441 ' OR 'BP2TW'                                 
166900          MOVE 'N' TO WS-FLDC                                             
167000          MOVE W-DAINLEV                TO SPAR-DAINLEV-ENTER             
167100          MOVE +0 TO INDX                                                 
167200        ELSE                                                              
167300          MOVE INLC-INL-DAINLEV         TO SPAR-DAINLEV-ENTER             
167400*         IF WC-DDC-SE = INLC-INL-IDDC                                    
167500*           MOVE +1           TO INDX                                     
167600*           MOVE 'J'          TO WS-FLDC                                  
167700*         ELSE                                                            
167800            IF (W-IDDC = '60' AND                                         
167900               (INLC-INL-IDDC = WC-NDC-JP-6A OR WC-NDC-JP-61 OR           
167910                                WC-NDC-AU))                               
168000               OR W-IDDC  = INLC-INL-IDDC                                 
168100               OR WC-DDC-SE = INLC-INL-IDDC                               
168200               OR W-IDDC = SPACE                                          
168300               IF W-IDPTYP-SOK = ' '                                      
168400                  OR W-IDPTYP-SOK = INLC-INL-IDPTYP                       
168500                 IF W-KDRT-SOK = 0  OR                                    
168600                    W-KDRT-SOK = INLC-INL-KDRT                            
168700                   IF W-IDLEVNR-SOK = SPACE  OR                           
168800                      W-IDLEVNR-SOK = INLC-INL-IDLEVNR                    
168900                     IF W-FLKDRT = 'N'                                    
169000                        MOVE 'J'       TO WS-FLDC                         
169100                     ELSE                                                 
169200                       IF W-KDRT-SOK = INLC-INL-KDRT                      
169300                          MOVE 'J'     TO WS-FLDC                         
169400                       ELSE                                               
169500                          MOVE 'N'     TO WS-FLDC                         
169600                       END-IF                                             
169700                     END-IF                                               
169800                   ELSE                                                   
169900                     MOVE 'N'       TO WS-FLDC                            
170000                   END-IF                                                 
170100                 ELSE                                                     
170200                   MOVE 'N'       TO WS-FLDC                              
170300                 END-IF                                                   
170400               ELSE                                                       
170500                 MOVE 'N'       TO WS-FLDC                                
170600               END-IF                                                     
170700            ELSE                                                          
170800               MOVE 'N'       TO WS-FLDC                                  
170900            END-IF                                                        
171000            IF WS-FLDC = 'J'                                              
171100               MOVE +1        TO INDX                                     
171200            ELSE                                                          
171300               MOVE +0        TO INDX                                     
171400            END-IF                                                        
171500*         END-IF                                                          
171600        END-IF                                                            
171700       ELSE                                                               
171800          MOVE W-DAINLEV TO SPAR-DAINLEV-ENTER                            
171900       END-IF                                                             
172000                                                                          
172100      PERFORM UNTIL INDX > MAX-INDX                                       
172200      IF SEGMENT-FINNS                                                    
172300        IF WS-FLDC = 'J'                                                  
172400          PERFORM FC-FLYTTA-IDAVINR                                       
172500          MOVE WS-IDAVINR-NUM     TO WS-TEXT-NUM                          
172600          PERFORM S04-CHECK-PATTERN                                       
172700                                                                          
172800          IF MATCH                                                        
172900             MOVE INLC-INL-IDPTYP      TO MOD-IDPTYP      (INDX)          
173000*            MOVE INLC-INL-IDDC        TO MOD-IDDC        (INDX)          
173100             MOVE INLC-INL-IDLEVNR     TO MOD-IDLEVNR     (INDX)          
173200             MOVE INLC-INL-KDRT        TO MOD-KDRT        (INDX)          
173300             MOVE INLC-INL-IDPTYP      TO IDPTYP-TAB      (INDX)          
173400             MOVE INLC-INL-IDLEVNR     TO IDLEVNR-TAB     (INDX)          
173500             MOVE INLC-INL-KDRT        TO KDRT-TAB        (INDX)          
173600                                                                          
173700             IF INLC-INL-IDANALYS NUMERIC                                 
173800             MOVE INLC-INL-IDANALYS    TO IDANALYS-TAB    (INDX)          
173900             ELSE                                                         
174000             MOVE SPACE TO IDANALYS-TAB (INDX)                            
174100             END-IF                                                       
174200                                                                          
174300             IF INLC-INL-IDKONTO NUMERIC                                  
174400               MOVE INLC-INL-IDKONTO     TO IDKONTO-TAB     (INDX)        
174500             ELSE                                                         
174600               MOVE ZERO TO IDKONTO-TAB (INDX)                            
174700             END-IF                                                       
174800                                                                          
174900               MOVE INLC-INL-IDKST       TO IDKST-TAB       (INDX)        
175000                                                                          
175100             IF INLC-INL-FLMAKUL = 'J' OR 'Y'                             
175200               MOVE '2'                TO MOD-KDAVVANT    (INDX)          
175300             ELSE                                                         
175400               IF INLC-INL-KVANTMOT = INLC-INL-KVAVIS OR                  
175500                 (INLC-INL-KVANTMOT = INLC-INL-KVAVIS * -1)               
175600                 MOVE '0'                TO MOD-KDAVVANT    (INDX)        
175700               ELSE                                                       
175800                 MOVE '1'                TO MOD-KDAVVANT    (INDX)        
175900               END-IF                                                     
176000             END-IF                                                       
176100                                                                          
176200             IF INLC-INL-IDPTYP = 'R30' OR '310'                          
176300               IF W-IDPTYP-SOK = ' ' OR INLC-INL-IDPTYP                   
176400                 MOVE INLC-INL-IDFAKT   TO MOD-IDLOPNRM    (INDX)         
176500                 MOVE INLC-INL-IDFAKT   TO IDLOPNRM-TAB    (INDX)         
176600                 MOVE INLC-INL-KVAVIS   TO MOD-KVAVIS      (INDX)         
176700               END-IF                                                     
176800             ELSE                                                         
176900               IF INLC-INL-IDPTYP = 'R31'                                 
177000                IF W-IDPTYP-SOK = ' ' OR INLC-INL-IDPTYP                  
177100                  MOVE INLC-INL-IDLOPNRM TO MOD-IDLOPNRM    (INDX)        
177200                  MOVE INLC-INL-IDLOPNRM TO IDLOPNRM-TAB    (INDX)        
177300                  MOVE INLC-INL-KVAVIS   TO MOD-KVAVIS      (INDX)        
177400                  MOVE INLC-INL-KVANTMOT TO MOD-KVANTMOT    (INDX)        
177500                END-IF                                                    
177600               ELSE                                                       
177700                 IF INLC-INL-IDPTYP = 'R32'                               
177800                   IF W-IDPTYP-SOK = ' ' OR INLC-INL-IDPTYP               
177900                    MOVE INLC-INL-KVANTMOT  TO MOD-KVANTMOT (INDX)        
178000                    MOVE INLC-INL-KVAVIS    TO MOD-KVAVIS   (INDX)        
178100                    IF INLC-INL-IDLOPNRM = 0                              
178200                     MOVE INLC-INL-IDFAKT   TO MOD-IDLOPNRM (INDX)        
178300                     MOVE INLC-INL-IDFAKT TO IDLOPNRM-TAB   (INDX)        
178400                    ELSE                                                  
178500                     MOVE INLC-INL-IDLOPNRM TO MOD-IDLOPNRM (INDX)        
178600                     MOVE INLC-INL-IDLOPNRM TO IDLOPNRM-TAB (INDX)        
178700                    END-IF                                                
178800                   END-IF                                                 
178900                 ELSE                                                     
179000                   MOVE INLC-INL-KVANTMOT TO MOD-KVANTMOT (INDX)          
179100                   MOVE INLC-INL-KVAVIS   TO MOD-KVAVIS  (INDX)           
179200                   MOVE INLC-INL-IDLOPNRM TO MOD-IDLOPNRM (INDX)          
179300                   MOVE INLC-INL-IDLOPNRM TO IDLOPNRM-TAB (INDX)          
179400                 END-IF                                                   
179500               END-IF                                                     
179600             END-IF                                                       
179700             MOVE WS-IDAVINR           TO MOD-IDAVINR (INDX)              
179800                                          IDAVINR-TAB (INDX)              
179900             MOVE INLC-INL-DAINLEV     TO WS-DAINLEV                      
180000             MOVE WS-DAINLEV (3:6)     TO WS-IDINLEV-REGDAT               
180100             COMPUTE WS-TIREGDAT = 999999 - WS-IDINLEV-REGDAT             
180200             MOVE WS-TIREGDAT          TO MOD-TIAVSDAT    (INDX)          
180300                                          TIAVSDAT-TAB    (INDX)          
180400                                                                          
180500             MOVE INLC-INL-TIINLINL    TO WS-TIUPPDAT                     
180600             PERFORM Y-KONV-AAVVD                                         
180700             IF DAT-KDSVAR-OK                                             
180800               MOVE DAT-TIAAVVD        TO MOD-TIAAVVD (INDX)              
180900                                          TIAAVVD-TAB (INDX)              
181000             ELSE                                                         
181100               MOVE ZERO               TO MOD-TIAAVVD (INDX)              
181200                                          TIAAVVD-TAB (INDX)              
181300             END-IF                                                       
181400             MOVE ZERO                 TO MOD-KVRETUR (INDX)              
181500*            MOVE ZERO                 TO MOD-KVFORDEL (INDX)             
181600                                                                          
181700          END-IF                                                          
181800        END-IF                                                            
181900      ELSE                                                                
182000          PERFORM MFS-RENSA-RAD-FAELT-UT                                  
182100       END-IF                                                             
182200                                                                          
182300       PERFORM FB-LAES-RADDATA                                            
182400                                                                          
182500       IF SEGMENT-FINNS                                                   
182600         MOVE INLC-INL-IDLEVNR TO W-IDLEVNR                               
182700         IF W-IDLEVNR = '1441 ' OR 'BP2TW'                                
182800           MOVE 'N' TO WS-FLDC                                            
182900         ELSE                                                             
183000*          IF WC-DDC-SE = INLC-INL-IDDC                                   
183100*           MOVE 'J'        TO WS-FLDC                                    
183200*          ELSE                                                           
183300             IF (W-IDDC = '60' AND                                        
183400                (INLC-INL-IDDC = WC-NDC-JP-6A OR WC-NDC-JP-61 OR          
183410                                 WC-NDC-AU))                              
183500                OR W-IDDC      = INLC-INL-IDDC                            
183600                OR WC-DDC-SE = INLC-INL-IDDC                              
183700                OR W-IDDC = SPACE                                         
183800                IF W-KDRT-SOK = 0 OR                                      
183900                   W-KDRT-SOK = INLC-INL-KDRT                             
184000                  IF W-IDPTYP-SOK = ' ' OR                                
184100                     W-IDPTYP-SOK = INLC-INL-IDPTYP                       
184200                    IF W-IDLEVNR-SOK = SPACE OR                           
184300                       W-IDLEVNR-SOK = INLC-INL-IDLEVNR                   
184400                        IF W-FLKDRT = 'N'                                 
184500                          MOVE 'J'       TO WS-FLDC                       
184600                        ELSE                                              
184700                          IF W-KDRT-SOK = INLC-INL-KDRT                   
184800                            MOVE 'J'     TO WS-FLDC                       
184900                          ELSE                                            
185000                            MOVE 'N'     TO WS-FLDC                       
185100                          END-IF                                          
185200                        END-IF                                            
185300                    ELSE                                                  
185400                      MOVE 'N'         TO WS-FLDC                         
185500                    END-IF                                                
185600                  ELSE                                                    
185700                    MOVE 'N' TO WS-FLDC                                   
185800                  END-IF                                                  
185900                ELSE                                                      
186000                  MOVE 'N'  TO WS-FLDC                                    
186100                END-IF                                                    
186200             ELSE                                                         
186300                MOVE 'N'    TO WS-FLDC                                    
186400             END-IF                                                       
186500*          END-IF                                                         
186600         END-IF                                                           
186700       END-IF                                                             
186800       IF WS-FLDC = 'J' OR SEGMENT-SAKNAS                                 
186900          ADD 1 TO INDX                                                   
187000       END-IF                                                             
187100                                                                          
187200       END-PERFORM                                                        
187300                                                                          
187400       IF SEGMENT-FINNS                                                   
187500         MOVE INLC-INL-DAINLEV     TO SPAR-DAINLEV-NEXT                   
187600         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
187700         CALL WMEDKONV             USING MED-WMEDAREA                     
187800         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
187900       ELSE                                                               
188000         MOVE SPAR-DAINLEV-ENTER TO SPAR-DAINLEV-NEXT                     
188100                                                                          
188200         MOVE LAST-PAGE          TO MED-IDMFSINF                          
188300         CALL WMEDKONV        USING MED-WMEDAREA                          
188400         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
188500       END-IF                                                             
188600                                                                          
188700       MOVE '002'      TO MSGI-KDCALL                                     
188800       MOVE '5107'     TO MSGI-IDTRANS                                    
188900       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
189000       CALL W005INIT   USING MSGI-WMSGINIT USEA-PCB                       
189100     END-IF                                                               
189200     .                                                                    
189300     EJECT                                                                
189400                                                                          
189500 FA-LAES-GRUNDDATA SECTION.                                               
189600     PERFORM IMS-GET-INLC-ART                                             
189700     .                                                                    
189800     EJECT                                                                
189900                                                                          
190000 FB-LAES-RADDATA SECTION.                                                 
190100     PERFORM IMS-GET-INLC-INL                                             
190200     .                                                                    
190300     EJECT                                                                
190400                                                                          
190500 FC-FLYTTA-IDAVINR SECTION.                                               
190600                                                                          
190700     MOVE +7 TO AX                                                        
190800     MOVE +7 TO BX                                                        
190900     MOVE +1 TO X                                                         
191000     MOVE ZERO TO WS-IDAVINR                                              
191100                  WS-IDAVINR-NUM                                          
191200     PERFORM UNTIL X > 7                                                  
191300       IF INLC-INL-IDKUNDRF(AX:1) >= 0                                    
191400         MOVE INLC-INL-IDKUNDRF(AX:1) TO WS-IDAVINR(BX:1)                 
191500                                         WS-IDAVINR-NUM(BX:1)             
191600         SUBTRACT 1 FROM AX                                               
191700         SUBTRACT 1 FROM BX                                               
191800         ADD 1 TO X                                                       
191900       ELSE                                                               
192000         SUBTRACT 1 FROM AX                                               
192100         ADD 1 TO X                                                       
192200       END-IF                                                             
192300     END-PERFORM                                                          
192400     EJECT                                                                
192500     .                                                                    
192600                                                                          
192700 X-RENSA-BILD SECTION.                                                    
192800     SKIP2                                                                
192900     MOVE ZERO               TO MOD-IDINLEV-NEXT                          
193000     MOVE +1 TO INDX                                                      
193100     PERFORM UNTIL                                                        
193200      NOT ( INDX < MAX-ANT-RADER-1 )                                      
193300       MOVE MFS-RENSA-FAELT TO MOD-IDPTYP(INDX)                           
193400                               MOD-IDLOPNRM(INDX)                         
193500                               MOD-TIAAVVD(INDX)                          
193600                               MOD-IDLEVNR(INDX)                          
193700                               MOD-KDRT(INDX)                             
193800                               MOD-TIAVSDAT(INDX)                         
193900*                              MOD-IDKONTO(INDX)                          
194000                               MOD-IDAVINR(INDX)                          
194100                               MOD-KVAVIS(INDX)                           
194200                               MOD-KVANTMOT(INDX)                         
194300                               MOD-KVRETUR(INDX)                          
194400                               MOD-KDAVVANT(INDX)                         
194500                               MOD-CMD(INDX)                              
194600       ADD +1 TO INDX                                                     
194700     END-PERFORM                                                          
194800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
194900     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
195000     MOVE MFS-RENSA-FAELT TO MOD-DATUM-IN                                 
195100     MOVE MFS-RENSA-FAELT TO MOD-IDPTYP-IN                                
195200     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
195300     MOVE MFS-RENSA-FAELT TO MOD-IDFS-IN                                  
195400     MOVE MFS-RENSA-FAELT TO MOD-KDRT-IN                                  
195500     .                                                                    
195600     EJECT                                                                
195700 Y-KONV-AAVVD SECTION.                                                    
195800     SKIP2                                                                
195900     MOVE WS-TIUPPDAT        TO DAT-I-TIDATUM                             
196000     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
196100                                                                          
196200     CALL WDATKONV USING DAT-KDDATFORM                                    
196300                         DAT-I-TIDATUM                                    
196400                         DAT-O-TIDATUM                                    
196500                         DAT-KDSVAR                                       
196600     .                                                                    
196700     EJECT                                                                
196800 S01-KONV-IDINLEV SECTION.                                                
196900     SKIP2                                                                
197000     COMPUTE DAT-I-TIDATUM = 999999 - SPLIT-TIAAMMDD                      
197100                                                                          
197200     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
197300                                                                          
197400     CALL WDATKONV USING DAT-KDDATFORM                                    
197500                         DAT-I-TIDATUM                                    
197600                         DAT-O-TIDATUM                                    
197700                         DAT-KDSVAR                                       
197800     .                                                                    
197900     EJECT                                                                
198000                                                                          
198100 S1-SECURITY-CHECK-PARTNO-IDLEV SECTION.                                  
198200     SKIP2                                                                
198300*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
198400     PERFORM IMS-GU-ARTIKEL-WDK601                                        
198500     IF  SEGMENT-FINNS                                                    
198600       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
198700       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
198800       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
198900*        --- USER AUTHORIZED                                              
199000         SET PASSED-SECURITY-CHECK TO TRUE                                
199100       ELSE                                                               
199200*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
199300         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
199400         CALL WMEDKONV USING MED-WMEDAREA                                 
199500         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
199600                                                                          
199700         SET BLOCKED-SECURITY-CHECK TO TRUE                               
199800       END-IF                                                             
199900     ELSE                                                                 
200000*       --- ARTIKEL SAKNAS                                                
200100        MOVE PARTNO-MISSING  TO MED-IDMFSFEL                              
200200        CALL WMEDKONV USING MED-WMEDAREA                                  
200300        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
200400        PERFORM X-RENSA-BILD                                              
200500     END-IF                                                               
200600     .                                                                    
200700     EJECT                                                                
200800                                                                          
200900                                                                          
201000*    M F S - SEKTIONER *                                                  
201100 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
201200                                                                          
201300     MOVE MFS-RENSA-FAELT   TO MOD-CMD      (INDX)                        
201400                               MOD-IDPTYP   (INDX)                        
201500                               MOD-IDLOPNRM (INDX)                        
201600                               MOD-TIAAVVD  (INDX)                        
201700                               MOD-IDLEVNR  (INDX)                        
201800                               MOD-KDRT     (INDX)                        
201900                               MOD-TIAVSDAT (INDX)                        
202000                               MOD-IDAVINR  (INDX)                        
202100                               MOD-KVAVIS   (INDX)                        
202200                               MOD-KVANTMOT (INDX)                        
202300                               MOD-KVRETUR  (INDX)                        
202400                               MOD-KDAVVANT (INDX)                        
202500     .                                                                    
202600     EJECT                                                                
202700 MFS-ROER-EJ-FAELT-UT SECTION.                                            
202800                                                                          
202900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
203000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-UT                                
203100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
203200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-IN                                
203300     MOVE MFS-ROER-EJ-FAELT TO MOD-DATUM-IN                               
203400     MOVE MFS-ROER-EJ-FAELT TO MOD-DATUM-UT                               
203500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPTYP-IN                              
203600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPTYP-UT                              
203700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                             
203800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-UT                             
203900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFS-IN                                
204000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFS-UT                                
204100     MOVE MFS-ROER-EJ-FAELT TO MOD-KDRT-IN                                
204200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDRT-UT                                
204300                                                                          
204400*    ALLA UT-FAELT                                                        
204500     MOVE +1 TO INDX                                                      
204600     PERFORM UNTIL INDX > MAX-INDX                                        
204700       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD      (INDX)                      
204800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPTYP   (INDX)                      
204900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLOPNRM (INDX)                      
205000       MOVE MFS-ROER-EJ-FAELT TO MOD-TIAAVVD  (INDX)                      
205100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR  (INDX)                      
205200       MOVE MFS-ROER-EJ-FAELT TO MOD-KDRT     (INDX)                      
205300       MOVE MFS-ROER-EJ-FAELT TO MOD-TIAVSDAT (INDX)                      
205400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVINR  (INDX)                      
205500       MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVIS   (INDX)                      
205600       MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTMOT (INDX)                      
205700       MOVE MFS-ROER-EJ-FAELT TO MOD-KVRETUR  (INDX)                      
205800       MOVE MFS-ROER-EJ-FAELT TO MOD-KDAVVANT (INDX)                      
205900       ADD +1 TO INDX                                                     
206000     END-PERFORM                                                          
206100     EJECT                                                                
206200     .                                                                    
206300 MFS-LAES-IN-FAELT SECTION.                                               
206400     MOVE +1 TO INDX                                                      
206500     PERFORM UNTIL INDX > MAX-INDX                                        
206600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(INDX)                     
206700     ADD +1 TO INDX                                                       
206800     END-PERFORM                                                          
206900     .                                                                    
207000     EJECT                                                                
207100                                                                          
207200*    I M S - SEKTIONER *                                                  
207300*                                                                         
207400 IMS-GET-MSG SECTION.                                                     
207500     MOVE '  QC' TO GODK-STATUSKODER                                      
207600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
207700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
207800     PERFORM IMS-STATUSKONTROLL                                           
207900     .                                                                    
208000     SKIP3                                                                
208100 IMS-INSERT-MSG SECTION.                                                  
208200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
208300       MOVE '0' TO MFS-KDHUVOMR                                           
208400     END-IF                                                               
208500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
208600     MOVE SPACE TO GODK-STATUSKODER                                       
208700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
208800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
208900     PERFORM IMS-STATUSKONTROLL                                           
209000     .                                                                    
209100     EJECT                                                                
209200 IMS-INSERT-ALT1-MSG SECTION.                                             
209300                                                                          
209400     MOVE SPACE TO GODK-STATUSKODER                                       
209500     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW1                  
209600     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
209700     PERFORM IMS-STATUSKONTROLL                                           
209800     .                                                                    
209900     EJECT                                                                
210000 IMS-GET-ART SECTION.                                                     
210100     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
210200             DELIMITED BY SIZE INTO SSA1                                  
210300     MOVE '  GE' TO GODK-STATUSKODER                                      
210400     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA SSA1                      
210500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     .                                                                    
210800     SKIP3                                                                
210900 IMS-GET-INLEV SECTION.                                                   
211000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
211100             DELIMITED BY SIZE INTO SSA1                                  
211200     STRING 'WLINLE11(DAINLEV =>' W-DAINLEV-X ')'                         
211300             DELIMITED BY SIZE INTO SSA2                                  
211400     MOVE '  GE' TO GODK-STATUSKODER                                      
211500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2                
211600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
211700     PERFORM IMS-STATUSKONTROLL                                           
211800     .                                                                    
211900     EJECT                                                                
212000 IMS-GET-31-32-310 SECTION.                                               
212100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
212200             DELIMITED BY SIZE INTO SSA1                                  
212300     MOVE 'WLINLE21' TO SSA2                                              
212400     MOVE '  GE' TO GODK-STATUSKODER                                      
212500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2                
212600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
212700     PERFORM IMS-STATUSKONTROLL                                           
212800     .                                                                    
212900     SKIP3                                                                
213000 IMS-GET-33-34 SECTION.                                                   
213100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
213200             DELIMITED BY SIZE INTO SSA1                                  
213300     MOVE 'WLINLE22' TO SSA2                                              
213400     MOVE '  GE' TO GODK-STATUSKODER                                      
213500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2                
213600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
213700     PERFORM IMS-STATUSKONTROLL                                           
213800     .                                                                    
213900     SKIP3                                                                
214000 IMS-GET-40 SECTION.                                                      
214100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
214200             DELIMITED BY SIZE INTO SSA1                                  
214300     MOVE 'WLINLE23' TO SSA2                                              
214400     MOVE '  GE' TO GODK-STATUSKODER                                      
214500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2                
214600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
214700     PERFORM IMS-STATUSKONTROLL                                           
214800     .                                                                    
214900     EJECT                                                                
215000 IMS-GET-DEL SECTION.                                                     
215100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
215200             DELIMITED BY SIZE INTO SSA1                                  
215300     MOVE 'WLINLE21' TO SSA2                                              
215400     MOVE 'WLINLE31' TO SSA3                                              
215500     MOVE '  GE' TO GODK-STATUSKODER                                      
215600     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
215700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
215800     PERFORM IMS-STATUSKONTROLL                                           
215900     .                                                                    
216000     EJECT                                                                
216100 IMS-GET-INLC-ART SECTION.                                                
216200                                                                          
216300     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
216400          DELIMITED BY SIZE INTO SSA1                                     
216500     MOVE '  GE'   TO GODK-STATUSKODER                                    
216600     CALL CBLTDLI  USING GU INLC-PCB DLI-IO-WLINLC01 SSA1                 
216700     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
216800     PERFORM IMS-STATUSKONTROLL                                           
216900     .                                                                    
217000     EJECT                                                                
217100 IMS-GET-INLC-INL SECTION.                                                
217200                                                                          
217300     STRING 'WLINLC11(DAINLEV =>' W-DAINLEV-X ')'                         
217400          DELIMITED BY SIZE INTO SSA1                                     
217500     MOVE '  GE'           TO GODK-STATUSKODER                            
217600     CALL CBLTDLI          USING GNP INLC-PCB DLI-IO-WLINLC11 SSA1        
217700     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
217800     PERFORM IMS-STATUSKONTROLL                                           
217900     .                                                                    
218000     EJECT                                                                
218100 IMS-GU-ARTIKEL-WDK601 SECTION.                                           
218200                                                                          
218300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
218400     DELIMITED BY SIZE INTO SSA1                                          
218500     MOVE '  GE' TO GODK-STATUSKODER                                      
218600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
218700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
218800     PERFORM IMS-STATUSKONTROLL                                           
218900     .                                                                    
219000     SKIP3                                                                
219100 IMS-STATUSKONTROLL SECTION.                                              
219200                                                                          
219300     SET STATUS-IX TO 1                                                   
219400     SEARCH GODK-STATUS                                                   
219500       AT END                                                             
219600         CALL FELLOG                                                      
219700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
219800         CONTINUE                                                         
219900     END-SEARCH                                                           
220000     .                                                                    
