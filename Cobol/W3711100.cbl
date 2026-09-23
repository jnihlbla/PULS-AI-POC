000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3711100.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   NOVEMBER 1999.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700**   FUNKTION:                                                            
000800*    . LÄGGER UPP ORDERTRANSAKTIONER FÖR BYTESSALDON SOM EV. SKALL        
000900*      TILL VIDARE BEHANDLING I PGM W40251/W40252 OCH SLUTLIGEN           
001000*      FAKTURERAS.                                                        
001100*    . ETT ORDERHUVUD + EN ORDERRAD SKAPAS FÖR VARJE                      
001200*      KONSOLIDERANDE DISTRIKT/KONTO                                      
001300*    . SKAPAR FIL MED KONSOLIDERANDE DISTRIKT SOM FÅR FAKTURA             
001400*                                                                         
001500*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
001600*                            WLKOMA-(WDP8)                                
001700*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
001800*                            WL4579-(WDR4)                                
001900*                                                                         
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*          --- ORDERTRANSAKTIONER TILL FAKTURERING                        
002800     SELECT W37111                     ASSIGN TO W37111D1.                
002900                                                                          
003000*          --- KONSOLIDERADE DISTRIKT SOM FÅR FAKTURA                     
003100     SELECT W37133                     ASSIGN TO W37111D2.                
003200     EJECT                                                                
003300                                                                          
003400 DATA DIVISION.                                                           
003500                                                                          
003600 FILE SECTION.                                                            
003700                                                                          
003800 FD  W37111                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W37111       -L.                                               
004300     EJECT                                                                
004400                                                                          
004500 FD  W37133                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  SIGNAL-REC -COPY W37133       -L.                                    
005000     EJECT                                                                
005100                                                                          
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400*    -- CHECKED BY WY2000                                                 
005500 77  IDPGM                       PIC X(8)    VALUE 'W3711100'.            
005600 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
005700                                                                          
005800 01  CHKP-VAR.                                                            
005900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006400     03 CHKP-MAX                 PIC S9(3)   VALUE +3   COMP-3.           
006500                                                                          
006600 77  POST-ANT                    PIC S9(3)   VALUE +0   COMP-3.           
006700 77  RAD-IX                      PIC S9(4)   VALUE +0  COMP SYNC.         
006800 77  MAX-RAD-IX                  PIC S9(4)   VALUE +1  COMP SYNC.         
006900 77  SPAR-IDDISTR-BET            PIC 9(5)    VALUE ZERO.                  
007000 77  SPAR-KDEXCHA                PIC 9(3)    VALUE ZERO.                  
007100 77  WS-IDDISTR-DISP             PIC 9(4).                                
007200 77  WS-IDKUNDNR-DISP            PIC 9(6).                                
007300 77  WS-IDORDNR-DISP             PIC 9(7).                                
007400 77  WS-IDARTNR-DISP             PIC 9(9).                                
007500 77  WS-PRARTNTO-DISP            PIC Z(6)9.99.                            
007600 77  WS-KVBEART-DISP             PIC 9(6).                                
007700 77  WS-SUM-KVANTAL              PIC S9(9)   VALUE ZERO COMP-3.           
007800 77  JA                          PIC X       VALUE 'J'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
008100 77  W37111-EOF-SW               PIC X       VALUE 'N'.                   
008200     88  END-OF-W37111                       VALUE 'J'.                   
008300 77  HUVUD-SW                    PIC X       VALUE 'J'.                   
008400     88  HUVUD                               VALUE 'J'.                   
008500     88  RAD                                 VALUE 'N'.                   
008600     EJECT                                                                
008700                                                                          
008800 01  FELTEXT.                                                             
008900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009100*                                                                         
009200 01  W-DATUM                     PIC 9(6)    VALUE ZERO.                  
009300 01  W-TIKLOCK                   PIC 9(8)    VALUE ZERO.                  
009400     EJECT                                                                
009500                                                                          
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010300     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
010400     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
010500     EJECT                                                                
010600                                                                          
010700*    --- PARAMETRAR TILL POSTSUM                                          
010800*                                                                         
010900*01  -COPY W0005   -PRE  POSTSUM-                                         
011000     EJECT                                                                
011100                                                                          
011200*    --- PARAMETRAR TILL KSIF                                             
011300*                                                                         
011400 01  KONTROLL-SIFFRA.                                                     
011500     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
011600     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
011700     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
011800     EJECT                                                                
011900                                                                          
012000 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
012100*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
012200*01 -COPY W411ORDN                                                        
012300     EJECT                                                                
012400                                                                          
012500 01  IN-AREA-START               PIC X(24)   VALUE                        
012600                                             'IN-AREA-START'.             
012700                                                                          
012800*01  AREA -COPY W37111      -PRE IN-                                      
012900*                                                                         
013000     EJECT                                                                
013100                                                                          
013200 01  SPAR-AREA-START             PIC X(24)   VALUE                        
013300                                             'SPAR-AREA-START'.           
013400                                                                          
013500*01  AREA -COPY W37111      -PRE INSPAR-                                  
013600*                                                                         
013700     EJECT                                                                
013800                                                                          
013900 01  UT-AREA-START               PIC X(24)   VALUE                        
014000                                             'UT-AREA-START'.             
014100                                                                          
014200*01  AREA -COPY W37133      -PRE UT-                                      
014300*                                                                         
014400     EJECT                                                                
014500                                                                          
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700                                                                          
014800 01  FILLER          PIC X(16)   VALUE 'NYCKLAR TILL DLI'.                
014900                                                                          
015000*01  -COPY WDGX01                                                         
015100     EJECT                                                                
015200                                                                          
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FINNS                       VALUE '  '.                  
015600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015900     88  IMS-EJ-OK                           VALUE 'XD'.                  
016000                                                                          
016100 01  GODK-STATUSKODER.                                                    
016200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016300                                                                          
016400 01  SSA1                        PIC X(64).                               
016500 01  SSA2                        PIC X(64).                               
016600     EJECT                                                                
016700                                                                          
016800*    --- IMS FUNKTIONSKODER                                               
016900*01  -COPY W0003                                                          
017000     EJECT                                                                
017100                                                                          
017200*    ---  DLI INPUT-OUTPUT AREA                                           
017300                                                                          
017400 01  FILLER                      PIC X(16) VALUE '4580-IO-AREA'.          
017500 01  4580-IO-AREA.                                                        
017600*    03  -COPY WDGX4580                                                   
017700     EJECT                                                                
017800                                                                          
017900 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
018000*01  -COPY WMSGKOM                                                        
018100     EJECT                                                                
018200                                                                          
018300 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
018400                                                                          
018500*01  -COPY WMSGAREA                                                       
018600     EJECT                                                                
018700*                                                                         
018800*    --- AREOR FÖR W006KOM SUBMODUL                                       
018900*                                                                         
019000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
019100 01  KOM-IO-AREA.                                                         
019200   03  KOM-AREA                  PIC X(2500) VALUE SPACE.                 
019300*03  FILLER  -COPY W4I25101 -PRE HUV-  -RED KOM-AREA.                     
019400     EJECT                                                                
019500                                                                          
019600*03  FILLER  -COPY W4I25201 -PRE RAD-  -RED KOM-AREA.                     
019700     EJECT                                                                
019800                                                                          
019900 LINKAGE SECTION.                                                         
020000*01  -COPY W0009   -PRE MSG-                                              
020100                                                                          
020200 01  DISP-PCB                    PIC X.                                   
020300 01  KOMA-PCB                    PIC X.                                   
020400                                                                          
020500*01  -COPY W0008  -PRE 4579-                                              
020600     05  FILLER                  PIC X.                                   
020700                                                                          
020800 01  ORDN-XXKP-PCB               PIC X.                                   
020900 01  ORDN-ORQL-PCB               PIC X.                                   
021000 01  ORDN-PROC-PCB               PIC X.                                   
021100 01  ORDN-ORQI-PCB               PIC X.                                   
021200     EJECT                                                                
021300                                                                          
021400 PROCEDURE DIVISION  USING MSG-PCB                                        
021500                           DISP-PCB                                       
021600                           KOMA-PCB                                       
021700                           4579-PCB                                       
021800                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
021900                           ORDN-PROC-PCB ORDN-ORQI-PCB.                   
022000 MAIN SECTION.                                                            
022100     ENTRY 'DLITCBL' USING MSG-PCB                                        
022200                           DISP-PCB                                       
022300                           KOMA-PCB                                       
022400                           4579-PCB                                       
022500                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
022600                           ORDN-PROC-PCB ORDN-ORQI-PCB.                   
022700                                                                          
022800     PERFORM A-INITIERA                                                   
022900                                                                          
023000     PERFORM IMS-LAS-ATERSTART                                            
023100                                                                          
023200     IF SEGMENT-SAKNAS                                                    
023300       MOVE SPACE                   TO 4580-WDGX4580-CTX                  
023400       MOVE '1'                     TO 4580-KDSEGKEY                      
023500       MOVE +0                      TO 4580-KVPOST                        
023600       MOVE W-DATUM                 TO 4580-TIUPPDAT                      
023700       MOVE W-TIKLOCK               TO 4580-TIUPPTID                      
023800       PERFORM IMS-ISRT-ATERSTART                                         
023900       PERFORM IMS-LAS-ATERSTART                                          
024000     END-IF                                                               
024100                                                                          
024200     IF 4580-KVPOST > +0                                                  
024300       PERFORM B-LAES-FRAM-TILL-CHKPOINT                                  
024400     ELSE                                                                 
024500       PERFORM S01-LAS-W37111                                             
024600     END-IF                                                               
024700     IF NOT END-OF-W37111                                                 
024800       MOVE IN-IDDISTR-BET          TO SPAR-IDDISTR-BET                   
024900       MOVE IN-KDEXCHA              TO SPAR-KDEXCHA                       
024910       MOVE IN-AREA                 TO INSPAR-AREA                        
025000     END-IF                                                               
025100                                                                          
025200     PERFORM UNTIL END-OF-W37111                                          
025300       IF IN-IDDISTR-BET = SPAR-IDDISTR-BET AND                           
025400          IN-KDEXCHA     = SPAR-KDEXCHA                                   
025500         COMPUTE WS-SUM-KVANTAL = WS-SUM-KVANTAL +                        
025600                                  IN-KVBEART                              
025700         END-COMPUTE                                                      
025800       ELSE                                                               
025900         IF WS-SUM-KVANTAL < ZERO                                         
026000           PERFORM S10-SKAPA-MSG-KOM-AREA                                 
026100           PERFORM C-BEARBETA                                             
026200           PERFORM S11-AVSLUTA-TRANS                                      
026300           PERFORM X-TAG-CHECKPOINT                                       
026400           MOVE SPAR-IDDISTR-BET     TO UT-IDDISTR-BET                    
026500           MOVE SPAR-KDEXCHA         TO UT-KDEXCHA                        
026600                                                                          
026700           PERFORM S02-SKRIV-W37133                                       
026800                                                                          
026900           MOVE ZERO                 TO WS-SUM-KVANTAL                    
027000           COMPUTE WS-SUM-KVANTAL = WS-SUM-KVANTAL +                      
027100                                     IN-KVBEART                           
027200           END-COMPUTE                                                    
027300           MOVE IN-IDDISTR-BET       TO SPAR-IDDISTR-BET                  
027400           MOVE IN-KDEXCHA           TO SPAR-KDEXCHA                      
027500           MOVE IN-AREA              TO INSPAR-AREA                       
027600         ELSE                                                             
027700           MOVE ZERO                 TO WS-SUM-KVANTAL                    
027800           COMPUTE WS-SUM-KVANTAL = WS-SUM-KVANTAL +                      
027900                                     IN-KVBEART                           
028000           END-COMPUTE                                                    
028100           MOVE IN-IDDISTR-BET       TO SPAR-IDDISTR-BET                  
028200           MOVE IN-KDEXCHA           TO SPAR-KDEXCHA                      
028300           MOVE IN-AREA              TO INSPAR-AREA                       
028400         END-IF                                                           
028500       END-IF                                                             
028600       PERFORM S01-LAS-W37111                                             
028700     END-PERFORM                                                          
028800                                                                          
028900     IF WS-SUM-KVANTAL < ZERO                                             
029000       PERFORM S10-SKAPA-MSG-KOM-AREA                                     
029100       PERFORM C-BEARBETA                                                 
029200       PERFORM S11-AVSLUTA-TRANS                                          
029300       PERFORM X-TAG-CHECKPOINT                                           
029400     END-IF                                                               
029500                                                                          
029600     PERFORM Z-FINIT                                                      
029700     MOVE ZERO                       TO RETURN-CODE                       
029800     GOBACK                                                               
029900     .                                                                    
030000     EJECT                                                                
030100                                                                          
030200 A-INITIERA SECTION.                                                      
030300     PERFORM IMS-RESTART                                                  
030400                                                                          
030500     OPEN INPUT  W37111                                                   
030600     OPEN OUTPUT W37133                                                   
030700                                                                          
030800     MOVE +0                   TO POST-ANT                                
030900                                  CHKP-ANT                                
031000                                  RAD-IX                                  
031100                                                                          
031200     MOVE SPACE                TO MSG-AREA                                
031300                                                                          
031400     ACCEPT W-DATUM            FROM DATE                                  
031500     ACCEPT W-TIKLOCK          FROM TIME                                  
031600                                                                          
031700     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
031800     .                                                                    
031900     EJECT                                                                
032000                                                                          
032100 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
032200     PERFORM S01-LAS-W37111                                               
032300                                                                          
032400     PERFORM UNTIL END-OF-W37111  OR                                      
032500                   POST-ANT = 4580-KVPOST                                 
032600       PERFORM S01-LAS-W37111                                             
032700     END-PERFORM                                                          
032800                                                                          
032900     IF END-OF-W37111                                                     
033000       MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                            
033100                             TO FELTEXT                                   
033200       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
033300     ELSE                                                                 
033400       IF IN-IDDISTR-BET NOT = SPAR-IDDISTR-BET OR                        
033500          IN-KDEXCHA     NOT = SPAR-KDEXCHA                               
033600         ADD +1              TO POST-ANT                                  
033700         MOVE IN-IDDISTR-BET TO SPAR-IDDISTR-BET                          
033800         MOVE IN-KDEXCHA     TO SPAR-KDEXCHA                              
033900         MOVE IN-AREA        TO INSPAR-AREA                               
034000       END-IF                                                             
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400                                                                          
034500 C-BEARBETA SECTION.                                                      
034600     MOVE SPACE                  TO KOM-AREA                              
034700                                                                          
034800     COMPUTE MSG-KVLL = LENGTH OF HUV-MID-W4I25101 + 17                   
034900                                                                          
035000     MOVE LOW-VALUE              TO MSG-KDZ1                              
035100     MOVE LOW-VALUE              TO MSG-KDZ2                              
035200     MOVE 'W4T251X '             TO MSG-KDTRANS-1                         
035300     MOVE '4251'                 TO MSG-IDTRANS-1                         
035400     MOVE '1'                    TO MSG-KDMFSFOR-1                        
035500                                                                          
035600     MOVE INSPAR-IDSYSTEM        TO HUV-MID-IDSYSTEM                      
035700     MOVE INSPAR-IDDISTR-BET     TO WS-IDDISTR-DISP                       
035800     MOVE WS-IDDISTR-DISP        TO HUV-MID-IDDISTR                       
035900     MOVE INSPAR-IDKUNDNR        TO WS-IDKUNDNR-DISP                      
036000     MOVE WS-IDKUNDNR-DISP       TO HUV-MID-IDKUNDNR                      
036100                                                                          
036200     PERFORM S03-HAMTA-ORDERNR                                            
036300     MOVE WS-IDORDNR-DISP        TO HUV-MID-IDORDNR                       
036400                                                                          
036500     MOVE '3'                    TO HUV-MID-KDORDKL                       
036600     MOVE WS-CDC-11              TO HUV-MID-IDDC                          
036700     MOVE SPACE                  TO HUV-MID-KDFRAKT                       
036800     MOVE SPACE                  TO HUV-MID-TIRFS                         
036900                                    HUV-MID-BEKUNDRF                      
037000     MOVE 'R'                    TO HUV-MID-KDFAKTYP                      
037100     MOVE NEJ                    TO HUV-MID-FLRESTN                       
037200     MOVE SPACE                  TO HUV-MID-KDTPOTYP                      
037300                                    HUV-MID-TITPO                         
037400                                    HUV-MID-BELAGINS                      
037500                                    HUV-MID-BEGMT                         
037600                                    HUV-MID-ADGMT-GATA                    
037700                                    HUV-MID-ADGMT-PADR                    
037800                                    HUV-MID-BEBET                         
037900                                    HUV-MID-ADBET                         
038000     MOVE SPACE                  TO HUV-MID-KDROPACK                      
038100                                    HUV-MID-IDKONTO                       
038200                                    HUV-MID-IDANALYS                      
038300                                    HUV-MID-IDKST                         
038400                                    HUV-MID-IDSKYLT                       
038500                                    HUV-MID-BEVARREF                      
038600                                    HUV-MID-KDTULLVE                      
038700                                    HUV-MID-KDNOTES                       
038710                                    HUV-MID-IDBILREG                      
038720                                    HUV-MID-IDVIN                         
038730                                    HUV-MID-IDCISNR                       
038800     MOVE JA                     TO HUV-MID-FLAUTFAK                      
038900     MOVE JA                     TO HUV-MID-FLAUTPAC                      
039000     MOVE NEJ                    TO HUV-MID-FLEMBORD                      
039100     MOVE NEJ                    TO HUV-MID-FLOVRLEV                      
039200     MOVE SPACE                  TO HUV-MID-IDKAMPRF                      
039300                                    HUV-MID-IDFTG                         
039400     MOVE NEJ                    TO HUV-MID-FLLSBOK                       
039500     MOVE NEJ                    TO HUV-MID-FLFORBI                       
039600                                    HUV-MID-FLORDTIL                      
039610     MOVE ZERO                   TO HUV-MID-IDGROSS                       
039630                                    HUV-MID-IDDEPT                        
039700                                                                          
039800     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
039900     CALL W006KOM USING MSG-PCB                                           
040000                        DISP-PCB                                          
040100                        KOMA-PCB                                          
040200                        MSG-KOM-WMSGKOM                                   
040300                        MSG-IO-AREA                                       
040400     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
040500*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
040600*       DUBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                  
040700        MOVE ' FELAKTIG DATUM,TID PÅ INPUT - PGM W37111 '                 
040800                                 TO FELTEXT                               
040900        DISPLAY ' FELAKTIG DATUM,TID INPUT - PGM W37111 '                 
041000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
041100     END-IF                                                               
041200                                                                          
041300     MOVE SPACE                  TO KOM-AREA                              
041400                                                                          
041500     COMPUTE MSG-KVLL = LENGTH OF RAD-MID-W4I25201 + 17                   
041600                                                                          
041700     MOVE LOW-VALUE              TO MSG-KDZ1                              
041800     MOVE LOW-VALUE              TO MSG-KDZ2                              
041900     MOVE 'W4T252X '             TO MSG-KDTRANS-1                         
042000     MOVE '4252'                 TO MSG-IDTRANS-1                         
042100     MOVE '1'                    TO MSG-KDMFSFOR-1                        
042200     MOVE +1                     TO RAD-IX                                
042300                                                                          
042400     MOVE INSPAR-IDSYSTEM        TO RAD-MID-IDSYSTEM                      
042500     MOVE INSPAR-IDDISTR-BET     TO WS-IDDISTR-DISP                       
042600     MOVE WS-IDDISTR-DISP        TO RAD-MID-IDDISTR                       
042700     MOVE INSPAR-IDKUNDNR        TO WS-IDKUNDNR-DISP                      
042800     MOVE WS-IDKUNDNR-DISP       TO RAD-MID-IDKUNDNR                      
042900     MOVE WS-IDORDNR-DISP        TO RAD-MID-IDORDNR                       
043000                                                                          
043100     MOVE SPACE                  TO RAD-MID-BEVOLREF                      
043110     MOVE SPACE                  TO RAD-MID-IDKUNDRF-RO                   
043200     MOVE 'J'                    TO RAD-MID-FLSLUT                        
043300     MOVE INSPAR-IDARTNR         TO WS-IDARTNR-DISP                       
043400                                    REK-IDARTNR                           
043500     MOVE 9                      TO REK-LNGD                              
043600     MOVE 0                      TO REK-REKSIFFR                          
043700                                                                          
043800     CALL W009KSIF            USING REK-IDARTNR                           
043900                                    REK-LNGD                              
044000                                    REK-REKSIFFR                          
044100                                                                          
044200     MOVE WS-IDARTNR-DISP        TO RAD-MID-IDARTNR   (RAD-IX)            
044300     MOVE REK-REKSIFFR           TO RAD-MID-REKSIFFR  (RAD-IX)            
044400     COMPUTE WS-SUM-KVANTAL = WS-SUM-KVANTAL * -1                         
044500     END-COMPUTE                                                          
044600     MOVE WS-SUM-KVANTAL         TO WS-KVBEART-DISP                       
044700     MOVE WS-KVBEART-DISP        TO RAD-MID-KVBEART   (RAD-IX)            
044800     MOVE INSPAR-PRARTNTO        TO WS-PRARTNTO-DISP                      
044900     MOVE WS-PRARTNTO-DISP       TO RAD-MID-PRARTNTO  (RAD-IX)            
045000     MOVE SPACE                  TO RAD-MID-TITPO     (RAD-IX)            
045100     MOVE NEJ                    TO RAD-MID-FLRESTN   (RAD-IX)            
045200     MOVE SPACE                  TO RAD-MID-KDKVBRYT  (RAD-IX)            
045300     MOVE NEJ                    TO RAD-MID-FLINVEST  (RAD-IX)            
045400     MOVE SPACE                  TO RAD-MID-KDVRINFO  (RAD-IX)            
045500                                    RAD-MID-IDKONTO   (RAD-IX)            
045600                                    RAD-MID-IDKST     (RAD-IX)            
045700     MOVE SPACE                  TO RAD-MID-BERADREF  (RAD-IX)            
045800                                    RAD-MID-KDDSP     (RAD-IX)            
045900     MOVE NEJ                    TO RAD-MID-FLSLATT   (RAD-IX)            
046000     MOVE NEJ                    TO RAD-MID-FLDIRLEV  (RAD-IX)            
046100     MOVE SPACE                  TO RAD-MID-IDBIL     (RAD-IX)            
046200     MOVE SPACE               TO RAD-MID-PRARTNTO-LOC (RAD-IX)            
046300     MOVE SPACE               TO RAD-MID-PRARTBTO-LOC (RAD-IX)            
046400     MOVE SPACE               TO     RAD-MID-KDVALISO (RAD-IX)            
046500     MOVE SPACE               TO     RAD-MID-KDVAT    (RAD-IX)            
046600     MOVE 0                   TO     RAD-MID-RERAB    (RAD-IX)            
046700     MOVE SPACE               TO     RAD-MID-KDRAB    (RAD-IX)            
046800     MOVE SPACE               TO RAD-MID-BEART-VIPS   (RAD-IX)            
046900     MOVE ZERO                TO RAD-MID-ADLAGOMR-CD  (RAD-IX)            
047000                                 RAD-MID-ADGANG-CD    (RAD-IX)            
047100                                 RAD-MID-ADPLATS-CD   (RAD-IX)            
047200     .                                                                    
047300     EJECT                                                                
047400                                                                          
047500 S01-LAS-W37111 SECTION.                                                  
047600     READ W37111 INTO IN-AREA                                             
047700       AT END                                                             
047800          MOVE JA          TO W37111-EOF-SW                               
047900     END-READ                                                             
048000                                                                          
048100     IF NOT END-OF-W37111                                                 
048200       MOVE 'W37111'       TO POSTSUM-FDNAMN                              
048300       MOVE 'W37111D1'     TO POSTSUM-DDNAMN2                             
048400       MOVE IN-IDSYSTEM    TO POSTSUM-TRANSTYP                            
048500       CALL POSTSUM USING POSTSUM-PARM                                    
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900                                                                          
049000 S02-SKRIV-W37133 SECTION.                                                
049100     WRITE SIGNAL-REC           FROM UT-AREA                              
049200                                                                          
049300     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
049400     MOVE 'W37133'              TO   POSTSUM-FDNAMN                       
049500     MOVE 'W37111D2'            TO   POSTSUM-DDNAMN2                      
049600     CALL POSTSUM USING POSTSUM-PARM                                      
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 S03-HAMTA-ORDERNR SECTION.                                               
050100     MOVE 'W371'         TO ORDN-IDSYSTEM                                 
050200     MOVE IN-IDDISTR-BET TO ORDN-IDDISTR                                  
050300     MOVE IN-IDKUNDNR    TO ORDN-IDKUNDNR                                 
050400     MOVE ZERO           TO ORDN-IDORDNR-IN                               
050500                                                                          
050600     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
050700                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
050800                                                                          
050900     MOVE ORDN-IDORDNR-UT TO WS-IDORDNR-DISP                              
051000     .                                                                    
051100     EJECT                                                                
051200                                                                          
051300 S10-SKAPA-MSG-KOM-AREA SECTION.                                          
051400     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
051500                                                                          
051600     MOVE +54                    TO MSG-KOM-KVLL                          
051700     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
051800     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
051900     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
052000     IF HUVUD                                                             
052100       MOVE 'W4I25101'           TO MSG-KOM-IDCPYTXT                      
052200     ELSE                                                                 
052300       MOVE 'W4I25201'           TO MSG-KOM-IDCPYTXT                      
052400     END-IF                                                               
052500     MOVE 'W371'                 TO MSG-KOM-IDSNDNOD                      
052600     MOVE 'W3711100'             TO MSG-KOM-IDSNDJOB                      
052700     MOVE W-DATUM                TO MSG-KOM-TIREGDAT                      
052800     ADD +1                      TO W-TIKLOCK                             
052900     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
053000     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
053100     .                                                                    
053200     EJECT                                                                
053300                                                                          
053400 S11-AVSLUTA-TRANS SECTION.                                               
053500     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
053600                                                                          
053700     CALL W006KOM USING MSG-PCB                                           
053800                        DISP-PCB                                          
053900                        KOMA-PCB                                          
054000                        MSG-KOM-WMSGKOM                                   
054100                        MSG-IO-AREA                                       
054200     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
054300*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
054400*       DUBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                  
054500       MOVE ' FELAKTIG DATUM,TID - INPUT W37111 '                         
054600                      TO FELTEXT                                          
054700        DISPLAY ' FELAKTIG DATUM,TID - INPUT W37111 '                     
054800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
054900     END-IF                                                               
055000                                                                          
055100     MOVE ZERO        TO RAD-IX                                           
055200     MOVE SPACE       TO KOM-AREA                                         
055300     .                                                                    
055400     EJECT                                                                
055500                                                                          
055600 X-TAG-CHECKPOINT SECTION.                                                
055700*    UPPDATERA ÅTERSTARTREGISTRET                                         
055800     PERFORM IMS-LAS-ATERSTART                                            
055900     ADD +1          TO 4580-KVPOST                                       
056000     ACCEPT 4580-TIUPPDAT FROM DATE                                       
056100     ACCEPT 4580-TIUPPTID FROM TIME                                       
056200                                                                          
056300     PERFORM IMS-REPL-ATERSTART                                           
056400                                                                          
056500*    TAG CHECKPOINT                                                       
056600     PERFORM IMS-CHECKPOINT                                               
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000 Z-FINIT    SECTION.                                                      
057100     CLOSE  W37111                                                        
057200            W37133                                                        
057300                                                                          
057400*    NOLLA ÅTERSTARTINFORMATIONEN                                         
057500     PERFORM IMS-LAS-ATERSTART                                            
057600     MOVE +0       TO 4580-KVPOST                                         
057700     ACCEPT 4580-TIUPPDAT FROM DATE                                       
057800     ACCEPT 4580-TIUPPTID FROM TIME                                       
057900                                                                          
058000     PERFORM IMS-REPL-ATERSTART                                           
058100                                                                          
058200     MOVE 'S'      TO POSTSUM-OPKOD                                       
058300     CALL POSTSUM USING POSTSUM-PARM                                      
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700* IMS SECTIONER                                                           
058800                                                                          
058900 IMS-RESTART SECTION.                                                     
059000     MOVE SPACE TO MSG-IO-AREA                                            
059100     MOVE '  ' TO GODK-STATUSKODER                                        
059200     CALL CBLTDLI USING XRST MSG-PCB                                      
059300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
059400                        CHKP-AREA-LENGTH CHKP-AREA                        
059500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059600     PERFORM IMS-STATUSKONTROLL                                           
059700     .                                                                    
059800                                                                          
059900 IMS-CHECKPOINT SECTION.                                                  
060000     MOVE SPACE        TO MSG-IO-AREA                                     
060100     MOVE '  XD'       TO GODK-STATUSKODER                                
060200     CALL CBLTDLI USING CHKP MSG-PCB                                      
060300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
060400                        CHKP-AREA-LENGTH CHKP-AREA                        
060500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     IF IMS-EJ-OK                                                         
060800       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
060900       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
061000                            TO FELTEXT                                    
061100       CALL FELLOG                                                        
061200     END-IF                                                               
061300     .                                                                    
061400                                                                          
061500 IMS-LAS-ATERSTART SECTION.                                               
061600     MOVE '4579'         TO IDHTYP                                        
061700     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
061800     MOVE 'W3711100'     TO NYCKEL-VALFRI(1:8)                            
061900     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
062000                    DELIMITED BY SIZE INTO SSA1                           
062100     MOVE 'WDR470   '    TO SSA2                                          
062200     MOVE '  GE'           TO GODK-STATUSKODER                            
062300     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
062400     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     .                                                                    
062700                                                                          
062800 IMS-ISRT-ATERSTART SECTION.                                              
062900     MOVE '4579'         TO IDHTYP                                        
063000     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
063100     MOVE 'W3711100'     TO NYCKEL-VALFRI(1:8)                            
063200     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
063300                    DELIMITED BY SIZE INTO SSA1                           
063400     MOVE 'WDR470   '    TO SSA2                                          
063500     MOVE '  '           TO GODK-STATUSKODER                              
063600     CALL CBLTDLI USING ISRT 4579-PCB 4580-IO-AREA SSA1 SSA2              
063700     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
063800     PERFORM IMS-STATUSKONTROLL                                           
063900     .                                                                    
064000                                                                          
064100 IMS-REPL-ATERSTART SECTION.                                              
064200     MOVE '  '             TO GODK-STATUSKODER                            
064300     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
064400     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 IMS-STATUSKONTROLL SECTION.                                              
065000     SET STATUS-IX TO 1                                                   
065100     SEARCH GODK-STATUS                                                   
065200       AT END                                                             
065300         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
065400                            TO FELTEXT                                    
065500         CALL FELLOG                                                      
065600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
065700         CONTINUE                                                         
065800     END-SEARCH                                                           
065900     .                                                                    
