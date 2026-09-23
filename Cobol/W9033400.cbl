000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9033400.                                                
000400 AUTHOR.         LARS THELL   CAP GEMINI LOGIC.                           
000500                 KOPIA AV W9030700. KDSTARAD+TIANNULL TILLAGD.            
000600 DATE-WRITTEN.   JUNI  90.                                                
000700*                                                                         
000800*REMARKS.                                                                 
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET HANTERAR BILDEN;                                      
001200*        FRÅGA PÅ RESTORDER TPO REGISTER VIA VDI-                         
001300*        SYSTEMET                                                         
001400*        PROGRAMMET LÄSER POSTER PÅ WDA5 BEROENDE PÅ                      
001500*        NYCKLAR I MOD'EN. ENDAST POSTER MED STATUS                       
001600*        1, 2 OCH 3 ÄR INTRESSANTA, ETY STATUS 4                          
001700*        BETYDER BIPACKAD.                                                
001800*                                                                         
001900*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
002000*        AV ETA-DATUM/NDC-LAGER.                                          
002100*       (ESTIMATED TIME AVAILABLE)                                        
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W90334T                                             
002500*        MID:         W9I33401                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W9O33401                                            
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP3                                                                
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700     SKIP3                                                                
003800 77  IDPGM                       PIC X(8)    VALUE 'W9033400'.            
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
004300 77  SW-ARTC11-LAEST             PIC X       VALUE 'N'.                   
004400                                                                          
004500 01  WS-IDORDER-X.                                                        
004600   05  WS-IDORDER1-5             PIC  9(5).                               
004700   05  FILLER                    PIC  X(2).                               
004800                                                                          
004900 77  WS-IDORDER                  PIC  9(7).                               
005000 77  MAX-RAD                     PIC S9(7)   VALUE +13 COMP-3.            
005100 77  MAX-MOD-LANGD               PIC S9(7)   VALUE +758 COMP-3.           
005200                                                                          
005300 77  W-TIAAMMDD                  PIC 9(6).                                
005400                                                                          
005500     EJECT                                                                
005600*      --- VALID IDDC CODES                                               
005700*                                                                         
005800*01    -COPY WWDC99                                                       
005900*    --- ARBETSAREA FÖR BESTÄMNING AV LAGER                               
006000*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
006100 01  WS-ETA-DATUM                PIC 9(6).                                
006200 01  FILLER REDEFINES WS-ETA-DATUM.                                       
006300     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
006400     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
006500     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
006600     EJECT                                                                
006700 01  W-TIAAVVD                   PIC 9(5).                                
006800 01  FILLER REDEFINES W-TIAAVVD.                                          
006900     03  W-TIAA                  PIC 9(2).                                
007000     03  W-TIVV                  PIC 9(2).                                
007100     03  W-TID                   PIC 9(1).                                
007200                                                                          
007300 01  DYNAMISKA-SUBPGM.                                                    
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007700     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
007800     EJECT                                                                
007900* ----- PARAMETRAR TILL SUBPROGRAM WDATKONV                               
008000                                                                          
008100*01  FILLER -COPY WDATAREA                                                
008200     EJECT                                                                
008300 01 FILLER                       PIC  X(8)   VALUE 'LETA'.                
008400*   -COPY W218LETA -PRE ETA-.                                             
008500     EJECT                                                                
008600* ----- INDEXFÄLT                                                         
008700 77  IX-RAD                      PIC S9(9)   VALUE +0  COMP SYNC.         
008800                                                                          
008900* ----- SWITCHAR                                                          
009000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009100     88  NYCKLAR-OK                          VALUE 'J'.                   
009200                                                                          
009300 01  WS-IDARTNR.                                                          
009400     05  WS-IDARTNR-8            PIC X(8).                                
009500     05  WS-REKSIFFR             PIC 9.                                   
009600                                                                          
009700     EJECT                                                                
009800 01    NYCKLAR-TILL-DLI.                                                  
009900     SKIP2                                                                
010000   03    W-WDA5DSEQ-X.                                                    
010100     05    W-A5DSEQ-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
010200     05    W-A5DSEQ-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
010300     SKIP2                                                                
010400   03    W-IDKUNDRF-X            PIC X(10)   VALUE SPACE.                 
010500   03    W-IDORDNR5-FILLER                                                
010600                           REDEFINES W-IDKUNDRF-X.                        
010700     05     W-IDORDNR5           PIC 9(5).                                
010800     05     FILLER               PIC X(5).                                
010900                                                                          
011000   03    W-IDORDNR7-FILLER                                                
011100                           REDEFINES W-IDKUNDRF-X.                        
011200     05     W-IDORDNR7           PIC 9(7).                                
011300     05     FILLER               PIC X(3).                                
011400     SKIP2                                                                
011500   03    W-IDARTNR-X.                                                     
011600     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
011700     SKIP2                                                                
011800   03    W-KDTPOTYP-X.                                                    
011900     05    W-KDTPOTYP            PIC S9(1)   VALUE ZERO  COMP-3.          
012000     SKIP2                                                                
012100   03    W-IDLOPNR-X.                                                     
012200     05    W-IDLOPNR             PIC S9(3)   VALUE ZERO  COMP-3.          
012300     SKIP2                                                                
012400   03    W-KDSTARAD-X.                                                    
012500     05    W-KDSTARAD            PIC S9(1)   VALUE ZERO  COMP-3.          
012600     SKIP2                                                                
012700   03    W-IDKUNDRF-MIN-X        PIC X(10)   VALUE SPACE.                 
012800   03    W-IDORDNR5-MIN-FILLER                                            
012900                           REDEFINES W-IDKUNDRF-MIN-X.                    
013000     05     W-IDORDNR5-MIN       PIC 9(5).                                
013100     05     FILLER               PIC X(5).                                
013200                                                                          
013300   03    W-IDORDNR7-MIN-FILLER                                            
013400                           REDEFINES W-IDKUNDRF-MIN-X.                    
013500     05     W-IDORDNR7-MIN       PIC 9(7).                                
013600     05     FILLER               PIC X(3).                                
013700     SKIP2                                                                
013800   03    W-IDKUNDRF-MAX-X        PIC X(10).                               
013900   03    W-IDORDNR5-MAX-FILLER                                            
014000                           REDEFINES W-IDKUNDRF-MAX-X.                    
014100     05     W-IDORDNR5-MAX       PIC 9(5).                                
014200     05     FILLER               PIC X(5).                                
014300                                                                          
014400   03    W-IDORDNR7-MAX-FILLER                                            
014500                           REDEFINES W-IDKUNDRF-MAX-X.                    
014600     05     W-IDORDNR7-MAX       PIC 9(7).                                
014700     05     FILLER               PIC X(3).                                
014800     SKIP2                                                                
014900   03    W-IDARTNR-MIN-X.                                                 
015000     05    W-IDARTNR-MIN         PIC S9(9)   VALUE ZERO  COMP-3.          
015100     SKIP2                                                                
015200   03    W-IDARTNR-MAX-X.                                                 
015300     05    W-IDARTNR-MAX         PIC S9(9)   VALUE ZERO  COMP-3.          
015400     SKIP2                                                                
015500   03    W-KDTPOTYP-MIN-X.                                                
015600     05    W-KDTPOTYP-MIN        PIC S9(1)   VALUE ZERO  COMP-3.          
015700     SKIP2                                                                
015800   03    W-KDTPOTYP-MAX-X.                                                
015900     05    W-KDTPOTYP-MAX        PIC S9(1)   VALUE ZERO  COMP-3.          
016000     SKIP2                                                                
016100   03    W-IDLOPNR-MIN-X.                                                 
016200     05    W-IDLOPNR-MIN         PIC S9(3)   VALUE ZERO  COMP-3.          
016300     SKIP2                                                                
016400   03    W-KDSTARAD-MIN-X.                                                
016500     05    W-KDSTARAD-MIN        PIC S9(1)   VALUE ZERO  COMP-3.          
016600     SKIP2                                                                
016700   03    W-KDSTARAD-MAX-X.                                                
016800     05    W-KDSTARAD-MAX        PIC S9(1)   VALUE ZERO  COMP-3.          
016900     EJECT                                                                
017000******************************************************************        
017100*                                                                         
017200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
017300*                                                                         
017400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
017500     SKIP3                                                                
017600*01    MID -COPY W9I33401.                                                
017700     EJECT                                                                
017800*01    -COPY WMSGAREA                                                     
017900     EJECT                                                                
018000*  03    MOD -COPY W9O33401  -RED MSG-AREA.                               
018100     EJECT                                                                
018200*01    -COPY WMFSAREA                                                     
018300     EJECT                                                                
018400******************************************************************        
018500*                                                                         
018600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018700*                                                                         
018800 01    IMS-WS.                                                            
018900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
019000     SKIP3                                                                
019100*                        **** STATUS-KOD FRÅN IMS                         
019200   03    STATUS-WS               PIC XX.                                  
019300     88    SEGMENT-FINNS                     VALUE '  '.                  
019400     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
019500     88    SEGMENT-SLUT                      VALUE 'GB'.                  
019600     SKIP3                                                                
019700   03    GODK-STATUSKODER.                                                
019800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
019900     SKIP3                                                                
020000 01    SSA1                      PIC X(256).                              
020100 01    SSA2                      PIC X(64).                               
020200     EJECT                                                                
020300*                            IMS FUNKTIONSKODER                           
020400*01    -COPY W0003                                                        
020500     EJECT                                                                
020600******************************************************************        
020700*                                                                         
020800*        ARBETS-AREOR TILL IO-AREORNA                                     
020900*                                                                         
021000*    ---  DLI INPUT-OUTPUT AREA 1                                         
021100*    ---  DLI-IO-AREA                                                     
021200*                                                                         
021300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDP01'.            
021400 01  IO-AREA-ORDP01.                                                      
021500*  03    WLORDP01  -COPY WDA501                                           
021600     EJECT                                                                
021700                                                                          
021800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
021900 01  IO-AREA-ARTC11.                                                      
022000*  03    WLARTC11  -COPY WDK611                                           
022100     EJECT                                                                
022200 01  FILLER                 PIC X(12) VALUE 'DUMMY-ARTC'.                 
022300 01  ETA-ARTC-PCB           PIC X.                                        
022400 01  FILLER                 PIC X(12) VALUE 'DUMMY-LEVA'.                 
022500 01  ETA-LEVA-PCB           PIC X.                                        
022600 LINKAGE SECTION.                                                         
022700                                                                          
022800*01    -COPY W0009     -PRE MSG-                                          
022900     EJECT                                                                
023000*01    -COPY W0008     -PRE ORDP-                                         
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01    -COPY W0008     -PRE ARTC-                                         
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600 01  ETA-WDK7-PCB           PIC X.                                        
023700 01  ETA-INLC-PCB           PIC X.                                        
023800 01  ETA-WDB6-PCB           PIC X.                                        
023900 01  ETA-WDD9-PCB           PIC X.                                        
024000     EJECT                                                                
024100 PROCEDURE DIVISION  USING MSG-PCB ORDP-PCB ARTC-PCB                      
024200                                   ETA-WDK7-PCB ETA-INLC-PCB              
024300                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
024400     ENTRY 'DLITCBL' USING MSG-PCB ORDP-PCB ARTC-PCB                      
024500                                   ETA-WDK7-PCB ETA-INLC-PCB              
024600                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
024700                                                                          
024800 STYR SECTION.                                                            
024900                                                                          
025000     PERFORM IMS-GET-MSG                                                  
025100     IF SEGMENT-FINNS                                                     
025200         PERFORM A-INIT                                                   
025300         PERFORM B-KONTROLLERA-NYCKLAR                                    
025400         IF NYCKLAR-OK                                                    
025500             PERFORM C-BEHANDLA-RADER                                     
025600         END-IF                                                           
025700     END-IF                                                               
025800     PERFORM E-KONTROLLERA-OM-TOM-SIDA                                    
025900     PERFORM F-BERAKNA-MAX-MOD-LANGD                                      
026000     MOVE MAX-MOD-LANGD        TO MSG-KVLL                                
026100     PERFORM IMS-INSERT-MSG                                               
026200     MOVE ZERO                 TO RETURN-CODE                             
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
026700 A-INIT SECTION.                                                          
026800                                                                          
026900     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I33401                    
027000     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
027100     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
027200     MOVE ' '                          TO MFS-KDTRTYP                     
027300     MOVE LOW-VALUE                    TO MSG-AREA                        
027400     MOVE 'W9O33401'                   TO MFS-IDMOD                       
027500     MOVE '9334'                       TO MOD-IDTRANS                     
027600     MOVE ZERO                         TO MOD-IDMFSFEL                    
027700     MOVE ZERO                         TO MOD-IDARTNR-NEXT                
027800                                          MOD-IDORDNR7-NEXT               
027900                                          MOD-IDLOPNR-NEXT                
028000                                                                          
028100     MOVE +1                   TO  IX-RAD                                 
028200     PERFORM UNTIL             IX-RAD > MAX-RAD                           
028300         MOVE ZERO             TO  MOD-IDARTNR-RAD  (IX-RAD)              
028400                                   MOD-KVART        (IX-RAD)              
028500                                   MOD-IDORDNR7-RAD (IX-RAD)              
028600                                   MOD-KDORDKL      (IX-RAD)              
028700                                   MOD-KDTPOTYP-RAD (IX-RAD)              
028800                                   MOD-TIRODAT      (IX-RAD)              
028900                                   MOD-TITPO        (IX-RAD)              
029000                                   MOD-TIANNULL     (IX-RAD)              
029100         MOVE SPACE            TO  MOD-BEVOLREF     (IX-RAD)              
029200                                   MOD-KDSTARAD-RAD (IX-RAD)              
029300                                   MOD-IDDC         (IX-RAD)              
029400         ADD +1                TO  IX-RAD                                 
029500     END-PERFORM                                                          
029600     .                                                                    
029700     EJECT                                                                
029800 B-KONTROLLERA-NYCKLAR SECTION.                                           
029900                                                                          
030000     IF MID-IDDISTR            NUMERIC AND                                
030100        MID-IDKUNDNR           NUMERIC                                    
030200         CONTINUE                                                         
030300      ELSE                                                                
030400         MOVE NEJ              TO NYCKLAR-SW                              
030500         MOVE 'B01'            TO MOD-IDMFSFEL                            
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 C-BEHANDLA-RADER SECTION.                                                
031000                                                                          
031100     IF MID-IDORDNR7-NEXT      >  ZERO                                    
031200         PERFORM CA-SKAPA-BLADDRINGS-NYCKEL                               
031300         PERFORM IMS-GET-WLORDP01-KVAL                                    
031400         PERFORM S01-SKAPA-OKVAL-NYCKEL                                   
031500      ELSE                                                                
031600         PERFORM S01-SKAPA-OKVAL-NYCKEL                                   
031700         PERFORM IMS-GET-WLORDP01-OKVAL                                   
031800     END-IF                                                               
031900     MOVE +1                   TO IX-RAD                                  
032000                                                                          
032100     PERFORM UNTIL SEGMENT-SAKNAS             OR                          
032200                   SEGMENT-SLUT               OR                          
032300                   IX-RAD      > MAX-RAD                                  
032400                                                                          
032500         PERFORM CB-REDIGERA-RESTORDERRAD                                 
032600         ADD +1                TO  IX-RAD                                 
032700                                                                          
032800         PERFORM IMS-GET-WLORDP01-OKVAL                                   
032900                                                                          
033000         IF SEGMENT-FINNS                  AND                            
033100            IX-RAD             >  MAX-RAD                                 
033200             PERFORM CC-SPARA-NYCKLAR                                     
033300         END-IF                                                           
033400     END-PERFORM                                                          
033500                                                                          
033600     .                                                                    
033700     EJECT                                                                
033800 CA-SKAPA-BLADDRINGS-NYCKEL SECTION.                                      
033900                                                                          
034000     MOVE MID-IDDISTR            TO  W-A5DSEQ-IDDISTR                     
034100     MOVE MID-IDKUNDNR           TO  W-A5DSEQ-IDKUNDNR                    
034200     MOVE SPACE                  TO  W-IDKUNDRF-X                         
034300     MOVE MID-IDORDNR7-NEXT(3:5) TO  W-IDORDNR5                           
034400     MOVE MID-IDARTNR-NEXT       TO  W-IDARTNR                            
034500     MOVE MID-IDLOPNR-NEXT       TO  W-IDLOPNR                            
034600     .                                                                    
034700     EJECT                                                                
034800 CB-REDIGERA-RESTORDERRAD SECTION.                                        
034900                                                                          
035000     MOVE RAD-IDARTNR          TO  MOD-IDARTNR-RAD(IX-RAD)                
035100     MOVE RAD-KVART            TO  MOD-KVART(IX-RAD)                      
035200     IF RAD-IDORDNR7           NUMERIC                                    
035300         MOVE RAD-IDORDNR7     TO  WS-IDORDER                             
035400      ELSE                                                                
035500         MOVE RAD-IDORDNR5     TO  WS-IDORDER                             
035600     END-IF                                                               
035700     MOVE WS-IDORDER           TO  MOD-IDORDNR7-RAD (IX-RAD)              
035800     MOVE RAD-BEVOLREF         TO  MOD-BEVOLREF     (IX-RAD)              
035900     MOVE RAD-IDDC             TO  MOD-IDDC         (IX-RAD)              
036000     MOVE RAD-KDORDKL          TO  MOD-KDORDKL      (IX-RAD)              
036100     MOVE RAD-KDTPOTYP         TO  MOD-KDTPOTYP-RAD (IX-RAD)              
036200     MOVE RAD-KDSTARAD         TO  MOD-KDSTARAD-RAD (IX-RAD)              
036300                                                                          
036400     IF RAD-KDTPOTYP >   +0                                               
036500       MOVE RAD-TITPO             TO  W-TIAAMMDD                          
036600       MOVE W-TIAAMMDD            TO  MOD-TITPO(IX-RAD)                   
036700     ELSE                                                                 
036800       MOVE RAD-IDARTNR           TO  W-IDARTNR                           
036900       PERFORM IMS-GU-ARTC11                                              
037000       MOVE JA                    TO  SW-ARTC11-LAEST                     
037100       MOVE RAD-IDDC              TO  WS-IDDC                             
037300       IF NDC                                                             
037400         PERFORM CBA-HAEMTA-TIBERANK                                      
037500                                                                          
037600         IF ETA-SVAR-OK = JA                                              
037800           IF NDC-NA OR NDC-CN                                            
037900             MOVE ETA-TIAAMMDD-SVAR TO W-TIAAMMDD                         
038000           ELSE                                                           
038100             IF ETA-KVAVIS-ETA > +0                                       
038200               MOVE ETA-TIAAMMDD-SVAR TO W-TIAAMMDD                       
038300             ELSE                                                         
038400               MOVE ZERO          TO  W-TIAAMMDD                          
038500             END-IF                                                       
038600           END-IF                                                         
038700         ELSE                                                             
038800           MOVE ZERO              TO  W-TIAAMMDD                          
038900         END-IF                                                           
039000       ELSE                                                               
039100         MOVE CLAG-TIDISPIN       TO  W-TIAAMMDD                          
039200       END-IF                                                             
039300       MOVE W-TIAAMMDD            TO  MOD-TIRODAT(IX-RAD)                 
039400     END-IF                                                               
039500     IF RAD-KDSTARAD = '1' AND                                            
039600        RAD-FLTPOBEK = JA                                                 
039700       IF SW-ARTC11-LAEST = NEJ                                           
039800         MOVE RAD-IDARTNR      TO  W-IDARTNR                              
039900         PERFORM IMS-GU-ARTC11                                            
040000       END-IF                                                             
040100       IF SEGMENT-FINNS                                                   
040200         PERFORM CBB-TIDIGAST-ANNULL-DATUM                                
040300       END-IF                                                             
040400     END-IF                                                               
040500     MOVE NEJ                  TO  SW-ARTC11-LAEST                        
040600     .                                                                    
040700     EJECT                                                                
040800 CBA-HAEMTA-TIBERANK SECTION.                                             
040900                                                                          
041000     MOVE '612'                TO ETA-KDCALL                              
041100     MOVE RAD-IDDC             TO ETA-IDDC-REC                            
041200     MOVE RAD-IDARTNR          TO ETA-IDARTNR                             
041300     MOVE SPACE                TO ETA-IDLEVNR                             
041400     MOVE ZERO                 TO ETA-KDFRAKT                             
041500     MOVE RAD-DARODAT (3:6)    TO ETA-TIAAMMDD-ANROP                      
041600                                  WS-ETA-DATUM                            
041700     MOVE RAD-DARODAT (1:2)    TO ETA-TISEKEL-ANROP                       
041800                                                                          
041900     CALL W218ETA  USING ETA-W218LETA                                     
042000                         ETA-ARTC-PCB ETA-WDK7-PCB                        
042100                         ETA-INLC-PCB ETA-LEVA-PCB                        
042200                         ETA-WDB6-PCB ETA-WDD9-PCB                        
042300     .                                                                    
042400                                                                          
042500     EJECT                                                                
042600 CBB-TIDIGAST-ANNULL-DATUM SECTION.                                       
042700                                                                          
042800     MOVE RAD-TITPO    TO DAT-I-TIDATUM                                   
042900     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
043000     CALL WDATKONV USING DAT-KDDATFORM                                    
043100                         DAT-I-TIDATUM                                    
043200                         DAT-O-TIDATUM                                    
043300                         DAT-KDSVAR                                       
043400                                                                          
043500     IF DAT-KDSVAR-OK                                                     
043600        MOVE DAT-TIAAVVD TO W-TIAAVVD                                     
043700        IF W-TIVV < CLAG-KVFRYSTI                                         
043800           IF W-TIAA = ZERO                                               
043900             MOVE 99 TO W-TIAA                                            
044000           ELSE                                                           
044100             COMPUTE W-TIAA      = W-TIAA        - 1                      
044200           END-IF                                                         
044300           COMPUTE CLAG-KVFRYSTI = CLAG-KVFRYSTI - W-TIVV                 
044400           COMPUTE W-TIVV        = 52            - CLAG-KVFRYSTI          
044500        ELSE                                                              
044600           IF W-TIVV = CLAG-KVFRYSTI                                      
044700              IF W-TIAA = ZERO                                            
044800                MOVE 99 TO W-TIAA                                         
044900              ELSE                                                        
045000                COMPUTE W-TIAA = W-TIAA - 1                               
045100              END-IF                                                      
045200              MOVE    52 TO W-TIVV                                        
045300           ELSE                                                           
045400              COMPUTE W-TIVV = W-TIVV - CLAG-KVFRYSTI                     
045500           END-IF                                                         
045600        END-IF                                                            
045700        MOVE W-TIAAVVD    TO DAT-I-TIDATUM                                
045800        MOVE 'AAVVD'      TO DAT-KDDATFORM                                
045900        CALL WDATKONV USING DAT-KDDATFORM                                 
046000                            DAT-I-TIDATUM                                 
046100                            DAT-O-TIDATUM                                 
046200                            DAT-KDSVAR                                    
046300        IF DAT-KDSVAR-OK                                                  
046400           MOVE DAT-TIAAMMDD    TO MOD-TIANNULL(IX-RAD)                   
046500        ELSE                                                              
046600           CALL FELLOG                                                    
046700           MOVE 'FEL AAVVD PÅ WDA5 I CBB-SECTION' TO FELTEXT              
046800        END-IF                                                            
046900     ELSE                                                                 
047000        CALL FELLOG                                                       
047100        MOVE 'FEL TITPO PÅ WDA5 I CBB-SECTION' TO FELTEXT                 
047200     END-IF                                                               
047300     .                                                                    
047400     EJECT                                                                
047500 CC-SPARA-NYCKLAR SECTION.                                                
047600                                                                          
047700     MOVE RAD-IDORDNR5         TO  MOD-IDORDNR7-NEXT                      
047800     MOVE RAD-IDARTNR          TO  MOD-IDARTNR-NEXT                       
047900     MOVE RAD-IDLOPNR          TO  MOD-IDLOPNR-NEXT                       
048000     .                                                                    
048100     EJECT                                                                
048200 E-KONTROLLERA-OM-TOM-SIDA SECTION.                                       
048300                                                                          
048400     IF IX-RAD                 = 1                                        
048500         MOVE 'B10'            TO MOD-IDMFSFEL                            
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 F-BERAKNA-MAX-MOD-LANGD SECTION.                                         
049000                                                                          
049100     MOVE 13                   TO  IX-RAD                                 
049200     PERFORM UNTIL IX-RAD      = 0                                        
049300         IF MOD-IDORDNR7-RAD(IX-RAD) = ZERO                               
049400             SUBTRACT +56      FROM MAX-MOD-LANGD                         
049500             SUBTRACT +1       FROM IX-RAD                                
049600          ELSE                                                            
049700             MOVE ZERO         TO IX-RAD                                  
049800         END-IF                                                           
049900     END-PERFORM                                                          
050000     .                                                                    
050100     EJECT                                                                
050200 S01-SKAPA-OKVAL-NYCKEL SECTION.                                          
050300                                                                          
050400     MOVE LOW-VALUE            TO  W-IDKUNDRF-MIN-X                       
050500     MOVE MID-IDDISTR          TO  W-A5DSEQ-IDDISTR                       
050600     MOVE MID-IDKUNDNR         TO  W-A5DSEQ-IDKUNDNR                      
050700                                                                          
050800     MOVE HIGH-VALUE           TO  W-IDKUNDRF-MAX-X                       
050900                                                                          
051000     IF MID-IDORDNR7(3:5)      >   ZERO                                   
051100         MOVE SPACE            TO  W-IDKUNDRF-MIN-X                       
051200         MOVE SPACE            TO  W-IDKUNDRF-MAX-X                       
051300         MOVE MID-IDORDNR7(3:5) TO W-IDORDNR5-MIN                         
051400                                   W-IDORDNR5-MAX                         
051500     END-IF                                                               
051600                                                                          
051700     IF MID-IDARTNR            >   ZERO                                   
051800         MOVE MID-IDARTNR      TO  W-IDARTNR-MIN                          
051900                               IN  W-IDARTNR-MIN-X                        
052000         MOVE MID-IDARTNR      TO  W-IDARTNR-MAX                          
052100                               IN  W-IDARTNR-MAX-X                        
052200      ELSE                                                                
052300         MOVE LOW-VALUE        TO  W-IDARTNR-MIN-X                        
052400         MOVE HIGH-VALUE       TO  W-IDARTNR-MAX-X                        
052500     END-IF                                                               
052600                                                                          
052700     IF  MID-KDTPOTYP  NUMERIC                                            
052800     AND MID-KDTPOTYP  >  ZERO                                            
052900         MOVE MID-KDTPOTYP     TO  W-KDTPOTYP-MIN                         
053000                               IN  W-KDTPOTYP-MIN-X                       
053100         MOVE MID-KDTPOTYP     TO  W-KDTPOTYP-MAX                         
053200                               IN  W-KDTPOTYP-MAX-X                       
053300     ELSE                                                                 
053400         MOVE LOW-VALUE        TO  W-KDTPOTYP-MIN-X                       
053500         MOVE HIGH-VALUE       TO  W-KDTPOTYP-MAX-X                       
053600     END-IF                                                               
053700                                                                          
053800     IF MID-KDSTARAD = '1' OR '2' OR '3'                                  
053900        MOVE MID-KDSTARAD      TO  W-KDSTARAD-MIN-X                       
054000                                   W-KDSTARAD-MAX-X                       
054100     ELSE                                                                 
054200        MOVE LOW-VALUE         TO  W-KDSTARAD-MIN-X                       
054300        MOVE +3                TO  W-KDSTARAD-MAX-X                       
054400     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
054700* IMS SEKTIONER                                                           
054800     SKIP3                                                                
054900 IMS-GET-MSG SECTION.                                                     
055000                                                                          
055100     MOVE '  QC' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
055300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     SKIP3                                                                
055700 IMS-INSERT-MSG SECTION.                                                  
055800                                                                          
055900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
056000     MOVE SPACE TO GODK-STATUSKODER                                       
056100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
056200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056300     PERFORM IMS-STATUSKONTROLL                                           
056400     .                                                                    
056500     EJECT                                                                
056600 IMS-GET-WLORDP01-KVAL SECTION.                                           
056700                                                                          
056800     STRING 'WLORDP01(WDA5DSEQ =' W-WDA5DSEQ-X                            
056900                    '&IDARTNR  =' W-IDARTNR-X                             
057000                    '&IDLOPNR  =' W-IDLOPNR-X                             
057100                    '&IDKUNDRF =' W-IDKUNDRF-X ')'                        
057200            DELIMITED BY SIZE INTO SSA1                                   
057300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
057400     CALL CBLTDLI USING GU ORDP-PCB IO-AREA-ORDP01 SSA1                   
057500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
057600     PERFORM IMS-STATUSKONTROLL                                           
057700     .                                                                    
057800     EJECT                                                                
057900 IMS-GET-WLORDP01-OKVAL SECTION.                                          
058000                                                                          
058100     STRING 'WLORDP01(WDA5DSEQ =' W-WDA5DSEQ-X                            
058200                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
058300                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
058400                    '&IDKUNDRF>=' W-IDKUNDRF-MIN-X                        
058500                    '&IDKUNDRF<=' W-IDKUNDRF-MAX-X                        
058600                    '&KDTPOTYP>=' W-KDTPOTYP-MIN-X                        
058700                    '&KDTPOTYP<=' W-KDTPOTYP-MAX-X                        
058800                    '&KDSTARAD>=' W-KDSTARAD-MIN-X                        
058900                    '&KDSTARAD<=' W-KDSTARAD-MAX-X ')'                    
059000            DELIMITED BY SIZE INTO SSA1                                   
059100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
059200     CALL CBLTDLI USING GN ORDP-PCB IO-AREA-ORDP01 SSA1                   
059300     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     .                                                                    
059600     EJECT                                                                
059700 IMS-GU-ARTC11 SECTION.                                                   
059800                                                                          
059900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
060000            DELIMITED BY SIZE INTO SSA1                                   
060100     MOVE   'WLARTC11'          TO SSA2                                   
060200     MOVE '  GE' TO GODK-STATUSKODER                                      
060300     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-ARTC11 SSA1 SSA2              
060400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
060500     PERFORM IMS-STATUSKONTROLL                                           
060600     .                                                                    
060700     EJECT                                                                
060800 IMS-STATUSKONTROLL SECTION.                                              
060900                                                                          
061000     SET STATUS-IX TO 1                                                   
061100     SEARCH GODK-STATUS                                                   
061200       AT END                                                             
061300         CALL FELLOG                                                      
061400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
061500         CONTINUE                                                         
061600     END-SEARCH                                                           
061700     .                                                                    
