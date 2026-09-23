000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411OHLK.                                                
000500 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000600 DATE-WRITTEN.   MARS -90.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET HANTERAR ALLA LOGISKA KONTROLLER SOM BEHÖVS           
001200*        FÖR TERMERNA PÅ ORDERHUVUDET.                                    
001300*        OM NÅGOT ELLER NÅGRA FEL UPPTÄCKS FELMÄRKS DE AKTUELLA           
001400*        TERMERNA MED XXXXXXXX-OK = NEJ.                                  
001500*                                                                         
001600*        PROGRAMMET ÄR GEMENSAMT SUBPROGRAM.                              
001700*        PROGRAMMET ANROPAR W411ORDN FÖR ATT TA FRAM EV                   
001800*                   AUTOMATISKT ORDERNUMMER (IDORDNR).                    
001900*        PROGRAMMET LÄSER           WDM2   KAMPANJREGISTRET               
002000*        PROGRAMMET LÄSER   WLGMTA (WDB2)  GODSMOTTAGAR REGISTER          
002100*        PROGRAMMET LÄSER   WDB6           DC-REGISTER                    
002200*                                                                         
002300*                                                                         
002400*    LÄNKAREA: W411OHLK                                                   
002500*                                                                         
002600* 9/7 '13 - ETRACKER 10196973, EJ P&H FÖR ADMINISTRATIVA ART.             
002700* 29NOV2021-STORY 2501985: ADDED 5400 TO WWDIST11.RECOMPILING PGM         
002800* 01DEC2021 - STORY 2375089 ADD IDSYSTEM VOUI, ECOM                       
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500*    -COPY WY2000W1                                                       
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(8)    VALUE 'W411OHLK'.            
003800 77  YES                         PIC X(1)    VALUE 'Y'.                   
003900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004000 77  WS-INDEX                    PIC S9(9)   VALUE +0 COMP SYNC.          
004100 77  WS-IDCOSTCTR                PIC  9(5).                               
004200 77  W-IDFTG-B6                  PIC  9(2)   VALUE ZERO.                  
004300                                                                          
004400 77  WS-ENGANGS-KUND             PIC 9(6)    VALUE 999999.                
004500                                                                          
004600 01  GENERELLA-SUBPROGRAM.                                                
004700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004900                                                                          
005000 01  GEMENSAMMA-SUBPROGRAM.                                               
005100     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
005200     03  W411SAP                 PIC X(8)    VALUE 'W411SAP '.            
005300                                                                          
005400*01  FILLER -COPY W411ORDN                                                
005500     EJECT                                                                
005600*01  FILLER -COPY W411SAP                                                 
005700     EJECT                                                                
005800*01  FILLER -COPY W402W001                                                
005900     EJECT                                                                
006000*01 -COPY WWIDFTG                                                         
006100     EJECT                                                                
006200                                                                          
006300 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
006400 01  FILLER REDEFINES TEST-IDDISTR.                                       
006500*    03   -COPY WWDIST11.                                                 
006600     EJECT                                                                
006700 01  FILLER REDEFINES TEST-IDDISTR.                                       
006800*    03   -COPY WWDIST18.                                                 
006900     EJECT                                                                
007000 01  FILLER REDEFINES TEST-IDDISTR.                                       
007100*    03   -COPY WWDIST19.                                                 
007200     EJECT                                                                
007300 01  FILLER REDEFINES TEST-IDDISTR.                                       
007400*    03   -COPY WWDIST47.                                                 
007500     EJECT                                                                
007600 01  FILLER REDEFINES TEST-IDDISTR.                                       
007700*    03   -COPY WWDIS125.                                                 
007800     EJECT                                                                
007900 01  FILLER REDEFINES TEST-IDDISTR.                                       
008000*    03   -COPY WWDIST34.                                                 
008100     EJECT                                                                
008200 01  FILLER REDEFINES TEST-IDDISTR.                                       
008300*    03   -COPY WWDIST35.                                                 
008400     EJECT                                                                
008500 01  FILLER REDEFINES TEST-IDDISTR.                                       
008600*    03   -COPY WWDIS134.                                                 
008700     EJECT                                                                
008800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009700     SKIP2                                                                
009800 01  GODK-STATUSKODER.                                                    
009900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010200 01  SSA2                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800                                                                          
010900     03  W-WDM201-X.                                                      
011000         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
011100         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
011200                                                                          
011300     03  W-IDGMT-X.                                                       
011400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
011500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
011600                                                                          
011700     03  W-IDDC-B6-X.                                                     
011800         05 W-IDDC-B6                  PIC X(2).                          
011900     EJECT                                                                
012000                                                                          
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
012300 01  DLI-IO-WDM201.                                                       
012400*    03 -COPY WDM201                                                      
012500     EJECT                                                                
012600                                                                          
012700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDB201'.        
012800 01  DLI-IO-WDB201.                                                       
012900*    03 WDB201   -COPY WDB201                                             
013000                                                                          
013100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013200 01   DLI-IO-AREA-B601.                                                   
013300*     03  -COPY WDB601                                                    
013400     EJECT                                                                
013500                                                                          
013600 LINKAGE SECTION.                                                         
013700*                                                                         
013800*   -COPY W411OHLK                                                        
013900*                                                                         
014000     EJECT                                                                
014100*01  -COPY W0008      -PRE WDM2-                                          
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400 01  XXKP-PCB                    PIC X.                                   
014500     EJECT                                                                
014600*01  -COPY W0008      -PRE GMTA-                                          
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900 01  SAPC-PCB                    PIC X.                                   
015000     EJECT                                                                
015100*01  -COPY W0008      -PRE WDB6-                                          
015200     05  FILLER                  PIC X.                                   
015300     EJECT                                                                
015400 PROCEDURE DIVISION  USING OHLK-W411OHLK WDM2-PCB XXKP-PCB                
015500                                         GMTA-PCB                         
015600                                         SAPC-PCB                         
015700                                         WDB6-PCB.                        
015800                                                                          
015900     ACCEPT DAGENS-DATUM FROM DATE                                        
016000                                                                          
016100     MOVE OHLK-IDDISTR TO TEST-IDDISTR                                    
016200     MOVE OHLK-IDDC    TO W-IDDC-B6                                       
016300     PERFORM IMS-GU-WDB601                                                
016400                                                                          
016500     PERFORM A-INITIERA-OK                                                
016600                                                                          
016700     PERFORM B-KOLLA-ORDERNUMMER                                          
016800     PERFORM C-KOLLA-ORDKL                                                
016900                                                                          
017000     IF DCS-NDC-NA                                                        
017100     OR (DCS-LAND-NON-VCC-OWNED AND DIST18-SCRAP-NDC)                     
017200     OR (OHLK-IDSYSTEM(1:3) = 'APD')                                      
017300                                                                          
017400       CONTINUE                                                           
017500     ELSE                                                                 
017600       IF (OHLK-KDFAKTYP = 'G'       AND                                  
017700           NOT DIST35-NL-SITTARD-OBJEKT AND                               
017800           NOT DIS134-BYTESREN-CN)      OR                                
017900          (OHLK-KDFAKTYP = 'N'       AND                                  
018000           NOT DIST18-SKROT          AND                                  
018100           NOT DIST19-SATS)                                               
018200         IF OHLK-IDKONTO  = ZERO                                          
018300           MOVE NEJ TO OHLK-IDKONTO-OK                                    
018400         ELSE                                                             
018500           PERFORM E-KOLLA-KTO-ANALYS-KST                                 
018600         END-IF                                                           
018700       ELSE                                                               
018800         IF OHLK-IDKONTO NOT = ZERO                                       
018900           IF OHLK-KDFAKTYP = 'N'    AND                                  
019000              DIST18-SKROT                                                
019100             PERFORM E-KOLLA-KTO-ANALYS-KST                               
019200           ELSE                                                           
019300             MOVE NEJ TO OHLK-IDKONTO-OK                                  
019400           END-IF                                                         
019500         END-IF                                                           
019600         IF OHLK-IDANALYS > SPACE                                         
019700           IF OHLK-KDFAKTYP = 'N'    AND                                  
019800              DIST18-SKROT                                                
019900             NEXT SENTENCE                                                
020000           ELSE                                                           
020100             MOVE NEJ TO OHLK-IDANALYS-OK                                 
020200           END-IF                                                         
020300         END-IF                                                           
020400         IF OHLK-IDKST NOT = SPACE                                        
020500           IF OHLK-IDKST-OK = 'J'                                         
020600             CONTINUE                                                     
020700           ELSE                                                           
020800             MOVE NEJ TO OHLK-IDKST-OK                                    
020900           END-IF                                                         
021000         END-IF                                                           
021100       END-IF                                                             
021200     END-IF                                                               
021300                                                                          
021400     PERFORM F-KOLLA-KAMPANJ                                              
021500     PERFORM G-KOLLA-FAKTURATYPER                                         
021600     PERFORM K-KOLLA-KUNDNR-FAKTURATYP                                    
021700     PERFORM M-KOLLA-KDTPOTYP-KDFAKTYP                                    
021800     PERFORM N-KOLLA-KDTPOTYP-KDORDKL                                     
021900     PERFORM O-KOLLA-KDTPOTYP-TITPO                                       
022000     PERFORM P-KOLLA-IDDC-TVS                                             
022100     PERFORM Q-KOLLA-KDTPOTYP-IDDC                                        
022200                                                                          
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INITIERA-OK SECTION.                                                   
022700                                                                          
022800     MOVE JA                          TO OHLK-IDANALYS-OK                 
022900                                         OHLK-IDDISTR-OK                  
023000                                         OHLK-IDORDNR-OK                  
023100                                         OHLK-KDORDKL-OK                  
023200                                         OHLK-IDFTG-OK                    
023300                                         OHLK-IDKONTO-OK                  
023400                                         OHLK-IDKST-OK                    
023500                                         OHLK-IDKAMPRF-OK                 
023600                                         OHLK-KDFAKTYP-OK                 
023700                                         OHLK-KDTPOTYP-OK                 
023800                                         OHLK-TITPO-OK                    
023900                                         OHLK-IDDC-TVS-OK                 
024000     .                                                                    
024100     EJECT                                                                
024200 B-KOLLA-ORDERNUMMER SECTION.                                             
024300                                                                          
024400     IF OHLK-IDSYSTEM NOT = 'OREL' AND 'REFB' AND 'REFT'                  
024500                                   AND 'W216'                             
024600                                   AND 'W371' AND 'W37A'                  
024700                                   AND 'LDCB' AND 'LDCD'                  
024800                                   AND 'LYNB' AND 'LYND'                  
024900                                   AND 'DROP' AND 'POLD'                  
025000                                   AND 'SPX ' AND 'XCEL'                  
025100                                   AND 'ECOB' AND 'ECOD'                  
025200                                   AND 'VOUB' AND 'VOUD'                  
025300                                   AND 'TADB' AND 'TADD'                  
025400                                   AND 'ACCB' AND 'ACCD'                  
025500                                   AND 'APAB' AND 'APAD'                  
025600                                   AND 'APBB' AND 'APBD'                  
025700                                   AND 'APCB' AND 'APCD'                  
025800                                   AND 'APDB' AND 'APDD'                  
025900                                   AND 'APEB' AND 'APED'                  
026000                                   AND 'APFB' AND 'APFD'                  
026100                                   AND 'APGB' AND 'APGD'                  
026200                                   AND 'APHB' AND 'APHD'                  
026300                                   AND 'APIB' AND 'APID'                  
026400                                   AND 'APJB' AND 'APJD'                  
026500        IF OHLK-IDORDNR > ZERO                                            
026600           MOVE 'OHLK'             TO ORDN-IDSYSTEM                       
026700           MOVE OHLK-IDDISTR       TO ORDN-IDDISTR                        
026800           MOVE OHLK-IDKUNDNR      TO ORDN-IDKUNDNR                       
026900           MOVE OHLK-IDORDNR       TO ORDN-IDORDNR-IN                     
027000                                                                          
027100           CALL W411ORDN USING ORDN-W411ORDN XXKP-PCB                     
027200                                                                          
027300           IF ORDN-IDORDNR-IN NOT = ORDN-IDORDNR-UT                       
027400              MOVE NEJ             TO OHLK-IDORDNR-OK                     
027500           END-IF                                                         
027600        ELSE                                                              
027700           IF OHLK-FLAUTORD = NEJ OR                                      
027800              (OHLK-IDSYSTEM = 'VR  ' OR 'VDI ' OR 'OVR ' OR              
027900                               'VIPS')                                    
028000              MOVE NEJ                TO OHLK-IDORDNR-OK                  
028100           END-IF                                                         
028200        END-IF                                                            
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 C-KOLLA-ORDKL SECTION.                                                   
028700                                                                          
028800     IF OHLK-KDORDKL = +0                                                 
028900        IF (DIST47-INTERNA AND NOT DIS125-VOR-DISTRIKT)  OR               
029000          (OHLK-SEC-KDSVAR = EJVOR AND OHLK-IDSYSTEM = 'IMS ')            
029100           MOVE NEJ            TO OHLK-KDORDKL-OK                         
029200        END-IF                                                            
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 E-KOLLA-KTO-ANALYS-KST SECTION.                                          
029700                                                                          
029800     MOVE OHLK-IDFTG TO W-IDFTG-B6                                        
029900     MOVE OHLK-IDFTG TO WS-IDFTG                                          
030000*                                                                         
030100*    IF IDFTG-PV                                                          
030200*       MOVE 'SEPV'            TO SAP-KDTRADP                             
030300*    ELSE                                                                 
030400*       IF IDFTG-CN                                                       
030500*          MOVE 'CN05'         TO SAP-KDTRADP                             
030600*       ELSE                                                              
030700*         IF IDFTG-IN                                                     
030800*            MOVE 'IN07'         TO SAP-KDTRADP                           
030900*         ELSE                                                            
031000*            MOVE 'SEPV'         TO SAP-KDTRADP                           
031100*         END-IF                                                          
031200*       END-IF                                                            
031300*    END-IF                                                               
031400     IF IDFTG-NON-VCC                                                     
031500       PERFORM IMS-GU-WDB601-FTG                                          
031600       IF SEGMENT-FINNS                                                   
031700         MOVE DCS-KDTRADP      TO SAP-KDTRADP                             
031800       END-IF                                                             
031900     ELSE                                                                 
032000       MOVE 'SEPV'             TO SAP-KDTRADP                             
032100     END-IF                                                               
032200     MOVE OHLK-IDDISTR         TO SAP-IDDISTR                             
032300     MOVE OHLK-KDFAKTYP        TO SAP-KDFAKTYP                            
032400     MOVE OHLK-IDFTG           TO SAP-IDFTG                               
032500     MOVE OHLK-IDKONTO         TO SAP-IDKONTO                             
032600     MOVE OHLK-IDANALYS        TO SAP-IDANALYS                            
032700     MOVE OHLK-IDKST           TO SAP-IDKST                               
032800     MOVE SPACE                TO SAP-IDPROFIT                            
032900     MOVE +1                   TO SAP-KDCALL                              
033000                                                                          
033100     CALL W411SAP  USING SAP-W411SAP SAPC-PCB                             
033200                                                                          
033300     MOVE SAP-IDKONTO          TO OHLK-IDKONTO                            
033400     MOVE SAP-IDANALYS         TO OHLK-IDANALYS                           
033500     MOVE SAP-IDKST            TO OHLK-IDKST                              
033600     MOVE SAP-IDFTG            TO OHLK-IDFTG                              
033700     MOVE SAP-IDKONTO-OK       TO OHLK-IDKONTO-OK                         
033800     MOVE SAP-IDANALYS-OK      TO OHLK-IDANALYS-OK                        
033900     MOVE SAP-IDKST-OK         TO OHLK-IDKST-OK                           
034000     MOVE SAP-IDFTG-OK         TO OHLK-IDFTG-OK                           
034100     .                                                                    
034200     EJECT                                                                
034300 F-KOLLA-KAMPANJ SECTION.                                                 
034400                                                                          
034500**** FIX KAMPREF FRÅN VDI OFTA FEL                                        
034600*    MOVE +0 TO OHLK-IDKAMPRF                                             
034700     IF OHLK-IDKAMPRF > +0 OR OHLK-KDTPOTYP = +4                          
034800        IF OHLK-KDTPOTYP = +0  OR                                         
034900                (OHLK-KDTPOTYP = +4 AND OHLK-IDKAMPRF > +0)               
035000           MOVE OHLK-IDKAMPRF  TO W-KAMP-IDKAMPRF                         
035100           MOVE OHLK-IDDC      TO W-KAMP-IDDC                             
035200           PERFORM IMS-GU-WDM201                                          
035300           IF SEGMENT-FINNS                                               
035400             IF KAMP-TISTADAT > +0                                        
035500               MOVE OHLK-TITPO        TO TMP1-YYMMDD                      
035600               MOVE KAMP-TISTADAT     TO TMP2-YYMMDD                      
035700               MOVE KAMP-TISTODAT     TO TMP3-YYMMDD                      
035800               MOVE DAGENS-DATUM      TO TMP4-YYMMDD                      
035900               PERFORM WY2000Q1                                           
036000               IF OHLK-KDTPOTYP = +0                                      
036100                 IF TMP4-YYMMDD < TMP2-YYMMDD  OR                         
036200                       (KAMP-TISTODAT > +0  AND                           
036300                        TMP4-YYMMDD > TMP3-YYMMDD)                        
036400                    MOVE NEJ    TO OHLK-IDKAMPRF-OK                       
036500                 END-IF                                                   
036600               ELSE                                                       
036700                 IF TMP1-YYMMDD < TMP2-YYMMDD OR                          
036800                       (KAMP-TISTODAT > +0  AND                           
036900                         TMP1-YYMMDD > TMP3-YYMMDD)                       
037000                    MOVE NEJ    TO OHLK-IDKAMPRF-OK                       
037100                                   OHLK-TITPO-OK                          
037200                 END-IF                                                   
037300               END-IF                                                     
037400             ELSE                                                         
037500               MOVE NEJ        TO OHLK-IDKAMPRF-OK                        
037600             END-IF                                                       
037700           ELSE                                                           
037800              MOVE NEJ         TO OHLK-IDKAMPRF-OK                        
037900           END-IF                                                         
038000        ELSE                                                              
038100           MOVE NEJ            TO OHLK-IDKAMPRF-OK                        
038200                                  OHLK-KDTPOTYP-OK                        
038300        END-IF                                                            
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700 G-KOLLA-FAKTURATYPER SECTION.                                            
038800                                                                          
038900     IF OHLK-KDFAKTYP = 'R'                                               
039000        IF OHLK-IDSYSTEM NOT = 'PROF'                                     
039100           IF NOT (OHLK-FLOKFAK-R = JA OR YES)                            
039200              MOVE NEJ            TO OHLK-KDFAKTYP-OK                     
039300           END-IF                                                         
039400        END-IF                                                            
039500     ELSE                                                                 
039600        IF OHLK-KDFAKTYP = 'G'                                            
039700           IF NOT (OHLK-FLOKFAK-G = JA OR YES)                            
039800              MOVE NEJ         TO OHLK-KDFAKTYP-OK                        
039900           END-IF                                                         
040000        ELSE                                                              
040100           IF OHLK-KDFAKTYP = 'K'                                         
040200              IF NOT (OHLK-FLOKFAK-K = JA OR YES)                         
040300                 MOVE NEJ      TO OHLK-KDFAKTYP-OK                        
040400              END-IF                                                      
040500           ELSE                                                           
040600              IF OHLK-KDFAKTYP = 'N'                                      
040700                 IF NOT (OHLK-FLOKFAK-N = JA OR YES)                      
040800                    MOVE NEJ   TO OHLK-KDFAKTYP-OK                        
040900                 END-IF                                                   
041000              ELSE                                                        
041100                 MOVE NEJ      TO OHLK-KDFAKTYP-OK                        
041200              END-IF                                                      
041300           END-IF                                                         
041400        END-IF                                                            
041500     END-IF                                                               
041600     IF DCS-CDC                                                           
041700       IF OHLK-KDFAKTYP-OK = JA  AND                                      
041800            (OHLK-IDSYSTEM = 'IMS ' OR 'PROF' OR 'OREL')                  
041900         IF (OHLK-SEC-KDSVAR = IMPORTER OR DEALER OR EJVOR                
042000               OR VOR-DEALER) AND OHLK-KDFAKTYP NOT = 'R'                 
042100           MOVE NEJ            TO OHLK-KDFAKTYP-OK                        
042200         END-IF                                                           
042300       END-IF                                                             
042400     ELSE                                                                 
042500       IF OHLK-KDFAKTYP-OK = JA  AND                                      
042600            (OHLK-IDSYSTEM = 'IMS ' OR 'PROF' OR 'OREL')                  
042700         IF (OHLK-SEC-KDSVAR = IMPORTER OR DEALER OR EJVOR                
042800               OR VOR-DEALER) AND OHLK-KDFAKTYP = 'G'                     
042900           MOVE NEJ            TO OHLK-KDFAKTYP-OK                        
043000         END-IF                                                           
043100       END-IF                                                             
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 K-KOLLA-KUNDNR-FAKTURATYP SECTION.                                       
043600                                                                          
043700     IF OHLK-IDSYSTEM = 'PROF'                                            
043800        CONTINUE                                                          
043900     ELSE                                                                 
044000***FIX START PROFORMA RELEASE                                             
044100*       IF OHLK-IDDISTR = +2679 AND                                       
044200*          OHLK-IDORDNR = +96725                                          
044300*          CONTINUE                                                       
044400*       ELSE                                                              
044500        IF OHLK-IDKUNDNR = WS-ENGANGS-KUND                                
044600           IF (OHLK-KDFAKTYP NOT = 'G' AND 'N')                           
044700              MOVE NEJ TO OHLK-KDFAKTYP-OK                                
044800           END-IF                                                         
044900        END-IF                                                            
045000*       END-IF                                                            
045100***FIX STOP                                                               
045200     END-IF                                                               
045300     .                                                                    
045400     EJECT                                                                
045500 M-KOLLA-KDTPOTYP-KDFAKTYP SECTION.                                       
045600                                                                          
045700     IF OHLK-KDTPOTYP = +1                                                
045800        IF (OHLK-KDFAKTYP NOT = SPACE AND 'R' AND 'N' AND 'K')            
045900           MOVE NEJ TO OHLK-KDFAKTYP-OK                                   
046000        END-IF                                                            
046100     END-IF                                                               
046200     IF OHLK-KDTPOTYP = +3                                                
046300        IF (OHLK-KDFAKTYP NOT = SPACE AND 'R' AND 'N')                    
046400           MOVE NEJ TO OHLK-KDFAKTYP-OK                                   
046500        END-IF                                                            
046600     END-IF                                                               
046700                                                                          
046800     IF OHLK-KDTPOTYP = +2 OR +4                                          
046900        IF (OHLK-KDFAKTYP NOT = SPACE AND 'R' AND 'G'                     
047000                                      AND 'N' AND 'K')                    
047100           MOVE NEJ TO OHLK-KDFAKTYP-OK                                   
047200        END-IF                                                            
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 N-KOLLA-KDTPOTYP-KDORDKL SECTION.                                        
047700                                                                          
047800     IF OHLK-KDTPOTYP = +1                                                
047900        IF OHLK-KDORDKL NOT = +3 AND +4                                   
048000           MOVE NEJ TO OHLK-KDORDKL-OK                                    
048100        END-IF                                                            
048200     ELSE                                                                 
048300       IF OHLK-KDTPOTYP > +0                                              
048400          IF OHLK-KDORDKL NOT = +1 AND +2 AND +3 AND +4                   
048500             MOVE NEJ TO OHLK-KDORDKL-OK                                  
048600          END-IF                                                          
048700       END-IF                                                             
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 O-KOLLA-KDTPOTYP-TITPO SECTION.                                          
049200                                                                          
049300     IF OHLK-KDTPOTYP > +0                                                
049400        IF OHLK-KDTPOTYP = +1                                             
049500           IF OHLK-TITPO > +0                                             
049600              MOVE NEJ TO OHLK-TITPO-OK                                   
049700           END-IF                                                         
049800        ELSE                                                              
049900           IF (OHLK-KDTPOTYP = +2 AND OHLK-IDSYSTEM = 'VIPS') OR          
050000              (OHLK-KDTPOTYP = +2 AND OHLK-IDSYSTEM = 'OVR ') OR          
050100              (OHLK-KDTPOTYP = +2 AND OHLK-IDSYSTEM = 'DROP')             
050200              CONTINUE                                                    
050300           ELSE                                                           
050400              IF OHLK-KDTPOTYP = +3                                       
050500                 IF OHLK-IDSYSTEM NOT = 'OVR ' AND 'DROP'                 
050600                    MOVE NEJ TO OHLK-KDTPOTYP-OK                          
050700                 END-IF                                                   
050800              ELSE                                                        
050900                 MOVE OHLK-TITPO     TO TMP1-YYMMDD                       
051000                 MOVE DAGENS-DATUM   TO TMP2-YYMMDD                       
051100                 PERFORM WY2000Q1                                         
051200                 IF TMP1-YYMMDD <= TMP2-YYMMDD                            
051300                    MOVE NEJ TO OHLK-TITPO-OK                             
051400                 END-IF                                                   
051500              END-IF                                                      
051600           END-IF                                                         
051700        END-IF                                                            
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 P-KOLLA-IDDC-TVS         SECTION.                                        
052200                                                                          
052300     IF DIST34-NDC-NA                                                     
052400     OR DIST34-KINA-NDC                                                   
052500     OR DIST34-INDIA-NDC                                                  
052600     OR DIST34-KOREA-NDC                                                  
052700     OR DIST34-EMIRATES-NDC                                               
052800     OR DIST34-TURKEY-NDC                                                 
052900     OR DIST34-MEXICO-NDC                                                 
053000     OR DIST34-BRAZIL-NDC                                                 
053010     OR DIST34-SOUTH-AFRICA-NDC                                           
053100     OR DIST34-MALAYSIA-NDC                                               
053200     OR DIST34-THAILAND-NDC                                               
053300     OR DIST34-TAIWAN-NDC                                                 
053400     OR DIST11-DELIVERIES-FROM-DUBAI                                      
053500     OR (DIST34-NDC-PACIFIC AND OHLK-KDORDKL > +0)                        
053600       IF DIST34-NDC-BYPASS                                               
053700         CONTINUE                                                         
053800       ELSE                                                               
053900         IF OHLK-IDDC-TVS NOT = DCS-IDDC                                  
054000            MOVE OHLK-IDDC-TVS   TO W-IDDC-B6                             
054100            PERFORM IMS-GU-WDB601                                         
054200         END-IF                                                           
054300                                                                          
054400         IF  DCS-CDC                                                      
054500         AND (DIST34-NDC-NA OR DIST34-KINA-NDC                            
054600              OR DIST34-INDIA-NDC                                         
054700              OR DIST34-KOREA-NDC                                         
054800              OR DIST34-EMIRATES-NDC                                      
054900              OR DIST34-TURKEY-NDC                                        
055000              OR DIST34-MEXICO-NDC                                        
055100              OR DIST34-BRAZIL-NDC                                        
055110              OR DIST34-SOUTH-AFRICA-NDC                                  
055200              OR DIST34-MALAYSIA-NDC                                      
055300              OR DIST34-THAILAND-NDC                                      
055400              OR DIST34-TAIWAN-NDC                                        
055500              OR DIST11-DELIVERIES-FROM-DUBAI)                            
055600         AND (OHLK-KDORDKL = 0                                            
055700          OR  OHLK-KDORDKL = 1)                                           
055800*        AND OHLK-FLVORKO = JA                                            
055900*        AND OHLK-FLORDSPE = JA                                           
056000            CONTINUE                                                      
056100         ELSE                                                             
056200           IF DCS-CDC AND                                                 
056300             (OHLK-FLVORKO = NEJ OR OHLK-FLORDSPE = JA)                   
056400             MOVE NEJ TO OHLK-IDDC-TVS-OK                                 
056500           END-IF                                                         
056600         END-IF                                                           
056700       END-IF                                                             
056800     END-IF                                                               
056900                                                                          
057000     IF DIST18-SKROT-SDC AND NOT DCS-CDC                                  
057100       MOVE NEJ TO OHLK-IDDC-TVS-OK                                       
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 Q-KOLLA-KDTPOTYP-IDDC  SECTION.                                          
057600                                                                          
057700*  TROLIGTVIS FULLSTÄNDIGT MENINGSLÖS EFTERSOM TROLIGTVIS ALLA            
057800*  TPO-ER HAMNAR PÅ DC 11                                                 
057900*  EVENTUELLT KAN SEKTIONEN ÄNDRAS FÖR ATT SPÄRRA VISSA DISTRIKT          
058000*  FRÅN TPO                                                               
058100*                                                                         
058200     IF OHLK-KDTPOTYP > +0                                                
058300                                                                          
058400        IF OHLK-IDDC NOT = DCS-IDDC                                       
058500           MOVE OHLK-IDDC           TO W-IDDC-B6                          
058600           PERFORM IMS-GU-WDB601                                          
058700        END-IF                                                            
058800                                                                          
058900        IF  DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                          
059000           MOVE NEJ                 TO OHLK-KDTPOTYP-OK                   
059100        ELSE                                                              
059200           IF OHLK-IDDC-TVS NOT = DCS-IDDC                                
059300              MOVE OHLK-IDDC        TO W-IDDC-B6                          
059400              PERFORM IMS-GU-WDB601                                       
059500              IF  DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                    
059600                 MOVE NEJ           TO OHLK-KDTPOTYP-OK                   
059700              END-IF                                                      
059800           END-IF                                                         
059900        END-IF                                                            
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300                                                                          
060400* --- IMS SEKTIONER ---                                                   
060500     SKIP3                                                                
060600 IMS-GU-WDM201 SECTION.                                                   
060700                                                                          
060800     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
060900          DELIMITED BY SIZE INTO SSA1                                     
061000     MOVE '  GE'              TO GODK-STATUSKODER                         
061100     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM201 SSA1                    
061200     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500                                                                          
061600     EJECT                                                                
061700 IMS-GET-WDB201 SECTION.                                                  
061800                                                                          
061900     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
062000          DELIMITED BY SIZE INTO SSA1                                     
062100     MOVE '  GE'           TO GODK-STATUSKODER                            
062200     CALL CBLTDLI USING GHU GMTA-PCB DLI-IO-WDB201 SSA1                   
062300     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
062400     PERFORM IMS-STATUSKONTROLL                                           
062500     .                                                                    
062600     EJECT                                                                
062700 IMS-GU-WDB601    SECTION.                                                
062800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
062900          DELIMITED BY SIZE INTO SSA1                                     
063000     MOVE '  GE' TO GODK-STATUSKODER                                      
063100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
063200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
063300     PERFORM IMS-STATUSKONTROLL                                           
063400     IF SEGMENT-SAKNAS                                                    
063500         MOVE SPACE TO DCS-KDDC                                           
063600     END-IF                                                               
063700     .                                                                    
063800 IMS-GU-WDB601-FTG SECTION.                                               
063900     STRING 'WDB601  (IDFTG    =' W-IDFTG-B6 ')'                          
064000            DELIMITED BY SIZE INTO SSA1                                   
064100     MOVE '  GE'                 TO GODK-STATUSKODER                      
064200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
064300     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
064400     PERFORM IMS-STATUSKONTROLL                                           
064500     .                                                                    
064600     SKIP3                                                                
064700                                                                          
064800 IMS-STATUSKONTROLL SECTION.                                              
064900                                                                          
065000     SET STATUS-IX TO 1                                                   
065100     SEARCH GODK-STATUS                                                   
065200       AT END CALL FELLOG                                                 
065300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
065400     END-SEARCH                                                           
065500     .                                                                    
065600     EJECT                                                                
065700*    -COPY WY2000Q1                                                       
