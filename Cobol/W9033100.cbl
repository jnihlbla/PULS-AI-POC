000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9033100.                                                
000300 AUTHOR.         SVANTE BJÖRKBERG, KATARINA KYMMER, HENRIKSSON.           
000400 DATE-WRITTEN.   NOV 1987/FEB 1990/AUG 1990/JAN 2002                      
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        TILLGÄNGLIGHETSFRÅGA - VDI.                                      
000900*                                                                         
001000*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
001100*        AV ETA-DATUM/NDC-LAGER,                                          
001200*        AV ETA-DATUM/KVAVIS ANDRA LAGER.                                 
001300*       (ESTIMATED TIME AVAILABLE)                                        
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W90331T                                             
001700*        MID:         W9I33101                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:        W9O33101                                             
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900     SKIP3                                                                
003000 77    IDPGM                     PIC X(8)    VALUE 'W9033100'.            
003100 77    JA                        PIC X       VALUE 'J'.                   
003200 77    NEJ                       PIC X       VALUE 'N'.                   
003300 77    STORT-UTTAG               PIC X(3)    VALUE '075'.                 
003400 77    IX                        PIC S9(3)   VALUE +0   COMP-3.           
003500 77    IX-DCCLEAR-MAX            PIC S9(3)   VALUE +99  COMP SYNC.        
003600 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +30  COMP SYNC.        
003700 77    SW-ALLT-OK                PIC X       VALUE 'J'.                   
003800 77    SPAR-ARTC-KDERS           PIC S9(3)   VALUE +0   COMP-3.           
003900 01  FILLER                      PIC X(16) VALUE 'REDIRLEV'.              
004000 77    SPAR-REDIRLEV             PIC S9V9(2) VALUE +0   COMP-3.           
004100 77    W-DISP                    PIC S9(8)   VALUE +0   COMP-3.           
004200 77    W-DISP-1                  PIC S9(8)   VALUE +0   COMP-3.           
004300 77    W-DISP-2                  PIC S9(8)   VALUE +0   COMP-3.           
004400 77    W-DISP-3                  PIC S9(8)   VALUE +0   COMP-3.           
004500 77    W-DISP-4                  PIC S9(8)   VALUE +0   COMP-3.           
004600 77    W-DISP-5                  PIC S9(8)   VALUE +0   COMP-3.           
004700 77    W-DISP-6                  PIC S9(8)   VALUE +0   COMP-3.           
004800 77    W-DISP-TOT                PIC S9(8)   VALUE +0   COMP-3.           
004900 77    W-DISP-HOME               PIC S9(8)   VALUE +0   COMP-3.           
005000 77    W-DISP-NUM                PIC 9(6)    VALUE ZERO.                  
005100 77    W-KVLS                    PIC 9(7)    VALUE ZERO.                  
005200 77    W-KVRESS                  PIC 9(7)    VALUE ZERO.                  
005300 77    W-KVUTRS                  PIC 9(7)    VALUE ZERO.                  
005400 77    W-KVOKS-DAG               PIC 9(7)    VALUE ZERO.                  
005500 77    W-KVSPARR-KVAL            PIC 9(7)    VALUE ZERO.                  
005600 77    WS-TIBERANK               PIC 9(6)    VALUE ZERO.                  
005700 77    WS-KVAVIS                 PIC 9(6)    VALUE ZERO.                  
005800 77    WS-TILOKDAT               PIC 9(6)    VALUE ZERO.                  
005900 77    WS-IDDC-RO                PIC X(2)    VALUE SPACE.                 
006000 77    WS-ERS-KDORDBEK           PIC X(2)    VALUE SPACE.                 
006100 77    DAGENS-DATUM              PIC  9(6).                               
006200                                                                          
006300 77    WS-KDORDBEK               PIC X(2)    VALUE SPACE.                 
006400   88  DLEV                                  VALUE '95'.                  
006500                                                                          
006600 01    WS-TEXT.                                                           
006700       05 WS-TEXT1               PIC X(1)   VALUE 'X'.                    
006800       05 WS-TEXT2               PIC X(1)   VALUE 'X'.                    
006900       05 WS-TEXT3               PIC X(1)   VALUE 'X'.                    
007000                                                                          
007100 01  DUMMY-PCB                   PIC X(4)  VALUE LOW-VALUE.               
007200*                                                                         
007300*01    -COPY WWPRODSL                                                     
007400*                                                                         
007500*      --- VALID IDDC CODES                                               
007600*                                                                         
007700*01    -COPY WWDC99                                                       
007800*01    -COPY WWDCKONS                                                     
007900       EJECT                                                              
008000 77    ARTC01-STATUS             PIC X      VALUE 'N'.                    
008100   88  ARTC01-FINNS                         VALUE 'J'.                    
008200   88  ARTC01-SAKNAS                        VALUE 'N'.                    
008300                                                                          
008400 77    ARTC11-STATUS             PIC X      VALUE 'N'.                    
008500   88  ARTC11-FINNS                         VALUE 'J'.                    
008600   88  ARTC11-SAKNAS                        VALUE 'N'.                    
008700                                                                          
008800 77    ARTS-STATUS               PIC X      VALUE 'N'.                    
008900   88  ARTS-FINNS                           VALUE 'J'.                    
009000   88  ARTS-SAKNAS                          VALUE 'N'.                    
009100                                                                          
009200 01  WS-GMT.                                                              
009300     03 WS-GMT-IDDC              PIC X(2) OCCURS 3.                       
009400                                                                          
009500 01  CLEAR-SW                    PIC X.                                   
009600   88  CLEARING                             VALUE 'J'.                    
009700     EJECT                                                                
009800*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
009900 01  WS-ETA-DATUM                PIC 9(6).                                
010000 01  FILLER REDEFINES WS-ETA-DATUM.                                       
010100     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
010200     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
010300     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
010400     EJECT                                                                
010500 01  DYNAMISKA-SUBPROGRAM.                                                
010600   03  W411SPAR                  PIC X(8)   VALUE 'W411SPAR'.             
010700   03  W411DLEV                  PIC X(8)   VALUE 'W411DLEV'.             
010800   03  W411RANS                  PIC X(8)   VALUE 'W411RANS'.             
010900   03  W411STOR                  PIC X(8)   VALUE 'W411STOR'.             
011000   03  W218ETA                   PIC X(8)   VALUE 'W218ETA '.             
011100   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
011200   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
011300   03  WDAGKONV                  PIC X(8)   VALUE 'WDAGKONV'.             
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL WDAGAREA                                         
011700 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
011800*01 -COPY WDAGAREA                                                        
011900     EJECT                                                                
012400*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
012500*                                                                         
012600*   -COPY W411SPAR                                                        
012700     EJECT                                                                
012800*   -COPY W411DLEV                                                        
012900     EJECT                                                                
013000*   -COPY W411RANS                                                        
013100     EJECT                                                                
013200 01   FILLER             PIC X(8)  VALUE 'STOR'.                          
013300*   -COPY W411STOR                                                        
013400     EJECT                                                                
013500 01 FILLER               PIC X(8)  VALUE 'LETA'.                          
013600*   -COPY W218LETA -PRE ETA-.                                             
013700     EJECT                                                                
013800 01  ARBETSAREOR.                                                         
013900   03  WS-KVAVBART-MDEC         PIC S9(6)V99.                             
014000   03  WS-KVAVBART-UDEC         PIC S9(6).                                
014100   03  WS-KVBEART               PIC S9(6).                                
014200   03  WS-IDRFTAB               PIC X(3).                                 
014300   03  WS-TIDISPIN              PIC 9(6).                                 
014400   03  WS-KVFRYSTI              PIC 9(2).                                 
014500     EJECT                                                                
014600 01    NYCKLAR-TILL-DLI.                                                  
014700   03    W-IDARTNR-X.                                                     
014800     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
014900*                                                                         
015000   03  KEY-DIST-KUND.                                                     
015100     05  W-IDGMT-X.                                                       
015200       07  W-IDDISTR             PIC S9(5)   VALUE ZERO  COMP-3.          
015300       07  W-IDKUNDNR            PIC S9(7)   VALUE ZERO  COMP-3.          
015400                                                                          
015500   03    W-IDDC-X.                                                        
015600     05  W-IDDC                  PIC  X(2)   VALUE SPACE.                 
015700*                                                                         
015800   03  W-WDF2A1KY-MIN-X.                                                  
015900       05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.             
016000       05  W-IDLEVNR-MIN       PIC X(5)    VALUE SPACE.                   
016100                                                                          
016200   03  W-WDF2A1KY-MAX-X.                                                  
016300       05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.             
016400       05  W-IDLEVNR-MAX       PIC X(5)    VALUE SPACE.                   
016500                                                                          
016600     03  W-IDDC-B6-X.                                                     
016700         05 W-IDDC-B6                  PIC X(2).                          
016800                                                                          
016900     EJECT                                                                
017000******************************************************************        
017100*                                                                         
017200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
017300*                                                                         
017400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
017500     SKIP3                                                                
017600*01    MID -COPY W9I33101.                                                
017700     EJECT                                                                
017800*01    -COPY WMSGAREA                                                     
017900     EJECT                                                                
018000*  03    MOD -COPY W9O33101  -RED MSG-AREA.                               
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
019500     SKIP3                                                                
019600   03    GODK-STATUSKODER.                                                
019700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
019800     SKIP3                                                                
019900 01    SSA1                      PIC X(64).                               
020000 01    SSA2                      PIC X(64).                               
020100     EJECT                                                                
020200*                            IMS FUNKTIONSKODER                           
020300*01    -COPY W0003                                                        
020400*                                                                         
020500*                                                                         
020600******************************************************************        
020700     EJECT                                                                
020800******************************************************************        
020900*                                                                         
021000*        ARBETS-AREOR TILL IO-AREORNA                                     
021100*                                                                         
021200*    ---  DLI INPUT-OUTPUT AREA 1                                         
021300*    ---  DLI-IO-AREA                                                     
021400                                                                          
021500 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC01'.            
021600 01  DLI-IO-ARTC01.                                                       
021700*  03    WLARTC01 -COPY WDK601                                            
021800     EJECT                                                                
021900                                                                          
022000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
022100 01  DLI-IO-ARTC11.                                                       
022200*  03    WLARTC11 -COPY WDK611                                            
022300     EJECT                                                                
022400                                                                          
022500 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-GMTA01'.            
022600 01  DLI-IO-GMTA01.                                                       
022700*  03    WLGMTA01 -COPY WDB201                                            
022800     EJECT                                                                
022900                                                                          
023000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-INLC11'.            
023100 01  DLI-IO-INLC11.                                                       
023200*  03    WLINLC11 -COPY WDL611                                            
023300     EJECT                                                                
023400                                                                          
023500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVG01'.                    
023600 01  DLI-IO-WLLEVG01.                                                     
023700*    03  -COPY WDF2A1                                                     
023800     EJECT                                                                
023900                                                                          
024000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTS11'.            
024100 01  DLI-IO-ARTS11.                                                       
024200*  03    WLARTS11 -COPY WDK711                                            
024300                                                                          
024400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024500 01   DLI-IO-AREA-B601.                                                   
024600*     03  -COPY WDB601                                                    
024700     EJECT                                                                
024800 01  FILLER                      PIC X(12) VALUE 'DUMMY-ARTC'.            
024900 01  ETA-ARTC-PCB                PIC X.                                   
025000 01  FILLER                      PIC X(12) VALUE 'DUMMY-LEVA'.            
025100 01  ETA-LEVA-PCB                PIC X.                                   
025200 LINKAGE SECTION.                                                         
025300                                                                          
025400*01    -COPY W0009     -PRE MSG-                                          
025500     EJECT                                                                
025900*01    -COPY W0008     -PRE ARTC-                                         
026000     05  FILLER                  PIC X.                                   
026100                                                                          
026200*01    -COPY W0008     -PRE GMTA-                                         
026300     05  FILLER                  PIC X.                                   
026400     EJECT                                                                
026500*01    -COPY W0008     -PRE INLC-                                         
026600     05  FILLER                  PIC X.                                   
026700     EJECT                                                                
026800*01    -COPY W0008     -PRE DLEV-LEVF-                                    
026900     05  FILLER                  PIC X.                                   
027000                                                                          
027100*01    -COPY W0008     -PRE DLEV-LEVG-                                    
027200     05  FILLER                  PIC X.                                   
027300                                                                          
027400*01    -COPY W0008     -PRE DLEV-LEVA-                                    
027500     05  FILLER                  PIC X.                                   
027600                                                                          
027700*01    -COPY W0008     -PRE DLEV-ARTS-                                    
027800     05  FILLER                  PIC X.                                   
027900                                                                          
028000*01    -COPY W0008     -PRE DLEV-WDB6-                                    
028100     05  FILLER                  PIC X.                                   
028200                                                                          
028300*01    -COPY W0008     -PRE XXKM-                                         
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600*01    -COPY W0008     -PRE RANS-ARTM-                                    
028700     05  FILLER                  PIC X.                                   
028800*01    -COPY W0008     -PRE RANS-ARTS-                                    
028900     05  FILLER                  PIC X.                                   
029000                                                                          
029100*01  -COPY W0008       -PRE LEVG-                                         
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008       -PRE ARTS-                                         
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008       -PRE WDB6-                                         
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000 01  ETA-WDK7-PCB                PIC X.                                   
030100 01  ETA-INLC-PCB                PIC X.                                   
030200 01  ETA-WDB6-PCB                PIC X.                                   
030300 01  ETA-WDD9-PCB                PIC X.                                   
030400 01  SPAR-WDF8-PCB               PIC X.                                   
030500 01  SPAR-WDF8A-PCB              PIC X.                                   
030600 01  SPAR-WDK6-PCB               PIC X.                                   
030700     EJECT                                                                
030800 PROCEDURE DIVISION  USING MSG-PCB                                        
030900                     ARTC-PCB      GMTA-PCB      INLC-PCB                 
031000                     DLEV-LEVF-PCB DLEV-LEVG-PCB DLEV-LEVA-PCB            
031100                     DLEV-ARTS-PCB DLEV-WDB6-PCB                          
031200                     XXKM-PCB                                             
031300                     RANS-ARTM-PCB                                        
031400                     RANS-ARTS-PCB                                        
031500                     LEVG-PCB      ARTS-PCB      WDB6-PCB                 
031600                     ETA-WDK7-PCB                                         
031700                     ETA-INLC-PCB                                         
031800                     ETA-WDB6-PCB                                         
031900                     ETA-WDD9-PCB                                         
032000                     SPAR-WDF8-PCB                                        
032100                     SPAR-WDF8A-PCB                                       
032200                     SPAR-WDK6-PCB.                                       
032300 MAIN SECTION.                                                            
032400     ENTRY 'DLITCBL' USING MSG-PCB                                        
032500                     ARTC-PCB      GMTA-PCB      INLC-PCB                 
032600                     DLEV-LEVF-PCB DLEV-LEVG-PCB DLEV-LEVA-PCB            
032700                     DLEV-ARTS-PCB DLEV-WDB6-PCB                          
032800                     XXKM-PCB                                             
032900                     RANS-ARTM-PCB                                        
033000                     RANS-ARTS-PCB                                        
033100                     LEVG-PCB      ARTS-PCB      WDB6-PCB                 
033200                     ETA-WDK7-PCB                                         
033300                     ETA-INLC-PCB                                         
033400                     ETA-WDB6-PCB                                         
033500                     ETA-WDD9-PCB                                         
033600                     SPAR-WDF8-PCB                                        
033700                     SPAR-WDF8A-PCB                                       
033800                     SPAR-WDK6-PCB.                                       
033900                                                                          
034000     PERFORM IMS-GET-MSG                                                  
034100     IF  SEGMENT-FINNS                                                    
034200                                                                          
034300       PERFORM A-INIT-SPARA-INPUT                                         
034400       IF MOD-IDMFSFEL = ZERO                                             
034500         PERFORM B-BEHANDLA-RAD                                           
034600                                                                          
034700*---------------------------------------                                  
034800*        MOVE WS-TEXT                TO MOD-IDMFSFEL                      
034900*---------------------------------------                                  
035000       END-IF                                                             
035100                                                                          
035200       MOVE MAX-MOD-LAENGD           TO MSG-KVLL                          
035300       PERFORM IMS-INSERT-MSG                                             
035400     END-IF                                                               
035500                                                                          
035600     MOVE ZERO                         TO RETURN-CODE                     
035700                                                                          
035800     GOBACK                                                               
035900     .                                                                    
036000     EJECT                                                                
036100                                                                          
036200 A-INIT-SPARA-INPUT SECTION.                                              
036300                                                                          
036400     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I33101                    
036500     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
036600     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
036700     MOVE ' '                          TO MFS-KDTRTYP                     
036800     MOVE LOW-VALUE                    TO MSG-AREA                        
036900     MOVE 'W9O33101'                   TO MFS-IDMOD                       
037000     MOVE '9331'                       TO MOD-IDTRANS                     
037100     MOVE ZERO                         TO MOD-IDMFSFEL                    
037200     MOVE SPACE                        TO DLEV-IDLEVNR-UT                 
037300     IF MID-IDDISTR  NUMERIC AND                                          
037400        MID-IDKUNDNR NUMERIC                                              
037500                                                                          
037600       MOVE MID-IDDISTR                TO W-IDDISTR                       
037700       MOVE MID-IDKUNDNR               TO W-IDKUNDNR                      
037800       PERFORM IMS-GU-GMTA01                                              
037900       IF SEGMENT-FINNS                                                   
038000         MOVE GMT-IDRFTAB              TO WS-IDRFTAB                      
038100*-HÄR                                                                     
038200         IF MID-IDVTYP < 3                                                
038300*..........ÄLDRE VERSION UTAN KDORDKL                                     
038400           MOVE 1                      TO MID-KDORDKL                     
038500         END-IF                                                           
038600                                                                          
038700         IF  (MID-IDVTYP >= 3)                                            
038800         AND (MID-KDORDKL = 3 OR 4)                                       
038900           MOVE GMT-IDDC-BULK(1)       TO WS-GMT-IDDC (1)                 
039000           MOVE GMT-IDDC-BULK(2)       TO WS-GMT-IDDC (2)                 
039100           MOVE GMT-IDDC-BULK(3)       TO WS-GMT-IDDC (3)                 
039200           MOVE 'B'                    TO WS-TEXT1                        
039300         ELSE                                                             
039400           MOVE GMT-IDDC-DAY (1)       TO WS-GMT-IDDC (1)                 
039500           MOVE GMT-IDDC-DAY (2)       TO WS-GMT-IDDC (2)                 
039600           MOVE GMT-IDDC-DAY (3)       TO WS-GMT-IDDC (3)                 
039700           MOVE 'D'                    TO WS-TEXT1                        
039800         END-IF                                                           
039900                                                                          
040000         MOVE WS-GMT-IDDC (1)          TO W-IDDC                          
040100                                          WS-IDDC                         
040200       ELSE                                                               
040300         MOVE 'B10'                    TO MOD-IDMFSFEL                    
040400         MOVE SPACE                    TO WS-IDRFTAB                      
040500       END-IF                                                             
040600     ELSE                                                                 
040700       MOVE 'B01'                      TO MOD-IDMFSFEL                    
040800     END-IF                                                               
040900     ACCEPT DAGENS-DATUM FROM DATE                                        
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041300 B-BEHANDLA-RAD SECTION.                                                  
041400                                                                          
041500     PERFORM BA-INITIERA-FAELT                                            
041600     IF SW-ALLT-OK    = JA                                                
041700       PERFORM BB-LAS-BASERNA                                             
041800       PERFORM BC-KONTROLL-ART                                            
041900     END-IF                                                               
042000     IF SW-ALLT-OK    = JA                                                
042100       PERFORM BD-KONTROLL-ERSATTNING                                     
042200     END-IF                                                               
042300     IF SW-ALLT-OK    = JA   AND                                          
042400        ARTC11-STATUS = JA                                                
042500       PERFORM BF-DIV-SPARRAR                                             
042600     END-IF                                                               
042700     IF SW-ALLT-OK = JA                                                   
042800       IF DLEV-FLSDCLEV-UT = 'J'                                          
042900         MOVE '1' TO WS-TEXT2                                             
043000         MOVE '10' TO WS-KDORDBEK                                         
043100         MOVE WS-KVBEART TO W-DISP-NUM                                    
043200       ELSE                                                               
043300         MOVE WS-IDDC TO W-IDDC-B6                                        
043400         PERFORM IMS-GU-WDB601                                            
043500         IF DCS-SDC AND                                                   
043600           (SW-ALLT-OK = JA OR WS-ERS-KDORDBEK > 0)                       
043700           PERFORM BH-BER-DISP-SDC                                        
043800           IF CLEARING                                                    
043900             PERFORM BI-BER-DISP-DC2                                      
044000             IF CLEARING                                                  
044100               PERFORM BJ-BER-DISP-DC3                                    
044200             END-IF                                                       
044300           END-IF                                                         
044400         END-IF                                                           
044500         IF NDC AND                                                       
044600            (SW-ALLT-OK = JA OR WS-ERS-KDORDBEK > 0)                      
044700           PERFORM BK-ALLOC-DC1                                           
044800           IF CLEARING                                                    
044900             PERFORM BL-ALLOC-DC2                                         
045000             IF CLEARING                                                  
045100               PERFORM BM-ALLOC-DC3                                       
045200             END-IF                                                       
045300           END-IF                                                         
045400         END-IF                                                           
045500         IF CDC                                                           
045600           IF SW-ALLT-OK = JA  OR                                         
045700              DLEV                                                        
045800             PERFORM S05-RANSONERA                                        
045900           END-IF                                                         
046000           IF SW-ALLT-OK = JA  OR                                         
046100              DLEV                                                        
046200             PERFORM BG-KONTROLLERA-STORT-UTTAG                           
046300           END-IF                                                         
046400         END-IF                                                           
046500         IF SW-ALLT-OK  = JA   OR                                         
046600            DLEV                                                          
046700           IF CDC OR                                                      
046800              DLEV                                                        
046900             PERFORM S06-BER-DISP-CDC                                     
047000           END-IF                                                         
047100         END-IF                                                           
047200       END-IF                                                             
047300     END-IF                                                               
047400     IF WS-ERS-KDORDBEK = '41' OR '54' OR '61'                            
047500       IF (CDC     AND WS-KVAVBART-UDEC > ZERO)                           
047600       OR (NOT CDC AND W-DISP-NUM       > ZERO)                           
047700         CONTINUE                                                         
047800       ELSE                                                               
047900         MOVE SPACE              TO WS-IDDC                               
048000         MOVE WS-ERS-KDORDBEK    TO WS-KDORDBEK                           
048100       END-IF                                                             
048200     END-IF                                                               
048300     IF WS-KDORDBEK NOT = SPACE                                           
048400       PERFORM BZ-FLYTTA-TILL-MOD                                         
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800                                                                          
048900 BA-INITIERA-FAELT SECTION.                                               
049000     MOVE 'A'                 TO WS-TEXT3                                 
049100                                                                          
049200     MOVE ZERO    TO WS-KVAVBART-MDEC                                     
049300                     WS-KVAVBART-UDEC                                     
049400                                                                          
049500     MOVE SPACE   TO WS-KDORDBEK                                          
049600     MOVE NEJ     TO ARTC01-STATUS                                        
049700                     ARTC11-STATUS                                        
049800                     ARTS-STATUS                                          
049900                     CLEAR-SW                                             
050000                                                                          
050100     INSPECT MID-IDARTNR  REPLACING LEADING SPACE BY ZERO                 
050200     INSPECT MID-KVBEART  REPLACING LEADING SPACE BY ZERO                 
050300                                                                          
050400     IF MID-IDARTNR NUMERIC    AND                                        
050500        MID-KVBEART NUMERIC                                               
050600       CONTINUE                                                           
050700     ELSE                                                                 
050800       MOVE '30'  TO WS-KDORDBEK                                          
050900       MOVE NEJ   TO SW-ALLT-OK                                           
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300                                                                          
051400 BB-LAS-BASERNA SECTION.                                                  
051500     MOVE 'B'                 TO WS-TEXT3                                 
051600                                                                          
051700     MOVE LOW-VALUE   TO W-WDF2A1KY-MIN-X                                 
051800     MOVE HIGH-VALUE  TO W-WDF2A1KY-MAX-X                                 
051900                                                                          
052000     MOVE MID-KVBEART TO WS-KVBEART                                       
052100     MOVE MID-IDARTNR TO W-IDARTNR                                        
052200                         W-IDARTNR-MIN                                    
052300                         W-IDARTNR-MAX                                    
052400                                                                          
052500     MOVE WS-IDDC TO W-IDDC-B6                                            
052600     PERFORM IMS-GU-WDB601                                                
052700     IF DCS-SDC OR DCS-NDC                                                
052800       PERFORM IMS-GU-ARTS11                                              
052900       IF SEGMENT-SAKNAS                                                  
053000*        MOVE WS-IDDC TO W-IDDC-B6                                        
053100*        PERFORM IMS-GU-WDB601                                            
053200*        IF DCS-SDC                                                       
053300*          MOVE WC-CDC-SE      TO WS-IDDC                                 
053400*          MOVE '15'           TO WS-KDORDBEK                             
053500*        END-IF                                                           
053600         MOVE NEJ              TO ARTS-STATUS                             
053700       ELSE                                                               
053800         MOVE JA               TO ARTS-STATUS                             
053900       END-IF                                                             
054000     END-IF                                                               
054100                                                                          
054200     PERFORM IMS-GU-ARTC01                                                
054300     IF SEGMENT-FINNS                                                     
054400       MOVE JA                 TO ARTC01-STATUS                           
054500                                                                          
054600       PERFORM IMS-GNP-ARTC11                                             
054700       IF SEGMENT-FINNS                                                   
054800         MOVE JA               TO ARTC11-STATUS                           
054900         IF ART-KDERS-UTG > 0                                             
055000           MOVE ART-KDERS-UTG  TO SPAR-ARTC-KDERS                         
055100         ELSE                                                             
055200           MOVE CLAG-KDERS     TO SPAR-ARTC-KDERS                         
055300         END-IF                                                           
055400         MOVE CLAG-REDIRLEV    TO SPAR-REDIRLEV                           
055500         IF CLAG-REDIRLEV > +0                                            
055600           PERFORM IMS-GU-LEVG01                                          
055700           IF SEGMENT-FINNS                                               
055800             CONTINUE                                                     
055900           ELSE                                                           
056000             MOVE +0           TO SPAR-REDIRLEV                           
056100           END-IF                                                         
056200         END-IF                                                           
056300       END-IF                                                             
056400     END-IF                                                               
056500     .                                                                    
056600     EJECT                                                                
056700                                                                          
056800 BC-KONTROLL-ART SECTION.                                                 
056900     MOVE 'C'                 TO WS-TEXT3                                 
057000                                                                          
057100     IF ARTC01-SAKNAS OR                                                  
057200        ARTC11-SAKNAS                                                     
057300       MOVE '58'  TO WS-KDORDBEK                                          
057400       MOVE NEJ   TO SW-ALLT-OK                                           
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 BD-KONTROLL-ERSATTNING SECTION.                                          
058000     MOVE 'D'                 TO WS-TEXT3                                 
058100                                                                          
058200*---ERSÄTTNINGSKOD FÖR NDC SKALL INTE VISAS INNAN LAGREN-----             
058300*---ÄR TÖMDA PÅ ARTIKELN FÖR KODERNA 21 24 27 28 OCH 29 -----             
058400     IF NDC                                                               
058500       IF SPAR-ARTC-KDERS = 29                                            
058600         PERFORM S15-KOLLA-LAGERSALDO                                     
058700         IF W-KVLS = 0                                                    
058800           MOVE '54'  TO WS-KDORDBEK                                      
058900                         WS-ERS-KDORDBEK                                  
059000         END-IF                                                           
059100       END-IF                                                             
059200                                                                          
059300       IF SPAR-ARTC-KDERS = 52                                            
059400         MOVE '52'  TO WS-KDORDBEK                                        
059500                       WS-ERS-KDORDBEK                                    
059600         MOVE NEJ   TO SW-ALLT-OK                                         
059700       END-IF                                                             
059800                                                                          
059900       IF SPAR-ARTC-KDERS = 22 OR 13 OR 23                                
060000         MOVE '41'  TO WS-KDORDBEK                                        
060100                       WS-ERS-KDORDBEK                                    
060200         IF SPAR-ARTC-KDERS = 13 OR 23                                    
060300           MOVE NEJ   TO SW-ALLT-OK                                       
060400         END-IF                                                           
060500       END-IF                                                             
060600                                                                          
060700       IF SPAR-ARTC-KDERS = 25 OR 16 OR 26                                
060800         MOVE '61'  TO WS-KDORDBEK                                        
060900                       WS-ERS-KDORDBEK                                    
061000         IF SPAR-ARTC-KDERS = 16 OR 26                                    
061100           MOVE NEJ   TO SW-ALLT-OK                                       
061200         END-IF                                                           
061300       END-IF                                                             
061400                                                                          
061500       IF SPAR-ARTC-KDERS = 21 OR 27                                      
061600         PERFORM S15-KOLLA-LAGERSALDO                                     
061700         IF W-KVLS = 0                                                    
061800           MOVE '41'  TO WS-KDORDBEK                                      
061900                         WS-ERS-KDORDBEK                                  
062000         END-IF                                                           
062100       END-IF                                                             
062200                                                                          
062300       IF SPAR-ARTC-KDERS = 24 OR 28                                      
062400         PERFORM S15-KOLLA-LAGERSALDO                                     
062500         IF W-KVLS = 0                                                    
062600           MOVE '61'  TO WS-KDORDBEK                                      
062700                         WS-ERS-KDORDBEK                                  
062800         END-IF                                                           
062900       END-IF                                                             
063000     ELSE                                                                 
063100       EVALUATE TRUE                                                      
063200         WHEN SPAR-ARTC-KDERS = 19 OR 29                                  
063300           MOVE '54'  TO WS-KDORDBEK                                      
063400                         WS-ERS-KDORDBEK                                  
063500                                                                          
063600         WHEN SPAR-ARTC-KDERS = 52                                        
063700           MOVE '52'  TO WS-KDORDBEK                                      
063800                         WS-ERS-KDORDBEK                                  
063900           MOVE NEJ   TO SW-ALLT-OK                                       
064000                                                                          
064100         WHEN (SPAR-ARTC-KDERS > 10 AND < 14) OR                          
064200              (SPAR-ARTC-KDERS > 20 AND < 24) OR                          
064300              (SPAR-ARTC-KDERS = 17 OR    27)                             
064400           MOVE '41'  TO WS-KDORDBEK                                      
064500                         WS-ERS-KDORDBEK                                  
064600           IF SPAR-ARTC-KDERS = 13 OR 23                                  
064700             MOVE NEJ   TO SW-ALLT-OK                                     
064800           END-IF                                                         
064900                                                                          
065000         WHEN (SPAR-ARTC-KDERS > 13 AND < 17) OR                          
065100              (SPAR-ARTC-KDERS > 23 AND < 27) OR                          
065200              (SPAR-ARTC-KDERS = 18 OR    28)                             
065300           MOVE '61'  TO WS-KDORDBEK                                      
065400                         WS-ERS-KDORDBEK                                  
065500           IF SPAR-ARTC-KDERS = 16 OR 26                                  
065600             MOVE NEJ   TO SW-ALLT-OK                                     
065700           END-IF                                                         
065800                                                                          
065900       END-EVALUATE                                                       
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300                                                                          
066400 BF-DIV-SPARRAR SECTION.                                                  
066500     MOVE 'F'                 TO WS-TEXT3                                 
066600                                                                          
066700     PERFORM BFA-SKAPA-W411SPAR-LANKAREA                                  
066800     PERFORM S10-CALL-W411SPAR                                            
066900     IF SPAR-KDORDBEK > +0                                                
067000       MOVE SPAR-KDORDBEK         TO WS-KDORDBEK                          
067100       IF SPAR-KDORDBEK = 67                                              
067200          MOVE JA                 TO SW-ALLT-OK                           
067300       ELSE                                                               
067400          MOVE NEJ                TO SW-ALLT-OK                           
067500       END-IF                                                             
067600     END-IF                                                               
067700                                                                          
067800     MOVE WS-IDDC TO W-IDDC-B6                                            
067900     PERFORM IMS-GU-WDB601                                                
068000     IF (DCS-CDC OR DCS-CDC-TR OR DCS-SDC) AND (SW-ALLT-OK = JA)          
068100                                                                          
068200       PERFORM BFB-SKAPA-W411DLEV-LANKAREA                                
068300       PERFORM S11-CALL-W411DLEV                                          
068400       IF DLEV-KDORDBEK-UT > ZERO                                         
068500         MOVE DLEV-KDORDBEK-UT    TO WS-KDORDBEK                          
068600         MOVE NEJ                 TO SW-ALLT-OK                           
068700         IF DLEV                                                          
068800           MOVE DLEV-IDDC-UT TO WS-IDDC                                   
068900         END-IF                                                           
069000       END-IF                                                             
069100       IF DLEV-FLSDCLEV-UT = 'J'                                          
069200         MOVE DLEV-IDDC-UT TO WS-IDDC                                     
069300       END-IF                                                             
069400     END-IF                                                               
069500     .                                                                    
069600     EJECT                                                                
069700                                                                          
069800 BFA-SKAPA-W411SPAR-LANKAREA SECTION.                                     
069900                                                                          
070000     MOVE +0                   TO SPAR-KDORDBEH                           
070100     MOVE WS-GMT-IDDC(1)       TO SPAR-IDDC                               
070200     MOVE 'VDI '               TO SPAR-IDSYSTEM                           
070300     MOVE MID-IDDISTR          TO SPAR-IDDISTR                            
070400     MOVE MID-IDKUNDNR         TO SPAR-IDKUNDNR                           
070500     MOVE MID-IDARTNR          TO SPAR-IDARTNR                            
070600     MOVE SPACE                TO SPAR-FLRESTN                            
070700     MOVE '0000000   '         TO SPAR-IDKUNDRF-RO                        
070800     MOVE +0                   TO SPAR-KDTPOTYP                           
070900     MOVE +0                   TO SPAR-TIRODAT                            
071000     MOVE +0                   TO SPAR-TITPO                              
071100     MOVE NEJ                  TO SPAR-FLFORBI                            
071200                                  SPAR-FLEMBORD                           
071300                                  SPAR-FLOVRLEV                           
071400                                  SPAR-FLORDSPE                           
071500     MOVE MID-KDORDKL          TO SPAR-KDORDKL                            
071600     MOVE SPACE                TO SPAR-KDFAKTYP                           
071700     MOVE ART-KDERS-UTG        TO SPAR-KDERS-UTG                          
071800     MOVE ART-TIFINLV          TO SPAR-TIFINLV                            
071900     MOVE CLAG-FLAVRART        TO SPAR-FLAVRART                           
072000     MOVE ART-FLIART           TO SPAR-FLIART                             
072100     MOVE SPAR-ARTC-KDERS      TO SPAR-KDERS                              
072200     MOVE CLAG-FLLSRDEL        TO SPAR-FLLSRDEL                           
072300     MOVE CLAG-FLRADREF        TO SPAR-FLRADREF                           
072400     MOVE CLAG-KDLEVSP         TO SPAR-KDLEVSP                            
072500     MOVE CLAG-KDUART          TO SPAR-KDUART                             
072600     MOVE ART-KDPRODSL         TO SPAR-KDPRODSL                           
072700     MOVE ART-KDSORT           TO SPAR-KDSORT                             
072800     MOVE CLAG-PRARTSTD        TO SPAR-PRARTSTD                           
072900     MOVE SPACE                TO SPAR-BERADREF                           
073000     MOVE SPACE                TO SPAR-BEKUNDRF                           
073100     MOVE SPACE                TO SPAR-KDPRTYP                            
073200     MOVE NEJ                  TO SPAR-FLSDCLEV                           
073300     MOVE ZERO                 TO SPAR-TIREPDAT                           
073400     .                                                                    
073500     EJECT                                                                
073600                                                                          
073700 BFB-SKAPA-W411DLEV-LANKAREA SECTION.                                     
073800                                                                          
073900     MOVE MID-IDDISTR          TO DLEV-IDDISTR-IN                         
074000     MOVE MID-IDKUNDNR         TO DLEV-IDKUNDNR-IN                        
074100     MOVE MID-KDORDKL          TO DLEV-KDORDKL-IN                         
074200     MOVE MID-IDARTNR          TO DLEV-IDARTNR-IN                         
074300     MOVE MID-KVBEART          TO DLEV-KVBEART-Q-IN                       
074400     MOVE ART-IDLEVNR          TO DLEV-IDLEVNR-IN                         
074500     MOVE CLAG-KDUART          TO DLEV-KDUART-IN                          
074600     MOVE SPAR-REDIRLEV        TO DLEV-REDIRLEV-IN                        
074700     MOVE NEJ                  TO DLEV-FLFORBI-IN                         
074800     MOVE NEJ                  TO DLEV-FLSDCLEV-UT                        
074900     MOVE SPACE                TO DLEV-FLRESTN-IN                         
075000                                  DLEV-IDDC-IN                            
075100     MOVE WS-IDDC              TO DLEV-IDDC-ORD-IN                        
075200     MOVE +0                   TO DLEV-IDKAMPRF-IN                        
075300     MOVE +0                   TO DLEV-KDTPOTYP-IN                        
075400     MOVE CLAG-FLREFILL        TO DLEV-FLREFILL-IN                        
075500     MOVE +3                   TO DLEV-KDORDING-IN                        
075600     MOVE SPACE                TO DLEV-CLEARGROUP                         
075700                                  DLEV-KDOI-UT                            
075800     MOVE +1 TO IX                                                        
075900     PERFORM UNTIL IX > IX-DCCLEAR-MAX                                    
076000        MOVE SPACE             TO DLEV-IDDC-CLEAR-IN(IX)                  
076100        ADD +1 TO IX                                                      
076200     END-PERFORM                                                          
076300                                                                          
076400     IF MID-KDORDKL > 1                                                   
076500                                                                          
076600        MOVE +1 TO IX                                                     
076700        PERFORM UNTIL IX > IX-DCCLEAR-MAX                                 
076800           MOVE GMT-IDDC-BULK(IX)  TO DLEV-IDDC-CLEAR-IN(IX)              
076900           ADD +1 TO IX                                                   
077000        END-PERFORM                                                       
077100                                                                          
077200     ELSE                                                                 
077300       IF MID-KDORDKL = 1                                                 
077400                                                                          
077500          MOVE +1 TO IX                                                   
077600          PERFORM UNTIL IX > IX-DCCLEAR-MAX                               
077700             MOVE GMT-IDDC-DAY(IX) TO DLEV-IDDC-CLEAR-IN(IX)              
077800             ADD +1 TO IX                                                 
077900          END-PERFORM                                                     
078000                                                                          
078100       ELSE                                                               
078200         IF MID-KDORDKL = 0                                               
078300                                                                          
078400            MOVE +1 TO IX                                                 
078500            PERFORM UNTIL IX > IX-DCCLEAR-MAX                             
078600               MOVE GMT-IDDC-VOR(IX) TO DLEV-IDDC-CLEAR-IN(IX)            
078700               ADD +1 TO IX                                               
078800            END-PERFORM                                                   
078900                                                                          
079000         END-IF                                                           
079100       END-IF                                                             
079200     END-IF                                                               
079300     .                                                                    
079400     EJECT                                                                
079500                                                                          
079600 BG-KONTROLLERA-STORT-UTTAG SECTION.                                      
079700     MOVE 'G'                 TO WS-TEXT3                                 
079800                                                                          
079900     PERFORM BGA-SKAPA-W411STOR-LANKAREA                                  
080000     PERFORM S13-CALL-W411STOR                                            
080100     IF STOR-KDORDBEK > +0                                                
080200       MOVE STOR-KDORDBEK TO WS-KDORDBEK                                  
080300       MOVE NEJ           TO SW-ALLT-OK                                   
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700                                                                          
080800 BGA-SKAPA-W411STOR-LANKAREA SECTION.                                     
080900                                                                          
081000     MOVE 'VDI '                     TO STOR-IDSYSTEM                     
081100     MOVE DLEV-IDLEVNR-UT            TO STOR-IDLEVNR                      
081200     MOVE '0000000   '               TO STOR-IDKUNDRF-RO                  
081300     MOVE SPACE                      TO STOR-BERADREF                     
081400     MOVE NEJ                        TO STOR-FLOVRLEV                     
081500                                        STOR-FLORDSPE                     
081600                                        STOR-FLFORBI                      
081700     MOVE ZERO                       TO STOR-IDKAMPRF                     
081800     MOVE MID-IDDISTR                TO STOR-IDDISTR                      
081900     MOVE MID-KDORDKL                TO STOR-KDORDKL                      
082000     MOVE WS-KVBEART                 TO STOR-KVBEART-Q                    
082100     MOVE SPAR-ARTC-KDERS            TO STOR-KDERS                        
082200     MOVE CLAG-KDVVKL                TO STOR-KDVVKL                       
082300     MOVE CLAG-KVPB-SEP              TO STOR-KVPB-SEP                     
082400     MOVE CLAG-KVSLUTKP              TO STOR-KVSLUTKP                     
082500     MOVE RANS-RERF-ART-UT           TO STOR-RERF-ART                     
082600     MOVE ART-KDPRODSL               TO STOR-KDPRODSL                     
082700     .                                                                    
082800     EJECT                                                                
082900                                                                          
083000 BH-BER-DISP-SDC SECTION.                                                 
083100     MOVE 'H'                   TO WS-TEXT3                               
083200                                                                          
083300     IF ARTS-STATUS = NEJ                                                 
083400       MOVE '15'              TO WS-KDORDBEK                              
083500       IF WS-GMT-IDDC(2) = SPACE OR WC-CDC-SE                             
083600         MOVE 'S'             TO WS-TEXT2                                 
083700         MOVE WC-CDC-SE       TO WS-IDDC                                  
083800       ELSE                                                               
083900         MOVE JA              TO CLEAR-SW                                 
084000       END-IF                                                             
084100     ELSE                                                                 
084200       MOVE NEJ                   TO CLEAR-SW                             
084300       IF SLAG-KVLS < +0                                                  
084400         MOVE +0                  TO SLAG-KVLS                            
084500       END-IF                                                             
084600       IF SLAG-KVOKS-DAG < +0                                             
084700         MOVE +0                  TO SLAG-KVOKS-DAG                       
084800       END-IF                                                             
084900       COMPUTE W-DISP =   SLAG-KVLS                                       
085000                        + SLAG-KVAKS-SDC                                  
085100                        - SLAG-KVOKS-DAG                                  
085200                        - SLAG-KVUTRS                                     
085300* ** JUSTERING 041007  GK                                                 
085400                        - SLAG-KVOKS-BULK                                 
085500                        - SLAG-KVSPARR-KVAL                               
085600*  ** SLUT JUSTERING                                                      
085700       IF W-DISP < ZERO                                                   
085800         MOVE +0                  TO W-DISP                               
085900       END-IF                                                             
086000                                                                          
086100       IF WS-KVBEART > W-DISP OR SLAG-KDLEVSP > +0                        
086200         IF WS-ERS-KDORDBEK = SPACE                                       
086300           MOVE '15'              TO WS-KDORDBEK                          
086400           IF WS-GMT-IDDC(2) = SPACE OR WC-CDC-SE                         
086500             MOVE 'S'             TO WS-TEXT2                             
086600             MOVE WC-CDC-SE       TO WS-IDDC                              
086700           ELSE                                                           
086800             MOVE JA              TO CLEAR-SW                             
086900           END-IF                                                         
087000         END-IF                                                           
087100       ELSE                                                               
087200         MOVE '1'                 TO WS-TEXT2                             
087300         MOVE    '10'             TO WS-KDORDBEK                          
087400*              010 = HAN FICK                                             
087500         MOVE    WS-KVBEART       TO W-DISP-NUM                           
087600       END-IF                                                             
087700     END-IF                                                               
087800     .                                                                    
087900     EJECT                                                                
088000 BI-BER-DISP-DC2 SECTION.                                                 
088100     MOVE 'I'                   TO WS-TEXT3                               
088200                                                                          
088300     MOVE NEJ                   TO CLEAR-SW                               
088400     MOVE WS-GMT-IDDC(2)        TO W-IDDC                                 
088500                                   WS-IDDC                                
088600                                                                          
088700     PERFORM IMS-GU-ARTS11                                                
088800     IF SEGMENT-FINNS                                                     
088900                                                                          
089000       IF SLAG-KVLS < +0                                                  
089100         MOVE +0                TO SLAG-KVLS                              
089200       END-IF                                                             
089300       IF SLAG-KVOKS-DAG < +0                                             
089400         MOVE +0                TO SLAG-KVOKS-DAG                         
089500       END-IF                                                             
089600       COMPUTE W-DISP = SLAG-KVLS                                         
089700                        + SLAG-KVAKS-SDC                                  
089800                        - SLAG-KVOKS-DAG                                  
089900                        - SLAG-KVUTRS                                     
090000*  ** JUSTERING 041007  GK                                                
090100                        - SLAG-KVOKS-BULK                                 
090200                        - SLAG-KVSPARR-KVAL                               
090300*  ** SLUT JUSTERING                                                      
090400       IF W-DISP < ZERO                                                   
090500         MOVE +0                TO W-DISP                                 
090600       END-IF                                                             
090700     END-IF                                                               
090800                                                                          
090900     IF SEGMENT-SAKNAS                                                    
091000     OR WS-KVBEART   > W-DISP                                             
091100     OR SLAG-KDLEVSP > +0                                                 
091200       IF WS-GMT-IDDC(3) = SPACE OR WC-CDC-SE                             
091300         MOVE 'S'               TO WS-TEXT2                               
091400         MOVE WC-CDC-SE         TO WS-IDDC                                
091500       ELSE                                                               
091600         MOVE JA                TO CLEAR-SW                               
091700       END-IF                                                             
091800     ELSE                                                                 
091900*......ANTAL FINNS PÅ NIVÅ2                                               
092000       MOVE '2'                 TO WS-TEXT2                               
092100       MOVE WS-KVBEART          TO W-DISP-NUM                             
092200     END-IF                                                               
092300     .                                                                    
092400     EJECT                                                                
092500 BJ-BER-DISP-DC3 SECTION.                                                 
092600     MOVE 'J'                 TO WS-TEXT3                                 
092700                                                                          
092800     MOVE WS-GMT-IDDC(3)        TO W-IDDC                                 
092900                                   WS-IDDC                                
093000                                                                          
093100     PERFORM IMS-GU-ARTS11                                                
093200     IF SEGMENT-FINNS                                                     
093300                                                                          
093400       IF SLAG-KVLS < +0                                                  
093500         MOVE +0                TO SLAG-KVLS                              
093600       END-IF                                                             
093700       IF SLAG-KVOKS-DAG < +0                                             
093800         MOVE +0                TO SLAG-KVOKS-DAG                         
093900       END-IF                                                             
094000       COMPUTE W-DISP = SLAG-KVLS                                         
094100                        + SLAG-KVAKS-SDC                                  
094200                        - SLAG-KVOKS-DAG                                  
094300                        - SLAG-KVUTRS                                     
094400*  ** JUSTERING 041007  GK                                                
094500                        - SLAG-KVOKS-BULK                                 
094600                        - SLAG-KVSPARR-KVAL                               
094700*  ** SLUT JUSTERING                                                      
094800       IF W-DISP < ZERO                                                   
094900         MOVE +0                TO W-DISP                                 
095000       END-IF                                                             
095100     END-IF                                                               
095200                                                                          
095300     IF SEGMENT-SAKNAS                                                    
095400     OR WS-KVBEART   > W-DISP                                             
095500     OR SLAG-KDLEVSP > +0                                                 
095600       MOVE 'S'                 TO WS-TEXT2                               
095700       MOVE WC-CDC-SE           TO WS-IDDC                                
095800     ELSE                                                                 
095900*......ANTAL FINNS PÅ NIVÅ 3                                              
096000       MOVE '3'                 TO WS-TEXT2                               
096100       MOVE WS-KVBEART          TO W-DISP-NUM                             
096200     END-IF                                                               
096300     .                                                                    
096400     EJECT                                                                
096500 BK-ALLOC-DC1 SECTION.                                                    
096600     MOVE 'K'                 TO WS-TEXT3                                 
096700                                                                          
096800     IF ARTS-FINNS                                                        
096900       MOVE SLAG-IDDC TO WS-IDDC-RO                                       
097000                         WS-IDDC                                          
097100       IF SLAG-FLORDSP = JA OR SLAG-KDLEVSP > 0                           
097200         IF WS-GMT-IDDC(2) = SPACE                                        
097300           IF WS-ERS-KDORDBEK = SPACE                                     
097400             MOVE '90'          TO WS-KDORDBEK                            
097500           END-IF                                                         
097600         ELSE                                                             
097700           MOVE JA              TO CLEAR-SW                               
097800           MOVE '15'            TO WS-KDORDBEK                            
097900         END-IF                                                           
098000       ELSE                                                               
098100         PERFORM BKA-KOLLA-DISP-DC1                                       
098200       END-IF                                                             
098300     ELSE                                                                 
098400       IF WS-GMT-IDDC(2) = SPACE                                          
098500         IF WS-ERS-KDORDBEK = SPACE                                       
098600           MOVE '55'            TO WS-KDORDBEK                            
098700           MOVE WS-GMT-IDDC(1) TO WS-IDDC                                 
098800         END-IF                                                           
098900       ELSE                                                               
099000         MOVE JA TO CLEAR-SW                                              
099100         MOVE '15'              TO WS-KDORDBEK                            
099200       END-IF                                                             
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700 BKA-KOLLA-DISP-DC1 SECTION.                                              
099800                                                                          
099900     PERFORM S01-NOLLA-EV-MINUS-SALDON                                    
100000     COMPUTE W-DISP = W-KVLS                                              
100100                    - W-KVOKS-DAG                                         
100200     IF W-DISP > ZERO                                                     
100300       COMPUTE W-DISP = W-DISP                                            
100400                      - W-KVRESS                                          
100500                      - W-KVUTRS                                          
100600                      - W-KVSPARR-KVAL                                    
100700                                                                          
100800     END-IF                                                               
100900     IF W-DISP < ZERO                                                     
101000       MOVE ZERO              TO W-DISP                                   
101100     END-IF                                                               
101200                                                                          
101300     IF WS-KVBEART > W-DISP                                               
101400       IF WS-GMT-IDDC(2) = SPACE                                          
101500         IF WS-ERS-KDORDBEK = SPACE                                       
101600           MOVE '90'          TO WS-KDORDBEK                              
101700           MOVE W-DISP        TO W-DISP-NUM                               
101800         END-IF                                                           
101900       ELSE                                                               
102000         MOVE JA              TO CLEAR-SW                                 
102100         MOVE '15'            TO WS-KDORDBEK                              
102200         MOVE W-DISP          TO W-DISP-HOME                              
102300       END-IF                                                             
102400     ELSE                                                                 
102500       MOVE '1'               TO WS-TEXT2                                 
102600       MOVE '10'              TO WS-KDORDBEK                              
102700       MOVE WS-KVBEART        TO W-DISP-NUM                               
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200 BL-ALLOC-DC2 SECTION.                                                    
103300     MOVE 'L'                 TO WS-TEXT3                                 
103400                                                                          
103500     MOVE NEJ TO CLEAR-SW                                                 
103600     MOVE WS-GMT-IDDC(2)     TO W-IDDC                                    
103700                                WS-IDDC                                   
103800                                                                          
103900     PERFORM IMS-GU-ARTS11                                                
104000     IF SEGMENT-FINNS                                                     
104100       IF WS-IDDC-RO = SPACE                                              
104200         MOVE SLAG-IDDC TO WS-IDDC-RO                                     
104300       END-IF                                                             
104400       IF SLAG-FLORDSP = JA OR SLAG-KDLEVSP > 0                           
104500          IF WS-GMT-IDDC(3) = SPACE                                       
104600            IF WS-ERS-KDORDBEK = SPACE                                    
104700              MOVE '90'      TO WS-KDORDBEK                               
104800              MOVE W-DISP-HOME TO W-DISP-NUM                              
104900              MOVE WS-IDDC-RO TO WS-IDDC                                  
105000            ELSE                                                          
105100              MOVE WS-ERS-KDORDBEK TO WS-KDORDBEK                         
105200              MOVE WS-GMT-IDDC(1) TO WS-IDDC                              
105300            END-IF                                                        
105400          ELSE                                                            
105500            MOVE JA          TO CLEAR-SW                                  
105600          END-IF                                                          
105700       ELSE                                                               
105800         PERFORM BLA-KOLLA-DISP-DC2                                       
105900       END-IF                                                             
106000     ELSE                                                                 
106100       IF WS-GMT-IDDC(3) = SPACE                                          
106200         IF WS-ERS-KDORDBEK = SPACE                                       
106300           IF WS-IDDC-RO = SPACE                                          
106400             MOVE '55'           TO WS-KDORDBEK                           
106500             MOVE WS-GMT-IDDC(1) TO WS-IDDC                               
106600           ELSE                                                           
106700             MOVE '90'           TO WS-KDORDBEK                           
106800             MOVE W-DISP-HOME    TO W-DISP-NUM                            
106900             MOVE WS-IDDC-RO     TO WS-IDDC                               
107000           END-IF                                                         
107100         ELSE                                                             
107200           MOVE WS-ERS-KDORDBEK  TO WS-KDORDBEK                           
107300           MOVE WS-GMT-IDDC(1) TO WS-IDDC                                 
107400         END-IF                                                           
107500       ELSE                                                               
107600         MOVE JA                 TO CLEAR-SW                              
107700       END-IF                                                             
107800     END-IF                                                               
107900     .                                                                    
108000     EJECT                                                                
108100 BLA-KOLLA-DISP-DC2 SECTION.                                              
108200                                                                          
108300     PERFORM S01-NOLLA-EV-MINUS-SALDON                                    
108400     COMPUTE W-DISP = W-KVLS                                              
108500                    - W-KVOKS-DAG                                         
108600     IF W-DISP > ZERO                                                     
108700       COMPUTE W-DISP = W-DISP                                            
108800                      - W-KVRESS                                          
108900                      - W-KVUTRS                                          
109000                      - W-KVSPARR-KVAL                                    
109100                                                                          
109200     END-IF                                                               
109300     IF W-DISP < ZERO                                                     
109400       MOVE ZERO              TO W-DISP                                   
109500     END-IF                                                               
109600                                                                          
109700     IF WS-KVBEART > W-DISP                                               
109800       IF WS-GMT-IDDC(3) = SPACE                                          
109900         IF WS-ERS-KDORDBEK = SPACE                                       
110000           MOVE '90'          TO WS-KDORDBEK                              
110100           IF WS-IDDC-RO = WS-IDDC                                        
110200             MOVE W-DISP      TO W-DISP-NUM                               
110300           ELSE                                                           
110400             MOVE W-DISP-HOME TO W-DISP-NUM                               
110500           END-IF                                                         
110600           MOVE WS-IDDC-RO    TO WS-IDDC                                  
110700         ELSE                                                             
110800           MOVE WS-ERS-KDORDBEK  TO WS-KDORDBEK                           
110900           MOVE WS-GMT-IDDC(1) TO WS-IDDC                                 
111000         END-IF                                                           
111100       ELSE                                                               
111200         MOVE JA              TO CLEAR-SW                                 
111300         IF WS-IDDC-RO = WS-IDDC                                          
111400           MOVE W-DISP        TO W-DISP-HOME                              
111500         END-IF                                                           
111600       END-IF                                                             
111700     ELSE                                                                 
111800       MOVE WS-KVBEART        TO W-DISP-NUM                               
111900       MOVE '2'               TO WS-TEXT2                                 
112000     END-IF                                                               
112100     .                                                                    
112200     EJECT                                                                
112300                                                                          
112400 BM-ALLOC-DC3 SECTION.                                                    
112500     MOVE 'M'                 TO WS-TEXT3                                 
112600                                                                          
112700     MOVE WS-GMT-IDDC(3)     TO W-IDDC                                    
112800                                WS-IDDC                                   
112900                                                                          
113000     PERFORM IMS-GU-ARTS11                                                
113100     IF SEGMENT-FINNS                                                     
113200       IF WS-IDDC-RO = SPACE                                              
113300         MOVE SLAG-IDDC  TO WS-IDDC-RO                                    
113400       END-IF                                                             
113500       IF SLAG-FLORDSP = JA OR SLAG-KDLEVSP > 0                           
113600         IF WS-ERS-KDORDBEK = SPACE                                       
113700           MOVE '90'         TO WS-KDORDBEK                               
113800           MOVE W-DISP-HOME  TO W-DISP-NUM                                
113900           MOVE WS-IDDC-RO   TO WS-IDDC                                   
114000         ELSE                                                             
114100           MOVE WS-ERS-KDORDBEK  TO WS-KDORDBEK                           
114200           MOVE WS-GMT-IDDC(1) TO WS-IDDC                                 
114300         END-IF                                                           
114400       ELSE                                                               
114500         PERFORM BMA-KOLLA-DISP-DC3                                       
114600       END-IF                                                             
114700     ELSE                                                                 
114800       IF WS-ERS-KDORDBEK = SPACE                                         
114900         IF WS-IDDC-RO = SPACE                                            
115000           MOVE '55'          TO WS-KDORDBEK                              
115100           MOVE WS-GMT-IDDC(1) TO WS-IDDC                                 
115200         ELSE                                                             
115300           IF SPAR-ARTC-KDERS = 0                                         
115400             MOVE '90'        TO WS-KDORDBEK                              
115500             MOVE W-DISP-HOME TO W-DISP-NUM                               
115600             MOVE WS-IDDC-RO  TO WS-IDDC                                  
115700           END-IF                                                         
115800         END-IF                                                           
115900       ELSE                                                               
116000         MOVE WS-ERS-KDORDBEK TO WS-KDORDBEK                              
116100         MOVE WS-GMT-IDDC(1) TO WS-IDDC                                   
116200       END-IF                                                             
116300     END-IF                                                               
116400     .                                                                    
116500     EJECT                                                                
116600                                                                          
116700 BMA-KOLLA-DISP-DC3 SECTION.                                              
116800                                                                          
116900     PERFORM S01-NOLLA-EV-MINUS-SALDON                                    
117000     COMPUTE W-DISP = W-KVLS                                              
117100                    - W-KVOKS-DAG                                         
117200     IF W-DISP > ZERO                                                     
117300       COMPUTE W-DISP = W-DISP                                            
117400                      - W-KVRESS                                          
117500                      - W-KVUTRS                                          
117600                      - W-KVSPARR-KVAL                                    
117700     END-IF                                                               
117800     IF W-DISP < ZERO                                                     
117900       MOVE ZERO              TO W-DISP                                   
118000     END-IF                                                               
118100                                                                          
118200     IF WS-KVBEART > W-DISP                                               
118300       IF WS-ERS-KDORDBEK = SPACE                                         
118400         MOVE '90'            TO WS-KDORDBEK                              
118500         IF WS-IDDC-RO = WS-IDDC                                          
118600           MOVE W-DISP        TO W-DISP-NUM                               
118700         ELSE                                                             
118800           MOVE W-DISP-HOME   TO W-DISP-NUM                               
118900         END-IF                                                           
119000         MOVE WS-IDDC-RO      TO WS-IDDC                                  
119100       ELSE                                                               
119200         MOVE WS-ERS-KDORDBEK TO WS-KDORDBEK                              
119300         MOVE WS-GMT-IDDC(1) TO WS-IDDC                                   
119400       END-IF                                                             
119500     ELSE                                                                 
119600       MOVE WS-KVBEART        TO W-DISP-NUM                               
119700       MOVE '3'               TO WS-TEXT2                                 
119800     END-IF                                                               
119900     .                                                                    
120000     EJECT                                                                
120100                                                                          
120200 BZ-FLYTTA-TILL-MOD SECTION.                                              
120300*    MOVE 'Z'                 TO WS-TEXT3                                 
120400                                                                          
120500     IF SPAR-KDORDBEK = 67                                                
120600         MOVE '67'             TO MOD-KDORDBEK                            
120700     ELSE                                                                 
120800         MOVE WS-KDORDBEK      TO MOD-KDORDBEK                            
120900     END-IF                                                               
121000     IF CDC                                                               
121100       MOVE WS-KVAVBART-UDEC   TO MOD-KVAVBART                            
121200     ELSE                                                                 
121300       MOVE W-DISP-NUM         TO MOD-KVAVBART                            
121400     END-IF                                                               
121500     MOVE WS-IDDC              TO MOD-IDDC                                
121600     IF DLEV-FLSDCLEV-UT = 'J'                                            
121700       MOVE '00'               TO MOD-KVFRYSTI                            
121800     ELSE                                                                 
121900       MOVE CLAG-KVFRYSTI      TO WS-KVFRYSTI                             
122000       MOVE WS-KVFRYSTI        TO MOD-KVFRYSTI                            
122100     END-IF                                                               
122200     MOVE CLAG-FLTPO1          TO MOD-FLTPO1                              
122300     IF WS-KDORDBEK = '70' OR '90'                                        
122400       MOVE WS-IDDC TO W-IDDC-B6                                          
122500       PERFORM IMS-GU-WDB601                                              
122600       IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                                
122700         MOVE CLAG-TIDISPIN         TO WS-TIDISPIN                        
122800         MOVE WS-TIDISPIN           TO MOD-TIDISPIN                       
122900       ELSE                                                               
123000         IF DCS-NDC                                                       
123100           PERFORM BZA-HAEMTA-TIBERANK                                    
123200           IF ETA-SVAR-OK = JA                                            
123300             IF DCS-NDC-NA OR DCS-NDC-CN                                  
123400               MOVE ETA-TIAAMMDD-SVAR TO MOD-TIDISPIN                     
123500             ELSE                                                         
123600               IF ETA-KVAVIS-ETA > +0                                     
123700                 MOVE ETA-TIAAMMDD-SVAR TO MOD-TIDISPIN                   
123800               ELSE                                                       
123900                 MOVE ZERO          TO MOD-TIDISPIN                       
124000               END-IF                                                     
124100             END-IF                                                       
124200           ELSE                                                           
124300             MOVE ZERO              TO MOD-TIDISPIN                       
124400           END-IF                                                         
124500         END-IF                                                           
124600       END-IF                                                             
124700     ELSE                                                                 
124800       MOVE ZERO                    TO MOD-TIDISPIN                       
124900     END-IF                                                               
125000* FIX SW-ARTIKLAR TILLS MAN SKRIVER OM PGM:ET                             
125100     IF ART-KDSORT = 'SW'                                                 
125200       IF MOD-KDORDBEK = '67'                                             
125300          CONTINUE                                                        
125400       ELSE                                                               
125500          MOVE '10'                 TO MOD-KDORDBEK                       
125600       END-IF                                                             
125700       MOVE DAGENS-DATUM            TO MOD-TIDISPIN                       
125800       MOVE NEJ                     TO MOD-FLTPO1                         
125900       MOVE '00'                    TO MOD-KVFRYSTI                       
126000       MOVE WS-KVBEART              TO MOD-KVAVBART                       
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400 BZA-HAEMTA-TIBERANK SECTION.                                             
126500                                                                          
126600     MOVE '612'                TO ETA-KDCALL                              
126700     MOVE WS-GMT-IDDC(1)       TO ETA-IDDC-REC                            
126800     MOVE MID-IDARTNR          TO ETA-IDARTNR                             
126900     MOVE SLAG-IDLEVNR         TO ETA-IDLEVNR                             
127000     MOVE ZERO                 TO ETA-KDFRAKT                             
127100     ACCEPT WS-ETA-DATUM     FROM DATE                                    
127200     MOVE WS-ETA-DATUM         TO ETA-TIAAMMDD-ANROP                      
127300     IF WS-ETA-DATUM-AAR > 50                                             
127400        MOVE 19                TO ETA-TISEKEL-ANROP                       
127500     ELSE                                                                 
127600        MOVE 20                TO ETA-TISEKEL-ANROP                       
127700     END-IF                                                               
127800                                                                          
127900     CALL W218ETA  USING ETA-W218LETA                                     
128000                         ETA-ARTC-PCB ETA-WDK7-PCB                        
128100                         ETA-INLC-PCB ETA-LEVA-PCB                        
128200                         ETA-WDB6-PCB ETA-WDD9-PCB                        
128300     .                                                                    
128400                                                                          
128500     EJECT                                                                
128600                                                                          
128700 S01-NOLLA-EV-MINUS-SALDON SECTION.                                       
128800                                                                          
128900     IF SLAG-KVLS < +0                                                    
129000       MOVE +0                 TO W-KVLS                                  
129100     ELSE                                                                 
129200       MOVE SLAG-KVLS          TO W-KVLS                                  
129300     END-IF                                                               
129400                                                                          
129500     IF SLAG-KVOKS-DAG < +0                                               
129600       MOVE +0                 TO W-KVOKS-DAG                             
129700     ELSE                                                                 
129800       MOVE SLAG-KVOKS-DAG     TO W-KVOKS-DAG                             
129900     END-IF                                                               
130000                                                                          
130100     IF SLAG-KVRESS < +0                                                  
130200       MOVE +0                 TO W-KVRESS                                
130300     ELSE                                                                 
130400       MOVE SLAG-KVRESS        TO W-KVRESS                                
130500     END-IF                                                               
130600                                                                          
130700     IF SLAG-KVUTRS < +0                                                  
130800       MOVE +0                 TO W-KVUTRS                                
130900     ELSE                                                                 
131000       MOVE SLAG-KVUTRS        TO W-KVUTRS                                
131100     END-IF                                                               
131200                                                                          
131300     IF SLAG-KVSPARR-KVAL < +0                                            
131400       MOVE +0                 TO W-KVSPARR-KVAL                          
131500     ELSE                                                                 
131600       MOVE SLAG-KVSPARR-KVAL  TO W-KVSPARR-KVAL                          
131700     END-IF                                                               
131800     .                                                                    
131900     EJECT                                                                
132000 S05-RANSONERA SECTION.                                                   
132100     MOVE '5'                 TO WS-TEXT3                                 
132200                                                                          
132300     MOVE SPACE                TO RANS-BERADREF                           
132400     MOVE NEJ                  TO RANS-FLFORBI                            
132500                                  RANS-FLORDSPE                           
132600                                  RANS-FLOVRLEV                           
132700                                  RANS-FLEMBORD                           
132800     MOVE +0                   TO RANS-IDKAMPRF                           
132900     MOVE MID-IDARTNR          TO RANS-IDARTNR                            
133000     MOVE DLEV-IDLEVNR-UT      TO RANS-IDLEVNR                            
133100     MOVE WS-IDRFTAB           TO RANS-IDRFTAB                            
133200     MOVE +0                   TO RANS-TIRODAT                            
133300     MOVE +1                   TO RANS-KDORDBEH                           
133400     MOVE MID-KDORDKL          TO RANS-KDORDKL                            
133500     MOVE MID-KVBEART          TO RANS-KVBEART-Q                          
133600     MOVE +0                   TO RANS-KDTPOTYP                           
133700     MOVE +0                   TO RANS-KDERS                              
133800     MOVE CLAG-KVLS            TO RANS-KVLS                               
133900     MOVE CLAG-KVPB-SATS       TO RANS-KVPB-SATS                          
134000     MOVE CLAG-KVPB-SEP        TO RANS-KVPB-SEP                           
134100     MOVE SPAR-REDIRLEV        TO RANS-REDIRLEV                           
134200     MOVE CLAG-KVRESS          TO RANS-KVRESS                             
134300     MOVE CLAG-KVSPANT         TO RANS-KVSPANT                            
134400     MOVE CLAG-KVUTRS          TO RANS-KVUTRS                             
134500     MOVE CLAG-TIDISPIN        TO RANS-TIDISPIN                           
134600                                                                          
134700     MOVE ART-KDPRODSL         TO TEST-KDPRODSL                           
134800     IF KDPRODSL-BIMA                                                     
134900       MOVE 1                  TO RANS-RERF-ART-UT                        
135000                                  RANS-RERF-RAD-UT                        
135100       MOVE ZERO               TO RANS-SUTPO-PB-UT                        
135200                                  RANS-SUTPO-EJPB-UT                      
135300     ELSE                                                                 
135400       CALL W411RANS USING RANS-W411RANS XXKM-PCB                         
135500                           RANS-ARTM-PCB RANS-ARTS-PCB                    
135600     END-IF                                                               
135700     .                                                                    
135800     EJECT                                                                
135900                                                                          
136000 S06-BER-DISP-CDC SECTION.                                                
136100     MOVE '6'                 TO WS-TEXT3                                 
136200                                                                          
136300     COMPUTE WS-KVAVBART-MDEC = WS-KVBEART * RANS-RERF-RAD-UT             
136400     IF WS-KVAVBART-MDEC <   +1 AND                                       
136500       RANS-RERF-RAD-UT > ZERO                                            
136600       MOVE +1               TO WS-KVAVBART-UDEC                          
136700     ELSE                                                                 
136800       MOVE WS-KVAVBART-MDEC TO WS-KVAVBART-UDEC                          
136900     END-IF                                                               
137000                                                                          
137100     IF WS-KVAVBART-UDEC < WS-KVBEART                                     
137200* RANSONERAT DISPONIBELT < BESTÄLLT                                       
137300       MOVE '90'             TO WS-KDORDBEK                               
137400*            080 = HAN FICK INGET                                         
137500     ELSE                                                                 
137600       IF WS-KDORDBEK NOT = '15'                                          
137700         MOVE '10'           TO WS-KDORDBEK                               
137800*            010 = HAN FICK                                               
137900       END-IF                                                             
138000       MOVE 'C'              TO WS-TEXT2                                  
138100       MOVE WS-KVBEART       TO WS-KVAVBART-UDEC                          
138200     END-IF                                                               
138300     .                                                                    
138400     EJECT                                                                
138500                                                                          
138600 S10-CALL-W411SPAR SECTION.                                               
138700                                                                          
138800     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
138900                                       SPAR-WDF8A-PCB                     
139000                                       SPAR-WDK6-PCB                      
139100     .                                                                    
139200     EJECT                                                                
139300                                                                          
139400 S11-CALL-W411DLEV SECTION.                                               
139500                                                                          
139600     MOVE ZERO     TO DLEV-KDCALL                                         
139700                      DLEV-IDKUNDRF-IN                                    
139800                                                                          
139900     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
140000                                       DLEV-LEVG-PCB                      
140100                                       DLEV-LEVA-PCB                      
140200                                       DLEV-ARTS-PCB                      
140300                                       DLEV-WDB6-PCB                      
140400                                       DUMMY-PCB                          
140500     .                                                                    
140600     EJECT                                                                
140700                                                                          
140800 S13-CALL-W411STOR SECTION.                                               
140900                                                                          
141000     CALL W411STOR USING STOR-W411STOR                                    
141100     .                                                                    
141200     EJECT                                                                
141300                                                                          
141400 S15-KOLLA-LAGERSALDO SECTION.                                            
141500                                                                          
141600*---KOLLAR ATT NDC LAGRET ÄR TÖMT PÅ ARTIKELN              ---            
141700*---FÖR USA + CN  SKA ALLA SEX DC:N VARA TOMMA PÅ ARTIKELN ---            
141800*---FÖR ERSÄTTNINGSKODER ÖVER 20 SÅ ÄR CDC-LAGRET TOMT     ---            
141900     IF NDC-US                                                            
142000       MOVE WC-NDC-US-RU        TO WS-IDDC                                
142100                                   W-IDDC                                 
142200       PERFORM IMS-GU-ARTS11                                              
142300       IF SEGMENT-FINNS                                                   
142400         PERFORM S16-KOLLA-NDC-LAGER-SALDO                                
142500         MOVE W-DISP            TO W-DISP-1                               
142600       ELSE                                                               
142700         MOVE 0                 TO W-DISP-1                               
142800       END-IF                                                             
142900                                                                          
143000       MOVE WC-NDC-US-LA        TO WS-IDDC                                
143100                                   W-IDDC                                 
143200       PERFORM IMS-GU-ARTS11                                              
143300       IF SEGMENT-FINNS                                                   
143400         PERFORM S16-KOLLA-NDC-LAGER-SALDO                                
143500         MOVE W-DISP            TO W-DISP-2                               
143600       ELSE                                                               
143700         MOVE 0                 TO W-DISP-2                               
143800       END-IF                                                             
143900                                                                          
144000       MOVE WC-NDC-US-SE        TO WS-IDDC                                
144100                                   W-IDDC                                 
144200       PERFORM IMS-GU-ARTS11                                              
144300       IF SEGMENT-FINNS                                                   
144400         PERFORM S16-KOLLA-NDC-LAGER-SALDO                                
144500         MOVE W-DISP            TO W-DISP-3                               
144600       ELSE                                                               
144700         MOVE 0                 TO W-DISP-3                               
144800       END-IF                                                             
144900                                                                          
145000       MOVE WC-NDC-US-CH        TO WS-IDDC                                
145100                                   W-IDDC                                 
145200       PERFORM IMS-GU-ARTS11                                              
145300       IF SEGMENT-FINNS                                                   
145400         PERFORM S16-KOLLA-NDC-LAGER-SALDO                                
145500         MOVE W-DISP            TO W-DISP-4                               
145600       ELSE                                                               
145700         MOVE 0                 TO W-DISP-4                               
145800       END-IF                                                             
145900                                                                          
146000       MOVE WC-NDC-US-JA        TO WS-IDDC                                
146100                                   W-IDDC                                 
146200       PERFORM IMS-GU-ARTS11                                              
146300       IF SEGMENT-FINNS                                                   
146400         PERFORM S16-KOLLA-NDC-LAGER-SALDO                                
146500         MOVE W-DISP            TO W-DISP-5                               
146600       ELSE                                                               
146700         MOVE 0                 TO W-DISP-5                               
146800       END-IF                                                             
146900                                                                          
147000       MOVE WC-NDC-US-DA        TO WS-IDDC                                
147100                                   W-IDDC                                 
147200       PERFORM IMS-GU-ARTS11                                              
147300       IF SEGMENT-FINNS                                                   
147400         PERFORM S16-KOLLA-NDC-LAGER-SALDO                                
147500         MOVE W-DISP            TO W-DISP-6                               
147600       ELSE                                                               
147700         MOVE 0                 TO W-DISP-6                               
147800       END-IF                                                             
147900                                                                          
148000       COMPUTE W-DISP-TOT = W-DISP-1                                      
148100                          + W-DISP-2                                      
148200                          + W-DISP-3                                      
148300                          + W-DISP-4                                      
148400                          + W-DISP-5                                      
148500                          + W-DISP-6                                      
148600       IF W-DISP-TOT > 0                                                  
148700         MOVE W-DISP-TOT        TO W-KVLS                                 
148800       ELSE                                                               
148900         MOVE 0                 TO W-KVLS                                 
149000       END-IF                                                             
149100     ELSE                                                                 
149200       IF NDC-CN                                                          
149300         MOVE WC-NDC-CN-71        TO WS-IDDC                              
149400                                     W-IDDC                               
149500         PERFORM IMS-GU-ARTS11                                            
149600         IF SEGMENT-FINNS                                                 
149700           PERFORM S16-KOLLA-NDC-LAGER-SALDO                              
149800           MOVE W-DISP            TO W-DISP-1                             
149900         ELSE                                                             
150000           MOVE 0                 TO W-DISP-1                             
150100         END-IF                                                           
150200                                                                          
150300         MOVE WC-NDC-CN-72        TO WS-IDDC                              
150400                                     W-IDDC                               
150500         PERFORM IMS-GU-ARTS11                                            
150600         IF SEGMENT-FINNS                                                 
150700           PERFORM S16-KOLLA-NDC-LAGER-SALDO                              
150800           MOVE W-DISP            TO W-DISP-2                             
150900         ELSE                                                             
151000             MOVE 0               TO W-DISP-2                             
151100         END-IF                                                           
151200                                                                          
151300         MOVE WC-NDC-CN-73        TO WS-IDDC                              
151400                                     W-IDDC                               
151500         PERFORM IMS-GU-ARTS11                                            
151600         IF SEGMENT-FINNS                                                 
151700           PERFORM S16-KOLLA-NDC-LAGER-SALDO                              
151800           MOVE W-DISP            TO W-DISP-3                             
151900         ELSE                                                             
152000           MOVE 0                 TO W-DISP-3                             
152100         END-IF                                                           
152200                                                                          
152300         MOVE WC-NDC-CN-74        TO WS-IDDC                              
152400                                     W-IDDC                               
152500         PERFORM IMS-GU-ARTS11                                            
152600         IF SEGMENT-FINNS                                                 
152700           PERFORM S16-KOLLA-NDC-LAGER-SALDO                              
152800           MOVE W-DISP            TO W-DISP-4                             
152900         ELSE                                                             
153000           MOVE 0                 TO W-DISP-4                             
153100         END-IF                                                           
153200                                                                          
153300         COMPUTE W-DISP-TOT = W-DISP-1                                    
153400                            + W-DISP-2                                    
153500                            + W-DISP-3                                    
153600                            + W-DISP-4                                    
153700         IF W-DISP-TOT > 0                                                
153800           MOVE W-DISP-TOT        TO W-KVLS                               
153900         ELSE                                                             
154000           MOVE 0                 TO W-KVLS                               
154100         END-IF                                                           
154200       ELSE                                                               
154300         MOVE WS-GMT-IDDC(1)      TO WS-IDDC                              
154400                                     W-IDDC                               
154500         PERFORM IMS-GU-ARTS11                                            
154600         IF SEGMENT-FINNS                                                 
154700           PERFORM S16-KOLLA-NDC-LAGER-SALDO                              
154800           IF W-DISP > 0                                                  
154900             MOVE W-DISP          TO W-KVLS                               
155000           ELSE                                                           
155100             MOVE 0               TO W-KVLS                               
155200           END-IF                                                         
155300         END-IF                                                           
155400       END-IF                                                             
155500     END-IF                                                               
155600     MOVE WS-GMT-IDDC(1)        TO W-IDDC                                 
155700                                   WS-IDDC                                
155800     PERFORM IMS-GU-ARTS11                                                
155900     .                                                                    
156000     EJECT                                                                
156100 S16-KOLLA-NDC-LAGER-SALDO SECTION.                                       
156200                                                                          
156300     PERFORM S01-NOLLA-EV-MINUS-SALDON                                    
156400     COMPUTE W-DISP = W-KVLS                                              
156500                    - W-KVOKS-DAG                                         
156600                    - W-KVRESS                                            
156700                    - W-KVUTRS                                            
156800                    - W-KVSPARR-KVAL                                      
156900     IF W-DISP < ZERO                                                     
157000       MOVE ZERO                TO W-DISP                                 
157100     END-IF                                                               
157200     .                                                                    
157300     EJECT                                                                
157400                                                                          
157500* IMS SEKTIONER                                                           
157600     SKIP3                                                                
157700 IMS-GET-MSG SECTION.                                                     
157800                                                                          
157900     MOVE '  QC' TO GODK-STATUSKODER                                      
158000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
158100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
158200     PERFORM IMS-STATUSKONTROLL                                           
158300     .                                                                    
158400     SKIP3                                                                
158500 IMS-INSERT-MSG SECTION.                                                  
158600                                                                          
158700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
158800     MOVE SPACE TO GODK-STATUSKODER                                       
158900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
159000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
159100     PERFORM IMS-STATUSKONTROLL                                           
159200     .                                                                    
159300     EJECT                                                                
159400 IMS-GU-ARTC01 SECTION.                                                   
159500                                                                          
159600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
159700            DELIMITED BY SIZE INTO SSA1                                   
159800     MOVE '  GE' TO GODK-STATUSKODER                                      
159900     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-ARTC01 SSA1                   
160000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
160100     PERFORM IMS-STATUSKONTROLL                                           
160200     .                                                                    
160300     SKIP3                                                                
160400 IMS-GNP-ARTC11 SECTION.                                                  
160500                                                                          
160600     MOVE 'WLARTC11' TO SSA1                                              
160700     MOVE '  GE' TO GODK-STATUSKODER                                      
160800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
160900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200     EJECT                                                                
161300 IMS-GU-GMTA01 SECTION.                                                   
161400                                                                          
161500     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
161600            DELIMITED BY SIZE INTO SSA1                                   
161700     MOVE '  GE' TO GODK-STATUSKODER                                      
161800     CALL CBLTDLI USING GU  GMTA-PCB DLI-IO-GMTA01 SSA1                   
161900     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
162000     PERFORM IMS-STATUSKONTROLL                                           
162100     .                                                                    
162200     EJECT                                                                
162300 IMS-GU-LEVG01 SECTION.                                                   
162400                                                                          
162500     STRING 'WLLEVG01(WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
162600                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X ')'                    
162700          DELIMITED BY SIZE INTO SSA1                                     
162800     MOVE '  GE' TO GODK-STATUSKODER                                      
162900     CALL CBLTDLI USING GU LEVG-PCB DLI-IO-WLLEVG01 SSA1                  
163000     MOVE LEVG-STATUS-CODE TO STATUS-WS                                   
163100     PERFORM IMS-STATUSKONTROLL                                           
163200     .                                                                    
163300     EJECT                                                                
163400 IMS-GU-ARTS11 SECTION.                                                   
163500                                                                          
163600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
163700            DELIMITED BY SIZE INTO SSA1                                   
163800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
163900            DELIMITED BY SIZE INTO SSA2                                   
164000     MOVE '  GE' TO GODK-STATUSKODER                                      
164100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2               
164200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     .                                                                    
164500                                                                          
164600 IMS-GU-WDB601    SECTION.                                                
164700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
164800          DELIMITED BY SIZE INTO SSA1                                     
164900     MOVE '    ' TO GODK-STATUSKODER                                      
165000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
165100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
165200     PERFORM IMS-STATUSKONTROLL                                           
165300     .                                                                    
165400     EJECT                                                                
165500 IMS-STATUSKONTROLL SECTION.                                              
165600                                                                          
165700     SET STATUS-IX TO 1                                                   
165800     SEARCH GODK-STATUS                                                   
165900       AT END                                                             
166000         CALL FELLOG                                                      
166100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
166200       CONTINUE                                                           
166300     END-SEARCH                                                           
166400     .                                                                    
