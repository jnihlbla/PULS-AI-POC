000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411CDCA.                                                
000500 AUTHOR.         SVANTE BJÖRKBERG.                                        
000600 DATE-WRITTEN.   JULI -90.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    DETTA ÄR EN SUBMODUL SOM ANROPAS I ORDER-ENTRY OCH                   
001100*    PRELIMINÄRAVBOKAR SALDO PÅ SÅ MÅNGA LAGER SOM BEHÖVS FÖR             
001200*    ATT HELA RADEN SKALL BLI LEVERERAD.                                  
001300*                                                                         
001400*    REGISTER :    WLARTM (WDK9) ARTIKELREGITER                           
001500*                  WLINLB (WDD9) ARTIKELINFOREGITER                       
001600*                                                                         
001700*    LÄNKAREA :    W411CDCA                                               
001800*                                                                         
001900* CHANGE LOG:                                                             
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 01  IDPGM                          PIC X(08) VALUE 'W411CDCA'.           
002900 01  JA                             PIC X     VALUE 'J'.                  
003000 01  NEJ                            PIC X     VALUE 'N'.                  
003100 01  SPEC-FORBI                     PIC X     VALUE 'S'.                  
003200 01  ERROR-TEXT                     PIC X(80) VALUE SPACE.                
003300 01  RKOD-ABEND                     PIC S9(4) VALUE +33 COMP SYNC.        
003400 01  FILLER                         PIC X(8)  VALUE 'AAAAAAAA'.           
003500 01  WS-DISP-AKT                    PIC S9(8).                            
003510 01  WS-DISP-KVANT                  PIC S9(8).                            
003600 01  WS-DISP-C1                     PIC S9(8).                            
003700 01  WS-AVBOKNINGS-DIFF             PIC S9(8).                            
003800 01  WS-FLRES                       PIC X.                                
003900 01  WS-KDKNOE                      PIC 9.                                
004000 01  FILLER                         PIC X(8)  VALUE 'BBBBBBBB'.           
004100 01  WS-KDORDBEK                    PIC 9(2).                             
004200 01  WS-KVBEART                     PIC S9(8)V9(5).                       
004300 01  WS-KVBEART-UTAN-DECIMALER      PIC S9(8).                            
004400 01  WS-KVBEART-REST                PIC S9(8).                            
004500 01  WS-KVBEART-TEMP                PIC S9(8)V9(5).                       
004600 01  WS-KVSLATT-SKALL-BERAKNAS      PIC X.                                
004700 01  WS-KVSLATT-TEMP                PIC S9(8)V9(5).                       
004800 01  WS-KVPRERO-VOR                 PIC S9(8).                            
004900 01  WS-BREST-NOLL-PUBV-OK          PIC X.                                
005000 01  WS-OKS-SKALL-UPPDATERAS        PIC X.                                
005100 01  WS-ORDBEK-99-SKALL-SATTAS      PIC X.                                
005200 01  WS-PRELAVB-SKALL-UTFORAS       PIC X.                                
005300 01  WS-RAD-HELT-ANNULLERAD         PIC X.                                
005400 01  WS-PRERO-FOR-VOR-SKALL-SATTAS  PIC X.                                
005500 01  WS-PRERO-FOR-VOR-AR-SATT       PIC X.                                
005600 01  WS-RERF-RAD-OMRAKNAD           PIC X.                                
005700 01  WS-SUDISP-KNO0                 PIC S9(8).                            
005800 01  WS-SUDISP-KNO1                 PIC S9(8).                            
005900 01  WS-SUDISP-KNO2                 PIC S9(8).                            
006000                                                                          
006100 01  FILLER                         PIC X(8)  VALUE 'CCCCCCCC'.           
006200 01  WS-DISP                        PIC S9(8).                            
006300 01  WS-DISP-KNO0                   PIC S9(8).                            
006400 01  WS-DISP-KNO1                   PIC S9(8).                            
006500 01  WS-DISP-KNO2                   PIC S9(8).                            
006600     EJECT                                                                
006700 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
006800*01  FILLER -COPY WWDIST18 -RED TEST-IDDISTR.                             
006900     EJECT                                                                
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007400     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
007500     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
007600     EJECT                                                                
007700                                                                          
007800*    --- ARBETS-AREOR TILL GENERELLA SUBPROGRAM                           
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'W411KVAN'.            
008100*   -COPY W411KVAN                                                        
008200     EJECT                                                                
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'W411ARTM'.            
008500*   -COPY W411ARTM                                                        
008600     EJECT                                                                
008700                                                                          
008800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009600     SKIP2                                                                
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP2                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200     EJECT                                                                
010300                                                                          
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700                                                                          
010800 01  NYCKLAR-TILL-DLI.                                                    
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC  S9(9)  COMP-3.                      
011100     03  W-WDD901KY-X.                                                    
011110         05  W-IDARTNR-D9        PIC  S9(9)  COMP-3.                      
011120         05  W-IDDC-D9           PIC  X(2).                               
011200     EJECT                                                                
011300                                                                          
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K901'.         
011700 01  DLI-IO-K901.                                                         
011800*    03  -COPY WDK901.                                                    
011900     EJECT                                                                
011901 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-D901'.         
011902 01  DLI-IO-D901.                                                         
011903*    03  -COPY WDD901.                                                    
011904     EJECT                                                                
011905 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-D902'.         
011906 01  DLI-IO-D902.                                                         
011907*    03  -COPY WDD902  -PRE INFO-                                         
011908     EJECT                                                                
012200                                                                          
012300 01  FILLER                      PIC X(08)   VALUE 'W411CDCA'.            
012400 LINKAGE SECTION.                                                         
012500*                                                                         
012600*   -COPY W411CDCA                                                        
012700     EJECT                                                                
012800                                                                          
012900*01  -COPY W0008      -PRE ARTM-                                          
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200*01  -COPY W0008      -PRE INLB-                                          
013300     05  FILLER                  PIC X.                                   
013400 01  KVAN-WDB2-PCB               PIC X.                                   
013410 01  KVAN-WDC1-PCB               PIC X.                                   
013500     EJECT                                                                
013600*                                                                         
013700 PROCEDURE DIVISION  USING CDCA-W411CDCA ARTM-PCB INLB-PCB                
013800                           KVAN-WDB2-PCB KVAN-WDC1-PCB.                   
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     IF WS-PRELAVB-SKALL-UTFORAS = JA                                     
014300       PERFORM B-BESTAM-BESTALLD-KVANTITET                                
014400       PERFORM C-EV-ANNULLERA-HEL-RAD                                     
014500                                                                          
014600       IF WS-RAD-HELT-ANNULLERAD = NEJ                                    
014700         PERFORM S01-BYGG-DISP-KVANT-TABELL                               
014800         PERFORM E-EVENTUELLT-MINSKA-OKS                                  
014900                                                                          
015000         IF CDCA-FLFORBI-IN = NEJ     AND                                 
015100           CDCA-IDLEVNR-IN = SPACE                                        
015200           PERFORM G-EV-SATTA-PRERO-FOR-VOR                               
015300           PERFORM H-BESTAM-KNOE-NIVAA                                    
015400         END-IF                                                           
015500                                                                          
015600         PERFORM I-BESTAM-ANTAL-OCH-CLAGER                                
015700       END-IF                                                             
015800                                                                          
015900       IF CDCA-KDCALL = +1                                                
016000         MOVE CDCA-RERF-ART-IN    TO ART-RERF-ART                         
016100         PERFORM IMS-REPL-ARTM01                                          
016200       END-IF                                                             
016300                                                                          
016400     END-IF                                                               
016500     GOBACK.                                                              
016600     EJECT                                                                
016700 A-INIT                                  SECTION.                         
016800                                                                          
016900     PERFORM AA-INITIERA-WS-FALTEN                                        
017000     PERFORM AB-INITIERA-NORMAL-AREAN                                     
017100                                                                          
017200     MOVE CDCA-IDDISTR-IN         TO TEST-IDDISTR                         
017300                                                                          
017400     IF CDCA-KDPROTYP-IN = 'L' OR 'O'                                     
017500        PERFORM AD-UPPDATERA-OFFERTSALDO                                  
017600        MOVE NEJ                  TO WS-PRELAVB-SKALL-UTFORAS             
017700     ELSE                                                                 
017800        IF CDCA-FLOVRLEV-IN = JA   OR                                     
017900           CDCA-FLORDSPE-IN = JA   OR                                     
018000           CDCA-KDPROTYP-IN = 'F'                                         
018100           MOVE NEJ               TO WS-PRELAVB-SKALL-UTFORAS             
018200        ELSE                                                              
018300           MOVE JA                TO WS-PRELAVB-SKALL-UTFORAS             
018400           PERFORM AE-SATT-SWITCHAR                                       
018500        END-IF                                                            
018600     END-IF                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 AA-INITIERA-WS-FALTEN                   SECTION.                         
019000                                                                          
019100     MOVE 0                    TO WS-DISP-AKT                             
019200                                  WS-DISP-C1                              
019300                                  WS-KDKNOE                               
019400                                  WS-KDORDBEK                             
019500                                  WS-KVBEART                              
019600                                  WS-KVBEART-REST                         
019700                                  WS-KVPRERO-VOR                          
019800                                  WS-SUDISP-KNO0                          
019900                                  WS-SUDISP-KNO1                          
020000                                  WS-SUDISP-KNO2                          
020100                                  WS-DISP                                 
020200                                  WS-DISP-KNO0                            
020300                                  WS-DISP-KNO1                            
020400                                  WS-DISP-KNO2                            
020500     MOVE NEJ                  TO WS-FLRES                                
020600                                  WS-KVSLATT-SKALL-BERAKNAS               
020700                                  WS-OKS-SKALL-UPPDATERAS                 
020800                                  WS-ORDBEK-99-SKALL-SATTAS               
020900                                  WS-RAD-HELT-ANNULLERAD                  
021000                                  WS-RERF-RAD-OMRAKNAD                    
021100                                  WS-PRERO-FOR-VOR-SKALL-SATTAS           
021200                                  WS-PRERO-FOR-VOR-AR-SATT                
021300                                  WS-BREST-NOLL-PUBV-OK                   
021400     .                                                                    
021500     EJECT                                                                
021600 AB-INITIERA-NORMAL-AREAN                SECTION.                         
021700                                                                          
021800     MOVE NEJ                  TO CDCA-FLAKPLOC-UT                        
021900                                                                          
022000     MOVE 0                    TO CDCA-KDORDBEK-UT                        
022100                                  CDCA-KVBEART-UT                         
022200                                  CDCA-KVBEART-Q-UT                       
022300                                  CDCA-KVPREAVB-UT                        
022400                                  CDCA-KVPRERO-UT                         
022500                                  CDCA-KVANNANT-UT                        
022600                                  CDCA-KVSLATT-UT                         
022700     MOVE CDCA-RERF-RAD-IN     TO CDCA-RERF-RAD-UT                        
022800     .                                                                    
022900     EJECT                                                                
023000                                                                          
023100 AD-UPPDATERA-OFFERTSALDO                SECTION.                         
023200                                                                          
023300     PERFORM S11-LAES-ARTM01                                              
023400     PERFORM S01-BYGG-DISP-KVANT-TABELL                                   
023500     PERFORM S09-SATT-DISP-PA-AKT-LAGER                                   
023600     PERFORM S10-KOLLA-ERSATTNINGAR                                       
023700                                                                          
023800     IF CDCA-KDERS-IN         = +1 AND                                    
023900        WS-BREST-NOLL-PUBV-OK = NEJ                                       
024000*EJ-Q?? MOVE CDCA-KVBEART-IN??? TO CDCA-KVPREAVB-UT                       
024100        MOVE CDCA-KVBEART-IN TO CDCA-KVPREAVB-UT                          
024200                                                                          
024300        COMPUTE ART-KVOFFERT =                                            
024400                ART-KVOFFERT + CDCA-KVBEART-Q-IN                          
024500        PERFORM IMS-REPL-ARTM01                                           
024600     ELSE                                                                 
024700        IF CDCA-KDERS-IN NOT = +1                                         
024800* INGEN MOVE HÄR?? TILL PREAVB-UT?????????? SOM OVAN                      
024900           COMPUTE ART-KVOFFERT =                                         
025000                   ART-KVOFFERT + CDCA-KVBEART-Q-IN                       
025100           PERFORM IMS-REPL-ARTM01                                        
025200        END-IF                                                            
025300     END-IF                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 AE-SATT-SWITCHAR                        SECTION.                         
025700                                                                          
025800     PERFORM AEC-ORDBEK-99-SKALL-SATTAS                                   
025900     PERFORM AED-FLRES-OCH-OKS-SKALL-UPPDAT                               
026000     PERFORM AEE-KVSLATT-SKALL-BERAKNAS                                   
026100     .                                                                    
026200     EJECT                                                                
026300 AEC-ORDBEK-99-SKALL-SATTAS              SECTION.                         
026400                                                                          
026500     IF CDCA-KDORDKL-IN  =  1   AND                                       
026600        CDCA-FLRESTN-IN  = JA   AND                                       
026700      ( CDCA-FLPRELRO-IN = JA OR 'Y' )                                    
026800       MOVE JA                      TO WS-ORDBEK-99-SKALL-SATTAS          
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 AED-FLRES-OCH-OKS-SKALL-UPPDAT          SECTION.                         
027300                                                                          
027400     IF ( CDCA-IDLEVNR-IN     = SPACE          AND                        
027500          CDCA-KDTPOTYP-IN    = 0              AND                        
027600          CDCA-IDKUNDRF-RO-IN > '0000000   ' ) OR                         
027700          CDCA-IDKAMPRF-IN    > 0                                         
027800       MOVE JA                        TO WS-FLRES                         
027900     ELSE                                                                 
028000       IF CDCA-IDLEVNR-IN  = SPACE                                        
028100         MOVE JA TO WS-OKS-SKALL-UPPDATERAS                               
028200       END-IF                                                             
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 AEE-KVSLATT-SKALL-BERAKNAS              SECTION.                         
028700                                                                          
028800     IF CDCA-FLRESTN-IN  = JA         AND                                 
028900        CDCA-RESLATT-IN  > 0          AND                                 
029000        CDCA-FLSLATT-IN  = JA         AND                                 
029100        CDCA-KDTPOTYP-IN = 0          AND                                 
029200      ( CDCA-KDORDKL-IN  = 3 OR 4 )                                       
029300                                                                          
029400       MOVE JA                   TO WS-KVSLATT-SKALL-BERAKNAS             
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 B-BESTAM-BESTALLD-KVANTITET             SECTION.                         
029900                                                                          
030000     MOVE CDCA-KVBEART-Q-IN              TO WS-KVBEART                    
030100                                                                          
030200     PERFORM S11-LAES-ARTM01                                              
030300     PERFORM BB-EV-RANSONERA-OCH-KVANTANPA                                
030400     PERFORM BC-OM-BRIST-EVENT-LEV-1-ST                                   
030500                                                                          
030600     MOVE WS-KVBEART                      TO WS-KVBEART-REST              
030700                                                                          
030800     PERFORM BD-EVENTUELLT-BER-SLATT-ANTAL                                
030900     .                                                                    
031000     EJECT                                                                
031100 BB-EV-RANSONERA-OCH-KVANTANPA           SECTION.                         
031200                                                                          
031300     IF CDCA-KDORDKL-IN =  0          OR                                  
031400        CDCA-FLFORBI-IN = JA          OR                                  
031500        CDCA-FLFORBI-IN = SPEC-FORBI  OR                                  
031600        WS-FLRES        = JA          OR                                  
031700        CDCA-IDLEVNR-IN NOT = SPACE                                       
031800       CONTINUE                                                           
031900     ELSE                                                                 
032000       COMPUTE WS-KVBEART      ROUNDED = WS-KVBEART                       
032100                                       * CDCA-RERF-RAD-IN                 
032200       END-COMPUTE                                                        
032300                                                                          
032400       IF CDCA-KVQPACK-1-IN > 0                                           
032500         PERFORM BBA-EVENTUELLT-KVANTANPASSA                              
032600       END-IF                                                             
032700     END-IF                                                               
032800     .                                                                    
032900     EJECT                                                                
033000 BBA-EVENTUELLT-KVANTANPASSA             SECTION.                         
033100                                                                          
033200     MOVE CDCA-KDKVBRYT-IN             TO KVAN-KDKVBRYT-IN                
033300     MOVE CDCA-IDSYSTEM-IN             TO KVAN-IDSYSTEM-IN                
033400     MOVE WS-KVBEART                   TO KVAN-KVBEART-IN                 
033500     MOVE CDCA-KVQPACK-0-IN            TO KVAN-KVQPACK-0-IN               
033510     MOVE CDCA-KVQPACK-1-IN            TO KVAN-KVQPACK-1-IN               
033520     MOVE CDCA-IDFKNGRP-IN             TO KVAN-IDFKNGRP-IN                
033600     MOVE CDCA-KDPRODSL-IN             TO KVAN-KDPRODSL-IN                
033700     MOVE CDCA-KDSORT-IN               TO KVAN-KDSORT-IN                  
033800     MOVE CDCA-KDORDKL-IN              TO KVAN-KDORDKL-IN                 
033900     MOVE CDCA-FLFORBI-IN              TO KVAN-FLFORBI-IN                 
034000     MOVE CDCA-IDKAMPRF-IN             TO KVAN-IDKAMPRF-IN                
034100     MOVE SPACE                        TO KVAN-IDDC-IN                    
034200     MOVE 0                            TO KVAN-KDKVBRYT-UT                
034300     MOVE 0                            TO KVAN-KVBEART-Q-UT               
034400     MOVE 0                            TO KVAN-KDORDBEK-UT                
034500     MOVE CDCA-IDDISTR-IN              TO KVAN-IDDISTR-IN                 
034600     MOVE CDCA-IDKUNDNR-IN             TO KVAN-IDKUNDNR-IN                
034700     MOVE CDCA-BERADREF-IN             TO KVAN-BERADREF-IN                
034710     MOVE CDCA-IDARTNR-IN              TO KVAN-IDARTNR-IN                 
034800     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
034900                                                                          
035000     IF KVAN-KDORDBEK-UT = 43                                             
035100       COMPUTE WS-KVBEART = KVAN-KVBEART-Q-UT - KVAN-KVQPACK-UT           
035200     ELSE                                                                 
035300        IF KVAN-KDORDBEK-UT = 44                                          
035400           MOVE KVAN-KVBEART-Q-UT      TO WS-KVBEART                      
035500        END-IF                                                            
035600     END-IF                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 BC-OM-BRIST-EVENT-LEV-1-ST             SECTION.                          
036000                                                                          
036100     IF ( WS-KVBEART        < 1    AND                                    
036200          CDCA-RERF-RAD-IN  > 0    AND                                    
036300          CDCA-KDORDKL-IN   = 1 )                                         
036400     OR ( CDCA-KVBEART-Q-IN = 1    AND                                    
036500          WS-KVBEART        < 1    AND                                    
036600          CDCA-RERF-RAD-IN  > 0.3  AND                                    
036700          CDCA-KDORDKL-IN   > 1 )                                         
036800     OR ( CDCA-KVBEART-Q-IN = 2    AND                                    
036900          WS-KVBEART        < 2    AND                                    
037000          CDCA-RERF-RAD-IN  > 0.3  AND                                    
037100          CDCA-KDORDKL-IN   > 1 )                                         
037200     OR ( CDCA-KVBEART-Q-IN = 3    AND                                    
037300          WS-KVBEART        < 3    AND                                    
037400          CDCA-RERF-RAD-IN  > 0.3  AND                                    
037500          CDCA-KDORDKL-IN   > 1 )                                         
037600                                                                          
037700       IF CDCA-KDORDKL-IN = 1                                             
037800         MOVE 1                          TO WS-KVBEART                    
037900       ELSE                                                               
038000         MOVE CDCA-KVBEART-Q-IN          TO WS-KVBEART                    
038100       END-IF                                                             
038200                                                                          
038300       COMPUTE                                                            
038400         CDCA-RERF-RAD-IN ROUNDED = WS-KVBEART / CDCA-KVBEART-Q-IN        
038500       END-COMPUTE                                                        
038600       MOVE CDCA-RERF-RAD-IN  TO CDCA-RERF-RAD-UT                         
038700                                                                          
038800       MOVE JA                 TO WS-RERF-RAD-OMRAKNAD                    
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 BD-EVENTUELLT-BER-SLATT-ANTAL           SECTION.                         
039300                                                                          
039400     IF WS-KVSLATT-SKALL-BERAKNAS = JA                                    
039500       COMPUTE WS-KVSLATT-TEMP ROUNDED = CDCA-KVBEART-Q-IN                
039600                                       * (100 - CDCA-RESLATT-IN)          
039700                                       / 100                              
039800       END-COMPUTE                                                        
039900       COMPUTE CDCA-KVSLATT-UT ROUNDED = WS-KVSLATT-TEMP                  
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 C-EV-ANNULLERA-HEL-RAD                  SECTION.                         
040400                                                                          
040500     IF CDCA-FLRESTN-IN = NEJ                   AND                       
040600        CDCA-KDORDKL-IN =   1                   AND                       
040700        WS-KVBEART-REST < CDCA-KVBEART-Q-IN                               
040800       MOVE JA                 TO WS-RAD-HELT-ANNULLERAD                  
040900       MOVE CDCA-KVBEART-Q-IN  TO CDCA-KVANNANT-UT                        
041000                                  CDCA-KVBEART-Q-UT                       
041100       MOVE 0                  TO CDCA-KVPREAVB-UT                        
041200                                  CDCA-KVPRERO-UT                         
041300       MOVE CDCA-KVBEART-IN    TO CDCA-KVBEART-UT                         
041400       MOVE 80                 TO CDCA-KDORDBEK-UT                        
041500     END-IF                                                               
041600     EJECT                                                                
041700     .                                                                    
041800 E-EVENTUELLT-MINSKA-OKS                 SECTION.                         
041900                                                                          
042000     IF CDCA-KDTPOTYP-IN        >  0    AND                               
042100        WS-OKS-SKALL-UPPDATERAS = JA                                      
042200       EVALUATE CDCA-KDORDKL-IN                                           
042300         WHEN 0                                                           
042400           SUBTRACT CDCA-KVBEART-Q-IN FROM                                
042500                    ART-KVOKS-VOR                                         
042600                                                                          
042700         WHEN 1                                                           
042800           SUBTRACT CDCA-KVBEART-Q-IN FROM                                
042900                    ART-KVOKS-DAG                                         
043000                                                                          
043100         WHEN 2 THRU 4                                                    
043200           SUBTRACT CDCA-KVBEART-Q-IN FROM                                
043300                    ART-KVOKS-BULK                                        
043400                                                                          
043500       END-EVALUATE                                                       
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900 G-EV-SATTA-PRERO-FOR-VOR                SECTION.                         
044000                                                                          
044100     IF CDCA-KDORDKL-IN   = 0                         AND                 
044200        CDCA-FLFORBI-IN   = NEJ                       AND                 
044300        ( WS-SUDISP-KNO2  < WS-KVBEART-REST           OR                  
044400          CDCA-KDUART-IN  = 'M' OR 'P' OR 'S' OR 'L' )                    
044500       MOVE JA                 TO WS-PRERO-FOR-VOR-SKALL-SATTAS           
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900 H-BESTAM-KNOE-NIVAA                     SECTION.                         
045000                                                                          
045100     IF CDCA-KDORDKL-IN = 0 OR 1                                          
045200       IF WS-KVBEART-REST <= WS-SUDISP-KNO0                               
045300         CONTINUE                                                         
045400                                                                          
045500       ELSE                                                               
045600         IF WS-KVBEART-REST <= WS-SUDISP-KNO1   OR                        
045700            CDCA-KDORDKL-IN  = 1                                          
045800           MOVE 1                      TO WS-KDKNOE                       
045900                                                                          
046000         ELSE                                                             
046100           IF WS-KVBEART-REST <= WS-SUDISP-KNO2 OR                        
046200              WS-FLRES        = JA              OR                        
046300              CDCA-KDORDKL-IN = 0                                         
046400             MOVE 2                    TO WS-KDKNOE                       
046500           END-IF                                                         
046600         END-IF                                                           
046700       END-IF                                                             
046800     END-IF                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 I-BESTAM-ANTAL-OCH-CLAGER               SECTION.                         
047200                                                                          
047300     PERFORM S09-SATT-DISP-PA-AKT-LAGER                                   
047400     PERFORM S10-KOLLA-ERSATTNINGAR                                       
047500                                                                          
047600     EVALUATE TRUE                                                        
047700       WHEN CDCA-KDORDKL-IN     =    0              AND                   
047800            CDCA-FLFORBI-IN     =  NEJ              AND                   
047900            ( CDCA-KDUART-IN    =  'M' OR 'P' OR 'S' OR 'L' )             
048000         PERFORM IB-UPPD-VOR-KON-FOR-UART                                 
048100                                                                          
048200       WHEN WS-DISP-C1          =    0              AND                   
048300            CDCA-KDERS-IN       =    01             AND                   
048400            WS-BREST-NOLL-PUBV-OK = JA                                    
048500         MOVE CDCA-KVBEART-IN     TO CDCA-KVBEART-UT                      
048600         MOVE CDCA-KVBEART-Q-IN   TO CDCA-KVBEART-Q-UT                    
048700         CONTINUE                                                         
048800                                                                          
048900       WHEN ( WS-KVBEART-REST   <=  WS-DISP-AKT     OR                    
049000              WS-FLRES          =   JA              OR                    
049100              CDCA-IDLEVNR-IN NOT = SPACE )            AND                
049200              WS-KVBEART-REST   >    0                                    
049300         PERFORM ID-SKAPA-RAD                                             
049400                                                                          
049500       WHEN OTHER                                                         
049600         PERFORM IG-EV-SKAPA-RAD-MOT-EGET-LAGER                           
049700         PERFORM IH-EVENTUELLT-NOLLRAD-PRERO                              
049800     END-EVALUATE                                                         
049900     .                                                                    
050000     EJECT                                                                
050100 IB-UPPD-VOR-KON-FOR-UART                SECTION.                         
050200                                                                          
050300     MOVE 0                         TO CDCA-KVPREAVB-UT                   
050400     MOVE WS-KVBEART-REST           TO CDCA-KVBEART-Q-UT                  
050500                                       CDCA-KVBEART-UT                    
050600                                       CDCA-KVPRERO-UT                    
050700     MOVE 0                         TO CDCA-KVANNANT-UT                   
050800     MOVE JA                        TO WS-PRERO-FOR-VOR-AR-SATT           
050900                                                                          
051000     PERFORM S02-SKAPA-RAD-MOT-EGET-CL                                    
051100     PERFORM S03-EV-UPPD-OKS-FOR-EGET-CL                                  
051200     .                                                                    
051300     EJECT                                                                
051400 ID-SKAPA-RAD                            SECTION.                         
051500                                                                          
051600     MOVE WS-KVBEART-REST          TO CDCA-KVPREAVB-UT                    
051700     MOVE CDCA-KVBEART-Q-IN        TO CDCA-KVBEART-Q-UT                   
051800     MOVE CDCA-KVBEART-IN          TO CDCA-KVBEART-UT                     
051900                                                                          
052000     PERFORM IDA-EV-SATT-FLAKPLOC                                         
052100     PERFORM IDB-BERAKNA-AVV-PA-RAD                                       
052200                                                                          
052300     PERFORM S02-SKAPA-RAD-MOT-EGET-CL                                    
052400     PERFORM S03-EV-UPPD-OKS-FOR-EGET-CL                                  
052500     .                                                                    
052600     EJECT                                                                
052700 IDA-EV-SATT-FLAKPLOC                       SECTION.                      
052800                                                                          
052900     IF WS-KDKNOE       = 2                            AND                
053000      ( WS-KVBEART-REST > WS-DISP-KNO2 -                                  
053100                          CDCA-KVAKS-CDC-IN )          AND                
053200        CDCA-KVAKS-CDC-IN > 0                          AND                
053300        CDCA-KDORDKL-IN = 0                                               
053400       MOVE JA                       TO CDCA-FLAKPLOC-UT                  
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 IDB-BERAKNA-AVV-PA-RAD            SECTION.                               
053900                                                                          
054000     IF WS-PRERO-FOR-VOR-SKALL-SATTAS = JA                                
054100       IF WS-PRERO-FOR-VOR-AR-SATT = NEJ                                  
054200         COMPUTE CDCA-KVPRERO-UT = CDCA-KVBEART-Q-IN -                    
054300                                   WS-SUDISP-KNO2                         
054400         MOVE JA                     TO WS-PRERO-FOR-VOR-AR-SATT          
054500       END-IF                                                             
054600     ELSE                                                                 
054700       COMPUTE CDCA-KVPRERO-UT  = CDCA-KVBEART-Q-UT -                     
054800                                  CDCA-KVPREAVB-UT                        
054900       MOVE 0                        TO CDCA-KVANNANT-UT                  
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300 IG-EV-SKAPA-RAD-MOT-EGET-LAGER           SECTION.                        
055400                                                                          
055500     IF WS-KVBEART-REST  > 0      AND                                     
055600        WS-DISP-AKT      > 0                                              
055700       PERFORM IGA-FLYTTA-TILL-UTAREA                                     
055800       PERFORM IGB-ADDERA-SALDON-PA-ARTREG                                
055900       PERFORM S02-SKAPA-RAD-MOT-EGET-CL                                  
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 IGA-FLYTTA-TILL-UTAREA                  SECTION.                         
056400                                                                          
056500     MOVE WS-DISP-AKT          TO CDCA-KVPREAVB-UT                        
056600     SUBTRACT WS-DISP-AKT      FROM WS-KVBEART-REST                       
056700                                                                          
056800     COMPUTE WS-KVBEART-TEMP   ROUNDED =                                  
056900             CDCA-KVPREAVB-UT / CDCA-RERF-RAD-IN                          
057000     END-COMPUTE                                                          
057100                                                                          
057200     MOVE WS-KVBEART-TEMP      TO CDCA-KVBEART-UT                         
057300                                  CDCA-KVBEART-Q-UT                       
057400                                                                          
057500     IF WS-KVSLATT-SKALL-BERAKNAS = JA                                    
057600       COMPUTE WS-KVSLATT-TEMP ROUNDED =                                  
057700            CDCA-KVBEART-Q-UT  * (100 - CDCA-RESLATT-IN)                  
057800           / 100                                                          
057900       END-COMPUTE                                                        
058000                                                                          
058100       COMPUTE CDCA-KVSLATT-UT ROUNDED = WS-KVSLATT-TEMP                  
058200     END-IF                                                               
058300     PERFORM IGAB-BERAKNA-AVV-PA-RAD                                      
058400                                                                          
058500     IF WS-KDKNOE       = 2                            AND                
058600      ( WS-KVBEART-REST > WS-DISP-KNO2 -                                  
058700                          CDCA-KVAKS-CDC-IN )          AND                
058800        CDCA-KVAKS-CDC-IN > 0                          AND                
058900        CDCA-KDORDKL-IN = 0                                               
059000       MOVE JA                       TO CDCA-FLAKPLOC-UT                  
059100     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 IGAB-BERAKNA-AVV-PA-RAD           SECTION.                               
059500                                                                          
059600     IF WS-PRERO-FOR-VOR-SKALL-SATTAS = JA                                
059700       IF WS-PRERO-FOR-VOR-AR-SATT = NEJ                                  
059800         PERFORM IGABA-SATT-PRERO-FOR-VOR                                 
059900         MOVE JA             TO WS-PRERO-FOR-VOR-AR-SATT                  
060000       END-IF                                                             
060100     ELSE                                                                 
060200       COMPUTE CDCA-KVPRERO-UT =                                          
060300               CDCA-KVBEART-Q-UT - CDCA-KVPREAVB-UT                       
060400       END-COMPUTE                                                        
060500                                                                          
060600       MOVE 0                  TO CDCA-KVANNANT-UT                        
060700     END-IF                                                               
060800     .                                                                    
060900     EJECT                                                                
061000 IGABA-SATT-PRERO-FOR-VOR                SECTION.                         
061100                                                                          
061200     COMPUTE CDCA-KVPRERO-UT = CDCA-KVBEART-Q-IN -                        
061300                               WS-SUDISP-KNO2                             
061400     END-COMPUTE                                                          
061500                                                                          
061600     MOVE CDCA-KVPRERO-UT    TO WS-KVPRERO-VOR                            
061700     MOVE 0                  TO CDCA-KVANNANT-UT                          
061800     .                                                                    
061900     EJECT                                                                
062000 IGB-ADDERA-SALDON-PA-ARTREG             SECTION.                         
062100                                                                          
062200     IF WS-OKS-SKALL-UPPDATERAS = JA                                      
062300       EVALUATE CDCA-KDORDKL-IN                                           
062400         WHEN 0                                                           
062500           COMPUTE ART-KVOKS-VOR  =                                       
062600                   ART-KVOKS-VOR  +                                       
062700                   CDCA-KVBEART-Q-UT          +                           
062800                   CDCA-KVPRERO-UT                                        
062900           END-COMPUTE                                                    
063000                                                                          
063100         WHEN 1                                                           
063200           ADD CDCA-KVBEART-Q-UT TO  ART-KVOKS-DAG                        
063300                                                                          
063400         WHEN 2 THRU 4                                                    
063500           ADD CDCA-KVBEART-Q-UT TO  ART-KVOKS-BULK                       
063600       END-EVALUATE                                                       
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 IH-EVENTUELLT-NOLLRAD-PRERO          SECTION.                            
064100                                                                          
064200     MOVE WS-KVBEART                TO WS-KVBEART-UTAN-DECIMALER          
064300                                                                          
064400     IF WS-KVBEART-UTAN-DECIMALER = WS-KVBEART-REST                       
064500       PERFORM IHB-SKAPA-NOLLRAD                                          
064600     END-IF                                                               
064700                                                                          
064800     PERFORM IHC-EV-PRERO-FOR-OVERALLOK-RAD                               
064900     .                                                                    
065000     EJECT                                                                
065100 IHB-SKAPA-NOLLRAD                       SECTION.                         
065200                                                                          
065300     MOVE CDCA-KVBEART-IN                TO CDCA-KVBEART-UT               
065400     MOVE CDCA-KVBEART-Q-IN              TO CDCA-KVBEART-Q-UT             
065500                                                                          
065600     PERFORM IHBA-BERAKNA-AVV-PA-RAD                                      
065700                                                                          
065800     IF WS-RERF-RAD-OMRAKNAD = JA                                         
065900       MOVE 0                            TO CDCA-RERF-RAD-UT              
066000     END-IF                                                               
066100                                                                          
066200     PERFORM S02-SKAPA-RAD-MOT-EGET-CL                                    
066300                                                                          
066400     IF WS-OKS-SKALL-UPPDATERAS = JA                                      
066500       EVALUATE CDCA-KDORDKL-IN                                           
066600         WHEN 0                                                           
066700           ADD CDCA-KVBEART-Q-IN TO ART-KVOKS-VOR                         
066800                                                                          
066900         WHEN 1                                                           
067000           ADD CDCA-KVBEART-Q-IN TO ART-KVOKS-DAG                         
067100                                                                          
067200         WHEN 2 THRU 4                                                    
067300           ADD CDCA-KVBEART-Q-IN TO ART-KVOKS-BULK                        
067400       END-EVALUATE                                                       
067500     END-IF                                                               
067600                                                                          
067700*??  SUBTRACT CDCA-KVBEART-Q-IN  FROM WS-KVBEART-REST                     
067800     .                                                                    
067900     EJECT                                                                
068000 IHBA-BERAKNA-AVV-PA-RAD                 SECTION.                         
068100                                                                          
068200     IF WS-PRERO-FOR-VOR-SKALL-SATTAS = JA                                
068300       IF WS-PRERO-FOR-VOR-AR-SATT = NEJ                                  
068400         MOVE CDCA-KVBEART-Q-IN  TO CDCA-KVPRERO-UT                       
068500         MOVE CDCA-KVPRERO-UT    TO WS-KVPRERO-VOR                        
068600         MOVE 0                  TO CDCA-KVANNANT-UT                      
068700         MOVE JA                 TO WS-PRERO-FOR-VOR-AR-SATT              
068800       END-IF                                                             
068900     ELSE                                                                 
069000       MOVE CDCA-KVBEART-Q-UT    TO CDCA-KVPRERO-UT                       
069100       MOVE 0                    TO CDCA-KVANNANT-UT                      
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 IHC-EV-PRERO-FOR-OVERALLOK-RAD          SECTION.                         
069600                                                                          
069700     PERFORM IHCA-BERAKNA-AVBOKNINGS-DIFF                                 
069800                                                                          
069900     IF WS-AVBOKNINGS-DIFF < 0                                            
070000       MOVE 'FELAKTIG AVBOKNING'           TO ERROR-TEXT                  
070100       CALL ABEND USING RKOD-ABEND                                        
070200     END-IF                                                               
070300                                                                          
070400     IF WS-AVBOKNINGS-DIFF > 0                                            
070500       IF CDCA-KDORDKL-IN = 0                                             
070600         MOVE 'OVERALLOKERING'               TO ERROR-TEXT                
070700         CALL ABEND USING RKOD-ABEND                                      
070800       ELSE                                                               
070900         PERFORM IHCB-PRERO-FOR-OVERALLOK-RAD                             
071000       END-IF                                                             
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400 IHCA-BERAKNA-AVBOKNINGS-DIFF            SECTION.                         
071500                                                                          
071600     COMPUTE WS-AVBOKNINGS-DIFF =                                         
071700             CDCA-KVBEART-Q-IN      -                                     
071800             CDCA-KVPREAVB-UT                                             
071900                                                                          
072000     IF CDCA-KVANNANT-UT        > 0                                       
072100       COMPUTE WS-AVBOKNINGS-DIFF =                                       
072200               WS-AVBOKNINGS-DIFF       -                                 
072300               CDCA-KVANNANT-UT                                           
072400     ELSE                                                                 
072500       COMPUTE WS-AVBOKNINGS-DIFF =                                       
072600               WS-AVBOKNINGS-DIFF       -                                 
072700               CDCA-KVPRERO-UT                                            
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100 IHCB-PRERO-FOR-OVERALLOK-RAD            SECTION.                         
073200                                                                          
073300     PERFORM IHCBA-JUSTERA-ANTAL-PA-RADEN                                 
073400     PERFORM IHCBB-JUSTERA-ANTAL-PA-ARTREG                                
073500                                                                          
073600     IF CDCA-KDORDBEK-UT = 0   AND                                        
073700        WS-ORDBEK-99-SKALL-SATTAS = JA                                    
073800       MOVE 99                           TO CDCA-KDORDBEK-UT              
073900     END-IF                                                               
074000     .                                                                    
074100     EJECT                                                                
074200 IHCBA-JUSTERA-ANTAL-PA-RADEN            SECTION.                         
074300                                                                          
074400     IF CDCA-KVBEART-Q-UT = 0                                             
074500       MOVE WS-AVBOKNINGS-DIFF           TO CDCA-KVBEART-UT               
074600                                            CDCA-KVBEART-Q-UT             
074700                                            CDCA-KVPRERO-UT               
074800     ELSE                                                                 
074900       ADD WS-AVBOKNINGS-DIFF            TO CDCA-KVBEART-UT               
075000                                            CDCA-KVBEART-Q-UT             
075100                                            CDCA-KVPRERO-UT               
075200     END-IF                                                               
075300     .                                                                    
075400     EJECT                                                                
075500 IHCBB-JUSTERA-ANTAL-PA-ARTREG           SECTION.                         
075600                                                                          
075700     EVALUATE CDCA-KDORDKL-IN                                             
075800       WHEN 1                                                             
075900         ADD WS-AVBOKNINGS-DIFF TO ART-KVOKS-DAG                          
076000                                   ART-KVPRERO-DAG                        
076100                                                                          
076200       WHEN 2 THRU 4                                                      
076300         ADD WS-AVBOKNINGS-DIFF TO ART-KVOKS-BULK                         
076400                                   ART-KVPRERO-BULK                       
076500     END-EVALUATE                                                         
076600     .                                                                    
076700     EJECT                                                                
076800 S01-BYGG-DISP-KVANT-TABELL            SECTION.                           
076900                                                                          
077000     IF CDCA-FLFORBI-IN = JA     OR                                       
077100        CDCA-FLFORBI-IN = SPEC-FORBI OR                                   
077200        WS-FLRES        = JA     OR                                       
077300        CDCA-IDLEVNR-IN NOT = SPACE                                       
077400         MOVE CDCA-KVBEART-IN    TO WS-DISP                               
077500                                    WS-DISP-KNO0                          
077600     ELSE                                                                 
077700                                                                          
077800       PERFORM S01A-BERAKNA-DISP-UTAN-KNOE                                
077900       PERFORM S01B-BERAKNA-DISP-MED-KNOEKOD1                             
078000       PERFORM S01C-BERAKNA-DISP-MED-KNOEKOD2                             
078100       PERFORM S01D-EV-KORRIGERA-DISP-KNO                                 
078200                                                                          
078300     END-IF                                                               
078400     .                                                                    
078500     EJECT                                                                
078600 S01A-BERAKNA-DISP-UTAN-KNOE           SECTION.                           
078700                                                                          
078800*LASSI FIX 22/7 '94 FÖR ATT KLARA AV BERÄKNING MED NEG.KVRESS!            
078900     IF CDCA-KVRESS-IN < ZERO                                             
079000       MOVE ZERO               TO CDCA-KVRESS-IN                          
079100     END-IF                                                               
079200*LASSI FIX SLUT                                                           
079300     IF CDCA-KDERS-IN = +01                                               
079400        COMPUTE WS-DISP         = CDCA-KVLS-IN                            
079500                                - CDCA-KVSPANT-IN                         
079600                                - CDCA-KVRESS-IN                          
079700                                - CDCA-KVSPARR-KVAL-IN                    
079800     ELSE                                                                 
079900        COMPUTE WS-DISP         = CDCA-KVLS-IN                            
080000                                - CDCA-KVUTRS-IN                          
080100                                - CDCA-KVSPANT-IN                         
080200                                - CDCA-KVRESS-IN                          
080300                                - CDCA-KVSPARR-KVAL-IN                    
080400     END-IF                                                               
080500     IF CDCA-RERF-ART-IN >= 1 AND CDCA-KDORDKL-IN > 0                     
080600       COMPUTE WS-DISP-KNO0         = WS-DISP                             
080700                                    - ART-KVOKS-BULK                      
080800                                    - ART-KVOKS-DAG                       
080900                                    - ART-KVOKS-VOR                       
081000     ELSE                                                                 
081100       COMPUTE WS-DISP-KNO0         = WS-DISP                             
081200                                    - ART-KVPREAVB-BULK                   
081300                                    - ART-KVPREAVB-DAG                    
081400                                    - ART-KVPREAVB-VOR                    
081500     END-IF                                                               
081600                                                                          
081700     IF WS-DISP-KNO0 > 0                                                  
081800       ADD WS-DISP-KNO0              TO WS-SUDISP-KNO0                    
081900     END-IF                                                               
082000     .                                                                    
082100     EJECT                                                                
082200 S01B-BERAKNA-DISP-MED-KNOEKOD1        SECTION.                           
082300                                                                          
082400     IF CDCA-KDORDKL-IN = 0 OR 1                                          
082500       COMPUTE WS-DISP-KNO1         = WS-DISP-KNO0                        
082600                                    + ART-KVPREAVB-BULK                   
082700     ELSE                                                                 
082800       MOVE WS-DISP-KNO0             TO WS-DISP-KNO1                      
082900     END-IF                                                               
083000                                                                          
083100     IF WS-DISP-KNO1 > 0                                                  
083200       ADD WS-DISP-KNO1              TO WS-SUDISP-KNO1                    
083300     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600 S01C-BERAKNA-DISP-MED-KNOEKOD2        SECTION.                           
083700                                                                          
083800     IF CDCA-KDORDKL-IN = 0                                               
083900       COMPUTE WS-DISP-KNO2         = WS-DISP-KNO1                        
084000                                    + ART-KVPREAVB-DAG                    
084100                                    + CDCA-KVAKS-CDC-IN                   
084200                                    + CDCA-KVSPANT-IN                     
084300                                    + CDCA-KVRESS-IN                      
084500       END-COMPUTE                                                        
084600*                                   + CDCA-KVAKS-PAV-IN                   
084700     ELSE                                                                 
084800       MOVE WS-DISP-KNO1             TO WS-DISP-KNO2                      
084900     END-IF                                                               
085000                                                                          
085100     IF WS-DISP-KNO2 > 0                                                  
085200       ADD WS-DISP-KNO2              TO WS-SUDISP-KNO2                    
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600 S01D-EV-KORRIGERA-DISP-KNO            SECTION.                           
085700                                                                          
085800     IF WS-DISP-KNO0 < 0                                                  
085900       MOVE 0                        TO WS-DISP-KNO0                      
086000     END-IF                                                               
086100                                                                          
086200     IF WS-DISP-KNO1 < 0                                                  
086300       MOVE 0                        TO WS-DISP-KNO1                      
086400     END-IF                                                               
086500                                                                          
086600     IF WS-DISP-KNO2 < 0                                                  
086700       MOVE 0                        TO WS-DISP-KNO2                      
086800     END-IF                                                               
086900     .                                                                    
087000     EJECT                                                                
087100 S02-SKAPA-RAD-MOT-EGET-CL               SECTION.                         
087200                                                                          
087300     PERFORM S02A-EV-SATT-ORDER-BEKRAFTELSE                               
087400                                                                          
087500     IF CDCA-IDLEVNR-IN = SPACE                                           
087600       PERFORM S02B-UPPD-PREAVB-OCH-PRERO                                 
087700     END-IF                                                               
087800     .                                                                    
087900     EJECT                                                                
088000 S02A-EV-SATT-ORDER-BEKRAFTELSE          SECTION.                         
088100                                                                          
088200     IF WS-PRERO-FOR-VOR-SKALL-SATTAS = JA    AND                         
088300        WS-PRERO-FOR-VOR-AR-SATT      = JA                                
088400       MOVE 92                     TO CDCA-KDORDBEK-UT                    
088500     ELSE                                                                 
088600       IF WS-ORDBEK-99-SKALL-SATTAS  = JA           AND                   
088700          CDCA-KVPRERO-UT            > 0                                  
088800                                                                          
088900         MOVE 99                   TO CDCA-KDORDBEK-UT                    
089000       END-IF                                                             
089100     END-IF                                                               
089200     .                                                                    
089300     EJECT                                                                
089400 S02B-UPPD-PREAVB-OCH-PRERO              SECTION.                         
089500                                                                          
089600     EVALUATE CDCA-KDORDKL-IN                                             
089700       WHEN 0                                                             
089800         ADD CDCA-KVPREAVB-UT            TO                               
089900             ART-KVPREAVB-VOR                                             
090000                                                                          
090100       WHEN 1                                                             
090200         ADD CDCA-KVPREAVB-UT            TO                               
090300             ART-KVPREAVB-DAG                                             
090400                                                                          
090500         ADD CDCA-KVPRERO-UT             TO                               
090600             ART-KVPRERO-DAG                                              
090700                                                                          
090800       WHEN 2 THRU 4                                                      
090900         ADD CDCA-KVPREAVB-UT            TO                               
091000             ART-KVPREAVB-BULK                                            
091100                                                                          
091200         ADD CDCA-KVPRERO-UT             TO                               
091300             ART-KVPRERO-BULK                                             
091400     END-EVALUATE                                                         
091500     .                                                                    
091600     EJECT                                                                
091700 S03-EV-UPPD-OKS-FOR-EGET-CL            SECTION.                          
091800                                                                          
091900     IF WS-OKS-SKALL-UPPDATERAS = JA                                      
092000       EVALUATE CDCA-KDORDKL-IN                                           
092100         WHEN 0                                                           
092200           ADD CDCA-KVBEART-Q-UT   TO ART-KVOKS-VOR                       
092300                                                                          
092400         WHEN 1                                                           
092500           ADD CDCA-KVBEART-Q-UT   TO ART-KVOKS-DAG                       
092600                                                                          
092700         WHEN 2 THRU 4                                                    
092800           ADD CDCA-KVBEART-Q-UT   TO ART-KVOKS-BULK                      
092900       END-EVALUATE                                                       
093000     END-IF                                                               
093100     .                                                                    
093200     EJECT                                                                
093300 S09-SATT-DISP-PA-AKT-LAGER   SECTION.                                    
093400                                                                          
093500     IF CDCA-FLFORBI-IN = JA      OR                                      
093600        CDCA-FLFORBI-IN = SPEC-FORBI OR                                   
093700        CDCA-IDLEVNR-IN NOT = SPACE                                       
093800       MOVE WS-KVBEART-REST              TO WS-DISP-AKT                   
093900                                            WS-DISP-C1                    
094000     ELSE                                                                 
094100       EVALUATE WS-KDKNOE                                                 
094200         WHEN 0                                                           
094300           MOVE WS-DISP-KNO0     TO WS-DISP-C1                            
094400                                                                          
094500         WHEN 1                                                           
094600           MOVE WS-DISP-KNO1     TO WS-DISP-C1                            
094700                                                                          
094800         WHEN 2                                                           
094900           MOVE WS-DISP-KNO2     TO WS-DISP-C1                            
095000       END-EVALUATE                                                       
095100                                                                          
095200       MOVE WS-DISP-C1                   TO WS-DISP-AKT                   
095201       IF CDCA-KDSORT-IN    = 'L ' AND                                    
095202          CDCA-KVQPACK-1-IN > ZERO                                        
095203          PERFORM S09A-ANPASSA-DISP-TILL-KVANT                            
095240       END-IF                                                             
095300     END-IF                                                               
095400     .                                                                    
095500     EJECT                                                                
095600 S09A-ANPASSA-DISP-TILL-KVANT SECTION.                                    
095700                                                                          
095701       IF WS-DISP-AKT < CDCA-KVBEART-Q-IN                                 
095703          COMPUTE WS-DISP-KVANT =                                         
095704                  WS-DISP-AKT / CDCA-KVQPACK-1-IN                         
095705          COMPUTE WS-DISP-KVANT =                                         
095706                  WS-DISP-KVANT * CDCA-KVQPACK-1-IN                       
095707          MOVE WS-DISP-KVANT TO WS-DISP-AKT                               
095721       END-IF                                                             
095722     .                                                                    
095723     EJECT                                                                
095730 S10-KOLLA-ERSATTNINGAR SECTION.                                          
095740                                                                          
095800     IF CDCA-KDERS-IN  = +01 AND                                          
095900        WS-DISP-C1     = +0                                               
096000                                                                          
096010        MOVE CDCA-IDARTNR-IN TO W-IDARTNR-D9                              
096020        MOVE CDCA-IDDC-IN    TO W-IDDC-D9                                 
096100        PERFORM IMS-GU-INLB01                                             
096200        IF SEGMENT-SAKNAS                                                 
096300           MOVE +0                      TO INFO-KVBR                      
096400        ELSE                                                              
096500           PERFORM IMS-GNP-INLB11                                         
096600           IF SEGMENT-SAKNAS                                              
096700              MOVE +0                 TO INFO-KVBR                        
096800           ELSE                                                           
096900              PERFORM UNTIL SEGMENT-SAKNAS OR INFO-KVBR > +0              
097000                                                                          
097100                 PERFORM IMS-GNP-INLB11                                   
097200              END-PERFORM                                                 
097300           END-IF                                                         
097400        END-IF                                                            
097500                                                                          
097600        ADD CDCA-KVAKS-CDC-IN         TO INFO-KVBR                        
097700                                                                          
097800        IF INFO-KVBR = +0 AND CDCA-FLFINLV-IN = JA                        
097900           MOVE JA                    TO WS-BREST-NOLL-PUBV-OK            
098000        END-IF                                                            
098100     END-IF                                                               
098200     .                                                                    
098300     EJECT                                                                
098400 S11-LAES-ARTM01                        SECTION.                          
098500                                                                          
098600     MOVE CDCA-IDARTNR-IN                TO W-IDARTNR                     
098700     PERFORM IMS-GHU-ARTM01-GODK-GE                                       
098800                                                                          
098900     IF SEGMENT-SAKNAS                                                    
098910       IF CDCA-KDCALL = +1                                                
099000          MOVE CDCA-IDARTNR-IN           TO ARTM-IDARTNR-IN               
099100          CALL W411ARTM USING ARTM-W411ARTM                               
099200          PERFORM IMS-GHU-ARTM01-GODK-EJ-GE                               
099210       ELSE                                                               
099211          MOVE ZERO TO ART-KVOFFERT                                       
099214                       ART-KVOKS-BULK                                     
099217                       ART-KVOKS-DAG                                      
099220                       ART-KVOKS-VOR                                      
099223                       ART-KVPREAVB-BULK                                  
099226                       ART-KVPREAVB-DAG                                   
099229                       ART-KVPREAVB-VOR                                   
099232                       ART-KVPRERO-BULK                                   
099235                       ART-KVPRERO-DAG                                    
099238                       ART-RERF-ART                                       
099241                       ART-SUTPO-TOT                                      
099250       END-IF                                                             
099260     ELSE                                                                 
099270*       **** FIX FÖR NEGATIVA SALDON ****                                 
099271       IF ART-KVOFFERT  < ZERO                                            
099280          MOVE ZERO TO ART-KVOFFERT                                       
099281       END-IF                                                             
099282       IF ART-KVOKS-BULK    < ZERO                                        
099290          MOVE ZERO TO ART-KVOKS-BULK                                     
099291       END-IF                                                             
099292       IF ART-KVOKS-DAG     < ZERO                                        
099293          MOVE ZERO TO ART-KVOKS-DAG                                      
099294       END-IF                                                             
099295       IF ART-KVOKS-VOR     < ZERO                                        
099296          MOVE ZERO TO ART-KVOKS-VOR                                      
099297       END-IF                                                             
099298       IF ART-KVPREAVB-BULK < ZERO                                        
099299          MOVE ZERO TO ART-KVPREAVB-BULK                                  
099300       END-IF                                                             
099301       IF ART-KVPREAVB-DAG  < ZERO                                        
099302          MOVE ZERO TO ART-KVPREAVB-DAG                                   
099303       END-IF                                                             
099304       IF ART-KVPREAVB-VOR  < ZERO                                        
099305          MOVE ZERO TO ART-KVPREAVB-VOR                                   
099306       END-IF                                                             
099307       IF ART-KVPRERO-BULK  < ZERO                                        
099308          MOVE ZERO TO ART-KVPRERO-BULK                                   
099309       END-IF                                                             
099310       IF ART-KVPRERO-DAG   < ZERO                                        
099311          MOVE ZERO TO ART-KVPRERO-DAG                                    
099312       END-IF                                                             
099313       IF ART-RERF-ART      < ZERO                                        
099314          MOVE ZERO TO ART-RERF-ART                                       
099315       END-IF                                                             
099316       IF ART-SUTPO-TOT     < ZERO                                        
099317          MOVE ZERO TO ART-SUTPO-TOT                                      
099318       END-IF                                                             
099320     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600 IMS-GHU-ARTM01-GODK-GE                  SECTION.                         
099700     SKIP2                                                                
099800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
099900            DELIMITED BY SIZE INTO SSA1                                   
100000     MOVE '  GE'               TO GODK-STATUSKODER                        
100100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-K901 SSA1                     
100200     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     SKIP2                                                                
100600 IMS-GHU-ARTM01-GODK-EJ-GE               SECTION.                         
100700     SKIP2                                                                
100800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
100900            DELIMITED BY SIZE INTO SSA1                                   
101000     MOVE '  '                 TO GODK-STATUSKODER                        
101100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-K901 SSA1                     
101200     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
101300     PERFORM IMS-STATUSKONTROLL                                           
101400     .                                                                    
101500     SKIP2                                                                
101600 IMS-REPL-ARTM01                         SECTION.                         
101700     SKIP2                                                                
101800     MOVE '  '                 TO GODK-STATUSKODER                        
101900     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-K901                         
102000     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
102100     PERFORM IMS-STATUSKONTROLL                                           
102200     .                                                                    
102300     EJECT                                                                
102400 IMS-GU-INLB01                           SECTION.                         
102500     SKIP2                                                                
102600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
102700            DELIMITED BY SIZE INTO SSA1                                   
102800     MOVE '  GE'               TO GODK-STATUSKODER                        
102900     CALL CBLTDLI USING GU  INLB-PCB DLI-IO-D901 SSA1                     
103000     MOVE INLB-STATUS-CODE     TO STATUS-WS                               
103100     PERFORM IMS-STATUSKONTROLL                                           
103200     .                                                                    
103300     SKIP2                                                                
103400 IMS-GNP-INLB11                          SECTION.                         
103500     SKIP2                                                                
103600     MOVE 'WLINLB11'           TO SSA1                                    
103700     MOVE '  GE'               TO GODK-STATUSKODER                        
103800     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-D902 SSA1                     
103900     MOVE INLB-STATUS-CODE     TO STATUS-WS                               
104000     PERFORM IMS-STATUSKONTROLL                                           
104100     .                                                                    
104200     SKIP2                                                                
104300 IMS-STATUSKONTROLL            SECTION.                                   
104400     SKIP2                                                                
104500     SET STATUS-IX             TO 1                                       
104600     SEARCH GODK-STATUS AT END CALL FELLOG                                
104700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
104800     END-SEARCH                                                           
104900     .                                                                    
105000     EJECT                                                                
