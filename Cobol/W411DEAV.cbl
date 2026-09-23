000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411DEAV.                                                
000500 AUTHOR.         LENA LINDBERG.                                           
000600 DATE-WRITTEN.   SEPT -90.                                                
000700                                                                          
000800*    FUNKTION                                                             
000900*                                                                         
001000*    DETTA ÄR EN SUBMODUL SOM BERÄKNAR HUR STOR KVANT I                   
001100*    ORDERRADEN SOM SKALL DEF.AVBOKAS FRÅN ART-REG.(WDK6+WDK7).           
001200*    SAMT UPPDATERAR SALDON PÅ ON-LINE-ARTREG (WDK9)                      
001300*                                                                         
001400*    REFILLREGLER FINNS I SEKTION G-. SÖK PÅ *REFILL .                    
001500*                                                                         
001600*    UPPDATERAR LARM PÅ WDR5                                              
001700*    REGISTER :    WLARTM (WDK9) ARTIKELREGISTER                          
001800*                          WDK7  ARTIKELREGISTER                          
001900*                                                                         
002000*    LÄNKAREA :    W411DEAV                                               
002100* CHANGE LOG:                                                             
002200*    E-TRACKER: 7450328  HÖST -08  VOHF                                   
002300*    ETRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                     
002400*    ETRACKER: 10130993 2015-04-22                                        
002500*    E-TRACKER:10254592 2015       DECOMISSION VOHF                       
002600*              REDUCE NUMBER OF DELIVERY SCHEDULES                        
002700*    STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,                  
002800*  AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC                        
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700                                                                          
003800 01  IDPGM                       PIC X(08)   VALUE 'W411DEAV'.            
003900 01  PGM-POS                     PIC X(16).                               
004000 01  FELTEXT                     PIC X(40)   VALUE SPACE.                 
004100 01  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004200 01  CURRENT-IMS-SECTION         PIC X(24)   VALUE SPACE.                 
004300 01  SW-WDB6                     PIC X       VALUE 'N'.                   
004400     88 WDB6-FINNS                           VALUE 'J'.                   
004500     88 WDB6-SAKNAS                          VALUE 'N'.                   
004600 01  FILLER                      PIC X(8)    VALUE 'ZZZZZZZZ'.            
004700 01  WS-FATTAS                   PIC S9(7)   VALUE 0  COMP-3.             
004800 01  WS-DISP-WDK7                PIC S9(7)   VALUE 0  COMP-3.             
004900 01  WS-KVAVBART                 PIC S9(7)   VALUE 0  COMP-3.             
005000 01  WS-KVBEART                  PIC S9(7)   VALUE 0  COMP-3.             
005100 01  WS-REST                     PIC S9(7)   VALUE 0  COMP-3.             
005200 01  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
005300 01  JA                          PIC X       VALUE 'J'.                   
005400 01  NEJ                         PIC X       VALUE 'N'.                   
005500 01  SPEC-FORBI                  PIC X       VALUE 'S'.                   
005600 01  WS-FLRES                    PIC X       VALUE 'N'.                   
005700 01  WS-ALLT-I-REST              PIC X       VALUE 'N'.                   
005800 01  WS-PREAVB                   PIC S9(9)   VALUE 0.                     
005900 01  WS-DEFAVB                   PIC S9(9)   VALUE 0.                     
006000 01  WS-DISPPL                   PIC S9(9)   VALUE 0.                     
006100 01  WS-DISP                     PIC S9(9)   VALUE 0.                     
006200 01  FILLER                      PIC X(8)    VALUE 'BBBBBBBB'.            
006300 01  WS-DISPUTSKR                PIC S9(9)   VALUE 0.                     
006400 01  WS-DISPUTSKR-KVANT          PIC S9(9)   VALUE 0.                     
006500 01  WS-DISPAK                   PIC S9(9)   VALUE 0.                     
006600 01  WS-PRERO                    PIC S9(9)   VALUE 0.                     
006700 01  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
006800 01  WS-KVAKS-SDC                PIC 9(7)    VALUE ZERO.                  
006900 01  WS-KVOKS-DAG                PIC 9(7)    VALUE ZERO.                  
007000 01  WS-KVOKS-BULK               PIC 9(7)    VALUE ZERO.                  
007100 01  WS-KVRESS                   PIC 9(7)    VALUE ZERO.                  
007200 01  IX                          PIC 9(2)    VALUE ZERO.                  
007300 01  FILLER                      PIC X(8)    VALUE 'DDDDDDDD'.            
007400*         80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.                     
007500 01  WS-KDORDBEK-80              PIC 9(2)    VALUE 80.                    
007600*         90 = RESTNOTERAD                                                
007700 01  WS-KDORDBEK-90              PIC 9(2)    VALUE 90.                    
007800*         91 = RESTNOTERAD IGEN.                                          
007900 01  WS-KDORDBEK-91              PIC 9(2)    VALUE 91.                    
008000*         92 = VOR, RESTNOTERAD KVANT                                     
008100 01  WS-KDORDBEK-92              PIC 9(2)    VALUE 92.                    
008200 01  WS-KDORDBEK-43              PIC 9(2)    VALUE 43.                    
008300 01  WS-DATUM                    PIC 9(6).                                
008400 01  WS-IDDISTR                  PIC 9(5).                                
008500 01  FILLER                      PIC X(8)    VALUE 'EEEEEEEE'.            
008600                                                                          
008700                                                                          
008800*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
008900 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
009000 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
009100                                                                          
009200 01  WS-TISENBEK-KL-TEST.                                                 
009300     03  WS-TISENBEK-KL-HH      PIC 9(2)     VALUE ZERO.                  
009400     03  WS-TISENBEK-KL-MM      PIC 9(2)     VALUE ZERO.                  
009500     03  WS-TISENBEK-KL-SS      PIC 9(2)     VALUE ZERO.                  
009600*                                                                         
009700 77  IX1                         PIC 9(2)    VALUE ZERO.                  
009800 77  IX2                         PIC 9(2)    VALUE ZERO.                  
009900 77  INDX                        PIC 9(3)    VALUE ZERO.                  
010000 77  W-SPAR-IDDC                 PIC X(2)    VALUE SPACES.                
010100*                                                                         
010200 01  GENERELLA-SUBPROGRAM.                                                
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010500     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
010800     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
010900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011000 01    ABENDKODER.                                                        
011100       03  FILLER               PIC X(16) VALUE 'ABENDKODER'.             
011200       03  RKOD-ABEND-UTAN-DUMP PIC S9(4) COMP SYNC VALUE +16.            
011300       03  RKOD-ABEND-MED-DUMP  PIC S9(4) COMP SYNC VALUE +33.            
011400       03  RKOD-FELTEXT         PIC X(32) VALUE SPACE.                    
011500     EJECT                                                                
011600*    --- ARBETS-AREOR TILL GENERELLA SUBPROGRAM                           
011700                                                                          
011800 01  FILLER                      PIC X(16)   VALUE 'W411KVAN'.            
011900*   -COPY W411KVAN                                                        
012000     EJECT                                                                
012100 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
012200*   -COPY W005WDK7                                                        
012300     EJECT                                                                
012400 01 FILLER                       PIC X(8)    VALUE 'W005WDL7'.            
012500*   -COPY W005WDL7                                                        
012600     EJECT                                                                
012700*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
012800*                                                                         
012900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013000*01  -COPY WDATAREA                                                       
013100 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
013200*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
013300     EJECT                                                                
013400*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
013500     EJECT                                                                
013600*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
013700     EJECT                                                                
013800*                                                                         
013900 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
014000*   -COPY WWDIST57                                                        
014100     EJECT                                                                
014200*   -COPY WWDC99                                                          
014300     EJECT                                                                
014400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP2                                                                
014800*    --- STATUS-KOD FRÅN IMS                                              
014900 01  STATUS-WS                   PIC XX.                                  
015000     88  SEGMENT-FINNS                       VALUE '  '.                  
015100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015300     SKIP2                                                                
015400 01  GODK-STATUSKODER.                                                    
015500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015600     SKIP2                                                                
015700 01  SSA1                        PIC X(96).                               
015800 01  SSA2                        PIC X(160).                              
015900 01  SSA3                        PIC X(160).                              
016000     EJECT                                                                
016100 01  FILLER                      PIC X(8)    VALUE 'GGGGGGGG'.            
016200                                                                          
016300*01  -COPY WWDCKONS                                                       
016400     EJECT                                                                
016500*    --- IMS FUNKTIONSKODER                                               
016600*01  -COPY W0003                                                          
016700     EJECT                                                                
016800                                                                          
016900 01  NYCKLAR-TILL-DLI.                                                    
017000     03  W-IDARTNR-X.                                                     
017100         05  W-IDARTNR           PIC  S9(9)  COMP-3.                      
017200                                                                          
017300     03  W-WDK711-IDDC-X.                                                 
017400         05  W-WDK711-IDDC       PIC  X(2).                               
017500                                                                          
017600     03  W-IDDC-B6-X.                                                     
017700         05 W-IDDC-B6            PIC X(2).                                
017800                                                                          
017900     03  W-IDDC-SLAG-X.                                                   
018000         05  W-IDDC-SLAG         PIC X(2)    VALUE SPACE.                 
018100                                                                          
018200     03  W-IDDC-X.                                                        
018300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
018400                                                                          
018500     03  W-KDLARM-X.                                                      
018600         05  W-KDLARM-S          PIC S9(3)   VALUE ZERO COMP-3.           
018700                                                                          
018800     03  W-WDGXKEY-2231-X.                                                
018900         05  FILLER              PIC X(4)    VALUE '2231'.                
019000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
019100     03  W-WDGXKEY-2232-X.                                                
019200         05  W-IDANSK-2232       PIC S9(3)   VALUE ZERO COMP-3.           
019300         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
019400                                                                          
019500     03  W-WDGX2223-X.                                                    
019600         05  W-IDHTYP            PIC X(4)    VALUE '2223'.                
019700         05  W-IDANSK            PIC S9(3)   VALUE ZERO COMP-3.           
019800         05  W-VALFRI            PIC X(24)   VALUE LOW-VALUE.             
019900     03  W-WDGX2224-X.                                                    
020000         05  W-TISENBEK-DAG      PIC S9(7)   VALUE ZERO COMP-3.           
020100         05  W-TISENBEK-KL       PIC S9(7)   VALUE ZERO COMP-3.           
020200         05  W-KDLARM            PIC S9(3)   VALUE ZERO COMP-3.           
020300                                                                          
020400                                                                          
020500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
020600 01  DLI-IO-WDK611.                                                       
020700*    03  -COPY WDK611                                                     
020800*    ---  DLI INPUT-OUTPUT AREA                                           
020900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK901'.         
021000                                                                          
021100 01  DLI-IO-WDK901.                                                       
021200*    03  WLARTM01 -COPY WDK901                                            
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
021500 01  DLI-IO-WDK711.                                                       
021600*    03  WDK711 -COPY WDK711                                              
021700     EJECT                                                                
021800 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.          
021900 01  DLI-IO-WLLOGA01.                                                     
022000*    03  WLLOGA01  -COPY WDL901                                           
022100                                                                          
022200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022300 01   DLI-IO-AREA-B601.                                                   
022400*     03  -COPY WDB601                                                    
022500     EJECT                                                                
022600                                                                          
022700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR220'.                      
022800 01  DLI-IO-WDR220.                                                       
022900*    03  -COPY WDGX2232                                                   
023000                                                                          
023100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501'.                      
023200 01  DLI-IO-WDR501.                                                       
023300*    03  -COPY WDGX2223                                                   
023400     SKIP3                                                                
023500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR550'.                      
023600 01  DLI-IO-WDR550.                                                       
023700*    03  -COPY WDGX2224                                                   
023800     SKIP3                                                                
023900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG302'.                      
024000 01  DLI-IO-WDG302.                                                       
024100*    03  -COPY WDGX2204                                                   
024200     EJECT                                                                
024300                                                                          
024400 01  FILLER                      PIC X(16) VALUE 'W411DEAV-AREA'.         
024500 LINKAGE SECTION.                                                         
024600*                                                                         
024700*01 -COPY W411DEAV                                                        
024800     EJECT                                                                
024900                                                                          
025000*01  FILLER                      PIC X(08) VALUE 'PCB-AREA'.              
025100*01  -COPY W0008      -PRE ARTM-                                          
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400*01  -COPY W0008      -PRE WDK7-                                          
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008      -PRE WDB6-                                          
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008  -PRE WLLOGA-                                            
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008      -PRE WDK6-                                          
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600 01  KVAN-WDB2-PCB            PIC X.                                      
026700     EJECT                                                                
026800*01  -COPY W0008  -PRE  OIGA-                                             
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100*01  -COPY W0008  -PRE  WDK7-2-                                           
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE  WDR2-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008  -PRE  WDR5-                                             
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000 01  KVAN-WDC1-PCB            PIC X.                                      
028100     EJECT                                                                
028200*01  -COPY W0008  -PRE  2203-                                             
028300     05  FILLER                  PIC X.                                   
028400     EJECT                                                                
028500*                                                                         
028600 PROCEDURE DIVISION  USING DEAV-W411DEAV  ARTM-PCB  WDK7-PCB              
028700                                          WDB6-PCB  WLLOGA-PCB            
028800                                          WDK6-PCB                        
028900                                          KVAN-WDB2-PCB                   
029000                                          OIGA-PCB                        
029100                                          WDK7-2-PCB    WDR2-PCB          
029200                                          WDR5-PCB  KVAN-WDC1-PCB.        
029300                                                                          
029400 MAIN                                    SECTION.                         
029500     PERFORM A-INIT                                                       
029600                                                                          
029700     IF WDB6-FINNS                                                        
029800        IF DCS-CDC OR DCS-CDC-TR                                          
029900          PERFORM B-CONTROL-RANSF                                         
030000          PERFORM C-BEST-AVBOKKV                                          
030100          PERFORM D-JUST-AVBOKKV                                          
030200          PERFORM F-UPDATE-DB-WDK9                                        
030300          PERFORM G-GIVE-VALUE-TO-WDK6                                    
030400        ELSE                                                              
030500          IF DCS-SDC                                                      
030600            PERFORM E-UPDATE-DB-WDK7                                      
030700          ELSE                                                            
030800            IF DCS-NDC                                                    
030900              PERFORM H-NDC-UPDATE-DB-WDK7                                
031000            END-IF                                                        
031100          END-IF                                                          
031200        END-IF                                                            
031300     END-IF                                                               
031400     GOBACK                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 A-INIT                                  SECTION.                         
031800     ACCEPT WS-DATUM   FROM DATE                                          
031900                                                                          
032000     PERFORM AA-INITIERA-LANK-AREAN                                       
032100     PERFORM AB-INITIERA-WS-FALT                                          
032200     MOVE DEAV-IDARTNR-IN                TO W-IDARTNR                     
032300                                                                          
032400     MOVE DEAV-IDDC-IN                   TO W-IDDC-B6                     
032500                                            W-WDK711-IDDC                 
032600                                            WS-IDDC                       
032700                                                                          
032800     PERFORM AC-LAES-WDK9                                                 
032900     PERFORM IMS-GU-WDB601                                                
033000     IF SEGMENT-FINNS                                                     
033100        MOVE JA TO SW-WDB6                                                
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 AA-INITIERA-LANK-AREAN                  SECTION.                         
033600                                                                          
033700     MOVE DEAV-FLAKPLOC-IN     TO DEAV-FLAKPLOC-UT                        
033800     MOVE DEAV-RERF-RAD-IN     TO DEAV-RERF-RAD-UT                        
033900     MOVE 0                    TO DEAV-KVAVBART-UT                        
034000                                  DEAV-KDORDBEK-UT                        
034100                                  DEAV-KVEFRS-UT                          
034200                                  DEAV-KVLS-UT                            
034300                                  DEAV-KVRESS-UT                          
034400                                  DEAV-KVROS-UT                           
034500                                  DEAV-KDROO-UT                           
034600     .                                                                    
034700     EJECT                                                                
034800 AB-INITIERA-WS-FALT                     SECTION.                         
034900                                                                          
035000     MOVE NEJ                  TO WS-FLRES                                
035100     MOVE +0                   TO WS-PREAVB                               
035200                                  WS-DEFAVB                               
035300                                  WS-DISPPL                               
035400                                  WS-DISP                                 
035500                                  WS-DISPUTSKR                            
035600                                  WS-DISPAK                               
035700                                  WS-PRERO                                
035800                                  WS-FATTAS                               
035900                                  WS-KVAVBART                             
036000     .                                                                    
036100     EJECT                                                                
036200 AC-LAES-WDK9                            SECTION.                         
036300                                                                          
036400     PERFORM IMS-GU-ARTM01                                                
036500                                                                          
036600     IF SEGMENT-FINNS                                                     
036700        CONTINUE                                                          
036800     ELSE                                                                 
036900        IF SEGMENT-SAKNAS                                                 
037000           MOVE ZERO   TO ART-KVOFFERT                                    
037100                          ART-KVOKS-BULK                                  
037200                          ART-KVOKS-DAG                                   
037300                          ART-KVOKS-VOR                                   
037400                          ART-KVPREAVB-BULK                               
037500                          ART-KVPREAVB-DAG                                
037600                          ART-KVPREAVB-VOR                                
037700                          ART-KVPRERO-BULK                                
037800                          ART-KVPRERO-DAG                                 
037900                          ART-RERF-ART                                    
038000                          ART-SUTPO-TOT                                   
038100        ELSE                                                              
038200           CALL FELLOG                                                    
038300        END-IF                                                            
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700 B-CONTROL-RANSF                           SECTION.                       
038800                                                                          
038900     IF (DEAV-IDKUNDRF-RO-IN NOT = '00000     ' AND                       
039000         DEAV-IDKUNDRF-RO-IN NOT = '0000000   ' AND                       
039100         DEAV-TIRODAT-IN > 0 )                                            
039200         OR                                                               
039300         DEAV-IDKAMPRF-IN NOT = 0                                         
039400         MOVE JA                         TO WS-FLRES                      
039500     END-IF                                                               
039600                                                                          
039700     IF (DEAV-KDORDKL-IN = 1 OR 2 OR 3 OR 4)                              
039800         AND                                                              
039900        (WS-FLRES = NEJ OR DEAV-IDLEVNR-IN = SPACE)                       
040000        IF DEAV-RERF-RAD-NY-IN > DEAV-RERF-RAD-IN                         
040100           AND                                                            
040200           DEAV-FLRESTN-IN = NEJ                                          
040300           AND                                                            
040400           DEAV-KDORDKL-IN = 1                                            
040500           MOVE DEAV-RERF-RAD-IN       TO DEAV-RERF-RAD-UT                
040600        ELSE                                                              
040700           IF DEAV-RERF-RAD-NY-IN < DEAV-RERF-RAD-IN                      
040800              IF DEAV-KDORDKL-IN = 1                                      
040900                 OR                                                       
041000                ((DCS-CDC OR DCS-CDC-TR)  AND                             
041100                (DEAV-ADLAGOMR-IN = 60 OR 61))                            
041200                 MOVE DEAV-RERF-RAD-IN    TO DEAV-RERF-RAD-UT             
041300              ELSE                                                        
041400                 MOVE DEAV-RERF-RAD-NY-IN TO DEAV-RERF-RAD-UT             
041500              END-IF                                                      
041600           ELSE                                                           
041700              MOVE DEAV-RERF-RAD-NY-IN TO DEAV-RERF-RAD-UT                
041800           END-IF                                                         
041900        END-IF                                                            
042000     END-IF                                                               
042100                                                                          
042200     PERFORM BA-BERAKNA-DISPPL                                            
042300                                                                          
042400     PERFORM BB-BERAKNA-PREAVB                                            
042500                                                                          
042600     COMPUTE WS-DISP = WS-DISPPL - WS-PREAVB                              
042700                                                                          
042800     IF WS-DISP NEGATIVE                                                  
042900        MOVE 0 TO WS-DISP                                                 
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 BA-BERAKNA-DISPPL SECTION.                                               
043400                                                                          
043500     MOVE DEAV-IDDISTR-IN     TO DIST18-IDDISTR                           
043600                                                                          
043700     IF DIST18-SKROT-KVAL-CDC  OR                                         
043800        DIST18-SKROT-KVAL-SDC  OR                                         
043900        DIST18-SKROT-KVAL-LDC                                             
044000       COMPUTE WS-DISPPL = DEAV-KVBEART-Q-IN                              
044100       END-COMPUTE                                                        
044200     ELSE                                                                 
044300       IF WS-FLRES = JA                                                   
044400          COMPUTE WS-DISPPL = DEAV-KVLS-IN                                
044500                              - DEAV-KVUTRS-IN                            
044600                              - DEAV-KVSPANT-IN                           
044700                              - DEAV-KVSPARR-KVAL-IN                      
044800       ELSE                                                               
044900          IF DEAV-KDORDKL-IN = 0                                          
045000             COMPUTE WS-DISPPL = DEAV-KVLS-IN                             
045100                                 - DEAV-KVUTRS-IN                         
045200                                 - DEAV-KVSPARR-KVAL-IN                   
045300          ELSE                                                            
045400             COMPUTE WS-DISPPL = DEAV-KVLS-IN                             
045500                                 - DEAV-KVUTRS-IN                         
045600                                 - DEAV-KVSPANT-IN                        
045700                                 - DEAV-KVRESS-IN                         
045800                                 - DEAV-KVSPARR-KVAL-IN                   
045900          END-IF                                                          
046000       END-IF                                                             
046100     END-IF                                                               
046200                                                                          
046300     IF WS-DISPPL NEGATIVE                                                
046400        MOVE 0 TO WS-DISPPL                                               
046500     END-IF                                                               
046600     .                                                                    
046700     EJECT                                                                
046800 BB-BERAKNA-PREAVB SECTION.                                               
046900                                                                          
047000     IF DEAV-KDORDKL-IN = 0                                               
047100     OR DIST18-SKROT-KVAL-CDC OR                                          
047200        DIST18-SKROT-KVAL-SDC OR                                          
047300        DIST18-SKROT-KVAL-LDC                                             
047400        MOVE 0 TO WS-PREAVB                                               
047500     ELSE                                                                 
047600        IF DEAV-KDORDKL-IN = 1                                            
047700           COMPUTE WS-PREAVB =  ART-KVPREAVB-VOR                          
047800        ELSE                                                              
047900           COMPUTE WS-PREAVB =  ART-KVPREAVB-VOR                          
048000                              + ART-KVPREAVB-DAG                          
048100        END-IF                                                            
048200     END-IF                                                               
048300     .                                                                    
048400     EJECT                                                                
048500 C-BEST-AVBOKKV                          SECTION.                         
048600                                                                          
048700     IF DEAV-KDORDKL-IN = 0  OR                                           
048800        DEAV-FLFORBI-IN = JA OR                                           
048900        DEAV-FLFORBI-IN = SPEC-FORBI OR                                   
049000        DEAV-IDLEVNR-IN NOT = SPACE OR                                    
049100        WS-FLRES        = JA                                              
049200        IF DEAV-KVPREAVB-IN = 0                                           
049300           MOVE DEAV-KVBEART-Q-IN TO WS-DEFAVB                            
049400        ELSE                                                              
049500           MOVE DEAV-KVPREAVB-IN  TO WS-DEFAVB                            
049600        END-IF                                                            
049700     ELSE                                                                 
049800        COMPUTE WS-DEFAVB =                                               
049900              ((DEAV-KVBEART-Q-IN                                         
050000              * DEAV-RERF-RAD-UT)                                         
050100              + 0.5)                                                      
050200        END-COMPUTE                                                       
050300                                                                          
050400        IF DEAV-KDORDKL-IN = 1                                            
050500           AND                                                            
050600           WS-DEFAVB = 0                                                  
050700                                                                          
050800           IF DIST18-SKROT-KVAL-CDC                                       
050900           OR DIST18-SKROT-KVAL-SDC                                       
051000           OR DIST18-SKROT-KVAL-LDC                                       
051100           OR DIST18-SKROT-SDC                                            
051200              MOVE DEAV-KVBEART-Q-IN TO WS-DEFAVB                         
051300           ELSE                                                           
051400              MOVE 1 TO WS-DEFAVB                                         
051500           END-IF                                                         
051600        ELSE                                                              
051700           IF (DEAV-KDORDKL-IN = 2 OR 3 OR 4)                             
051800               AND                                                        
051900             (( WS-DEFAVB = 0 AND DEAV-KVBEART-Q-IN = 1                   
052000                             AND DEAV-RERF-RAD-UT  > 0.3000 )             
052100                OR                                                        
052200              ( WS-DEFAVB = 1 AND DEAV-KVBEART-Q-IN = 2                   
052300                             AND DEAV-RERF-RAD-UT  > 0.3000 )             
052400                OR                                                        
052500              ( WS-DEFAVB = 2 AND DEAV-KVBEART-Q-IN = 3                   
052600                             AND DEAV-RERF-RAD-UT  > 0.3000 ))            
052700                                                                          
052800               MOVE DEAV-KVBEART-Q-IN TO WS-DEFAVB                        
052900           END-IF                                                         
053000        END-IF                                                            
053100                                                                          
053200        IF WS-DEFAVB NOT = DEAV-KVPREAVB-IN                               
053300           AND                                                            
053400           DEAV-KDKVBRYT-IN = 0                                           
053500           PERFORM CA-INIT-W411KVAN-AREA                                  
053600                                                                          
053700           MOVE DEAV-IDDISTR-IN TO TEST-IDDISTR                           
053800           IF DIST18-SKROT OR DIST18-SCRAP-NDC                            
053900             MOVE JA TO KVAN-FLORDSPE-IN                                  
054000           END-IF                                                         
054100                                                                          
054200           CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB                
054300                                             KVAN-WDC1-PCB                
054400           IF KVAN-KDORDBEK-UT = WS-KDORDBEK-43                           
054500              COMPUTE WS-DEFAVB =                                         
054600                      KVAN-KVBEART-Q-UT - KVAN-KVQPACK-UT                 
054700           ELSE                                                           
054800              MOVE KVAN-KVBEART-Q-UT TO WS-DEFAVB                         
054900           END-IF                                                         
055000           IF WS-DEFAVB NEGATIVE                                          
055100              MOVE 0 TO WS-DEFAVB                                         
055200           END-IF                                                         
055300        END-IF                                                            
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700 CA-INIT-W411KVAN-AREA                   SECTION.                         
055800                                                                          
055900     MOVE DEAV-KDKVBRYT-IN               TO KVAN-KDKVBRYT-IN              
056000     MOVE DEAV-IDSYSTEM-IN               TO KVAN-IDSYSTEM-IN              
056100     MOVE WS-DEFAVB                      TO KVAN-KVBEART-IN               
056200     MOVE DEAV-KVQPACK-0-IN              TO KVAN-KVQPACK-0-IN             
056300     MOVE DEAV-KVQPACK-1-IN              TO KVAN-KVQPACK-1-IN             
056400     MOVE DEAV-IDFKNGRP-IN               TO KVAN-IDFKNGRP-IN              
056500     MOVE DEAV-KDPRODSL-IN               TO KVAN-KDPRODSL-IN              
056600     MOVE DEAV-KDSORT-IN                 TO KVAN-KDSORT-IN                
056700     MOVE DEAV-KDORDKL-IN                TO KVAN-KDORDKL-IN               
056800     MOVE DEAV-FLFORBI-IN                TO KVAN-FLFORBI-IN               
056900     MOVE DEAV-IDKAMPRF-IN               TO KVAN-IDKAMPRF-IN              
057000     MOVE DEAV-IDDC-IN                   TO KVAN-IDDC-IN                  
057100     MOVE DEAV-IDDISTR-IN                TO KVAN-IDDISTR-IN               
057200     MOVE DEAV-IDKUNDNR-IN               TO KVAN-IDKUNDNR-IN              
057300     MOVE DEAV-BERADREF-IN               TO KVAN-BERADREF-IN              
057400     MOVE DEAV-IDARTNR-IN                TO KVAN-IDARTNR-IN               
057500                                                                          
057600     IF NOT CDC-SE                                                        
057700        MOVE ZERO                        TO KVAN-KVQPACK-1-IN             
057800                                            KVAN-KDSORT-IN                
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 D-JUST-AVBOKKV                          SECTION.                         
058300                                                                          
058400     MOVE DEAV-IDDISTR-IN     TO DIST18-IDDISTR                           
058500                                                                          
058600     IF DIST18-SKROT-KVAL-CDC                                             
058700     OR DIST18-SKROT-KVAL-SDC                                             
058800     OR DIST18-SKROT-KVAL-LDC                                             
058900     OR DIST18-SKROT-SDC                                                  
059000                                                                          
059100       IF DIST18-SKROT-SDC                                                
059200         IF WS-DISP > DEAV-KVBEART-Q-IN                                   
059300           MOVE DEAV-KVBEART-Q-IN   TO DEAV-KVAVBART-UT                   
059400         ELSE                                                             
059500           MOVE WS-DISP             TO DEAV-KVAVBART-UT                   
059600         END-IF                                                           
059700       ELSE                                                               
059800         MOVE DEAV-KVBEART-Q-IN     TO DEAV-KVAVBART-UT                   
059900       END-IF                                                             
060000     ELSE                                                                 
060100                                                                          
060200        PERFORM DA-BEST-DISPAK                                            
060300        PERFORM DB-BEST-DISPUTSKR                                         
060400                                                                          
060500        IF DEAV-KDORDKL-IN = 0                                            
060600           IF WS-DISPUTSKR < DEAV-KVBEART-Q-IN                            
060700              IF WS-DISPAK > 0                                            
060800                 AND                                                      
060900                 DEAV-FLAKPLOC-IN = NEJ                                   
061000                 MOVE JA              TO DEAV-FLAKPLOC-UT                 
061100                 COMPUTE WS-DISPUTSKR =  WS-DISPUTSKR + WS-DISPAK         
061200                 END-COMPUTE                                              
061300              END-IF                                                      
061400           END-IF                                                         
061500        END-IF                                                            
061600                                                                          
061700        IF WS-DISPUTSKR >= WS-DEFAVB                                      
061800           MOVE WS-DEFAVB       TO DEAV-KVAVBART-UT                       
061900        ELSE                                                              
062000           MOVE WS-DISPUTSKR    TO DEAV-KVAVBART-UT                       
062100        END-IF                                                            
062200                                                                          
062300        IF DEAV-KVAVBART-UT NEGATIVE                                      
062400           MOVE 0 TO DEAV-KVAVBART-UT                                     
062500        END-IF                                                            
062600                                                                          
062700        IF DEAV-KVAVBART-UT < DEAV-KVBEART-Q-IN                           
062800           IF DEAV-KDORDKL-IN = 0                                         
062900              IF DEAV-KVBEART-Q-IN > WS-DISPUTSKR                         
063000                 PERFORM DC-SKAPA-VORKO                                   
063100              END-IF                                                      
063200           ELSE                                                           
063300              PERFORM DD-BEST-RO-SLATT                                    
063400           END-IF                                                         
063500        END-IF                                                            
063600                                                                          
063700        MOVE DEAV-IDDISTR-IN         TO TEST-IDDISTR                      
063800        IF ((DIST35-REFILL AND NOT DIST35-REFILL-NA) OR                   
063900             DIST35-CDC-NL-RETUR)                   AND                   
064000            DEAV-KVBEART-Q-IN > DEAV-KVAVBART-UT                          
064100           COMPUTE WS-FATTAS = DEAV-KVBEART-Q-IN                          
064200                             - DEAV-KVAVBART-UT                           
064300           END-COMPUTE                                                    
064400                                                                          
064500        ELSE                                                              
064600           MOVE 0                    TO WS-FATTAS                         
064700        END-IF                                                            
064800     END-IF                                                               
064900     .                                                                    
065000     EJECT                                                                
065100 DA-BEST-DISPAK                          SECTION.                         
065200                                                                          
065300     COMPUTE WS-DISPAK = DEAV-KVAKS-CDC-IN                                
065400                       + DEAV-KVAKS-PAV-IN                                
065500     END-COMPUTE                                                          
065600                                                                          
065700     IF WS-DISPAK NEGATIVE                                                
065800        MOVE 0 TO WS-DISPAK                                               
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 DB-BEST-DISPUTSKR                       SECTION.                         
066300                                                                          
066400     IF DEAV-FLFORBI-IN = JA OR                                           
066500        DEAV-FLFORBI-IN = SPEC-FORBI OR                                   
066600        DEAV-IDLEVNR-IN NOT = SPACE                                       
066700        MOVE WS-DEFAVB TO WS-DISPUTSKR                                    
066800     ELSE                                                                 
066900        IF DEAV-KDORDKL-IN = 0                                            
067000           IF DEAV-FLAKPLOC-IN = JA                                       
067100              COMPUTE WS-DISPUTSKR = WS-DISP + WS-DISPAK                  
067200           ELSE                                                           
067300              COMPUTE WS-DISPUTSKR = WS-DISP                              
067400           END-IF                                                         
067500        ELSE                                                              
067600           IF WS-FLRES = JA                                               
067700              COMPUTE WS-DISPUTSKR = WS-DISPPL                            
067800           ELSE                                                           
067900              IF DEAV-KDORDKL-IN = 1                                      
068000                 COMPUTE WS-DISPUTSKR = WS-DISP                           
068100              ELSE                                                        
068200                 IF DEAV-KDORDKL-IN = 2 OR 3 OR 4                         
068300                    IF DEAV-RERF-ART-IN < 1.0000                          
068400                       MOVE WS-DISP TO WS-DISPUTSKR                       
068500                    ELSE                                                  
068600                       COMPUTE WS-DISPUTSKR = WS-DISP -                   
068700                              (ART-KVOKS-DAG - ART-KVPREAVB-DAG)          
068800                       END-COMPUTE                                        
068900                       IF WS-DISPUTSKR NEGATIVE                           
069000                          MOVE 0 TO WS-DISPUTSKR                          
069100                       END-IF                                             
069200                    END-IF                                                
069300                 END-IF                                                   
069400              END-IF                                                      
069500           END-IF                                                         
069600        END-IF                                                            
069700     END-IF                                                               
069800                                                                          
069900     IF DEAV-KDSORT-IN = 'L ' AND                                         
070000        DEAV-KVQPACK-1-IN > ZERO                                          
070100                                                                          
070200       IF WS-DISPUTSKR < DEAV-KVBEART-Q-IN                                
070300          COMPUTE WS-DISPUTSKR-KVANT =                                    
070400                  WS-DISPUTSKR / DEAV-KVQPACK-1-IN                        
070500          COMPUTE WS-DISPUTSKR-KVANT =                                    
070600                  WS-DISPUTSKR-KVANT * DEAV-KVQPACK-1-IN                  
070700          MOVE WS-DISPUTSKR-KVANT TO WS-DISPUTSKR                         
070800       END-IF                                                             
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200 DC-SKAPA-VORKO                          SECTION.                         
071300                                                                          
071400     IF DEAV-KVAVBART-UT = 0                                              
071500        MOVE WS-KDORDBEK-92            TO DEAV-KDORDBEK-UT                
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 DD-BEST-RO-SLATT                        SECTION.                         
072000                                                                          
072100     MOVE DEAV-KVPRERO-IN              TO WS-PRERO                        
072200                                                                          
072300     IF DEAV-KVAVBART-UT = 0                                              
072400        IF DEAV-FLRESTN-IN = NEJ                                          
072500           MOVE WS-KDORDBEK-80         TO DEAV-KDORDBEK-UT                
072600        ELSE                                                              
072700           IF DEAV-TIRODAT-IN > 0                                         
072800              OR                                                          
072900              DEAV-KDTPOTYP-IN = 6                                        
073000              MOVE WS-KDORDBEK-91      TO DEAV-KDORDBEK-UT                
073100           ELSE                                                           
073200              MOVE WS-KDORDBEK-90      TO DEAV-KDORDBEK-UT                
073300           END-IF                                                         
073400        END-IF                                                            
073500     ELSE                                                                 
073600        COMPUTE WS-FATTAS = DEAV-KVBEART-Q-IN                             
073700                          - DEAV-KVAVBART-UT                              
073800        END-COMPUTE                                                       
073900        IF DEAV-KVPRERO-IN NOT = WS-FATTAS                                
074000           MOVE WS-FATTAS        TO WS-PRERO                              
074100        END-IF                                                            
074200     END-IF                                                               
074300     .                                                                    
074400     EJECT                                                                
074500 F-UPDATE-DB-WDK9                           SECTION.                      
074600                                                                          
074700     IF DEAV-FLORDSPE-IN = NEJ                                            
074800       IF DEAV-FLOVRLEV-IN = NEJ                                          
074900         IF SEGMENT-FINNS                                                 
075000            PERFORM FA-LAS-ARTM01                                         
075100            IF DEAV-TIRODAT-IN > 0                                        
075200               PERFORM FE-COMPUTE-WDK9-PRERO                              
075300            ELSE                                                          
075400               IF DEAV-KDORDKL-IN = 0                                     
075500                  PERFORM FB-COMPUTE-WDK9-VOR                             
075600               ELSE                                                       
075700                  IF DEAV-KDORDKL-IN = 1                                  
075800                     PERFORM FC-COMPUTE-WDK9-DAG                          
075900                  ELSE                                                    
076000                     PERFORM FD-COMPUTE-WDK9-BULK                         
076100                  END-IF                                                  
076200               END-IF                                                     
076300            END-IF                                                        
076400            IF DEAV-TIRODAT-IN = 0                                        
076500               AND                                                        
076600            (DEAV-IDKUNDRF-RO-IN  = '00000     ' OR '0000000   ')         
076700               MOVE DEAV-RERF-ART-IN  TO ART-RERF-ART                     
076800            END-IF                                                        
076900            PERFORM IMS-REPL-ARTM01                                       
077000         END-IF                                                           
077100       END-IF                                                             
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
077500 FA-LAS-ARTM01                           SECTION.                         
077600                                                                          
077700     PERFORM IMS-GHU-ARTM01                                               
077800     .                                                                    
077900     EJECT                                                                
078000 FB-COMPUTE-WDK9-VOR                      SECTION.                        
078100                                                                          
078200     IF DEAV-KVAVBART-UT > 0                                              
078300       COMPUTE ART-KVOKS-VOR  = ART-KVOKS-VOR                             
078400                              - DEAV-KVBEART-Q-IN                         
078500       END-COMPUTE                                                        
078600     END-IF                                                               
078700                                                                          
078800     COMPUTE ART-KVPREAVB-VOR = ART-KVPREAVB-VOR                          
078900                              - DEAV-KVBEART-Q-IN                         
079000     END-COMPUTE                                                          
079100     .                                                                    
079200     EJECT                                                                
079300 FC-COMPUTE-WDK9-DAG                     SECTION.                         
079400                                                                          
079500     IF DEAV-TIRODAT-IN     = ZERO         AND                            
079600        DEAV-IDKAMPRF-IN    = ZERO                                        
079700        COMPUTE ART-KVOKS-DAG = ART-KVOKS-DAG                             
079800                              - DEAV-KVBEART-Q-IN                         
079900        END-COMPUTE                                                       
080000     END-IF                                                               
080100                                                                          
080200     IF DEAV-KVAVBART-UT = 0                                              
080300        COMPUTE ART-KVPRERO-DAG = ART-KVPRERO-DAG - WS-PRERO              
080400        END-COMPUTE                                                       
080500     ELSE                                                                 
080600        IF WS-PRERO NOT = DEAV-KVPRERO-IN                                 
080700           COMPUTE WS-PRERO = DEAV-KVPRERO-IN - WS-PRERO                  
080800           END-COMPUTE                                                    
080900           COMPUTE ART-KVPRERO-DAG = ART-KVPRERO-DAG - WS-PRERO           
081000           END-COMPUTE                                                    
081100        END-IF                                                            
081200     END-IF                                                               
081300                                                                          
081400     COMPUTE ART-KVPREAVB-DAG = ART-KVPREAVB-DAG                          
081500                                - DEAV-KVPREAVB-IN                        
081600     END-COMPUTE                                                          
081700     .                                                                    
081800     EJECT                                                                
081900 FD-COMPUTE-WDK9-BULK                    SECTION.                         
082000                                                                          
082100     IF DEAV-TIRODAT-IN     = ZERO         AND                            
082200        DEAV-IDKAMPRF-IN    = ZERO                                        
082300        COMPUTE ART-KVOKS-BULK = ART-KVOKS-BULK                           
082400                               - DEAV-KVBEART-Q-IN                        
082500        END-COMPUTE                                                       
082600     END-IF                                                               
082700                                                                          
082800     IF DEAV-KVAVBART-UT = 0                                              
082900        COMPUTE ART-KVPRERO-BULK = ART-KVPRERO-BULK - WS-PRERO            
083000        END-COMPUTE                                                       
083100     ELSE                                                                 
083200        IF WS-PRERO NOT = DEAV-KVPRERO-IN                                 
083300           COMPUTE WS-PRERO = DEAV-KVPRERO-IN - WS-PRERO                  
083400           END-COMPUTE                                                    
083500           COMPUTE ART-KVPRERO-BULK = ART-KVPRERO-BULK - WS-PRERO         
083600           END-COMPUTE                                                    
083700        END-IF                                                            
083800     END-IF                                                               
083900                                                                          
084000     COMPUTE ART-KVPREAVB-BULK = ART-KVPREAVB-BULK                        
084100                               - DEAV-KVPREAVB-IN                         
084200     END-COMPUTE                                                          
084300     .                                                                    
084400     EJECT                                                                
084500 FE-COMPUTE-WDK9-PRERO                   SECTION.                         
084600                                                                          
084700     IF DEAV-KDORDKL-IN > 0                                               
084800        IF DEAV-KVAVBART-UT > 0 AND < DEAV-KVBEART-Q-IN                   
084900           IF WS-PRERO NOT = DEAV-KVPRERO-IN                              
085000              COMPUTE WS-PRERO = DEAV-KVPRERO-IN - WS-PRERO               
085100              END-COMPUTE                                                 
085200              IF DEAV-KDORDKL-IN = 1                                      
085300                 COMPUTE ART-KVPRERO-DAG =                                
085400                         ART-KVPRERO-DAG - WS-PRERO                       
085500                 END-COMPUTE                                              
085600              ELSE                                                        
085700                 COMPUTE ART-KVPRERO-BULK =                               
085800                         ART-KVPRERO-BULK - WS-PRERO                      
085900                 END-COMPUTE                                              
086000              END-IF                                                      
086100           END-IF                                                         
086200        END-IF                                                            
086300     END-IF                                                               
086400     .                                                                    
086500     EJECT                                                                
086600 G-GIVE-VALUE-TO-WDK6                           SECTION.                  
086700     MOVE 'G-GIVE-STA'              TO FELTEXT                            
086800* WDK6 UPPDATERAS I W40375 M.H.A. VÄRDEN FRÅN W411DEAV-UT-AREA.           
086900                                                                          
087000     MOVE DEAV-KVAVBART-UT        TO DEAV-KVEFRS-UT                       
087100                                     DEAV-KVLS-UT                         
087200     MOVE 1                       TO DEAV-KDROO-UT                        
087300                                                                          
087400     IF WS-FLRES = JA                                                     
087500        MOVE DEAV-KVBEART-Q-IN    TO DEAV-KVRESS-UT                       
087600     END-IF                                                               
087700                                                                          
087800     IF DEAV-KDORDKL-IN = 0                                               
087900        CONTINUE                                                          
088000     ELSE                                                                 
088100        IF DEAV-KVAVBART-UT = 0                                           
088200           IF DEAV-FLRESTN-IN = JA                                        
088300              MOVE DEAV-KVBEART-Q-IN  TO DEAV-KVROS-UT                    
088400           END-IF                                                         
088500        END-IF                                                            
088600     END-IF                                                               
088700                                                                          
088800*REFILLORDER                                                              
088900                                                                          
089000     IF (DIST35-REFILL                                                    
089100     OR  DIST35-REFILL-INOM-NDC                                           
089200     OR  DIST35-NONVCC-NONVCC-REFILL                                      
089300     OR  DIST35-REFILL-NA-JAP                                             
089310     OR  DIST35-NONVCC-VCC-REFILL                                         
089400     OR  DIST35-CDC-NL-RETUR)                                             
089500     AND DEAV-FLRESTN-IN = NEJ                                            
089600     AND WS-FATTAS = DEAV-KVBEART-Q-IN                                    
089700       MOVE 'G-GIVE-Z'              TO FELTEXT                            
089800                                                                          
089900       IF DIST35-CDC-NL-RETUR                                             
090000         MOVE WC-SDC-NL             TO W-WDK711-IDDC                      
090100       ELSE                                                               
090200                                                                          
090300         PERFORM S02-GET-SDC-IDDC-VALUE                                   
090400       END-IF                                                             
090500                                                                          
090600       PERFORM IMS-GHU-WDK711                                             
090700       SUBTRACT WS-FATTAS           FROM SLAG-KVBEART                     
090800                                                                          
090900       PERFORM IMS-REPL-WDK711                                            
091000     ELSE                                                                 
091100*GK DISTRIKT 9111 BOKADE INTE NER KVBEART                                 
091200*ES DISTRIKT 9211 OCKSÅ TILLAGT                                           
091300        IF DIST35-NONVCC-CDC-REFILL                                       
091400                                                                          
091500           PERFORM IMS-GHU-WDK611                                         
091600           SUBTRACT WS-FATTAS       FROM CLAG-KVBEART                     
091700           PERFORM IMS-REPL-WDK611                                        
091800        END-IF                                                            
091900     END-IF                                                               
092000     .                                                                    
092100     EJECT                                                                
092200 E-UPDATE-DB-WDK7                           SECTION.                      
092300                                                                          
092400     MOVE DEAV-IDDISTR-IN     TO DIST18-IDDISTR                           
092500                                 DIST35-IDDISTR                           
092600                                                                          
092700     MOVE 'E-SEC-STA'              TO FELTEXT                             
092800     PERFORM IMS-GHU-WDK711                                               
092900                                                                          
093000     IF SEGMENT-FINNS                                                     
093100                                                                          
093200       IF DEAV-FLOVRLEV-IN = NEJ                                          
093300                                                                          
093400         EVALUATE TRUE                                                    
093500         WHEN DIST18-SKROT-KVAL-CDC                                       
093600           OR DIST18-SKROT-KVAL-SDC                                       
093700           OR DIST18-SKROT-KVAL-LDC                                       
093800*          COMPUTE WS-DISP-WDK7 = SLAG-KVLS                               
093900*          END-COMPUTE                                                    
094000           COMPUTE WS-DISP-WDK7 = DEAV-KVBEART-Q-IN                       
094100           END-COMPUTE                                                    
094200         WHEN DIST18-SKROT                                                
094300           COMPUTE WS-DISP-WDK7 = SLAG-KVLS                               
094400                                  - SLAG-KVRESS                           
094500                                  - SLAG-KVOKS-DAG                        
094600                                  - SLAG-KVOKS-BULK                       
094700                                  - SLAG-KVSPARR-KVAL                     
094800                                  - SLAG-KVUTRS                           
094900           END-COMPUTE                                                    
095000         WHEN OTHER                                                       
095100           COMPUTE WS-DISP-WDK7 = SLAG-KVLS + SLAG-KVAKS-SDC              
095200           END-COMPUTE                                                    
095300         END-EVALUATE                                                     
095400                                                                          
095500         IF WS-DISP-WDK7 > ZERO                                           
095600           IF DIST18-SKROT                                                
095700           OR DIST35-RETUR                                                
095800             CONTINUE                                                     
095900           ELSE                                                           
096000             COMPUTE WS-DISP-WDK7 = WS-DISP-WDK7 - SLAG-KVUTRS            
096100                                              - SLAG-KVSPARR-KVAL         
096200             END-COMPUTE                                                  
096300             IF WS-DISP-WDK7 < ZERO                                       
096400                MOVE 0 TO WS-DISP-WDK7                                    
096500             END-IF                                                       
096600           END-IF                                                         
096700         ELSE                                                             
096800           MOVE 0 TO WS-DISP-WDK7                                         
096900         END-IF                                                           
097000       END-IF                                                             
097100                                                                          
097200       IF DEAV-KVBEART-Q-IN > WS-DISP-WDK7 AND                            
097300          DEAV-FLOVRLEV-IN = NEJ                                          
097400*                     AVVIKELSE-UPPDATERING                               
097500         COMPUTE WS-FATTAS = DEAV-KVBEART-Q-IN - WS-DISP-WDK7             
097600         END-COMPUTE                                                      
097700                                                                          
097800         IF DEAV-KVBEART-Q-IN > WS-FATTAS                                 
097900           COMPUTE WS-KVAVBART = DEAV-KVBEART-Q-IN - WS-FATTAS            
098000           END-COMPUTE                                                    
098100           MOVE WS-KVAVBART            TO DEAV-KVAVBART-UT                
098200         ELSE                                                             
098300           MOVE ZERO                   TO WS-KVAVBART                     
098400                                            DEAV-KVAVBART-UT              
098500         END-IF                                                           
098600                                                                          
098700         IF  DEAV-KDORDKL-IN = +0                                         
098800         AND DEAV-FLSDCLEV-IN NOT = JA                                    
098900*          92 = VOR KÖ, RESTNOTERAD KVANT WDR4 HTR-4542.                  
099000           MOVE WS-KDORDBEK-92       TO DEAV-KDORDBEK-UT                  
099100           IF DEAV-FLORDSPE-IN = NEJ                                      
099200             IF DEAV-FLOVRLEV-IN = NEJ                                    
099300                IF WS-KVAVBART > ZERO                                     
099400                  SUBTRACT DEAV-KVBEART-Q-IN FROM SLAG-KVOKS-DAG          
099500                  ADD      WS-FATTAS         TO   SLAG-KVOKS-DAG          
099600                END-IF                                                    
099700             END-IF                                                       
099800           END-IF                                                         
099900           ADD WS-KVAVBART           TO   SLAG-KVEFRS                     
100000           SUBTRACT WS-KVAVBART      FROM SLAG-KVLS                       
100100           MOVE ZERO                 TO DEAV-KVROS-UT                     
100200                                                                          
100300***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
100400           MOVE WS-KVAVBART          TO LOGG-KVART-SALDO                  
100500           PERFORM S03-SKAPA-SALDOLOGG                                    
100600         ELSE                                                             
100700             MOVE DEAV-IDDISTR-IN    TO TEST-IDDISTR                      
100800             IF DIST18-SKROT                                              
100900             OR DIST35-RETUR                                              
101000               IF DEAV-FLORDSPE-IN = NEJ                                  
101100                 IF DEAV-FLOVRLEV-IN = NEJ                                
101200                   IF DEAV-KDORDKL-IN > +1                                
101300                  SUBTRACT DEAV-KVBEART-Q-IN FROM SLAG-KVOKS-BULK         
101400                   ELSE                                                   
101500                  SUBTRACT DEAV-KVBEART-Q-IN FROM SLAG-KVOKS-DAG          
101600                   END-IF                                                 
101700                 END-IF                                                   
101800               END-IF                                                     
101900               IF DEAV-KVBEART-Q-IN > SLAG-KVLS                           
102000                 MOVE DEAV-KVBEART-Q-IN     TO WS-KVBEART                 
102100                 COMPUTE WS-REST = WS-KVBEART - SLAG-KVLS                 
102200                 END-COMPUTE                                              
102300                 COMPUTE WS-KVBEART = WS-KVBEART - WS-REST                
102400                 END-COMPUTE                                              
102500                 SUBTRACT WS-KVBEART        FROM SLAG-KVLS                
102600                 ADD WS-KVBEART             TO SLAG-KVEFRS                
102700                 MOVE WS-KVBEART            TO DEAV-KVAVBART-UT           
102800                                               LOGG-KVART-SALDO           
102900               ELSE                                                       
103000                 SUBTRACT WS-KVAVBART       FROM SLAG-KVLS                
103100                 ADD WS-KVAVBART            TO SLAG-KVEFRS                
103200                 MOVE WS-KVAVBART           TO LOGG-KVART-SALDO           
103300               END-IF                                                     
103400               PERFORM S03-SKAPA-SALDOLOGG                                
103500               MOVE ZERO                    TO DEAV-KVROS-UT              
103600             ELSE                                                         
103700               IF DEAV-FLRESTN-IN = NEJ                                   
103800*          80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.ANNUL ANT.          
103900                 MOVE WS-KDORDBEK-80   TO DEAV-KDORDBEK-UT                
104000                 IF DEAV-FLORDSPE-IN = NEJ                                
104100                   IF DEAV-FLOVRLEV-IN = NEJ                              
104200                     IF DEAV-KDORDKL-IN > +1                              
104300                       SUBTRACT DEAV-KVBEART-Q-IN                         
104400                                             FROM SLAG-KVOKS-BULK         
104500                     ELSE                                                 
104600                       SUBTRACT DEAV-KVBEART-Q-IN                         
104700                                             FROM SLAG-KVOKS-DAG          
104800                     END-IF                                               
104900                   END-IF                                                 
105000                 END-IF                                                   
105100                                                                          
105200                 IF WS-KVAVBART > ZERO                                    
105300                   SUBTRACT WS-KVAVBART     FROM SLAG-KVLS                
105400                   ADD WS-KVAVBART       TO SLAG-KVEFRS                   
105500                                                                          
105600***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. **          
105700                   MOVE WS-KVAVBART      TO LOGG-KVART-SALDO              
105800                   PERFORM S03-SKAPA-SALDOLOGG                            
105900                 ELSE                                                     
106000                   MOVE ZERO             TO DEAV-KVAVBART-UT              
106100                 END-IF                                                   
106200                 MOVE ZERO               TO DEAV-KVROS-UT                 
106300               ELSE                                                       
106400*            90 = RESTNOTERING PÅ CDC WDQ1.                               
106500                 MOVE WS-KDORDBEK-90     TO DEAV-KDORDBEK-UT              
106600                 IF DEAV-FLORDSPE-IN = NEJ                                
106700                   IF DEAV-FLOVRLEV-IN = NEJ                              
106800                     IF DEAV-KDORDKL-IN > +1                              
106900                       SUBTRACT DEAV-KVBEART-Q-IN                         
107000                                             FROM SLAG-KVOKS-BULK         
107100                     ELSE                                                 
107200                       SUBTRACT DEAV-KVBEART-Q-IN                         
107300                                             FROM SLAG-KVOKS-DAG          
107400                     END-IF                                               
107500                   END-IF                                                 
107600                 END-IF                                                   
107700                 SUBTRACT WS-KVAVBART    FROM SLAG-KVLS                   
107800                 ADD WS-KVAVBART         TO   SLAG-KVEFRS                 
107900                 MOVE WS-FATTAS          TO DEAV-KVROS-UT                 
108000                                                                          
108100***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
108200                 MOVE WS-KVAVBART        TO LOGG-KVART-SALDO              
108300                 PERFORM S03-SKAPA-SALDOLOGG                              
108400               END-IF                                                     
108500             END-IF                                                       
108600         END-IF                                                           
108700       ELSE                                                               
108800                                                                          
108900         ADD    DEAV-KVBEART-Q-IN      TO   SLAG-KVEFRS                   
109000         SUBTRACT DEAV-KVBEART-Q-IN    FROM SLAG-KVLS                     
109100                                                                          
109200***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
109300         MOVE DEAV-KVBEART-Q-IN        TO LOGG-KVART-SALDO                
109400         PERFORM S03-SKAPA-SALDOLOGG                                      
109500                                                                          
109600         IF DEAV-FLORDSPE-IN = NEJ                                        
109700           IF DEAV-FLOVRLEV-IN = NEJ                                      
109800             IF DEAV-KDORDKL-IN > +1                                      
109900               SUBTRACT DEAV-KVBEART-Q-IN                                 
110000                                     FROM SLAG-KVOKS-BULK                 
110100             ELSE                                                         
110200               SUBTRACT DEAV-KVBEART-Q-IN                                 
110300                                     FROM SLAG-KVOKS-DAG                  
110400             END-IF                                                       
110500           END-IF                                                         
110600         END-IF                                                           
110700                                                                          
110800         MOVE DEAV-KVBEART-Q-IN        TO DEAV-KVAVBART-UT                
110900         MOVE SLAG-KVLS                TO DEAV-KVLS-UT                    
111000         MOVE ZERO                     TO DEAV-KDORDBEK-UT                
111100                                            DEAV-KVROS-UT                 
111200       END-IF                                                             
111300                                                                          
111400       MOVE 'G-GIVE-Y'              TO FELTEXT                            
111600       IF DCS-SDC                                                         
111700           PERFORM IMS-REPL-WDK711                                        
112000       END-IF                                                             
112100     ELSE                                                                 
112200       MOVE 'E-SEC-Z'              TO FELTEXT                             
112300                                                                          
112400       MOVE ALL '+'        TO WDK7-W005WDK7                               
112500       MOVE 'WDK711'       TO WDK7-IDSEGM                                 
112600       MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                            
112700       MOVE DEAV-IDDC-IN   TO WDK7-IDDC-KFB                               
112800                              WDK7-IDDC                                   
112900       MOVE DEAV-KDLEVSP-IN       TO WDK7-KDLEVSP                         
113000       MOVE DEAV-IDUSER-SPKVAL-IN TO WDK7-IDUSER-SPKVAL                   
113100                                                                          
113200       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                
113300                                         WDK7-PCB                         
113400     END-IF                                                               
113500                                                                          
113600     PERFORM EA-UPPDAT-SKROT                                              
113700                                                                          
113800     MOVE ZERO                         TO DEAV-KVRESS-UT                  
113900                                          DEAV-KVEFRS-UT                  
114000                                          DEAV-RERF-RAD-UT                
114100     MOVE 1                            TO DEAV-KDROO-UT                   
114200     MOVE NEJ                          TO DEAV-FLAKPLOC-UT                
114300     .                                                                    
114400     EJECT                                                                
114500 EA-UPPDAT-SKROT                SECTION.                                  
114600                                                                          
114700     MOVE DEAV-IDDISTR-IN              TO TEST-IDDISTR                    
114800                                                                          
114900     IF DIST18-SKROT                                                      
115000        IF DEAV-KVAVBART-UT > 0                                           
115100           MOVE 'EA-UPP-1'              TO FELTEXT                        
115200           PERFORM IMS-GHU-WDK711                                         
115300                                                                          
115400           MOVE NEJ                    TO SLAG-FLSKROT-BEORD              
115500           MOVE DEAV-KVBEART-Q-IN      TO SLAG-KVSKROT                    
115600           MOVE WS-DATUM               TO SLAG-TISKROT                    
115700           MOVE ZERO                   TO SLAG-TISKROT-BEORD              
115800           MOVE 'EA-UPP-2'              TO FELTEXT                        
115900           PERFORM IMS-REPL-WDK711                                        
116000        END-IF                                                            
116100     END-IF                                                               
116200     .                                                                    
116300     EJECT                                                                
123200                                                                          
123300 EC-ANPASSA-DISP-TILL-KVANT SECTION.                                      
123400                                                                          
123500     IF DEAV-KDSORT-IN = 'L ' AND                                         
123600        DEAV-KVQPACK-1-IN > ZERO                                          
123700                                                                          
123800       IF WS-DISP-WDK7 < DEAV-KVBEART-Q-IN                                
123900          COMPUTE WS-DISPUTSKR-KVANT =                                    
124000                  WS-DISP-WDK7 / DEAV-KVQPACK-1-IN                        
124100          COMPUTE WS-DISPUTSKR-KVANT =                                    
124200                  WS-DISPUTSKR-KVANT * DEAV-KVQPACK-1-IN                  
124300          MOVE WS-DISPUTSKR-KVANT TO WS-DISP-WDK7                         
124400       END-IF                                                             
124500     END-IF                                                               
124600     .                                                                    
124700     EJECT                                                                
124800                                                                          
124900 H-NDC-UPDATE-DB-WDK7                       SECTION.                      
125000     MOVE 'H-NDC-UPDATE-DB-WDK7'   TO CURRENT-SECTION                     
125100     MOVE 'H-NDC-STA'              TO FELTEXT                             
125200     MOVE ZERO TO DEAV-KVAVBART-UT                                        
125300                  DEAV-KVLS-UT                                            
125400                  DEAV-KDORDBEK-UT                                        
125500                  DEAV-KVROS-UT                                           
125600     MOVE 1    TO DEAV-KDROO-UT                                           
125700                                                                          
125800     MOVE DEAV-IDDISTR-IN TO TEST-IDDISTR                                 
125900                                                                          
126000     MOVE NEJ  TO WS-ALLT-I-REST                                          
126100                                                                          
126200     PERFORM IMS-GHU-WDK711                                               
126300                                                                          
126400     IF SEGMENT-FINNS                                                     
126500       IF  DEAV-FLORDSPE-IN = NEJ                                         
126600       AND DEAV-FLOVRLEV-IN = NEJ                                         
126700       AND NOT (DEAV-KDORDKL-IN = 0                                       
126800            AND (DEAV-FLFORBI-IN = JA OR                                  
126900                 DEAV-FLFORBI-IN = SPEC-FORBI)                            
127000            AND SLAG-KVSPARR-KVAL =  0                                    
127100            AND SLAG-KVAKS-SDC    =  0                                    
127200            AND SLAG-KDLEVSP      =  0)                                   
127300         PERFORM HD-NDC-KOLLA-DISPONIBELT                                 
127400         MOVE WS-DISP TO WS-DISP-WDK7                                     
127500                                                                          
127600         IF  DEAV-KDORDKL-IN > +1                                         
127700           MOVE DEAV-IDDISTR-IN TO TEST-IDDISTR                           
127800           IF   SLAG-KDLEVSP  > 0                                         
127900           AND NOT DIST18-SCRAP-NDC-QUAL                                  
128000           AND NOT DIST18-SCRAP-NDC-SC                                    
128100           AND NOT DIST18-SKROT                                           
128200           AND NOT DIST35-NA-CDC-RETURN                                   
128300           AND NOT DIST35-CDC-RETURNS-NON-VCC                             
128400           AND NOT DIST35-CN-NDC-RETURNS                                  
128500           AND NOT DIST35-JPAU-CDC-RETUR-Q                                
128600           OR  ((NOT DIST35-NA-TRANSFER                                   
128700           AND   NOT DIST35-NA-NDC-RETURNS                                
128800           AND   NOT DIST35-CN-TRANSFER                                   
128900           AND   NOT DIST35-REFILL-INOM-NDC                               
129000           AND   NOT DIST35-REFILL-INOM-JP                                
129100           AND   NOT DIST35-PACIFIC-TRANSFER)                             
129200           AND (SLAG-FLSPBULK = JA                                        
129300           OR   SLAG-FLORDSP  = JA)                                       
129400           AND NOT DIST18-SCRAP-NDC-QUAL                                  
129500           AND NOT DIST18-SCRAP-NDC-SC                                    
129600           AND NOT DIST18-SKROT                                           
129700           AND NOT DIST35-NA-CDC-RETURN                                   
129800           AND NOT DIST35-CDC-RETURNS-NON-VCC                             
129900           AND NOT DIST35-CN-NDC-RETURNS)                                 
130000             IF DEAV-FLRESTN-IN = NEJ                                     
130100               MOVE WS-KDORDBEK-80     TO DEAV-KDORDBEK-UT                
130200               MOVE ZERO               TO DEAV-KVROS-UT                   
130300             ELSE                                                         
130400               MOVE WS-KDORDBEK-90     TO DEAV-KDORDBEK-UT                
130500               MOVE DEAV-KVBEART-Q-IN  TO DEAV-KVROS-UT                   
130600             END-IF                                                       
130700             MOVE 2                  TO DEAV-KDROO-UT                     
130800             MOVE ZERO               TO DEAV-KVAVBART-UT                  
130900             MOVE JA                 TO WS-ALLT-I-REST                    
131000           ELSE                                                           
131100             IF  WS-DISP-WDK7  < +1                                       
131200               IF DEAV-FLRESTN-IN = NEJ                                   
131300                 MOVE WS-KDORDBEK-80     TO DEAV-KDORDBEK-UT              
131400                 MOVE ZERO               TO DEAV-KVROS-UT                 
131500               ELSE                                                       
131600                 MOVE WS-KDORDBEK-90     TO DEAV-KDORDBEK-UT              
131700                 MOVE DEAV-KVBEART-Q-IN  TO DEAV-KVROS-UT                 
131800               END-IF                                                     
131900               MOVE 1                  TO DEAV-KDROO-UT                   
132000               MOVE ZERO               TO DEAV-KVAVBART-UT                
132100               MOVE JA                 TO WS-ALLT-I-REST                  
132200             END-IF                                                       
132300           END-IF                                                         
132400         ELSE                                                             
132500           IF DEAV-KDORDKL-IN = +1                                        
132600             MOVE DEAV-IDDISTR-IN TO TEST-IDDISTR                         
132700             IF   SLAG-KDLEVSP  > 0                                       
132800             AND NOT DIST18-SCRAP-NDC-QUAL                                
132900             AND NOT DIST18-SCRAP-NDC-SC                                  
133000             AND NOT DIST18-SKROT                                         
133100             AND NOT DIST35-NA-CDC-RETURN                                 
133200             AND NOT DIST35-CDC-RETURNS-NON-VCC                           
133300             AND NOT DIST35-CN-NDC-RETURNS                                
133400             AND NOT DIST35-RETUR-Q                                       
133500             OR ((NOT DIST35-NA-TRANSFER                                  
133600             AND  NOT DIST35-NA-NDC-RETURNS                               
133700             AND  NOT DIST35-CN-TRANSFER                                  
133800             AND  NOT DIST35-REFILL-INOM-NDC                              
133900             AND  NOT DIST35-REFILL-INOM-JP                               
134000             AND  NOT DIST35-PACIFIC-TRANSFER) AND                        
134100                 (SLAG-FLORDSP = JA)                                      
134200             AND NOT DIST18-SCRAP-NDC-QUAL                                
134300             AND NOT DIST18-SCRAP-NDC-SC                                  
134400             AND NOT DIST18-SKROT                                         
134500             AND NOT DIST35-NA-CDC-RETURN                                 
134600             AND NOT DIST35-CDC-RETURNS-NON-VCC                           
134700             AND NOT DIST35-CN-NDC-RETURNS)                               
134800               IF DEAV-FLRESTN-IN = NEJ                                   
134900                 MOVE WS-KDORDBEK-80   TO DEAV-KDORDBEK-UT                
135000                 MOVE ZERO             TO DEAV-KVROS-UT                   
135100               ELSE                                                       
135200                 MOVE WS-KDORDBEK-90   TO DEAV-KDORDBEK-UT                
135300                 MOVE DEAV-KVBEART-Q-IN TO DEAV-KVROS-UT                  
135400               END-IF                                                     
135500               MOVE 2                TO DEAV-KDROO-UT                     
135600               MOVE ZERO             TO DEAV-KVAVBART-UT                  
135700               MOVE JA               TO WS-ALLT-I-REST                    
135800             ELSE                                                         
135900               IF WS-DISP-WDK7 < +1                                       
136000                 IF DEAV-FLRESTN-IN = NEJ                                 
136100                   MOVE WS-KDORDBEK-80   TO DEAV-KDORDBEK-UT              
136200                   MOVE ZERO             TO DEAV-KVROS-UT                 
136300                 ELSE                                                     
136400                   MOVE WS-KDORDBEK-90   TO DEAV-KDORDBEK-UT              
136500                   MOVE DEAV-KVBEART-Q-IN TO DEAV-KVROS-UT                
136600                 END-IF                                                   
136700                 MOVE 1                TO DEAV-KDROO-UT                   
136800                 MOVE ZERO             TO DEAV-KVAVBART-UT                
136900                 MOVE JA               TO WS-ALLT-I-REST                  
137000               END-IF                                                     
137100             END-IF                                                       
137200           ELSE                                                           
137300             IF DEAV-KDORDKL-IN = +0                                      
137400             AND DEAV-FLFORBI-IN = NEJ                                    
137500               IF SLAG-KDLEVSP > 0                                        
137600*****BORT  OR  SLAG-FLORDSP  = JA  **970310/BS**************              
137700                 MOVE ZERO           TO DEAV-KVAVBART-UT                  
137800                 MOVE WS-KDORDBEK-92 TO DEAV-KDORDBEK-UT                  
137900                 MOVE 0              TO DEAV-KVROS-UT                     
138000                 MOVE 2              TO DEAV-KDROO-UT                     
138100                 PERFORM HB-UPDATE-WDK7-VOR                               
138200                 MOVE JA             TO WS-ALLT-I-REST                    
138300               ELSE                                                       
138400                 IF WS-DISP-WDK7 < +1                                     
138500                   MOVE ZERO           TO DEAV-KVAVBART-UT                
138600                   MOVE WS-KDORDBEK-92 TO DEAV-KDORDBEK-UT                
138700                   MOVE 0              TO DEAV-KVROS-UT                   
138800                   MOVE 1              TO DEAV-KDROO-UT                   
138900                   PERFORM HB-UPDATE-WDK7-VOR                             
139000                   MOVE JA             TO WS-ALLT-I-REST                  
139100                 END-IF                                                   
139200               END-IF                                                     
139300             ELSE                                                         
139400               IF SLAG-KDLEVSP > 0                                        
139500                 MOVE ZERO           TO DEAV-KVAVBART-UT                  
139600                 MOVE WS-KDORDBEK-92 TO DEAV-KDORDBEK-UT                  
139700                 MOVE 0              TO DEAV-KVROS-UT                     
139800                 MOVE 2              TO DEAV-KDROO-UT                     
139900                 PERFORM HB-UPDATE-WDK7-VOR                               
140000                 MOVE JA             TO WS-ALLT-I-REST                    
140100               ELSE                                                       
140200                 IF WS-DISP-WDK7 < +1                                     
140300                   MOVE ZERO           TO DEAV-KVAVBART-UT                
140400                   MOVE WS-KDORDBEK-92 TO DEAV-KDORDBEK-UT                
140500                   MOVE 0              TO DEAV-KVROS-UT                   
140600                   MOVE 1              TO DEAV-KDROO-UT                   
140700                   PERFORM HB-UPDATE-WDK7-VOR                             
140800                   MOVE JA             TO WS-ALLT-I-REST                  
140900                 END-IF                                                   
141000               END-IF                                                     
141100             END-IF                                                       
141200           END-IF                                                         
141300         END-IF                                                           
141400                                                                          
141500         IF WS-ALLT-I-REST = NEJ                                          
141600           IF  DEAV-KVBEART-Q-IN > WS-DISP-WDK7                           
141700*-------------------- DELTÄCKNING AV RAD                                  
141800*                                                                         
141900             COMPUTE WS-FATTAS = DEAV-KVBEART-Q-IN                        
142000                               - WS-DISP-WDK7                             
142100             END-COMPUTE                                                  
142200                                                                          
142300             COMPUTE WS-KVAVBART = DEAV-KVBEART-Q-IN                      
142400                                 - WS-FATTAS                              
142500             END-COMPUTE                                                  
142600                                                                          
142700             MOVE WS-KVAVBART          TO DEAV-KVAVBART-UT                
142800             ADD  WS-KVAVBART          TO SLAG-KVEFRS                     
142900             SUBTRACT WS-KVAVBART    FROM SLAG-KVLS                       
143000                                                                          
143100***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
143200             MOVE WS-KVAVBART          TO LOGG-KVART-SALDO                
143300             PERFORM S03-SKAPA-SALDOLOGG                                  
143400                                                                          
143500             MOVE 0                    TO DEAV-KDORDBEK-UT                
143600             MOVE ZERO                 TO DEAV-KVROS-UT                   
143700             MOVE WS-KVAVBART          TO DEAV-KVLS-UT                    
143800                                                                          
143900*EJ UPP PÅ   I   DEAV-KDORDKL-IN = +0                                     
144000*VOR-KÖN       MOVE WS-KDORDBEK-92     TO DEAV-KDORDBEK-UT                
144100*FÖRRÄN I      PERFORM HB-UPDATE-WDK7-VOR                                 
144200*PACKN.      END-I                                                        
144300           ELSE                                                           
144400*--------- DISP FINNS                                                     
144500             ADD DEAV-KVBEART-Q-IN         TO SLAG-KVEFRS                 
144600             SUBTRACT DEAV-KVBEART-Q-IN  FROM SLAG-KVLS                   
144700                                                                          
144800***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
144900             MOVE DEAV-KVBEART-Q-IN        TO LOGG-KVART-SALDO            
145000             PERFORM S03-SKAPA-SALDOLOGG                                  
145100                                                                          
145200             MOVE DEAV-KVBEART-Q-IN        TO DEAV-KVAVBART-UT            
145300                                              DEAV-KVLS-UT                
145400             MOVE ZERO                     TO DEAV-KDORDBEK-UT            
145500                                              DEAV-KVROS-UT               
145600           END-IF                                                         
145700         END-IF                                                           
145800       ELSE                                                               
145900*--------- ÖVERLEV ELLER FÖRBI SOM BOKAR UNDER NOLL                       
146000*--------- OBS FLLSBOK SAKNAS I DEAV-COPY.                                
146100*        I   DEAV-FLLSBOK-IN = JA                                         
146200           ADD DEAV-KVBEART-Q-IN       TO SLAG-KVEFRS                     
146300           SUBTRACT DEAV-KVBEART-Q-IN FROM SLAG-KVLS                      
146400                                                                          
146500***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
146600           MOVE DEAV-KVBEART-Q-IN        TO LOGG-KVART-SALDO              
146700           PERFORM S03-SKAPA-SALDOLOGG                                    
146800*        END-I                                                            
146900                                                                          
147000         MOVE DEAV-KVBEART-Q-IN        TO DEAV-KVAVBART-UT                
147100                                          DEAV-KVLS-UT                    
147200         MOVE ZERO                     TO DEAV-KDORDBEK-UT                
147300                                          DEAV-KVROS-UT                   
147400       END-IF                                                             
147500                                                                          
147600*--------- RÄKNA NER KVOKS                                                
147700*                                                                         
147800       IF  DEAV-FLOVRLEV-IN = NEJ                                         
147900       AND DEAV-FLORDSPE-IN = NEJ                                         
148000       AND DEAV-TIRODAT-IN = ZERO                                         
148100           IF DEAV-KDORDKL-IN > +1                                        
148200             SUBTRACT DEAV-KVBEART-Q-IN FROM SLAG-KVOKS-BULK              
148300           ELSE                                                           
148400             SUBTRACT DEAV-KVBEART-Q-IN FROM SLAG-KVOKS-DAG               
148500           END-IF                                                         
148600       END-IF                                                             
148700                                                                          
148800*--------- RÄKNA NER KVRESS                                               
148900*                                                                         
149000       IF  DEAV-TIRODAT-IN > ZERO                                         
149100           SUBTRACT DEAV-KVBEART-Q-IN FROM SLAG-KVRESS                    
149200       END-IF                                                             
149300                                                                          
149400*--------- RÄKNA UPP KVROS I RESPEKTIVE NDC                               
149500*          ENDAST NÄR HELA RADEN RESTAS                                   
149600*                                                                         
149700       IF  WS-ALLT-I-REST   = JA                                          
149800       AND DEAV-KDORDBEK-UT = WS-KDORDBEK-90                              
149900                                                                          
150000         IF  DEAV-IDDC-RO-IN = DEAV-IDDC-IN                               
150100                                                                          
150200           IF  DCS-NDC-CN                                                 
150300           OR (DCS-NDC-NA AND DCS-USA)                                    
150400              IF SLAG-IDDC-REF = SPACE                                    
150500                 PERFORM S04-EV-UPD-LARM-WDR550-CN-US                     
150600              END-IF                                                      
150700           END-IF                                                         
150800           IF  DEAV-KDORDKL-IN > +1                                       
150900             ADD DEAV-KVROS-UT        TO SLAG-KVROS-BULK                  
151000           ELSE                                                           
151100             ADD DEAV-KVROS-UT        TO SLAG-KVROS-DAG                   
151200           END-IF                                                         
151300           MOVE 'H-NDC-4'              TO FELTEXT                         
151400           PERFORM IMS-REPL-WDK711                                        
151500                                                                          
151600         ELSE                                                             
151700           MOVE 'H-NDC-2'              TO FELTEXT                         
151800           PERFORM IMS-REPL-WDK711                                        
151900           MOVE DEAV-IDDC-RO-IN       TO W-WDK711-IDDC                    
152000           PERFORM IMS-GHU-WDK711                                         
152100           IF  DCS-NDC-CN                                                 
152200           OR (DCS-NDC-NA AND DCS-USA)                                    
152300              IF SLAG-IDDC-REF = SPACE                                    
152400                 PERFORM S04-EV-UPD-LARM-WDR550-CN-US                     
152500              END-IF                                                      
152600           END-IF                                                         
152700           IF  DEAV-KDORDKL-IN > +1                                       
152800             ADD DEAV-KVROS-UT        TO SLAG-KVROS-BULK                  
152900           ELSE                                                           
153000             ADD DEAV-KVROS-UT        TO SLAG-KVROS-DAG                   
153100           END-IF                                                         
153200           MOVE 'H-NDC-5'              TO FELTEXT                         
153300           PERFORM IMS-REPL-WDK711                                        
153400         END-IF                                                           
153500       ELSE                                                               
153600         MOVE 'H-NDC-6'              TO FELTEXT                           
153700         PERFORM IMS-REPL-WDK711                                          
153800                                                                          
153900*--------- OM NA-TRANSFER, RÄKNA NER KVBEART                              
154000*                                                                         
154100         IF  WS-ALLT-I-REST   = JA                                        
154200         AND DEAV-KDORDBEK-UT = WS-KDORDBEK-80                            
154300         AND (DIST35-NA-TRANSFER OR                                       
154400              DIST35-NA-NDC-RETURNS OR                                    
154500              DIST35-CN-TRANSFER OR                                       
154600              DIST35-REFILL-INOM-NDC OR                                   
154700              DIST35-REFILL-INOM-JP  OR                                   
154800              DIST35-NONVCC-NONVCC-REFILL OR                              
154900              DIST35-PACIFIC-TRANSFER OR                                  
154920              DIST35-NONVCC-VCC-REFILL OR                                 
155000              DIST35-CN-NDC-RETURNS)                                      
155100                                                                          
155200             PERFORM S02-GET-SDC-IDDC-VALUE                               
155300                                                                          
155400             MOVE 'H-NDC-3'              TO FELTEXT                       
155500             PERFORM IMS-GHU-WDK711                                       
155600             SUBTRACT DEAV-KVBEART-Q-IN   FROM SLAG-KVBEART               
155700                                                                          
155800             MOVE 'H-NDC-7'              TO FELTEXT                       
155900             PERFORM IMS-REPL-WDK711                                      
156000          ELSE                                                            
156100           COMPUTE WS-FATTAS = DEAV-KVBEART-Q-IN                          
156200                             - DEAV-KVAVBART-UT                           
156300             IF DIST35-NONVCC-CDC-REFILL                                  
156400             AND DEAV-FLRESTN-IN = NEJ                                    
156500             AND WS-FATTAS = DEAV-KVBEART-Q-IN                            
156600                MOVE 'G-GIVE-Z'            TO FELTEXT                     
156700                                                                          
156800                PERFORM S02-GET-SDC-IDDC-VALUE                            
156900                                                                          
157000                PERFORM IMS-GHU-WDK611                                    
157100                SUBTRACT WS-FATTAS       FROM CLAG-KVBEART                
157200                                                                          
157300                PERFORM IMS-REPL-WDK611                                   
157400             END-IF                                                       
157500         END-IF                                                           
157600       END-IF                                                             
157700                                                                          
157800     ELSE                                                                 
157900       MOVE 'H-SEC-Z'              TO FELTEXT                             
158000       MOVE ALL '+'        TO WDK7-W005WDK7                               
158100       MOVE 'WDK711'       TO WDK7-IDSEGM                                 
158200       MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                            
158300       MOVE DEAV-IDDC-IN   TO WDK7-IDDC-KFB                               
158400                              WDK7-IDDC                                   
158500       MOVE DEAV-KDLEVSP-IN       TO WDK7-KDLEVSP                         
158600       MOVE DEAV-IDUSER-SPKVAL-IN TO WDK7-IDUSER-SPKVAL                   
158700                                                                          
158800       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                
158900                                         WDK7-PCB                         
159000     END-IF                                                               
159100                                                                          
159200     PERFORM HA-UPPDAT-SKROT                                              
159300                                                                          
159400     MOVE ZERO                         TO DEAV-KVRESS-UT                  
159500                                          DEAV-KVEFRS-UT                  
159600                                          DEAV-RERF-RAD-UT                
159700     MOVE NEJ                          TO DEAV-FLAKPLOC-UT                
159800     .                                                                    
159900     EJECT                                                                
160000 HA-UPPDAT-SKROT                SECTION.                                  
160100     MOVE 'HA-UPPDAT-SKROT     '   TO CURRENT-SECTION                     
160200     MOVE 'HA-UPP-STA'            TO FELTEXT                              
160300                                                                          
160400     MOVE DEAV-IDDC-IN                 TO W-WDK711-IDDC                   
160500                                                                          
160600     IF DIST18-SCRAP-NDC                                                  
160700        IF DEAV-KVAVBART-UT > 0                                           
160800           MOVE 'HA-UPP-1'              TO FELTEXT                        
160900           PERFORM IMS-GHU-WDK711                                         
161000                                                                          
161100           MOVE NEJ                    TO SLAG-FLSKROT-BEORD              
161200           MOVE DEAV-KVBEART-Q-IN      TO SLAG-KVSKROT                    
161300           MOVE WS-DATUM               TO SLAG-TISKROT                    
161400           MOVE ZERO                   TO SLAG-TISKROT-BEORD              
161500           MOVE 'HA-UPP-2'              TO FELTEXT                        
161600           PERFORM IMS-REPL-WDK711                                        
161700        END-IF                                                            
161800     END-IF                                                               
161900     .                                                                    
162000     EJECT                                                                
162100 HB-UPDATE-WDK7-VOR                      SECTION.                         
162200     MOVE 'HB-UPDATE-WDK7-VOR  '   TO CURRENT-SECTION                     
162300                                                                          
162400     IF DEAV-FLORDSPE-IN  = NEJ                                           
162500                                                                          
162600        ADD DEAV-KVBEART-Q-IN TO SLAG-KVOKS-DAG                           
162700                                                                          
162800     END-IF                                                               
162900     .                                                                    
163000     EJECT                                                                
163100 HD-NDC-KOLLA-DISPONIBELT SECTION.                                        
163200     MOVE 'HD-NDC-KOLLA-DISPONIBELT' TO CURRENT-SECTION                   
163300                                                                          
163400     PERFORM S01-NOLLA-EV-MINUS-SALDON                                    
163500     MOVE ZERO TO WS-DISP                                                 
163600                                                                          
163700     IF DIST18-SKROT-KVAL-SDC                                             
163800     OR DIST18-SCRAP-NDC                                                  
163900       COMPUTE WS-DISP = SLAG-KVLS                                        
164000       END-COMPUTE                                                        
164100     ELSE                                                                 
164200*----------------------------------- DISP KLASS 0                         
164300       IF DEAV-KDORDKL-IN = +0                                            
164400         MOVE SLAG-KVLS          TO WS-DISP                               
164500         IF DEAV-FLFORBI-IN = NEJ                                         
164600           COMPUTE WS-DISP = WS-DISP                                      
164700                           - SLAG-KVUTRS                                  
164800                           - SLAG-KVSPARR-KVAL                            
164900           END-COMPUTE                                                    
165000         ELSE                                                             
165100           IF SLAG-KVSPARR-KVAL > 0                                       
165200           OR WS-KVAKS-SDC     > 0                                        
165300             COMPUTE WS-DISP = WS-DISP                                    
165400                             - SLAG-KVSPARR-KVAL                          
165500             END-COMPUTE                                                  
165600           END-IF                                                         
165700         END-IF                                                           
165800       END-IF                                                             
165900                                                                          
166000*----------------------------------- DISP KLASS 1                         
166100*                                                                         
166200*     SKROT, RETUR DIST FÖR JAP, AUS???                                   
166300       IF DEAV-KDORDKL-IN = +1                                            
166400         MOVE SLAG-KVLS          TO WS-DISP                               
166500         IF DIST35-NA-CDC-RETURN                                          
166600         OR DIST35-CDC-RETURNS-NON-VCC                                    
166700*SKROT   DISTRIKT, BESTÄLLT ANTAL ARTIKLAR SKALL SKROTAS.                 
166800           COMPUTE WS-DISP = WS-DISP                                      
166900                           - SLAG-KVUTRS                                  
167000           END-COMPUTE                                                    
167100         ELSE                                                             
167200           COMPUTE WS-DISP = WS-DISP                                      
167300                           - SLAG-KVUTRS                                  
167400                           - SLAG-KVSPARR-KVAL                            
167500                           - DEAV-KVSPANT-IN                              
167600           END-COMPUTE                                                    
167700*-OBS     INGEN AVRÄKNING AV KVRESS OM RESTORDER                          
167800           IF DEAV-IDKUNDRF-RO-IN = '00000     ' OR '0000000   '          
167900             COMPUTE WS-DISP = WS-DISP                                    
168000                             - WS-KVRESS                                  
168100             END-COMPUTE                                                  
168200           END-IF                                                         
168300         END-IF                                                           
168400       END-IF                                                             
168500                                                                          
168600*----------------------------------- DISP KLASS > 1                       
168700*                                                                         
168800       IF DEAV-KDORDKL-IN > +1                                            
168900         MOVE SLAG-KVLS          TO WS-DISP                               
169000         IF DIST35-NA-CDC-RETURN                                          
169100         OR DIST35-CDC-RETURNS-NON-VCC                                    
169200*SKROT   DISTRIKT, BESTÄLLT ANTAL ARTIKLAR SKALL SKROTAS.                 
169300           COMPUTE WS-DISP = WS-DISP                                      
169400                           - SLAG-KVUTRS                                  
169500           END-COMPUTE                                                    
169600         ELSE                                                             
169700           COMPUTE WS-DISP = WS-DISP                                      
169800                           - WS-KVOKS-DAG                                 
169900                           - SLAG-KVUTRS                                  
170000                           - SLAG-KVSPARR-KVAL                            
170100                           - DEAV-KVSPANT-IN                              
170200           END-COMPUTE                                                    
170300*-OBS   INGEN AVRÄKNING AV KVRESS OM RESTORDER                            
170400           IF DEAV-IDKUNDRF-RO-IN = '00000     ' OR '0000000   '          
170500             COMPUTE WS-DISP = WS-DISP                                    
170600                             - WS-KVRESS                                  
170700             END-COMPUTE                                                  
170800           END-IF                                                         
170900         END-IF                                                           
171000       END-IF                                                             
171100       IF DIST07-KINA                                                     
171200          IF DEAV-FLFORBI-IN = 'J' OR 'S'                                 
171300             COMPUTE WS-DISP = WS-DISP                                    
171400                             + WS-KVAKS-SDC                               
171500          END-IF                                                          
171600       END-IF                                                             
171700     END-IF                                                               
171800                                                                          
171900     IF DEAV-KDSORT-IN = 'L ' AND                                         
172000        DEAV-KVQPACK-1-IN > ZERO                                          
172100                                                                          
172200       IF WS-DISP < DEAV-KVBEART-Q-IN                                     
172300          COMPUTE WS-DISPUTSKR-KVANT =                                    
172400                  WS-DISP / DEAV-KVQPACK-1-IN                             
172500          COMPUTE WS-DISPUTSKR-KVANT =                                    
172600                  WS-DISPUTSKR-KVANT * DEAV-KVQPACK-1-IN                  
172700          MOVE WS-DISPUTSKR-KVANT TO WS-DISP                              
172800       END-IF                                                             
172900     END-IF                                                               
173000     .                                                                    
173100     EJECT                                                                
173200 S01-NOLLA-EV-MINUS-SALDON     SECTION.                                   
173300     MOVE 'S01-NOLLA-EV-MINUS-SALDON    ' TO  CURRENT-SECTION             
173400                                                                          
173500     IF SLAG-KVAKS-SDC < +0                                               
173600       MOVE +0                 TO WS-KVAKS-SDC                            
173700     ELSE                                                                 
173800       MOVE SLAG-KVAKS-SDC     TO WS-KVAKS-SDC                            
173900     END-IF                                                               
174000     IF SLAG-KVOKS-DAG < +0                                               
174100       MOVE +0                 TO WS-KVOKS-DAG                            
174200     ELSE                                                                 
174300       MOVE SLAG-KVOKS-DAG     TO WS-KVOKS-DAG                            
174400     END-IF                                                               
174500     IF SLAG-KVOKS-BULK < +0                                              
174600       MOVE +0                 TO WS-KVOKS-BULK                           
174700     ELSE                                                                 
174800       MOVE SLAG-KVOKS-BULK    TO WS-KVOKS-BULK                           
174900     END-IF                                                               
175000     IF SLAG-KVRESS < +0                                                  
175100       MOVE +0                 TO WS-KVRESS                               
175200     ELSE                                                                 
175300       MOVE SLAG-KVRESS        TO WS-KVRESS                               
175400     END-IF                                                               
175500     .                                                                    
175600     EJECT                                                                
175700 S02-GET-SDC-IDDC-VALUE            SECTION.                               
175800     MOVE 'S02-GET-SDC-IDDC-VALUE       ' TO  CURRENT-SECTION             
175900                                                                          
176000     MOVE DEAV-IDDISTR-IN   TO WS-IDDISTR                                 
176100*    ANVÄND UNSIGNAT WS-IDDISTR FÖR ATT WHEN-SATSEN SKALL FUNKA           
176200                                                                          
176300     SEARCH ALL DIST57-REFILL-DC                                          
176400        AT END                                                            
176500           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
176600                            TO FELTEXT                                    
176700           CALL FELLOG                                                    
176800        WHEN DIST57-SOK-IDDISTR(DIST57-IX) = WS-IDDISTR                   
176900           MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-WDK711-IDDC           
177000     END-SEARCH                                                           
177100     .                                                                    
177200     EJECT                                                                
177300 S03-SKAPA-SALDOLOGG               SECTION.                               
177400     MOVE 'S03-SKAPA-SALDOLOGG          ' TO  CURRENT-SECTION             
177500                                                                          
177600     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
177700                                                                          
177800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
177900     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
178000                                   - WS-AAAAMMDD                          
178100     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
178200     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
178300                                   - WS-TTMMSSTH                          
178400     MOVE 9                        TO LOGG-IDSEKVNR                       
178500     MOVE DEAV-IDDC-IN             TO LOGG-IDDC                           
178600     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
178700     MOVE 'PRT'                    TO LOGG-IDSUBTYP                       
178800     MOVE 'W411DEAV'               TO LOGG-IDPGM                          
178900     MOVE 'DEAV'                   TO LOGG-IDTRANS                        
179000     MOVE 'W411DEAV'               TO LOGG-IDUSER                         
179100     MOVE SPACE                    TO LOGG-REF                            
179200     MOVE DEAV-IDDISTR-IN          TO LOGG-IDDISTR                        
179300     MOVE DEAV-IDKUNDNR-IN         TO LOGG-IDKUNDNR                       
179400     MOVE DEAV-IDORDNR5-IN         TO LOGG-IDORDNR5                       
179500     MOVE DEAV-IDPRODNR-IN         TO LOGG-IDPRODNR                       
179600     MOVE DEAV-IDPLKLST-IN         TO LOGG-IDPLKLST                       
179700***************************************************************           
179800**   GÅR EJ FÅ FRAM REF I DETTA SUB-PGM, SE IST. ÖVERORDNADE  *           
179900**   PROGRAMMET W4037500.                                     *           
180000**   MOVE WS-IDPRODNR              TO LOGG-IDPRODNR           *           
180100**   MOVE WS-IDPLKLST              TO LOGG-IDPLKLST           *           
180200***************************************************************           
180300                                                                          
180400     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
180500     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
180600     MOVE '+'                      TO LOGG-IDTECKEN-KVEFRS                
180700     MOVE '-'                      TO LOGG-IDTECKEN-KVLS                  
180800     MOVE SLAG-KVAKS-SDC           TO LOGG-KVAKS                          
180900     MOVE SLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
181000     MOVE SLAG-KVEFRS              TO LOGG-KVEFRS                         
181100     MOVE SLAG-KVLS                TO LOGG-KVLS                           
181200     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
181300                                                                          
181400     PERFORM IMS-ISRT-WDL901                                              
181500     IF SEGMENT-FINNS-REDAN                                               
181600       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
181700         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
181800         PERFORM IMS-ISRT-WDL901                                          
181900       END-PERFORM                                                        
182000     END-IF                                                               
182100     .                                                                    
182200     EJECT                                                                
182300 S04-EV-UPD-LARM-WDR550-CN-US SECTION.                                    
182400     MOVE 'S04-EV-UPD-LARM-WDR550-CN-US ' TO CURRENT-SECTION              
182500                                                                          
182600** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
182700                                                                          
182800     IF  SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0                       
182900     AND SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC = 0                        
183000                                                                          
183100        MOVE DEAV-IDANSK-IN      TO W-IDANSK-2232                         
183200        PERFORM IMS-GU-WDR220                                             
183300        IF SEGMENT-FINNS                                                  
183400           MOVE 2232-IDANSK-LARM TO 2223-IDANSK                           
183500        ELSE                                                              
183600           MOVE ZERO             TO 2223-IDANSK                           
183700        END-IF                                                            
183800                                                                          
183900        MOVE 2223-IDANSK          TO W-IDANSK                             
184000        MOVE '2223'   TO 2223-IDHTYP                                      
184100        MOVE W-IDANSK TO 2223-IDANSK                                      
184200        MOVE LOW-VALUE TO 2223-LOW-VALUE                                  
184300        PERFORM IMS-ISRT-XXBU-WDR501                                      
184400                                                                          
184500       MOVE +0                   TO W-TISENBEK-DAG                        
184600       MOVE +0                   TO W-TISENBEK-KL                         
184700       MOVE '210'                TO W-KDLARM                              
184800       MOVE SLAG-IDDC            TO W-IDDC-SLAG                           
184900                                                                          
185000       PERFORM IMS-GU-XXBU-WDR550                                         
185100       IF SEGMENT-SAKNAS                                                  
185200**** LARM KAN SAKNAS FÖR MID-IDDC MEN FINNAS FÖR CDC,SAMMA NYCKEL.        
185300          PERFORM S04A-FLYTTA-DATA                                        
185400          PERFORM IMS-ISRT-XXBU-WDR550                                    
185500          IF SEGMENT-FINNS-REDAN                                          
185600            PERFORM UNTIL SEGMENT-FINNS                                   
185700              ADD 1              TO W-TISENBEK-KL                         
185800              PERFORM S04B-KOLLA-TIDEN                                    
185900              MOVE W-TISENBEK-KL TO 2224-TISENBEK-KL                      
186000              PERFORM IMS-ISRT-XXBU-WDR550                                
186100            END-PERFORM                                                   
186200          END-IF                                                          
186300       ELSE                                                               
186400          PERFORM UNTIL SEGMENT-SAKNAS                                    
186500             ADD 1               TO W-TISENBEK-KL                         
186600             PERFORM S04B-KOLLA-TIDEN                                     
186700             MOVE SLAG-IDDC      TO W-IDDC-SLAG                           
186800             PERFORM IMS-GU-XXBU-WDR550                                   
186900          END-PERFORM                                                     
187000          PERFORM S04A-FLYTTA-DATA                                        
187100          PERFORM IMS-ISRT-XXBU-WDR550                                    
187200          IF SEGMENT-FINNS-REDAN                                          
187300**** LARM KAN SAKNAS FÖR MID-IDDC MEN FINNAS FÖR CDC,SAMMA NYCKEL.        
187400            PERFORM UNTIL SEGMENT-FINNS                                   
187500              ADD 1              TO W-TISENBEK-KL                         
187600              PERFORM S04B-KOLLA-TIDEN                                    
187700              MOVE W-TISENBEK-KL TO 2224-TISENBEK-KL                      
187800              PERFORM IMS-ISRT-XXBU-WDR550                                
187900            END-PERFORM                                                   
188000          END-IF                                                          
188100       END-IF                                                             
188200       PERFORM S04C-DELETE-ALARM-222-223                                  
188300     END-IF                                                               
188400     .                                                                    
188500                                                                          
188600 S04A-FLYTTA-DATA SECTION.                                                
188700     MOVE 'S04A-FLYTTA-DATA             ' TO  CURRENT-SECTION             
188800                                                                          
188900     MOVE W-TISENBEK-DAG      TO 2224-TISENBEK-DAG                        
189000     MOVE W-TISENBEK-KL       TO 2224-TISENBEK-KL                         
189100     MOVE '210'               TO 2224-KDLARM                              
189200     MOVE 'DEAV'              TO 2224-IDTRANS                             
189300     MOVE '2'                 TO 2224-KDMFSFOR                            
189400     MOVE DEAV-IDARTNR-IN     TO 2224-IDARTNR                             
189500     MOVE SLAG-IDDC           TO 2224-IDDC                                
189600     MOVE 'J'                 TO 2224-FLNYLARM                            
189700     MOVE DEAV-IDDISTR-IN     TO 2224-IDDISTR                             
189800     MOVE DEAV-IDKUNDNR-IN    TO 2224-IDKUNDNR                            
189900     MOVE DEAV-IDKUNDRF-RO-IN TO 2224-IDKUNDRF                            
190000     MOVE ZERO                TO 2224-IDKR                                
190100     MOVE WS-DATUM            TO 2224-TIREGDAT                            
190200     MOVE ZERO                TO 2224-IDLOPNR                             
190300     MOVE SLAG-IDLEVNR        TO 2224-IDLEVNR                             
190400     .                                                                    
190500     EJECT                                                                
190600                                                                          
190700                                                                          
190800 S04B-KOLLA-TIDEN SECTION.                                                
190900     MOVE 'S04B-KOLLA-TIDEN             ' TO  CURRENT-SECTION             
191000                                                                          
191100     MOVE W-TISENBEK-KL      TO WS-TISENBEK-KL-TEST                       
191200     IF WS-TISENBEK-KL-SS > 59                                            
191300        ADD 1 TO WS-TISENBEK-KL-MM                                        
191400        MOVE ZERO TO WS-TISENBEK-KL-SS                                    
191500        MOVE WS-TISENBEK-KL-TEST TO W-TISENBEK-KL                         
191600        IF WS-TISENBEK-KL-MM > 59                                         
191700           ADD 1 TO WS-TISENBEK-KL-HH                                     
191800           MOVE ZERO TO WS-TISENBEK-KL-MM                                 
191900           MOVE WS-TISENBEK-KL-TEST TO W-TISENBEK-KL                      
192000        END-IF                                                            
192100     END-IF                                                               
192200                                                                          
192300     .                                                                    
192400 S04C-DELETE-ALARM-222-223 SECTION.                                       
192500     MOVE 'S04C-DELETE-ALARM-222-223    ' TO  CURRENT-SECTION             
192600                                                                          
192700*** LARM 222 OCH 223 BORTTAGES NÄR LARM 210 SKAPAS FÖR SAMMA ARTNR        
192800     IF SEGMENT-FINNS                                                     
192900        MOVE +222               TO W-KDLARM-S                             
193000        MOVE SLAG-IDDC          TO W-IDDC                                 
193100        PERFORM IMS-GHU-XXBU-WDR550                                       
193200        IF SEGMENT-FINNS                                                  
193300           PERFORM IMS-DLET-XXBU-WDR550                                   
193400        END-IF                                                            
193500                                                                          
193600        MOVE +223                 TO W-KDLARM-S                           
193700        MOVE SLAG-IDDC            TO W-IDDC                               
193800        PERFORM IMS-GHU-XXBU-WDR550                                       
193900        IF SEGMENT-FINNS                                                  
194000           PERFORM IMS-DLET-XXBU-WDR550                                   
194100        END-IF                                                            
194200     END-IF                                                               
194300     .                                                                    
194400                                                                          
194500     EJECT                                                                
194600*IMS-SECTIONS                                                             
194700                                                                          
194800 IMS-GHU-ARTM01                          SECTION.                         
194900                                                                          
195000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
195100            DELIMITED BY SIZE INTO SSA1                                   
195200     MOVE '  '                 TO GODK-STATUSKODER                        
195300     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-WDK901 SSA1                   
195400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
195500     PERFORM IMS-STATUSKONTROLL                                           
195600     .                                                                    
195700     SKIP2                                                                
195800 IMS-GU-ARTM01                           SECTION.                         
195900     SKIP2                                                                
196000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
196100            DELIMITED BY SIZE INTO SSA1                                   
196200     MOVE '  GE'               TO GODK-STATUSKODER                        
196300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-WDK901 SSA1                    
196400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700     SKIP2                                                                
196800 IMS-REPL-ARTM01                         SECTION.                         
196900     SKIP2                                                                
197000     MOVE '  '                 TO GODK-STATUSKODER                        
197100     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-WDK901                       
197200     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
197300     PERFORM IMS-STATUSKONTROLL                                           
197400     .                                                                    
197500     SKIP2                                                                
197600 IMS-GHU-WDK611 SECTION.                                                  
197700                                                                          
197800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
197900          DELIMITED BY SIZE INTO SSA1                                     
198000     MOVE 'WDK611 '           TO SSA2                                     
198100     MOVE '    ' TO GODK-STATUSKODER                                      
198200     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
198300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
198400     PERFORM IMS-STATUSKONTROLL                                           
198500     .                                                                    
198600 IMS-REPL-WDK611 SECTION.                                                 
198700                                                                          
198800     MOVE '  ' TO GODK-STATUSKODER                                        
198900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
199000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
199100     PERFORM IMS-STATUSKONTROLL                                           
199200     .                                                                    
199300 IMS-GHU-WDK711 SECTION.                                                  
199400                                                                          
199500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
199600          DELIMITED BY SIZE INTO SSA1                                     
199700     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
199800          DELIMITED BY SIZE INTO SSA2                                     
199900     MOVE '  GE' TO GODK-STATUSKODER                                      
200000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
200100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
200200     PERFORM IMS-STATUSKONTROLL                                           
200300     .                                                                    
200400 IMS-REPL-WDK711 SECTION.                                                 
200500                                                                          
200600     MOVE '  ' TO GODK-STATUSKODER                                        
200700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
200800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
200900     PERFORM IMS-STATUSKONTROLL                                           
201000     .                                                                    
201100 IMS-ISRT-WDL901 SECTION.                                                 
201200                                                                          
201300     MOVE 'WLLOGA01 ' TO SSA1                                             
201400     MOVE '  II' TO GODK-STATUSKODER                                      
201500     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
201600     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
201700     PERFORM IMS-STATUSKONTROLL                                           
201800     .                                                                    
201900                                                                          
202000 IMS-GU-WDB601    SECTION.                                                
202100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
202200          DELIMITED BY SIZE INTO SSA1                                     
202300     MOVE '  GE' TO GODK-STATUSKODER                                      
202400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
202500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
202600     PERFORM IMS-STATUSKONTROLL                                           
202700     .                                                                    
202800     EJECT                                                                
202900 IMS-GU-WDK7-WDK711 SECTION.                                              
203000                                                                          
203100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
203200          DELIMITED BY SIZE INTO SSA1                                     
203300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
203400          DELIMITED BY SIZE INTO SSA2                                     
203500     MOVE '  GE' TO GODK-STATUSKODER                                      
203600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
203700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
203800     PERFORM IMS-STATUSKONTROLL                                           
203900     .                                                                    
204000     SKIP3                                                                
204100 IMS-GU-WDR220 SECTION.                                                   
204200                                                                          
204300     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
204400          DELIMITED BY SIZE INTO SSA1                                     
204500     STRING 'WDR220  (WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
204600          DELIMITED BY SIZE INTO SSA2                                     
204700     MOVE '  GE' TO GODK-STATUSKODER                                      
204800     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDR220 SSA1 SSA2               
204900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
205000     PERFORM IMS-STATUSKONTROLL                                           
205100     .                                                                    
205200     EJECT                                                                
205300 IMS-ISRT-XXBU-WDR501 SECTION.                                            
205400                                                                          
205500     MOVE 'WDR501   ' TO SSA1                                             
205600     MOVE '  II' TO GODK-STATUSKODER                                      
205700     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR501 SSA1                  
205800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
205900     PERFORM IMS-STATUSKONTROLL                                           
206000     .                                                                    
206100 IMS-GU-XXBU-WDR550 SECTION.                                              
206200     MOVE 'IMS-GU-XXBU-WDR550    ' TO CURRENT-IMS-SECTION                 
206300                                                                          
206400     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
206500          DELIMITED BY SIZE INTO SSA1                                     
206600     STRING 'WDR550  (WDGXKEY  =' W-WDGX2224-X                            
206700                    '&IDDC     =' W-IDDC-SLAG-X ')'                       
206800          DELIMITED BY SIZE INTO SSA2                                     
206900                                                                          
207000     MOVE '  GE' TO GODK-STATUSKODER                                      
207100     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR550 SSA1 SSA2               
207200     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
207300     PERFORM IMS-STATUSKONTROLL                                           
207400     .                                                                    
207500     EJECT                                                                
207600 IMS-ISRT-XXBU-WDR550 SECTION.                                            
207700     MOVE 'IMS-ISRT-XXBU-WDR550  ' TO CURRENT-IMS-SECTION                 
207800                                                                          
207900     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
208000          DELIMITED BY SIZE INTO SSA1                                     
208100     STRING 'WDR550     '                                                 
208200          DELIMITED BY SIZE INTO SSA2                                     
208300     MOVE '  II' TO GODK-STATUSKODER                                      
208400     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR550 SSA1 SSA2             
208500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
208600     PERFORM IMS-STATUSKONTROLL                                           
208700     .                                                                    
208800     EJECT                                                                
208900 IMS-GHU-XXBU-WDR550 SECTION.                                             
209000     MOVE 'IMS-GHU-XXBU-WDR550   ' TO CURRENT-IMS-SECTION                 
209100                                                                          
209200     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
209300          DELIMITED BY SIZE INTO SSA1                                     
209400     STRING 'WDR550  (KDLARM   =' W-KDLARM-X                              
209500                    '&IDARTNR  =' W-IDARTNR-X                             
209600                    '&IDDC     =' W-IDDC-X ')'                            
209700          DELIMITED BY SIZE INTO SSA2                                     
209800     MOVE '  GE' TO GODK-STATUSKODER                                      
209900     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDR550 SSA1 SSA2              
210000     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
210100     PERFORM IMS-STATUSKONTROLL                                           
210200     .                                                                    
210300                                                                          
210400 IMS-DLET-XXBU-WDR550 SECTION.                                            
210500                                                                          
210600     MOVE '  ' TO GODK-STATUSKODER                                        
210700     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDR550                       
210800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100     EJECT                                                                
211200                                                                          
211300 IMS-STATUSKONTROLL            SECTION.                                   
211400     SKIP2                                                                
211500     SET STATUS-IX             TO 1                                       
211600     SEARCH GODK-STATUS AT END CALL FELLOG                                
211700      WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                   
211800     END-SEARCH                                                           
211900     .                                                                    
