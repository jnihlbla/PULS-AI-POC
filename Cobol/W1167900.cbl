000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1167900.                                                
000400 AUTHOR.         SHILPA MADHURI.                                          
000500 DATE-WRITTEN.   11/12/07.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        STYR VILKA ERSÄTTNINGAR SOM SKA SKICKAS TILL VIPS.               
001100*        SKICKAR POSTER TILL INVENTERINGEN.                               
001110*        UPPDATERAR TIERSDAT-VIPS I BMP W1167D00.                         
001200*                                                                         
001201*        THE INPUT-FILE MUST BE SORTED BY IDLANDX2 (AND THEN PART)        
001202*                                                                         
001210*        ARRAY IDDC-IN-IDLANDX2 STORES THE DC-S THAT ARE IN THE           
001220*        COUNTRY (IDLANDX2) THAT IS CURRENTLY BEING PROCESSED.            
001222*        ARRAY IDDC-IN-IDLANDX2 IS THEN SEARCHED BEFORE DC-S ARE          
001223*        PROCESSED FURTHER OR ALLOWED INTO ARRAY TAB-IDDC.                
001240*                                                                         
001300*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
001510*        PROGRAMMET LÄSER      WLERSA (WDD9)                              
001520*        PROGRAMMET LÄSER      WLARTC (WDB6)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- ERSATTA ARTIKLAR                                           
003000     SELECT W11661                     ASSIGN TO W11679D1.                
003100     SKIP2                                                                
003200*          --- ERSATTA ARTIKLAR FÖR BEVAKNING AV ERS-MEDDELANDEN          
003300     SELECT W11679                     ASSIGN TO W11679D2.                
003400     SKIP2                                                                
003500*          --- ERSÄTTNINGSMEDDELANDEN                                     
003600     SELECT W1167B                     ASSIGN TO W11679D3.                
003700     SKIP2                                                                
004100     SELECT W1167C                     ASSIGN TO W11679D4.                
004110     SKIP2                                                                
004120     SELECT W1167D                     ASSIGN TO W11679D5.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W11661                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY W11661      -L.                                                
005200     SKIP3                                                                
005300 FD  W11679                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W11679  -PRE  W11679-  -L.                                
005800     SKIP3                                                                
005900 FD  W1167B                                                               
006000     RECORDING       V                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W11636 -PRE  W1167B-  -L.                                 
006400     SKIP3                                                                
007000 FD  W1167C                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W11121 -PRE  W1167C-  -L.                                 
007410     SKIP3                                                                
007420 FD  W1167D                                                               
007430     RECORDING       F                                                    
007440     BLOCK CONTAINS  0.                                                   
007450                                                                          
007460*01  POST -COPY W1167D -PRE  W1167D-  -L.                                 
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007700     SKIP2                                                                
007800                                                                          
007900*    -- CHECKED BY WY2000                                                 
008000 77  IDPGM                       PIC X(8)    VALUE 'W1167900'.            
008100 77  JA                          PIC X       VALUE 'J'.                   
008200 77  NEJ                         PIC X       VALUE 'N'.                   
008210                                                                          
008220 77  IX1-DC                      PIC S9(4)  VALUE +0    COMP SYNC.        
008230 77  IX1-DC-MAX                  PIC S9(4)  VALUE +10   COMP SYNC.        
008300 77  WS-KVAKS-SDC                PIC S9(7)   VALUE ZERO COMP-3.           
008400 77  WS-RETUR                    PIC S9(7)   VALUE ZERO COMP-3.           
008410 77  ANTAL-TRAFF-ERS             PIC S9(7)   VALUE ZERO COMP-3.           
008420 77  ANTAL-ERS                   PIC S9(7)   VALUE ZERO COMP-3.           
008901 77  WS-TAB-IDDC                 PIC X(02).                               
008910 01  IDDC-TAB.                                                            
009000     03  TAB-FLSALDO             PIC X.                                   
009200     03  TAB-FLERSATT            PIC X.                                   
009400     03  TAB-FLTILLK-SALDO       PIC X     VALUE SPACE.                   
009500     03  TAB-WDK711-FINNS        PIC X.                                   
009510     03  TAB-IDDC OCCURS 10      PIC X(02).                               
009511                                                                          
009520 01  DCS-IN-IDLANDX2.                                                     
009530     03  IDDC-IN-IDLANDX2 OCCURS 20                                       
009531                          INDEXED BY DC-LAND-IX                           
009532                          PIC XX.                                         
009534     03  DC-LAND-IX-LAST        PIC S9(9)  VALUE +0    COMP SYNC.         
009535     03  DC-LAND-IX-MAX         PIC S9(9)  VALUE +20   COMP SYNC.         
009536     03  DC-IDLANDX2            PIC X(2)   VALUE SPACE.                   
009540                                                                          
009600 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
009700 77  WS-FL-TYP1                  PIC X.                                   
009800 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
009900     SKIP2                                                                
010000 01 WS-KDERS-NUM                PIC 9(2).                                 
010100 01 WS-KDERS                             REDEFINES WS-KDERS-NUM.          
010200     03 WS-KDERSPOS1            PIC X.                                    
010300     03 WS-KDERSPOS2            PIC X.                                    
010400                                                                          
010500 01  WS-IDARTNR.                                                          
010600     03 WS-IDARTNR-BLANK         PIC X(11) VALUE SPACE.                   
010700     03 WS-IDARTNR-ALFA          PIC X(9).                                
010800                                                                          
010900 01  WS-IDARTNR-TILLK.                                                    
011000     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
011100     03 WS-IDARTNR-TILLK-ALFA    PIC X(9).                                
011200                                                                          
011300 01  FELTEXT.                                                             
011400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011600                                                                          
011710 77  W11661-EOF-SW               PIC X       VALUE 'N'.                   
011800     88  END-OF-W11661                       VALUE 'J'.                   
011900     EJECT                                                                
011910*01    -COPY WWPRODSL                                                     
011920     EJECT                                                                
012000*      --- VALID IDDC CODES                                               
012100*                                                                         
012200*01    -COPY WWDC99                                                       
012210*01    -COPY WWDCKONS                                                     
012220*01    -COPY WWLNDKON                                                     
012300       EJECT                                                              
012301                                                                          
012302 01  WS-MQ-LINE1.                                                         
012303    03  FILLER                  PIC X(336)  VALUE                         
012304                                '¤MQMPROP Vidb_Source=Delta'.             
012305 01  WS-MQ-LINE2.                                                         
012306    03  FILLER                  PIC X(336)  VALUE                         
012307                                '¤MQMPROP LoadType=Delta'.                
012308 01  WS-MQ-LINE3.                                                         
012309    03  FILLER                  PIC X(16)   VALUE                         
012310                                '¤MQMPROP Market='.                       
012320    03  WS-MQ-IDLANDX2          PIC X(2)    VALUE SPACE.                  
012330    03  FILLER                  PIC X(318)  VALUE SPACE.                  
012340    EJECT                                                                 
012400 01  DYNAMISKA-SUBPROGRAM.                                                
012500*                                                                         
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013000     EJECT                                                                
013100*    ---- PARAMETRAR TILL WDATKONV                                        
013200 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
013300*01  -COPY WDATAREA                                                       
013400*    --- PARAMETRAR TILL POSTSUM                                          
013500*                                                                         
013600*01  -COPY W0005   -PRE  POSTSUM-                                         
013700     EJECT                                                                
013800 01  IN-AREA-START               PIC X(24)   VALUE                        
013900                                             'IN-AREA-START'.             
014000     SKIP2                                                                
014100                                                                          
014200*01  AREA -COPY W11661     -PRE IN-                                       
014300     EJECT                                                                
014400 01  UT1-AREA-START              PIC X(24)   VALUE                        
014500                                             'UT1-AREA-START'.            
014600     SKIP2                                                                
014700                                                                          
014800*01  AREA -COPY W11679     -PRE UT1-                                      
014900     EJECT                                                                
015000 01  UT7B-AREA-START             PIC X(24)   VALUE                        
015100                                             'UT7B-AREA-START'.           
015200     SKIP2                                                                
015300                                                                          
015400*01  AREA -COPY W11636     -PRE UT7B-                                     
015500     EJECT                                                                
015600 01  UT7C-AREA-START             PIC X(24)   VALUE                        
015700                                             'UT7C-AREA-START'.           
015800     SKIP2                                                                
015900                                                                          
016000*01  AREA -COPY W11121     -PRE UT7C-                                     
016010                                                                          
016020     SKIP2                                                                
016030                                                                          
016031 01  UT7D-AREA-START             PIC X(24)   VALUE                        
016032                                             'UT7D-AREA-START'.           
016040*01  AREA -COPY W1167D     -PRE UT7D-                                     
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016300     SKIP3                                                                
016400 01  NYCKLAR-TILL-DLI.                                                    
016500     03  W-IDARTNR-X.                                                     
016600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016700     03  W-IDDC-X.                                                        
016800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016810     03  W-IDDC-MIN-X.                                                    
016820         05  W-IDDC-MIN          PIC  X(2)   VALUE '71'.                  
016830     03  W-IDDC-MAX-X.                                                    
016840         05  W-IDDC-MAX          PIC  X(2)   VALUE '79'.                  
016900     03  W-IDPTYP-X.                                                      
017000         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
017100     03  W-IDKORTNR-X.                                                    
017200         05  W-IDKORTNR          PIC S9(2)   VALUE ZERO COMP-3.           
017210     03  W-WDD901KY-X.                                                    
017211         05 W-IDARTNR-D9         PIC S9(9)           COMP-3.              
017213         05 W-IDDC-D9            PIC X(2).                                
017214     03  W-KDAVROP-X.                                                     
017215         05  W-KDAVROP           PIC S9      VALUE 2    COMP-3.           
017220                                                                          
017300     SKIP2                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018000     88  IMS-EJ-OK                           VALUE 'XD'.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(64).                               
018600 01  SSA2                        PIC X(64).                               
018700     EJECT                                                                
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200                                                                          
019300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS01'.                    
019400 01  DLI-IO-WLARTS01.                                                     
019500*    03  -COPY WDK701  -PRE ARTS-                                         
019600     EJECT                                                                
019700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
019800 01  DLI-IO-WLARTS11.                                                     
019900*    03  -COPY WDK711  -PRE ARTS-                                         
020000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
020100 01  DLI-IO-WLARTC01.                                                     
020200*    03  -COPY WDK601  -PRE ARTC-                                         
020300     EJECT                                                                
020400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA01'.                    
020500 01  DLI-IO-WLERSA01.                                                     
020600*    03  -COPY WDD701  -PRE ERSA-                                         
020700     EJECT                                                                
020800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA11'.                    
020900 01  DLI-IO-WLERSA11.                                                     
021000*    03  -COPY WDD702  -PRE ERSA-                                         
021100     EJECT                                                                
021200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA13'.                    
021300 01  DLI-IO-WLERSA13.                                                     
021400*    03  -COPY WDD704  -PRE ERSA-                                         
021500     EJECT                                                                
021600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
021700 01  DLI-IO-WDL601.                                                       
021800     03  -COPY WDL601                                                     
021900     EJECT                                                                
022000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
022100 01  DLI-IO-WDL611.                                                       
022200     03  -COPY WDL611                                                     
022300     EJECT                                                                
022310 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD901'.                      
022320 01  DLI-IO-WDD901.                                                       
022330     03  -COPY WDD901                                                     
022340     EJECT                                                                
022350 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD905'.                      
022360 01  DLI-IO-WDD905.                                                       
022370     03  -COPY WDD905   -PRE D905-                                        
022380     EJECT                                                                
022390 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
022391 01  DLI-IO-WDB601.                                                       
022392     03  -COPY WDB601                                                     
022393     EJECT                                                                
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009   -PRE MSG-                                              
022700     EJECT                                                                
022800*01  -COPY W0008  -PRE ARTS-                                              
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008  -PRE ARTC-                                              
023200     05  FILLER                  PIC X.                                   
023300     EJECT                                                                
023400*01  -COPY W0008  -PRE ERSA-                                              
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700*01  -COPY W0008  -PRE WDL6-                                              
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
023910*01  -COPY W0008  -PRE WDD9-                                              
023920     05  FILLER                  PIC X.                                   
023930     EJECT                                                                
023940*01  -COPY W0008  -PRE WDB6-                                              
023950     05  FILLER                  PIC X.                                   
023960     EJECT                                                                
024000 PROCEDURE DIVISION  USING MSG-PCB ARTS-PCB ARTC-PCB ERSA-PCB             
024100                           WDL6-PCB WDD9-PCB WDB6-PCB.                    
024200 MAIN SECTION.                                                            
024300     ENTRY 'DLITCBL' USING MSG-PCB ARTS-PCB ARTC-PCB ERSA-PCB             
024310                           WDL6-PCB WDD9-PCB WDB6-PCB.                    
024500                                                                          
024600     SKIP2                                                                
024700     PERFORM A-INIT                                                       
024800     PERFORM S01-LAES-W11661                                              
024900                                                                          
025000     PERFORM UNTIL END-OF-W11661                                          
025100       PERFORM B-BEARBETA                                                 
025200       PERFORM S01-LAES-W11661                                            
025300     END-PERFORM                                                          
025400                                                                          
025500     PERFORM Z-FINIT                                                      
025600                                                                          
025700     MOVE ZERO TO RETURN-CODE                                             
025800     GOBACK                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 A-INIT SECTION.                                                          
026200                                                                          
026300     OPEN INPUT  W11661                                                   
026400                                                                          
026500     OPEN OUTPUT W11679                                                   
026600                 W1167B                                                   
026700                 W1167C                                                   
026800                 W1167D                                                   
026900                                                                          
027000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027100     .                                                                    
027200     EJECT                                                                
027300 B-BEARBETA SECTION.                                                      
027310                                                                          
027500     MOVE IN-IDARTNR  TO W-IDARTNR                                        
027600     MOVE IN-KDERS    TO WS-KDERS-NUM                                     
027700     MOVE NEJ         TO TAB-FLTILLK-SALDO                                
027900                                                                          
027905     IF IN-IDLANDX2 = DC-IDLANDX2                                         
027906       CONTINUE                                                           
027907     ELSE                                                                 
027908       PERFORM BA-GET-DCS-FOR-COUNTRY                                     
027971     END-IF                                                               
027980                                                                          
028000     IF IN-KDERS = 0                                                      
028100       PERFORM BB-SKAPA-EV-BACKTRANSFIL-7B-7D                             
028200     ELSE                                                                 
028210       MOVE 'N'                       TO TAB-FLERSATT                     
028300       IF IN-KDERS > 20                                                   
028400          MOVE 'N'       TO  TAB-FLSALDO                                  
028500          IF IN-KDERS = 22 OR 23 OR 25 OR 26 OR 52                        
028700            PERFORM BC-KOLLA-FINNS                                        
028800          ELSE                                                            
028900            IF IN-FLSALDOCH = 'J'                                         
029000              PERFORM BD-SALDOCHECK                                       
029100            END-IF                                                        
029200          END-IF                                                          
029300                                                                          
029400          IF IN-KDERS = 22                                                
029500            PERFORM BE-TILLK-SALDO                                        
029600*                                                                         
029700* FÖR ERSKOD = 22 SKA ARTIKELN ERSÄTTAS UTE PÅ RESP. NDC                  
029800* VID FÖRSTA INLEVERANS DVS HAR FÅTT SALDO                                
029900* SÅ LÄNGE SALDO EJ FINNS SÄTTS WS-FLSALDO-XXX TILL JA                    
030000* FÖR ATT EJ BEARBETAS SENARE I PROGRAMMET                                
030100*                                                                         
030200            IF TAB-FLTILLK-SALDO = 'N'                                    
030300              MOVE JA      TO TAB-FLSALDO                                 
030400            END-IF                                                        
030800          END-IF                                                          
030900                                                                          
031000          IF IN-FLERSATT = 'N'                                            
031100            IF TAB-FLSALDO = 'N'                                          
031200              PERFORM BF-SKAPA-EV-FILER-7B-7C-7D                          
031300            END-IF                                                        
031400          END-IF                                                          
031500                                                                          
031600******* SÄTTER FLAGGOR TILL W11679-FILEN                                  
031700          IF IN-FLERSATT = 'N'                                            
031710*---------- TAB-FLERSATT IS SET IN SECTION BF-                            
031800            CONTINUE                                                      
032300          ELSE                                                            
032400            MOVE 'J'   TO TAB-FLERSATT                                    
032500          END-IF                                                          
032600                                                                          
033700       ELSE                                                               
033710          PERFORM S03-INIT-TAB-IDDC                                       
034400                                                                          
034401          MOVE +0                     TO IX1-DC                           
034410          IF IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                           
034500            PERFORM IMS-GU-ARTS-ARTS                                      
034501            IF SEGMENT-FINNS                                              
034510             PERFORM IMS-GNP-ARTS-SLAG                                    
034520             PERFORM UNTIL SEGMENT-SAKNAS                                 
034530               SET DC-LAND-IX TO 1                                        
034540               SEARCH IDDC-IN-IDLANDX2                                    
034560                AT END                                                    
034570                  CONTINUE                                                
034700                WHEN IDDC-IN-IDLANDX2(DC-LAND-IX) = ARTS-SLAG-IDDC        
034800*CC*BUG1*         IF ARTS-SLAG-KVLS > ZERO                                
034910                  IF ARTS-SLAG-KVLS > ZERO                                
034920                     IF ARTS-SLAG-ADLAGOMR > ZERO                         
034930                        ADD +1              TO IX1-DC                     
035000                        MOVE ARTS-SLAG-IDDC TO TAB-IDDC(IX1-DC)           
035017                     END-IF                                               
035018*CC*BUG1*         END-IF                                                  
035019                  END-IF                                                  
035020               END-SEARCH                                                 
035030               PERFORM IMS-GNP-ARTS-SLAG                                  
035100             END-PERFORM                                                  
035101            END-IF                                                        
035102          ELSE                                                            
035104            ADD +1                          TO IX1-DC                     
035105            MOVE IDDC-IN-IDLANDX2(1)        TO TAB-IDDC(IX1-DC)           
035106          END-IF                                                          
037200                                                                          
037500          IF IN-FLSALDOCH = 'J'                                           
037600             PERFORM S04-SEARCH-TAB-IDDC                                  
037610             IF WS-TAB-IDDC NOT = SPACE                                   
037700                PERFORM BG-SKAPA-EV-PREL-FIL-7B                           
037800             END-IF                                                       
037900          END-IF                                                          
038000       END-IF                                                             
038100                                                                          
038200       PERFORM BH-SKAPA-FIL-W11679                                        
038300     END-IF                                                               
038400     .                                                                    
038500     EJECT                                                                
038600 BA-GET-DCS-FOR-COUNTRY SECTION.                                          
038700                                                                          
038701     MOVE IN-IDLANDX2  TO DC-IDLANDX2                                     
038702     PERFORM IMS-GU-WDB601-FIRST                                          
038703     SET DC-LAND-IX TO 1                                                  
038704     MOVE ZERO   TO DC-LAND-IX-LAST                                       
038705     PERFORM UNTIL SEGMENT-SLUT                                           
038706                OR DC-LAND-IX > DC-LAND-IX-MAX                            
038707        IF DCS-IDLANDX2 = IN-IDLANDX2                                     
038708           MOVE DCS-IDDC TO IDDC-IN-IDLANDX2(DC-LAND-IX)                  
038709           ADD 1 TO DC-LAND-IX-LAST                                       
038710                                                                          
038711           SET DC-LAND-IX UP BY 1                                         
038712        END-IF                                                            
038713        PERFORM IMS-GN-WDB601                                             
038714     END-PERFORM                                                          
038715                                                                          
038716     IF DC-LAND-IX > DC-LAND-IX-MAX                                       
038717        MOVE 'DC-LAND TABLE FULL' TO FELTEXT                              
038718        CALL FELLOG                                                       
038719     ELSE                                                                 
038720       IF DC-LAND-IX-LAST = ZERO                                          
038721         MOVE 'DC-LAND TABLE EMPTY' TO FELTEXT                            
038722         CALL FELLOG                                                      
038723       ELSE                                                               
038724         MOVE HIGH-VALUE      TO W-IDDC-MIN                               
038725         MOVE LOW-VALUE       TO W-IDDC-MAX                               
038726         SET DC-LAND-IX TO 1                                              
038727         PERFORM UNTIL  DC-LAND-IX >  DC-LAND-IX-LAST                     
038728           IF IDDC-IN-IDLANDX2(DC-LAND-IX) < W-IDDC-MIN                   
038729              MOVE IDDC-IN-IDLANDX2(DC-LAND-IX) TO W-IDDC-MIN             
038730           END-IF                                                         
038731           IF IDDC-IN-IDLANDX2(DC-LAND-IX) > W-IDDC-MAX                   
038732              MOVE IDDC-IN-IDLANDX2(DC-LAND-IX) TO W-IDDC-MAX             
038733           END-IF                                                         
038734           SET DC-LAND-IX UP BY 1                                         
038735         END-PERFORM                                                      
038736                                                                          
038737         PERFORM UNTIL  DC-LAND-IX >  DC-LAND-IX-MAX                      
038738           MOVE SPACE TO IDDC-IN-IDLANDX2(DC-LAND-IX)                     
038739           SET DC-LAND-IX UP BY 1                                         
038740         END-PERFORM                                                      
038741                                                                          
038742         DISPLAY ' '                                                      
038743         DISPLAY '***> DC-S FOUND FOR COUNTRY=' IN-IDLANDX2               
038744         SET DC-LAND-IX TO 1                                              
038745         PERFORM UNTIL  DC-LAND-IX >  DC-LAND-IX-MAX                      
038746           IF IDDC-IN-IDLANDX2(DC-LAND-IX) > SPACE                        
038747             DISPLAY '       ' IDDC-IN-IDLANDX2(DC-LAND-IX)               
038748           END-IF                                                         
038749           SET DC-LAND-IX UP BY 1                                         
038750         END-PERFORM                                                      
038751         DISPLAY '     MIN & MAX DC-S FOR DB-READ:'                       
038752         DISPLAY '     ' W-IDDC-MIN '  & ' W-IDDC-MAX                     
038753         DISPLAY ' '                                                      
038754                                                                          
038755       END-IF                                                             
038756     END-IF                                                               
038757     .                                                                    
038758     EJECT                                                                
038759 BB-SKAPA-EV-BACKTRANSFIL-7B-7D SECTION.                                  
038760                                                                          
038800     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
038900     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
039000     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
039100     MOVE WS-IDARTNR      TO UT7B-IDARTNR20                               
039200                                                                          
039300     PERFORM IMS-GET-ARTC-ARTC                                            
039400                                                                          
039500     IF SEGMENT-FINNS                                                     
039600       MOVE ARTC-ART-TIERSDAT    TO DAT-I-TIDATUM                         
039700       MOVE 'AAVVD'              TO DAT-KDDATFORM                         
039800                                                                          
039900       PERFORM S02-WDATKONV                                               
040000                                                                          
040100       IF DAT-KDSVAR-OK                                                   
040200         MOVE DAT-TIAAMMDD TO UT7B-TIERSDAT-002                           
040300       ELSE                                                               
040400         MOVE 0            TO UT7B-TIERSDAT-002                           
040500       END-IF                                                             
040600     ELSE                                                                 
040700       MOVE 0              TO UT7B-TIERSDAT-002                           
040800     END-IF                                                               
040900                                                                          
041000     MOVE 'Y'             TO UT7B-FLDEL                                   
041100     MOVE IN-KDERS        TO UT7B-KDERS                                   
041200     MOVE 0               TO UT7B-IDKORTNR                                
041300     MOVE 0               TO UT7B-KDTEXTGR                                
041400     MOVE 'N'             TO UT7B-FLTEXT                                  
041500     MOVE SPACE           TO UT7B-IDARTNR20-TILLK                         
041600     MOVE 0               TO UT7B-DIERS-TILLK                             
041700     MOVE 0               TO UT7B-KDARTUTG                                
041800     MOVE SPACE           TO UT7B-KDUTGSTA                                
041900     MOVE SPACE           TO UT7B-KDUTGSTR                                
042000     MOVE 0               TO UT7B-KDPRODSL                                
042100                                                                          
042210     IF IN-FLERSATT = 'J'                                                 
042220     OR NOT (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                          
042300        PERFORM S12-SKRIV-W1167B                                          
042310        MOVE 'BAC' TO UT7D-IDPTYP                                         
042320        PERFORM S15-SKRIV-W1167D                                          
042400     ELSE                                                                 
042500        PERFORM IMS-GU-ARTS-ARTS                                          
042600        IF SEGMENT-FINNS                                                  
042800           PERFORM IMS-GNP-ARTS-SLAG                                      
042900           PERFORM UNTIL SEGMENT-SAKNAS                                   
043000             SET DC-LAND-IX TO 1                                          
043010             SEARCH IDDC-IN-IDLANDX2                                      
043020              AT END                                                      
043021                PERFORM IMS-GNP-ARTS-SLAG                                 
043031              WHEN IDDC-IN-IDLANDX2(DC-LAND-IX) = ARTS-SLAG-IDDC          
043032                PERFORM S12-SKRIV-W1167B                                  
043033                MOVE 'BAC' TO UT7D-IDPTYP                                 
043034                PERFORM S15-SKRIV-W1167D                                  
043035                                                                          
043036*------------- EXIT LOOP NOW (SINCE RECORDS FOR PART AND COUNTRY          
043037*-------------                HAVE BEEN WRITTEN)                          
043038                SET SEGMENT-SAKNAS TO TRUE                                
043039             END-SEARCH                                                   
043040           END-PERFORM                                                    
044400        END-IF                                                            
044500     END-IF                                                               
044600                                                                          
045900     .                                                                    
046000     EJECT                                                                
046010 BC-KOLLA-FINNS SECTION.                                                  
046020                                                                          
046021     IF IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                                
046030        MOVE 'J' TO TAB-FLSALDO                                           
046031     ELSE                                                                 
046033        MOVE 'N' TO TAB-FLSALDO                                           
046034     END-IF                                                               
046040     MOVE NEJ TO TAB-WDK711-FINNS                                         
046050     PERFORM S03-INIT-TAB-IDDC                                            
046060                                                                          
046070     MOVE +0                 TO IX1-DC                                    
046071                                                                          
046080     PERFORM IMS-GU-ARTS-ARTS                                             
046090     IF SEGMENT-FINNS                                                     
046091       PERFORM IMS-GNP-ARTS-SLAG                                          
046092       IF SEGMENT-FINNS                                                   
046093        PERFORM UNTIL SEGMENT-SAKNAS                                      
046094**** ADD CHECK FOR EACH DC                                                
046095          SET DC-LAND-IX TO 1                                             
046096          SEARCH IDDC-IN-IDLANDX2                                         
046097            AT END                                                        
046098              CONTINUE                                                    
046099            WHEN IDDC-IN-IDLANDX2(DC-LAND-IX) = ARTS-SLAG-IDDC            
046100              MOVE JA               TO TAB-WDK711-FINNS                   
046101              MOVE 'N'              TO TAB-FLSALDO                        
046102              IF IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                       
046103                IF ARTS-SLAG-ADLAGOMR = ZERO                              
046104*CC*BUG1*       AND ARTS-SLAG-KVLS   = ZERO                               
046105                AND ARTS-SLAG-KVLS   = ZERO                               
046106                  CONTINUE                                                
046107                ELSE                                                      
046108                  ADD +1            TO IX1-DC                             
046109                  MOVE ARTS-SLAG-IDDC TO TAB-IDDC(IX1-DC)                 
046110                END-IF                                                    
046111              ELSE                                                        
046112                ADD +1                TO IX1-DC                           
046113                MOVE IDDC-IN-IDLANDX2(DC-LAND-IX)                         
046116                                      TO TAB-IDDC(IX1-DC)                 
046118              END-IF                                                      
046119          END-SEARCH                                                      
046120          PERFORM IMS-GNP-ARTS-SLAG                                       
046121        END-PERFORM                                                       
046122       ELSE                                                               
046123         IF NOT (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                      
046125           ADD +1                     TO IX1-DC                           
046126           MOVE IDDC-IN-IDLANDX2(1)   TO TAB-IDDC(IX1-DC)                 
046127         END-IF                                                           
046128       END-IF                                                             
046129     ELSE                                                                 
046130       IF NOT (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                        
046132         ADD +1                       TO IX1-DC                           
046133         MOVE IDDC-IN-IDLANDX2(1)     TO TAB-IDDC(IX1-DC)                 
046134       END-IF                                                             
046135     END-IF                                                               
046136     .                                                                    
046137     EJECT                                                                
046140 BD-SALDOCHECK SECTION.                                                   
046200                                                                          
046300     MOVE NEJ TO TAB-WDK711-FINNS                                         
046700     PERFORM IMS-GU-ARTS-ARTS                                             
046800     IF SEGMENT-FINNS                                                     
046910         PERFORM S03-INIT-TAB-IDDC                                        
047000         MOVE +0                      TO IX1-DC                           
047100         PERFORM IMS-GNP-ARTS-SLAG                                        
047110         PERFORM UNTIL SEGMENT-SAKNAS                                     
047120           SET DC-LAND-IX TO 1                                            
047130           SEARCH IDDC-IN-IDLANDX2                                        
047140            AT END                                                        
047150             CONTINUE                                                     
047160            WHEN IDDC-IN-IDLANDX2(DC-LAND-IX) = ARTS-SLAG-IDDC            
047300             MOVE JA                    TO TAB-WDK711-FINNS               
047301             IF IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                        
047310*CC*BUG1*      IF ARTS-SLAG-KVLS > ZERO                                   
047311               IF ARTS-SLAG-KVLS > ZERO                                   
047320                 IF ARTS-SLAG-ADLAGOMR > ZERO                             
047330                   ADD +1               TO IX1-DC                         
047400                   MOVE ARTS-SLAG-IDDC  TO TAB-IDDC(IX1-DC)               
047410                 END-IF                                                   
047411*CC*BUG1*      END-IF                                                     
047412               END-IF                                                     
047413             ELSE                                                         
047414               ADD +1                   TO IX1-DC                         
047415               MOVE IDDC-IN-IDLANDX2(1) TO TAB-IDDC(IX1-DC)               
047416             END-IF                                                       
047417             MOVE ARTS-SLAG-KVAKS-SDC TO WS-KVAKS-SDC                     
047418             PERFORM BDA-KOLLA-KVAKS-SDC                                  
047419             IF WS-KVAKS-SDC > ZERO                                       
047420                MOVE 'J'              TO TAB-FLSALDO                      
047421             ELSE                                                         
047422                IF ARTS-SLAG-KVLS + ARTS-SLAG-KVAKS-PAV +                 
047423                            ARTS-SLAG-KVBEART > 0                         
047424                  MOVE 'J'            TO TAB-FLSALDO                      
047425                END-IF                                                    
047426             END-IF                                                       
047427           END-SEARCH                                                     
047428                                                                          
047429           PERFORM IMS-GNP-ARTS-SLAG                                      
047430         END-PERFORM                                                      
052410                                                                          
052500** OM INGET SALDO HITTATS KOLLAS OM ARTIKELN FANNS PÅ NÅT                 
052600** DC INOM LANDET.                                                        
052700** I ANNAT FALL BEHANDLAS ARTIKELN SOM OM LAGERSALDO EXISTERAT            
052800         IF TAB-FLSALDO = NEJ                                             
052810         AND IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                           
052900            IF TAB-WDK711-FINNS = NEJ                                     
053000               MOVE JA                TO TAB-FLSALDO                      
053010            END-IF                                                        
053100         END-IF                                                           
053300                                                                          
055400     ELSE                                                                 
055410       IF IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                              
055500         MOVE 'J'                       TO TAB-FLSALDO                    
055600       ELSE                                                               
055601         MOVE 'N'                       TO TAB-FLSALDO                    
055610       END-IF                                                             
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 BDA-KOLLA-KVAKS-SDC SECTION.                                             
056100                                                                          
056110     IF ARTS-SLAG-KVAKS-SDC > 0                                           
056200        PERFORM IMS-GU-WDL601                                             
056300        IF SEGMENT-FINNS                                                  
056310           MOVE '310' TO W-IDPTYP                                         
056320           MOVE ARTS-SLAG-IDDC    TO W-IDDC                               
056400           PERFORM IMS-GNP-WDL611                                         
056500           PERFORM UNTIL SEGMENT-SAKNAS                                   
056600              IF INL-KDRT = 07                                            
056700                 COMPUTE WS-RETUR = INL-KVAVIS - INL-KVANTMOT             
056800                 SUBTRACT WS-RETUR FROM WS-KVAKS-SDC                      
056900              END-IF                                                      
057000              PERFORM IMS-GNP-WDL611                                      
057100           END-PERFORM                                                    
057200        END-IF                                                            
057201     END-IF                                                               
057210                                                                          
057220     IF IN-KDERS = 21 OR 24 OR 29                                         
057221     AND IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                               
057222        MOVE W-IDARTNR         TO W-IDARTNR-D9                            
057223        MOVE ARTS-SLAG-IDDC    TO W-IDDC-D9                               
057224        PERFORM IMS-GU-WDD901                                             
057225        IF SEGMENT-FINNS                                                  
057226           PERFORM IMS-GNP-WDD905                                         
057227           PERFORM UNTIL SEGMENT-SAKNAS                                   
057228              ADD D905-KVAVROP TO WS-KVAKS-SDC                            
057229              PERFORM IMS-GNP-WDD905                                      
057230           END-PERFORM                                                    
057231        END-IF                                                            
057240     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 BE-TILLK-SALDO SECTION.                                                  
057600                                                                          
057610     MOVE ZERO TO ANTAL-ERS                                               
057620                  ANTAL-TRAFF-ERS                                         
057630     PERFORM IMS-GET-ERSA-WLERSA                                          
057640     IF SEGMENT-FINNS                                                     
057650                                                                          
057660       PERFORM IMS-GET-ERSA-ERSA11                                        
057661                                                                          
057662       PERFORM UNTIL SEGMENT-SAKNAS                                       
057663         ADD +1                       TO ANTAL-ERS                        
057664                                                                          
057665         IF ERSA-FLTEXT = 'N'                                             
057666           MOVE ERSA-IDARTNR-TILLK    TO W-IDARTNR                        
057667           PERFORM IMS-GU-ARTS-ARTS                                       
057668           IF SEGMENT-FINNS                                               
057669             PERFORM IMS-GNP-ARTS-SLAG                                    
057670             PERFORM UNTIL SEGMENT-SAKNAS                                 
057671               SET DC-LAND-IX TO 1                                        
057672               SEARCH IDDC-IN-IDLANDX2                                    
057673                AT END                                                    
057674                  CONTINUE                                                
057675                WHEN IDDC-IN-IDLANDX2(DC-LAND-IX) = ARTS-SLAG-IDDC        
057676                  IF (ARTS-SLAG-KVLS +                                    
057677                      ARTS-SLAG-KVAKS-SDC) > 0                            
057678                      ADD +1 TO ANTAL-TRAFF-ERS                           
057679*------------------- EXIT LOOP FOR THIS PART                              
057680                      SET SEGMENT-SAKNAS TO TRUE                          
057681                  ELSE                                                    
057682                   IF IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'                  
057683                    PERFORM IMS-GU-WDL601                                 
057684                    IF SEGMENT-FINNS                                      
057685                      MOVE 'R32' TO W-IDPTYP                              
057686                      MOVE ARTS-SLAG-IDDC    TO W-IDDC                    
057687                      PERFORM IMS-GNP-WDL611                              
057688                      IF SEGMENT-FINNS                                    
057689                         ADD +1 TO ANTAL-TRAFF-ERS                        
057690*---------------------- EXIT LOOP FOR THIS PART                           
057691                         SET SEGMENT-SAKNAS TO TRUE                       
057692                      END-IF                                              
057693                    END-IF                                                
057694                   END-IF                                                 
057695                  END-IF                                                  
057696               END-SEARCH                                                 
057697               PERFORM IMS-GNP-ARTS-SLAG                                  
057698             END-PERFORM                                                  
057699           END-IF                                                         
057700         END-IF                                                           
057701                                                                          
057702         PERFORM IMS-GET-ERSA-ERSA11                                      
057703       END-PERFORM                                                        
057704                                                                          
057705       IF ANTAL-ERS = ANTAL-TRAFF-ERS                                     
057706         MOVE JA TO TAB-FLTILLK-SALDO                                     
057707       END-IF                                                             
057708                                                                          
057709     END-IF                                                               
057710                                                                          
057711********************************************************                  
057712*                                                                         
057713*        ÅTERSTÄLL NYCKELN                                                
057714*                                                                         
057715********************************************************                  
057716                                                                          
057717     MOVE IN-IDARTNR         TO W-IDARTNR                                 
057718     .                                                                    
057719     EJECT                                                                
057720 BF-SKAPA-EV-FILER-7B-7C-7D SECTION.                                      
057721                                                                          
057730     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
057800     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
057900     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
058000     MOVE WS-IDARTNR      TO UT7B-IDARTNR20                               
058100                                                                          
058200     PERFORM IMS-GET-ARTC-ARTC                                            
058210     MOVE ARTC-ART-KDPRODSL      TO TEST-KDPRODSL                         
058300                                                                          
058400     IF SEGMENT-FINNS                                                     
058500       MOVE ARTC-ART-TIERSDAT    TO DAT-I-TIDATUM                         
058600       MOVE 'AAVVD'              TO DAT-KDDATFORM                         
058700                                                                          
058800       PERFORM S02-WDATKONV                                               
058900                                                                          
059000       IF DAT-KDSVAR-OK                                                   
059100         MOVE DAT-TIAAMMDD TO UT7B-TIERSDAT-002                           
059200       ELSE                                                               
059300         MOVE 0            TO UT7B-TIERSDAT-002                           
059400       END-IF                                                             
059500                                                                          
059600       MOVE ARTC-ART-KDPRODSL  TO UT7B-KDPRODSL                           
059700     ELSE                                                                 
059800       MOVE 0             TO UT7B-TIERSDAT-002                            
059900       MOVE 0             TO UT7B-KDPRODSL                                
060000     END-IF                                                               
060100                                                                          
060200     MOVE 'N'             TO UT7B-FLDEL                                   
060300     MOVE IN-KDERS        TO UT7B-KDERS                                   
060400     IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR                 
060500                       '7' OR '8'                                         
060600       MOVE 1  TO UT7B-KDARTUTG                                           
060700     ELSE                                                                 
060800       IF WS-KDERSPOS2 = '2' OR '5'                                       
060900         MOVE 2  TO UT7B-KDARTUTG                                         
061000       END-IF                                                             
061100     END-IF                                                               
061200     MOVE 'F'    TO UT7B-KDUTGSTA                                         
061300                                                                          
061310     IF IN-KDERS = 29 OR 52                                               
061320        MOVE 0        TO UT7B-IDKORTNR                                    
061330        MOVE 0        TO UT7B-KDTEXTGR                                    
061340        MOVE 'N'      TO UT7B-FLTEXT                                      
061350        MOVE SPACE    TO UT7B-IDARTNR20-TILLK                             
061360        MOVE 0        TO UT7B-DIERS-TILLK                                 
061370        MOVE SPACE    TO UT7B-KDUTGSTR                                    
061380                                                                          
061390        IF IN-FLERSATT = 'N'                                              
061391           IF  TAB-FLSALDO = 'N'                                          
061392             EVALUATE TRUE                                                
061393             WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                    
061394              AND TAB-WDK711-FINNS = NEJ                                  
061395               CONTINUE                                                   
061396             WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                    
061397              AND TAB-WDK711-FINNS = JA                                   
061398             WHEN KDPRODSL-LOCAL                                          
061399              AND TAB-WDK711-FINNS = JA                                   
061400             WHEN NOT KDPRODSL-LOCAL                                      
061404              PERFORM S12-SKRIV-W1167B                                    
061405              MOVE 'ERS' TO UT7D-IDPTYP                                   
061406              PERFORM S15-SKRIV-W1167D                                    
061407              PERFORM BFA-SKAPA-EV-INVFIL-7C                              
061408              MOVE 'J' TO TAB-FLERSATT                                    
061409             END-EVALUATE                                                 
061410           END-IF                                                         
061411        END-IF                                                            
061412                                                                          
061413     ELSE                                                                 
061420        PERFORM IMS-GET-ERSA-WLERSA                                       
061500        IF SEGMENT-FINNS                                                  
061600           IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                     
061700              IF ERSA-KVKORT = 1                                          
061800                 MOVE 'S'   TO UT7B-KDUTGSTR                              
061900              ELSE                                                        
062000                 MOVE 'M'   TO UT7B-KDUTGSTR                              
062100              END-IF                                                      
062200           ELSE                                                           
062300              IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                  
062400                MOVE 'V'   TO UT7B-KDUTGSTR                               
062500              END-IF                                                      
062600           END-IF                                                         
062700                                                                          
062800                                                                          
062900           PERFORM IMS-GET-ERSA-ERSA11                                    
063000                                                                          
063100           IF SEGMENT-FINNS                                               
063200              MOVE 1     TO WS-KDTEXTGR-RAKNARE                           
063300              MOVE 'N'   TO WS-FL-TYP1                                    
063400                                                                          
063500              PERFORM UNTIL SEGMENT-SAKNAS                                
063600                                                                          
063700                 MOVE ERSA-IDKORTNR   TO UT7B-IDKORTNR                    
063800                                                                          
063900                 IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR                   
063910                                   '9' OR '7'                             
064000                    MOVE 0        TO UT7B-KDTEXTGR                        
064100                 ELSE                                                     
064200                    IF ERSA-FLTEXT = 'N'                                  
064300                       MOVE 'J'  TO WS-FL-TYP1                            
064400                    END-IF                                                
064500                    IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'             
064600                       ADD  +1   TO WS-KDTEXTGR-RAKNARE                   
064700                       MOVE 'N'  TO WS-FL-TYP1                            
064800                    END-IF                                                
064900                    MOVE WS-KDTEXTGR-RAKNARE TO UT7B-KDTEXTGR             
065000                 END-IF                                                   
065100                                                                          
065200                 MOVE ERSA-FLTEXT     TO UT7B-FLTEXT                      
065300                 IF ERSA-FLTEXT = 'N'                                     
065400                    MOVE ERSA-IDARTNR-TILLK                               
065410                                      TO WS-IDARTNR-NUM                   
065500                    MOVE WS-IDARTNR-NUM                                   
065510                                      TO WS-IDARTNR-TILLK-ALFA            
065600                    INSPECT WS-IDARTNR-TILLK-ALFA                         
065700                                 REPLACING LEADING ZERO BY SPACE          
065800                    MOVE WS-IDARTNR-TILLK                                 
065810                                      TO UT7B-IDARTNR20-TILLK             
065900                    MOVE ERSA-DIERS-TILLK                                 
065910                                      TO UT7B-DIERS-TILLK                 
066000                 ELSE                                                     
066100                    IF ERSA-FLTEXT = 'J'                                  
066200                       MOVE ERSA-BEERS  TO UT7B-IDARTNR20-TILLK           
066300                       MOVE  0          TO UT7B-DIERS-TILLK               
066400                    END-IF                                                
066500                 END-IF                                                   
066600                                                                          
066700                 IF IN-FLERSATT = 'N'                                     
066800                    IF  TAB-FLSALDO      = 'N'                            
066801                      EVALUATE TRUE                                       
066802                      WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')           
066803                       AND TAB-WDK711-FINNS = NEJ                         
066804                        CONTINUE                                          
066805                      WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')           
066806                       AND TAB-WDK711-FINNS = JA                          
066807                      WHEN KDPRODSL-LOCAL                                 
066808                       AND TAB-WDK711-FINNS = JA                          
066809                      WHEN NOT KDPRODSL-LOCAL                             
066900                       PERFORM S12-SKRIV-W1167B                           
066910                       MOVE 'ERS' TO UT7D-IDPTYP                          
066920                       PERFORM S15-SKRIV-W1167D                           
066921                       MOVE 'J' TO TAB-FLERSATT                           
066930                      END-EVALUATE                                        
067000                    END-IF                                                
067100                 END-IF                                                   
067200                                                                          
067800                                                                          
067900                 PERFORM IMS-GET-ERSA-ERSA11                              
068000                                                                          
068100              END-PERFORM                                                 
068200                                                                          
068300              IF IN-FLERSATT = 'N'                                        
068400                 IF TAB-FLSALDO = 'N'                                     
068500                    PERFORM BFA-SKAPA-EV-INVFIL-7C                        
068600                 END-IF                                                   
068700              END-IF                                                      
068800                                                                          
069500                                                                          
069600           END-IF                                                         
069800                                                                          
069900        END-IF                                                            
072200     END-IF                                                               
072300     .                                                                    
072400     EJECT                                                                
072410 BFA-SKAPA-EV-INVFIL-7C SECTION.                                          
072420                                                                          
072430     MOVE +1              TO IX1-DC                                       
072440     PERFORM UNTIL IX1-DC > IX1-DC-MAX                                    
072450       IF TAB-IDDC(IX1-DC) = SPACE                                        
072460          MOVE IX1-DC-MAX       TO IX1-DC                                 
072470       ELSE                                                               
072480          MOVE TAB-IDDC(IX1-DC) TO UT7C-IDDC                              
072481          EVALUATE TRUE                                                   
072482          WHEN UT7C-IDDC = 'KR'                                           
072483            MOVE '65'   TO UT7C-IDDC                                      
072484          WHEN UT7C-IDDC = '6A'                                           
072485            MOVE '61'   TO UT7C-IDDC                                      
072486          END-EVALUATE                                                    
072487          IF (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                         
072488          OR TAB-WDK711-FINNS = JA                                        
072490            PERFORM S14-SKRIV-W1167C                                      
072491          END-IF                                                          
072492       END-IF                                                             
072493       ADD +1                   TO IX1-DC                                 
072494     END-PERFORM                                                          
072495     .                                                                    
072496     EJECT                                                                
078600 BG-SKAPA-EV-PREL-FIL-7B SECTION.                                         
078700                                                                          
078800     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
078900     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
079000     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
079100     MOVE WS-IDARTNR      TO UT7B-IDARTNR20                               
079200                                                                          
079300     PERFORM IMS-GET-ARTC-ARTC                                            
079400     IF SEGMENT-FINNS                                                     
079500        MOVE ARTC-ART-KDPRODSL TO UT7B-KDPRODSL                           
079510                                  TEST-KDPRODSL                           
079700     ELSE                                                                 
079900        MOVE 0                 TO UT7B-KDPRODSL                           
079910                                  TEST-KDPRODSL                           
080000     END-IF                                                               
080010     MOVE 0                 TO UT7B-TIERSDAT-002                          
080100                                                                          
080200     MOVE 'N'             TO UT7B-FLDEL                                   
080300     MOVE IN-KDERS        TO UT7B-KDERS                                   
080400     IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR                 
080500                       '7' OR '8'                                         
080600        MOVE 1 TO UT7B-KDARTUTG                                           
080700     ELSE                                                                 
080800        IF WS-KDERSPOS2 = '2' OR '5'                                      
080900           MOVE 2 TO UT7B-KDARTUTG                                        
081000        END-IF                                                            
081100     END-IF                                                               
081200                                                                          
081300     MOVE 'P' TO UT7B-KDUTGSTA                                            
081400                                                                          
081500     PERFORM IMS-GET-ERSA-WLERSA                                          
081600     IF SEGMENT-FINNS                                                     
081700        IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                        
081800           IF ERSA-KVKORT = 1                                             
081900              MOVE 'S'   TO UT7B-KDUTGSTR                                 
082000           ELSE                                                           
082100              MOVE 'M'   TO UT7B-KDUTGSTR                                 
082200           END-IF                                                         
082300        ELSE                                                              
082400           IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                     
082500              MOVE 'V'   TO UT7B-KDUTGSTR                                 
082600           END-IF                                                         
082700        END-IF                                                            
082800                                                                          
082900        PERFORM IMS-GET-ERSA-ERSA13                                       
083000        IF SEGMENT-FINNS                                                  
083100           IF IN-KDERS > 20                                               
083200              CONTINUE                                                    
083300           ELSE                                                           
083400              IF IN-KDERS > 10 AND < 20                                   
083500                 MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM              
083600                 MOVE 'AAVVD' TO DAT-KDDATFORM                            
083700                 PERFORM S02-WDATKONV                                     
083800                 IF DAT-KDSVAR-OK                                         
083900                    MOVE DAT-TIAAMMDD TO UT7B-TIERSDAT-002                
084000                 ELSE                                                     
084100                    MOVE 0            TO UT7B-TIERSDAT-002                
084200                 END-IF                                                   
084300              ELSE                                                        
084400                 IF IN-KDERS > 0 AND < 10                                 
084500                    MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM               
084600                    MOVE 'AAVVD'      TO DAT-KDDATFORM                    
084700                    PERFORM S02-WDATKONV                                  
084800                    IF DAT-KDSVAR-OK                                      
084900                       MOVE DAT-TIAAMMDD TO UT7B-TIERSDAT-002             
085000                    ELSE                                                  
085100                       MOVE 0            TO UT7B-TIERSDAT-002             
085200                    END-IF                                                
085300                 END-IF                                                   
085400              END-IF                                                      
085500           END-IF                                                         
085600        END-IF                                                            
085700                                                                          
085720        IF  (IN-KDERS = 09 OR 19)                                         
085730        AND (NOT (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US'))                    
085740          PERFORM BGA-MAYBE-WRITE-09-OR-19                                
085750        ELSE                                                              
085800          PERFORM IMS-GET-ERSA-ERSA11-FIRST                               
085900                                                                          
086000          IF SEGMENT-FINNS                                                
086100            MOVE 1     TO WS-KDTEXTGR-RAKNARE                             
086200            MOVE 'N'   TO WS-FL-TYP1                                      
086300                                                                          
086400            PERFORM UNTIL SEGMENT-SAKNAS                                  
086500              MOVE ERSA-IDKORTNR   TO UT7B-IDKORTNR                       
086600              IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'           
086700                 MOVE 0        TO UT7B-KDTEXTGR                           
086800              ELSE                                                        
086900                 IF ERSA-FLTEXT = 'N'                                     
087000                    MOVE 'J'  TO WS-FL-TYP1                               
087100                 END-IF                                                   
087200                 IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'                
087300                    ADD  +1   TO WS-KDTEXTGR-RAKNARE                      
087400                    MOVE 'N'  TO WS-FL-TYP1                               
087500                 END-IF                                                   
087600                 MOVE WS-KDTEXTGR-RAKNARE TO UT7B-KDTEXTGR                
087700              END-IF                                                      
087800                                                                          
087900              MOVE ERSA-FLTEXT     TO UT7B-FLTEXT                         
088000              IF ERSA-FLTEXT = 'N'                                        
088100                 MOVE ERSA-IDARTNR-TILLK  TO WS-IDARTNR-NUM               
088200                 MOVE WS-IDARTNR-NUM      TO WS-IDARTNR-TILLK-ALFA        
088300                 INSPECT WS-IDARTNR-TILLK-ALFA                            
088400                                 REPLACING LEADING ZERO BY SPACE          
088500                 MOVE WS-IDARTNR-TILLK TO UT7B-IDARTNR20-TILLK            
088600                 MOVE ERSA-DIERS-TILLK     TO UT7B-DIERS-TILLK            
088700              ELSE                                                        
088800                 IF ERSA-FLTEXT = 'J'                                     
088900                    MOVE ERSA-BEERS  TO UT7B-IDARTNR20-TILLK              
089000                    MOVE  0     TO UT7B-DIERS-TILLK                       
089100                 END-IF                                                   
089200              END-IF                                                      
089300                                                                          
089310              PERFORM S04-SEARCH-TAB-IDDC                                 
089500              EVALUATE TRUE                                               
089510              WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                   
089520               AND WS-TAB-IDDC     = SPACE                                
089530                CONTINUE                                                  
089540              WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                   
089541               AND WS-TAB-IDDC NOT = SPACE                                
089560              WHEN KDPRODSL-LOCAL                                         
089561               AND WS-TAB-IDDC NOT = SPACE                                
089580              WHEN NOT KDPRODSL-LOCAL                                     
089600                 PERFORM S12-SKRIV-W1167B                                 
089610              END-EVALUATE                                                
089800                                                                          
090200                                                                          
090300              PERFORM IMS-GET-ERSA-ERSA11                                 
090400            END-PERFORM                                                   
090500                                                                          
090600          ELSE                                                            
090700            IF IN-KDERS = 09 OR 19                                        
090710               PERFORM BGA-MAYBE-WRITE-09-OR-19                           
092300            END-IF                                                        
092400          END-IF                                                          
092410        END-IF                                                            
092500     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
092800 BGA-MAYBE-WRITE-09-OR-19 SECTION.                                        
092900                                                                          
092904     MOVE 0        TO UT7B-IDKORTNR                                       
092905     MOVE 0        TO UT7B-KDTEXTGR                                       
092906     MOVE 'N'      TO UT7B-FLTEXT                                         
092907     MOVE SPACE    TO UT7B-IDARTNR20-TILLK                                
092908     MOVE 0        TO UT7B-DIERS-TILLK                                    
092909     MOVE SPACE    TO UT7B-KDUTGSTR                                       
092910                                                                          
092915     PERFORM S04-SEARCH-TAB-IDDC                                          
092917     EVALUATE TRUE                                                        
092918     WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                            
092919      AND WS-TAB-IDDC     = SPACE                                         
092920       CONTINUE                                                           
092921     WHEN (IN-IDLANDX2 = 'CN' OR 'CA' OR 'US')                            
092922      AND WS-TAB-IDDC NOT = SPACE                                         
092923     WHEN KDPRODSL-LOCAL                                                  
092924      AND WS-TAB-IDDC NOT = SPACE                                         
092926     WHEN NOT KDPRODSL-LOCAL                                              
092927        PERFORM S12-SKRIV-W1167B                                          
092928     END-EVALUATE                                                         
092929     .                                                                    
092930     EJECT                                                                
092931 BH-SKAPA-FIL-W11679 SECTION.                                             
092932                                                                          
092933     MOVE IN-IDARTNR       TO UT1-IDARTNR                                 
092934     MOVE IN-IDLANDX2      TO UT1-IDLANDX2                                
092935     MOVE IN-KDERS         TO UT1-KDERS                                   
092936     MOVE TAB-FLERSATT     TO UT1-FLERSATT                                
092937     PERFORM S11-SKRIV-W11679                                             
092938     .                                                                    
092940     EJECT                                                                
099990 Z-FINIT SECTION.                                                         
100000                                                                          
100100     CLOSE W11661                                                         
100200           W11679                                                         
100300           W1167B                                                         
100500           W1167C                                                         
100510           W1167D                                                         
100600     MOVE 'S'     TO POSTSUM-OPKOD                                        
100700     CALL POSTSUM USING POSTSUM-PARM                                      
100800     .                                                                    
100900     EJECT                                                                
101000 S01-LAES-W11661  SECTION.                                                
101100                                                                          
101200     READ W11661          INTO IN-AREA                                    
101300     AT END                                                               
101400        SET END-OF-W11661 TO TRUE                                         
101500                                                                          
101600     NOT AT END                                                           
101700        MOVE 'W11661'     TO    POSTSUM-FDNAMN                            
101800        MOVE 'W11679D1'   TO    POSTSUM-DDNAMN2                           
101810        MOVE SPACE        TO    POSTSUM-TRANSTYP                          
101900        CALL POSTSUM      USING POSTSUM-PARM                              
102000     END-READ                                                             
102100     .                                                                    
102200     EJECT                                                                
102300 S02-WDATKONV SECTION.                                                    
102400     SKIP2                                                                
102500     CALL WDATKONV USING DAT-KDDATFORM                                    
102600                         DAT-I-TIDATUM                                    
102700                         DAT-O-TIDATUM                                    
102800                         DAT-KDSVAR                                       
102900     .                                                                    
103000     EJECT                                                                
103010 S03-INIT-TAB-IDDC SECTION.                                               
103020                                                                          
103030     MOVE +1 TO IX1-DC                                                    
103040     PERFORM UNTIL IX1-DC > IX1-DC-MAX                                    
103050       MOVE SPACE              TO TAB-IDDC(IX1-DC)                        
103060       ADD +1                  TO IX1-DC                                  
103061     END-PERFORM                                                          
103070     .                                                                    
103071 S04-SEARCH-TAB-IDDC SECTION.                                             
103073                                                                          
103074     MOVE SPACE                TO WS-TAB-IDDC                             
103075     MOVE +1 TO IX1-DC                                                    
103076     PERFORM UNTIL IX1-DC > IX1-DC-MAX                                    
103077       IF TAB-IDDC(IX1-DC) NOT = SPACE                                    
103078         MOVE TAB-IDDC(IX1-DC) TO WS-TAB-IDDC                             
103080         MOVE IX1-DC-MAX       TO IX1-DC                                  
103081       END-IF                                                             
103082       ADD +1                  TO IX1-DC                                  
103083     END-PERFORM                                                          
103085     .                                                                    
103090     EJECT                                                                
103100 S11-SKRIV-W11679 SECTION.                                                
103200                                                                          
103300     WRITE W11679-POST    FROM UT1-AREA                                   
103400                                                                          
103500     MOVE 'W11679 '       TO POSTSUM-FDNAMN                               
103600     MOVE 'W11679D2'      TO POSTSUM-DDNAMN2                              
103603     MOVE IN-IDLANDX2     TO POSTSUM-TRANSTYP                             
103700     CALL POSTSUM         USING POSTSUM-PARM                              
103800     .                                                                    
103900     EJECT                                                                
104000 S12-SKRIV-W1167B SECTION.                                                
104100                                                                          
104110     IF IN-IDLANDX2 NOT = WS-MQ-IDLANDX2                                  
104120       WRITE W1167B-POST    FROM WS-MQ-LINE1                              
104121       MOVE  SPACE            TO W1167B-POST                              
104122       WRITE W1167B-POST    FROM WS-MQ-LINE2                              
104123       MOVE  SPACE            TO W1167B-POST                              
104124       MOVE IN-IDLANDX2       TO WS-MQ-IDLANDX2                           
104156       WRITE W1167B-POST    FROM WS-MQ-LINE3                              
104157       MOVE SPACE             TO W1167B-POST                              
104160     END-IF                                                               
104170                                                                          
104200     MOVE 'WB1'           TO UT7B-IDPTYP                                  
104300     WRITE W1167B-POST    FROM UT7B-AREA                                  
104400                                                                          
104500     MOVE 'W1167B '       TO POSTSUM-FDNAMN                               
104600     MOVE 'W11679D3'      TO POSTSUM-DDNAMN2                              
104610     MOVE SPACE           TO POSTSUM-TRANSTYP                             
104700     CALL POSTSUM         USING POSTSUM-PARM                              
104800     .                                                                    
104900     EJECT                                                                
106000 S14-SKRIV-W1167C SECTION.                                                
106100                                                                          
106200     MOVE IN-IDARTNR      TO UT7C-IDARTNR                                 
106300     MOVE +5              TO UT7C-KDINVKAT                                
106400     WRITE W1167C-POST    FROM UT7C-AREA                                  
106500                                                                          
106600     MOVE 'W1167C '       TO POSTSUM-FDNAMN                               
106700     MOVE 'W11679D4'      TO POSTSUM-DDNAMN2                              
106710     MOVE SPACE           TO POSTSUM-TRANSTYP                             
106800     CALL POSTSUM         USING POSTSUM-PARM                              
106900     .                                                                    
107000     EJECT                                                                
107010 S15-SKRIV-W1167D SECTION.                                                
107020                                                                          
107030     MOVE IN-IDARTNR      TO UT7D-IDARTNR                                 
107040     MOVE IN-IDLANDX2     TO UT7D-IDLANDX2                                
107050     WRITE W1167D-POST    FROM UT7D-AREA                                  
107060                                                                          
107070     MOVE 'W1167D '       TO POSTSUM-FDNAMN                               
107080     MOVE 'W11679D5'      TO POSTSUM-DDNAMN2                              
107081     MOVE SPACE           TO POSTSUM-TRANSTYP                             
107090     CALL POSTSUM         USING POSTSUM-PARM                              
107091     .                                                                    
107092     EJECT                                                                
107100* --- IMS SEKTIONER ---                                                   
107200     SKIP3                                                                
107300 IMS-GU-ARTS-ARTS SECTION.                                                
107400                                                                          
107500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
107600          DELIMITED BY SIZE INTO SSA1                                     
107700     MOVE '  GE' TO GODK-STATUSKODER                                      
107800     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS01 SSA1                  
107900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     EJECT                                                                
108300 IMS-GNP-ARTS-SLAG SECTION.                                               
108400                                                                          
108500     STRING 'WLARTS11(IDDC    >=' W-IDDC-MIN-X                            
108510                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
108600          DELIMITED BY SIZE INTO SSA1                                     
108700     MOVE '  GE' TO GODK-STATUSKODER                                      
108800     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WLARTS11 SSA1                 
108900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     .                                                                    
109200     EJECT                                                                
109300 IMS-GET-ARTC-ARTC SECTION.                                               
109400                                                                          
109500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
109600          DELIMITED BY SIZE INTO SSA1                                     
109700     MOVE '  GE' TO GODK-STATUSKODER                                      
109800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
109900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
110000     PERFORM IMS-STATUSKONTROLL                                           
110100     .                                                                    
110200     EJECT                                                                
110300 IMS-GET-ERSA-WLERSA SECTION.                                             
110400                                                                          
110500     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
110600          DELIMITED BY SIZE INTO SSA1                                     
110700     MOVE '  GE' TO GODK-STATUSKODER                                      
110800     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-WLERSA01 SSA1                  
110900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     .                                                                    
111200     SKIP3                                                                
111300 IMS-GET-ERSA-ERSA11-FIRST SECTION.                                       
111400                                                                          
111500     MOVE 'WLERSA11*F' TO SSA1                                            
111600     MOVE '  GE' TO GODK-STATUSKODER                                      
111700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
111800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     SKIP3                                                                
112200 IMS-GET-ERSA-ERSA13 SECTION.                                             
112300                                                                          
112400     STRING 'WLERSA13 '                                                   
112500          DELIMITED BY SIZE INTO SSA1                                     
112600     MOVE '  GE' TO GODK-STATUSKODER                                      
112700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA13 SSA1                 
112800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     EJECT                                                                
113200 IMS-GET-ERSA-ERSA11 SECTION.                                             
113300                                                                          
113400     STRING 'WLERSA11 '                                                   
113500          DELIMITED BY SIZE INTO SSA1                                     
113600     MOVE '  GE' TO GODK-STATUSKODER                                      
113700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
113800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
113900     PERFORM IMS-STATUSKONTROLL                                           
114000     .                                                                    
114100     EJECT                                                                
114200 IMS-GU-WDL601 SECTION.                                                   
114300     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
114400          DELIMITED BY SIZE INTO SSA1                                     
114500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
114600     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
114700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
114800     PERFORM IMS-STATUSKONTROLL                                           
114900     .                                                                    
115000     SKIP2                                                                
115100 IMS-GNP-WDL611 SECTION.                                                  
115200     STRING 'WDL611  (IDDC     =' W-IDDC-X                                
115300                    '&IDPTYP   =' W-IDPTYP-X ')'                          
115400            DELIMITED BY SIZE INTO SSA1                                   
115500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
115600     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
115700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000     EJECT                                                                
116010 IMS-GU-WDD901 SECTION.                                                   
116012                                                                          
116020     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
116030          DELIMITED BY SIZE INTO SSA1                                     
116040     MOVE '  GE'              TO GODK-STATUSKODER                         
116050     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
116060     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
116070     PERFORM IMS-STATUSKONTROLL                                           
116080     .                                                                    
116090     SKIP2                                                                
116091 IMS-GNP-WDD905 SECTION.                                                  
116093                                                                          
116094     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
116095          DELIMITED BY SIZE INTO SSA1                                     
116096     MOVE '  GE'              TO GODK-STATUSKODER                         
116097     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
116098     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
116099     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116101     SKIP2                                                                
116102 IMS-GU-WDB601-FIRST   SECTION.                                           
116103                                                                          
116104     MOVE   'WDB601  *F '   TO SSA1                                       
116105     MOVE '  GE'            TO GODK-STATUSKODER                           
116106     CALL CBLTDLI        USING GU WDB6-PCB DLI-IO-WDB601 SSA1             
116107     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
116108     PERFORM IMS-STATUSKONTROLL                                           
116109     .                                                                    
116110     EJECT                                                                
116111 IMS-GN-WDB601 SECTION.                                                   
116113                                                                          
116114     MOVE 'WDB601 '        TO SSA1                                        
116115     MOVE '  GB' TO GODK-STATUSKODER                                      
116116     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
116117     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
116118     PERFORM IMS-STATUSKONTROLL                                           
116119     .                                                                    
116120     EJECT                                                                
116130 IMS-STATUSKONTROLL SECTION.                                              
116200     SKIP2                                                                
116300     SET STATUS-IX TO 1                                                   
116400     SEARCH GODK-STATUS                                                   
116500       AT END                                                             
116600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
116700           DELIMITED BY SIZE INTO FELTEXT                                 
116800         DISPLAY FELTEXT                                                  
116900         CALL FELLOG                                                      
117000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
117100         CONTINUE                                                         
117200     END-SEARCH                                                           
117300     .                                                                    
