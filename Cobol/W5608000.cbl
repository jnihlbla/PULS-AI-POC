000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5608000.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   96/11/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        TÖMMER EN KÖ FÖR ÄNDRING AV LOKALT PRODUKTSLAG                   
001100*        FÖR VARJE NDC SOM HAR ARTIKELN SKAPAS EN TRANS TYP A17           
001200*        SAMTIDIGT SOM DET NYA LOKALA PRODUKTSLAGET UPPDATERAS            
001300*        PÅ BASEN WDK6 (ARTIKELREGISTRET)                                 
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001600*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001700*        PROGRAMMET LÄSER/UPPDATERAR HÄNDELSEBAS (KÖBAS)                  
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W5608000'.            
004000 77  W-KVPOST-3144               PIC S9(7)   VALUE ZERO COMP-3.           
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005010 01  -COPY WWDC99                                                         
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500     EJECT                                                                
005900                                                                          
006000 01  WS-ARBAREA.                                                          
006100     03  WS-HHMMSSTH             PIC 9(8)    VALUE ZERO.                  
006200                                                                          
006300                                                                          
006400 01  DAGENS-TIAAAAMMDD           PIC 9(8).                                
006500                                                                          
006600 01  W-IDSEKVNR                  PIC S9(3)   VALUE ZERO COMP-3.           
006700                                                                          
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007400     EJECT                                                                
007500*---- PARAMETRAR TILL ABEND                                               
007600 01  RETURKODER.                                                          
007700     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
007800     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
007900     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
008000                                                                          
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)   VALUE 'A17-TRANS'.           
008600**   ---- A17-TRANS                                                       
008700 01  -COPY W510A17    -PRE A17-                                           
008800                                                                          
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009200 01  NYCKLAR-TILL-DLI.                                                    
009300     03  W-IDARTNR-X.                                                     
009400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009500     03  W-IDARTNR-ART-X.                                                 
009600         05  W-IDARTNR-ART       PIC S9(9)   VALUE ZERO COMP-3.           
009700     03  W-IDDC-X.                                                        
009800         05 W-IDDC               PIC X(2).                                
009900     03  W-IDLEVNR-X.                                                     
010000         05 W-IDLEVNR            PIC X(5)   VALUE SPACE.                  
010100     03  W-WDGXKEY-X.                                                     
010200         05  FILLER          PIC X(4)   VALUE '5141'.                     
010300         05  FILLER          PIC X(26)  VALUE LOW-VALUE.                  
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011100     88  IMS-EJ-OK                           VALUE 'XD'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300                                                                          
012400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS01'.                    
012500 01  DLI-IO-WLARTS01.                                                     
012600*    03  -COPY WDK701  -PRE ARTS-                                         
012700     EJECT                                                                
012800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
012900 01  DLI-IO-WLARTS11.                                                     
013000*    03  -COPY WDK711  -PRE ARTS-                                         
013100     EJECT                                                                
013200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
013300 01  DLI-IO-WLARTC01.                                                     
013400*    03  -COPY WDK601  -PRE ARTC-                                         
013500     EJECT                                                                
013600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
013700 01  DLI-IO-WLARTC11.                                                     
013800*    03  -COPY WDK611  -PRE ARTC-                                         
013900     EJECT                                                                
014000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLFILB01'.                    
014100 01  DLI-IO-WLFILB01.                                                     
014200*    03  -COPY WDR801  -PRE FILB-                                         
014300     EJECT                                                                
014400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WL514101'.                    
014500 01  DLI-IO-WL514101.                                                     
014600*    03  -COPY WDGX01   -PRE 5141-                                        
014700     EJECT                                                                
014800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WL514111'.                    
014900 01  DLI-IO-WL514111.                                                     
015000*    03  -COPY WDGX5142 -PRE 5141-                                        
015100     EJECT                                                                
015110 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601  '.                    
015120 01  DLI-IO-WDB601.                                                       
015130*    03  -COPY WDB601                                                     
015140     EJECT                                                                
015200 LINKAGE SECTION.                                                         
015300                                                                          
015400*01  -COPY W0009   -PRE MSG-                                              
015500     EJECT                                                                
015600*01  -COPY W0008  -PRE ARTS-                                              
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008  -PRE ARTC-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200*01  -COPY W0008  -PRE FILB-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE 5141-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016710*01  -COPY W0008  -PRE WDB6-                                              
016720     05  FILLER                  PIC X.                                   
016730     EJECT                                                                
016800 PROCEDURE DIVISION  USING MSG-PCB ARTS-PCB ARTC-PCB                      
016900                           FILB-PCB 5141-PCB WDB6-PCB.                    
017000 MAIN SECTION.                                                            
017100     ENTRY 'DLITCBL' USING MSG-PCB ARTS-PCB ARTC-PCB                      
017200                           FILB-PCB 5141-PCB WDB6-PCB.                    
017300                                                                          
017400     PERFORM A-INIT                                                       
017500     PERFORM IMS-GET-WL514101                                             
017600     PERFORM IMS-GHNP-WL514111                                            
017700     IF SEGMENT-FINNS                                                     
017800        PERFORM UNTIL SEGMENT-SAKNAS                                      
017900          IF CHKP-ANT > CHKP-MAX                                          
018000             PERFORM X-TAG-CHECKPOINT                                     
018100          END-IF                                                          
018200          IF CHKP-ANT = ZERO                                              
018300             PERFORM IMS-GET-WL514101                                     
018400             PERFORM IMS-GHNP-WL514111                                    
018500          END-IF                                                          
018600          MOVE 5141-5142-IDARTNR TO W-IDARTNR                             
018700          PERFORM IMS-GET-ARTC-WLARTC01                                   
018800          IF SEGMENT-FINNS                                                
018900             PERFORM C-BEHANDLA                                           
019000             PERFORM IMS-DLET-WL514111                                    
019100             ADD +1 TO CHKP-ANT                                           
019200          END-IF                                                          
019300          PERFORM IMS-GHNP-WL514111                                       
019400        END-PERFORM                                                       
019500     END-IF                                                               
019600                                                                          
019700     MOVE ZERO TO RETURN-CODE                                             
019800     GOBACK                                                               
019900     .                                                                    
020000     EJECT                                                                
020100 A-INIT SECTION.                                                          
020200     SKIP2                                                                
020300                                                                          
020400     MOVE +1 TO CHKP-ANT                                                  
020500     PERFORM IMS-RESTART                                                  
020600     .                                                                    
020700     EJECT                                                                
020800 C-BEHANDLA SECTION.                                                      
020900                                                                          
021000     PERFORM IMS-GET-ARTC-WLARTC11                                        
021100     IF SEGMENT-FINNS                                                     
021200        MOVE 5141-5142-KDPSLLOC-NEW TO                                    
021300                               ARTC-CLAG-KDPSLLOC                         
021400        PERFORM IMS-REPL-ARTC                                             
021500        ADD +1 TO CHKP-ANT                                                
021600        PERFORM IMS-GN-WDB601                                             
021700        PERFORM UNTIL SEGMENT-SLUT                                        
021800           MOVE DCS-IDDC TO WS-IDDC                                       
021900           IF NDC-NA                                                      
022300              MOVE DCS-IDDC     TO W-IDDC                                 
022400              PERFORM IMS-GET-ARTS-WLARTS11                               
022500              IF SEGMENT-FINNS                                            
022600                 MOVE DCS-IDDC  TO A17-IDDC-SEND                          
022700                 MOVE DCS-IDFTG TO A17-IDFTG                              
022800                 PERFORM CA-SKAPA-TRANS-WDR8                              
022900              END-IF                                                      
023000           END-IF                                                         
023100           PERFORM IMS-GN-WDB601                                          
023200        END-PERFORM                                                       
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 CA-SKAPA-TRANS-WDR8 SECTION.                                             
024900                                                                          
025000     MOVE 'A17'                  TO A17-IDPTYP                            
025100     MOVE 'M21'                  TO A17-KDEKOHT                           
025400     MOVE A17-IDDC-SEND          TO A17-IDDC-REC                          
025500     MOVE ARTC-ART-KDPRODSL      TO A17-KDPRODSL                          
025600     MOVE 5141-5142-IDARTNR      TO A17-IDARTNR                           
025700     MOVE 5141-5142-KDPSLLOC     TO A17-KDPSLLOC-OLD                      
025800     MOVE 5141-5142-KDPSLLOC-NEW TO A17-KDPSLLOC-NEW                      
025900     MOVE ARTS-SLAG-PRAVCOST     TO A17-PRAVCOST                          
026000     MOVE ARTS-SLAG-KVLS         TO A17-KVLS                              
026100     MOVE ARTS-SLAG-KVEFRS       TO A17-KVEFRS                            
026200                                                                          
026300                                                                          
026400     MOVE 'W5608000'             TO FILB-FIL-IDPGM                        
026500     MOVE FUNCTION CURRENT-DATE (1:8) TO                                  
026600     DAGENS-TIAAAAMMDD                                                    
026700     ACCEPT WS-HHMMSSTH FROM TIME                                         
026800     MOVE WS-HHMMSSTH            TO FILB-FIL-TIKLOCK                      
026900                                                                          
027000     MOVE DAGENS-TIAAAAMMDD      TO FILB-FIL-TIREGDAT                     
027100     MOVE DAGENS-TIAAAAMMDD      TO A17-DAJUSTDA                          
027200     ADD +1                      TO W-IDSEKVNR                            
027300     MOVE W-IDSEKVNR             TO FILB-FIL-IDSEKVNR                     
027400     MOVE 'W510A17 '             TO FILB-FIL-IDCPYTXT                     
027500     MOVE A17-W510A17            TO FILB-FIL-WDR801-DATA                  
027600                                                                          
027700     PERFORM IMS-ISRT-WLFILB01                                            
027800     ADD +1 TO CHKP-ANT                                                   
027900     .                                                                    
028000     EJECT                                                                
028100 X-TAG-CHECKPOINT   SECTION.                                              
028200                                                                          
028300* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
028400* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
028500     PERFORM IMS-CHECKPOINT                                               
028600     MOVE ZERO TO CHKP-ANT                                                
028700* --- LÄS OM DATABAS OM DET BEHÖVS                                        
028800     .                                                                    
028900     EJECT                                                                
029000* --- IMS SEKTIONER ---                                                   
029100     SKIP3                                                                
029200     EJECT                                                                
029300 IMS-GET-ARTS-WLARTS11 SECTION.                                           
029400                                                                          
029500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
029600           DELIMITED BY SIZE INTO SSA1                                    
029700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
029800           DELIMITED BY SIZE INTO SSA2                                    
029900     MOVE '  GE' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
030100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     SKIP3                                                                
030500 IMS-GET-ARTC-WLARTC01 SECTION.                                           
030600                                                                          
030700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
030800          DELIMITED BY SIZE INTO SSA1                                     
030900     MOVE '  GE' TO GODK-STATUSKODER                                      
031000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
031100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031200     PERFORM IMS-STATUSKONTROLL                                           
031300     .                                                                    
031400     EJECT                                                                
031500 IMS-GET-ARTC-WLARTC11 SECTION.                                           
031600                                                                          
031700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031800          DELIMITED BY SIZE INTO SSA1                                     
031900     STRING 'WLARTC11(KDSEGKEY =1)'                                       
032000          DELIMITED BY SIZE INTO SSA2                                     
032100     MOVE '  ' TO GODK-STATUSKODER                                        
032200     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2            
032300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032600     EJECT                                                                
032700 IMS-REPL-ARTC SECTION.                                                   
032800                                                                          
032900     MOVE '  ' TO GODK-STATUSKODER                                        
033000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
033100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
033200     PERFORM IMS-STATUSKONTROLL                                           
033300     .                                                                    
033400     SKIP3                                                                
033500 IMS-ISRT-WLFILB01 SECTION.                                               
033600                                                                          
033700     MOVE 'WLFILB01 ' TO SSA1                                             
033800     MOVE '  II' TO GODK-STATUSKODER                                      
033900     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-WLFILB01 SSA1                
034000     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300     EJECT                                                                
034400 IMS-GET-WL514101 SECTION.                                                
034500                                                                          
034600     STRING 'WL514101(WDGXKEY  =' W-WDGXKEY-X ')'                         
034700          DELIMITED BY SIZE INTO SSA1                                     
034800     MOVE '  GE' TO GODK-STATUSKODER                                      
034900     CALL CBLTDLI USING GU 5141-PCB DLI-IO-WL514101 SSA1                  
035000     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
035100     PERFORM IMS-STATUSKONTROLL                                           
035200     .                                                                    
035300     EJECT                                                                
035400 IMS-GHNP-WL514111 SECTION.                                               
035500                                                                          
035600     MOVE 'WL514111 ' TO SSA1                                             
035700     MOVE '  GE' TO GODK-STATUSKODER                                      
035800     CALL CBLTDLI USING GHNP 5141-PCB DLI-IO-WL514111 SSA1                
035900     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
036000     PERFORM IMS-STATUSKONTROLL                                           
036100     .                                                                    
036200     EJECT                                                                
036300 IMS-DLET-WL514111 SECTION.                                               
036400                                                                          
036500     MOVE '  ' TO GODK-STATUSKODER                                        
036600     CALL CBLTDLI USING DLET 5141-PCB DLI-IO-WL514111                     
036700     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     .                                                                    
037000     SKIP3                                                                
037100 IMS-RESTART SECTION.                                                     
037200     SKIP2                                                                
037300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037400     MOVE '  ' TO GODK-STATUSKODER                                        
037500     CALL CBLTDLI USING XRST MSG-PCB                                      
037600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037700                        CHKP-AREA-LENGTH CHKP-AREA                        
037800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037900     PERFORM IMS-STATUSKONTROLL                                           
038000     .                                                                    
038100     EJECT                                                                
038200 IMS-CHECKPOINT SECTION.                                                  
038300     SKIP2                                                                
038400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038500     MOVE '  XD' TO GODK-STATUSKODER                                      
038600     CALL CBLTDLI USING CHKP MSG-PCB                                      
038700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038800                        CHKP-AREA-LENGTH CHKP-AREA                        
038900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039000     PERFORM IMS-STATUSKONTROLL                                           
039100                                                                          
039200     IF IMS-EJ-OK                                                         
039300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
039400       DISPLAY FELTEXT                                                    
039500       CALL FELLOG                                                        
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039810 IMS-GN-WDB601 SECTION.                                                   
039820                                                                          
039830     MOVE 'WDB601 ' TO SSA1                                               
039840                                                                          
039850     MOVE '  GB' TO GODK-STATUSKODER                                      
039860     CALL CBLTDLI USING                                                   
039870           GN WDB6-PCB DLI-IO-WDB601 SSA1                                 
039880     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
039890     PERFORM IMS-STATUSKONTROLL                                           
039891     .                                                                    
039900 IMS-STATUSKONTROLL SECTION.                                              
040000     SKIP2                                                                
040100     SET STATUS-IX TO 1                                                   
040200     SEARCH GODK-STATUS                                                   
040300       AT END                                                             
040400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040500           DELIMITED BY SIZE INTO FELTEXT                                 
040600         DISPLAY FELTEXT                                                  
040700         CALL FELLOG                                                      
040800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040900         CONTINUE                                                         
041000     END-SEARCH                                                           
041100     .                                                                    
