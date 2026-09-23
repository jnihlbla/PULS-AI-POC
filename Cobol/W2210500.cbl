000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2210500.                                                
000300 AUTHOR.         ARVIDSSON LENA.                                          
000400 DATE-WRITTEN.   05/03/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        TAR BORT LARM 225 OM AVBOKAT ANTAL TÄCKER AVROP OCH SLÄP         
001000*        FÖR ARTIKELN.                                                    
001100*        DÅ LARMET EJ SLÄCKS UPPDATERAS "P.ADV/G.REC" VÄRDEN FÖR          
001200*        LARMAD (NOT ARRIVED, 225) ARTIKEL PÅ BILD 2172.                  
001300*                                                                         
001400*        TILLÄGG:LAK-IDDC, SAMMA PGM ANVÄNDS FÖR KINAS LARM 225.          
001500*                                                                         
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WDD4                                       
001800*        PROGRAMMET LÄSER      WDD9                                       
001900*                                                                         
002000*    ÄNDRINGAR:                                                           
002100*    ETRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                     
002200*                                                                         
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100*          --- LOG FILE FOR ALARMS DELETED FROM WDR5                      
003200     SELECT W22105               ASSIGN W22105D1.                         
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700                                                                          
003800 FD  W22105                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100*01  POST -COPY W214ALOG -PRE  UT1-     -L.                               
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W2210500'.            
004600 01  CHKP-VAR.                                                            
004700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005200     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 01  W1-DAREGDAT                 PIC 9(08).                               
005600 01  W1-DAREGDAT-9KOMPL          PIC 9(08).                               
005700 77  W1-TIKLOCK                  PIC 9(09).                               
005800 77  W1-TIKLOCK-9KOMPL           PIC 9(09)   COMP-3.                      
005900     SKIP2                                                                
006000 01  SUM-KVAVROP-AVB             PIC S9(7)   COMP-3.                      
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
006800                                                                          
006900 01  401-AAAAVVD                 PIC 9(7)    VALUE ZERO.                  
007000 01  FILLER REDEFINES 401-AAAAVVD.                                        
007100     03  401-SEKEL               PIC 9(2).                                
007200     03  401-AAVVD               PIC 9(5).                                
007300     EJECT                                                                
007400 01  WS-AAAAVVD                  PIC 9(7)    VALUE ZERO.                  
007500 01  FILLER REDEFINES WS-AAAAVVD.                                         
007600     03  WS-AAAAVV               PIC 9(6).                                
007700     03  WS-TILEVDAG             PIC 9(1).                                
007800 01  WS-ALARM-AAVV               PIC 9(4).                                
007900     EJECT                                                                
008000 01  WS-DAREGDAT                 PIC 9(8).                                
008100 01  FILLER REDEFINES WS-DAREGDAT.                                        
008200     03  FILLER                  PIC 9(2).                                
008300     03  WS-TIREGDAT             PIC 9(6).                                
008400     EJECT                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900*                                                                         
009000*    --- PARAMETRAR TILL ABEND                                            
009100                                                                          
009200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009500                                                                          
009600*01  -COPY WWDCKONS                                                       
009700                                                                          
009800     SKIP2                                                                
009900                                                                          
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100*                                                                         
010200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     EJECT                                                                
010800*01  -COPY W0005  -PRE  POSTSUM-                                          
010900     EJECT                                                                
011000*01  -COPY WDATAREA                                                       
011100     EJECT                                                                
011200*01  -COPY WWDC99                                                         
011300     EJECT                                                                
011400 01  UT-AREA-START               PIC X(16)   VALUE                        
011500                                             'UT-AREA-START'.             
011600*01  AREA -COPY W214ALOG    -PRE UT1-                                     
011700     EJECT                                                                
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012000     SKIP3                                                                
012100 01  NYCKLAR-TILL-DLI.                                                    
012200     03  W-WDD401KY-MIN-X.                                                
012300         05  W-DAREGDAT-9KOMPL  PIC 9(8)            VALUE ZERO.           
012400         05  W-TIKLOCK-9KOMPL   PIC S9(9) COMP-3  VALUE ZERO.             
012500     03  W-WDD401KY-MAX-X.                                                
012600         05  W-WDD401KY-MAX      PIC X(13)   VALUE HIGH-VALUE.            
012700     03  W-WDD901KY-X.                                                    
012800         05  W-IDARTNR           PIC S9(9) COMP-3   VALUE ZERO.           
012900         05  W-IDDC              PIC X(2)           VALUE SPACE.          
013000     03  W-IDLEVNR-X.                                                     
013100         05  W-IDLEVNR           PIC X(5)           VALUE SPACE.          
013200     03  W-KDLARM-X.                                                      
013300         05  W-KDLARM            PIC S9(3) COMP-3   VALUE 225.            
013400     SKIP2                                                                
013500     03  W-WDD905KY-X.                                                    
013600         05  W-DAAVROP-X.                                                 
013700             07  W-DAAVROP       PIC 9(6)    VALUE ZERO.                  
013800         05  W-TILEVDAG-X.                                                
013900             07  W-TILEVDAG      PIC S9(1)   VALUE ZERO COMP-3.           
014000     03  W-KDAVROP-X.                                                     
014100         05  W-KDAVROP           PIC S9(1)   VALUE 2    COMP-3.           
014200     SKIP2                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FINNS                       VALUE '  '.                  
014600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014900     88  IMS-EJ-OK                           VALUE 'XD'.                  
015000     SKIP2                                                                
015100 01  GODK-STATUSKODER.                                                    
015200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015300     SKIP3                                                                
015400 01  SSA1                        PIC X(128).                              
015500 01  SSA2                        PIC X(64).                               
015600 01  SSA3                        PIC X(64).                               
015700*01  SSA4                        PIC X(64).                               
015800     EJECT                                                                
015900*    --- IMS FUNKTIONSKODER                                               
016000*01  -COPY W0003                                                          
016100     EJECT                                                                
016200*    ---  DLI INPUT-OUTPUT AREA                                           
016300                                                                          
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401'.                      
016500 01  DLI-IO-WDD401.                                                       
016600*    03  -COPY WDD401                                                     
016700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
016800 01  DLI-IO-WDD905.                                                       
016900*    03  -COPY WDD905                                                     
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD906'.                      
017100 01  DLI-IO-WDD906.                                                       
017200*    03  -COPY WDD906                                                     
017300                                                                          
017400     EJECT                                                                
017500 LINKAGE SECTION.                                                         
017600                                                                          
017700*01  -COPY W0009   -PRE MSG-                                              
017800                                                                          
017900*01  -COPY W0008  -PRE WDD4-                                              
018000     05  FILLER                  PIC X.                                   
018100                                                                          
018200*01  -COPY W0008  -PRE WDD9-                                              
018300     05  FILLER                  PIC X.                                   
018400                                                                          
018500*                                                                         
018600 PROCEDURE DIVISION  USING MSG-PCB WDD4-PCB WDD9-PCB.                     
018700 MAIN SECTION.                                                            
018800     ENTRY 'DLITCBL' USING MSG-PCB WDD4-PCB WDD9-PCB.                     
018900                                                                          
019000     SKIP2                                                                
019100     PERFORM A-INIT                                                       
019200     PERFORM IMS-GHU-WDD401                                               
019300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
019400       PERFORM C-OMVANDLA-DATUM                                           
019500       MOVE LAK-IDARTNR       TO W-IDARTNR                                
019600       MOVE LAK-IDDC          TO W-IDDC                                   
019700       MOVE LAK-IDLEVNR       TO W-IDLEVNR                                
019800       MOVE 401-AAAAVVD       TO WS-AAAAVVD                               
019900       MOVE WS-AAAAVV         TO W-DAAVROP                                
020000       MOVE WS-TILEVDAG       TO W-TILEVDAG                               
020100       IF W-IDARTNR = 1058                                                
020200          MOVE 4              TO W-TILEVDAG                               
020300       END-IF                                                             
020400       PERFORM IMS-GU-WDD905                                              
020500       IF SEGMENT-FINNS                                                   
020600         PERFORM D-KOLL-WDD906                                            
020700         PERFORM IMS-REPL-WDD401                                          
020800         ADD +1 TO CHKP-ANT                                               
020900       ELSE                                                               
021000         PERFORM IMS-DLET-WDD401                                          
021100         PERFORM E-PREPARE-ALARM-LOG                                      
021200         ADD +1 TO CHKP-ANT                                               
021300       END-IF                                                             
021400       IF CHKP-ANT = CHKP-MAX                                             
021500          PERFORM X-TAG-CHECKPOINT                                        
021600       END-IF                                                             
021700       PERFORM IMS-GHN-WDD401                                             
021800     END-PERFORM                                                          
021900                                                                          
022000     PERFORM Z-FINIT                                                      
022100                                                                          
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT SECTION.                                                          
022700     SKIP2                                                                
022800     OPEN OUTPUT W22105                                                   
022900*                                                                         
023000     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
023100*                                                                         
023200     MOVE FUNCTION CURRENT-DATE (1:8) TO  W1-DAREGDAT                     
023300     COMPUTE W1-DAREGDAT-9KOMPL = 99999999 -                              
023400                                          W1-DAREGDAT                     
023500     PERFORM IMS-RESTART                                                  
023600     .                                                                    
023700     EJECT                                                                
023800 C-OMVANDLA-DATUM SECTION.                                                
023900                                                                          
024000                                                                          
024100     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
024200     MOVE LAK-TIAAMMDD-AVS  TO DAT-I-TIDATUM                              
024300                                                                          
024400     CALL WDATKONV USING DAT-KDDATFORM                                    
024500                         DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR           
024600                                                                          
024700     IF DAT-KDSVAR-OK                                                     
024800        MOVE DAT-TIAAVVD TO 401-AAVVD                                     
024900        MOVE 20          TO 401-SEKEL                                     
025000     ELSE                                                                 
025100        MOVE 26 TO RKOD-ABEND                                             
025200        PERFORM S99-ABEND                                                 
025300     END-IF                                                               
025400*                                                                         
025500     ACCEPT DAGENS-DATUM     FROM DATE                                    
025600     MOVE DAGENS-DATUM         TO DAT-I-TIDATUM                           
025700     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
025800     CALL WDATKONV          USING DAT-KDDATFORM                           
025900                                  DAT-I-TIDATUM                           
026000                                  DAT-O-TIDATUM                           
026100                                  DAT-KDSVAR                              
026200     MOVE DAT-TIAAVV-GRP       TO DAGENS-AAVV                             
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 D-KOLL-WDD906 SECTION.                                                   
026700                                                                          
026800     MOVE ZERO                TO SUM-KVAVROP-AVB                          
026900     PERFORM IMS-GNP-WDD906                                               
027000     PERFORM UNTIL SEGMENT-SAKNAS                                         
027100*         SUMMERING AV AVBOKAT ANTAL FÖR DAAVROP-AVS                      
027200       ADD KVAVROP-AVB     TO SUM-KVAVROP-AVB                             
027300       PERFORM IMS-GNP-WDD906                                             
027400     END-PERFORM                                                          
027500                                                                          
027600*       (KVAVROP + SUM-KVAVROP-AVB))                                      
027700*       JUSTERING AV LARM 225                                             
027800     MOVE SUM-KVAVROP-AVB    TO LAK-KVAVIS                                
027900     MOVE KVAVROP            TO LAK-KVAVROP                               
028000     ADD  SUM-KVAVROP-AVB    TO LAK-KVAVROP                               
028100     .                                                                    
028200     EJECT                                                                
028300 E-PREPARE-ALARM-LOG SECTION.                                             
028400                                                                          
028500***  SAVE ALARM IN LOG FILE IF ALARM CREATED IN CURRENT WEEK              
028600     MOVE LAK-IDDC          TO WS-IDDC                                    
028700     COMPUTE WS-DAREGDAT     = 99999999 - LAK-DAREGDAT-9KOMPL             
028800                                                                          
028900     MOVE WS-TIREGDAT       TO DAT-I-TIDATUM                              
029000                                                                          
029100     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
029200     CALL WDATKONV       USING DAT-KDDATFORM                              
029300                               DAT-I-TIDATUM                              
029400                               DAT-O-TIDATUM                              
029500                               DAT-KDSVAR                                 
029600     IF DAT-KDSVAR = ' '                                                  
029700        MOVE DAT-TIAAVV-GRP TO WS-ALARM-AAVV                              
029800        IF  CDC-SE                                                        
029900        AND WS-ALARM-AAVV    = DAGENS-AAVV                                
030000           PERFORM EA-LOG-ALARM                                           
030100        END-IF                                                            
030200     END-IF                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 EA-LOG-ALARM SECTION.                                                    
030600                                                                          
030700     MOVE 'WDD4'            TO UT1-ALOG-IDSYSTEM                          
030800     MOVE ZERO              TO UT1-ALOG-TIAAVVD                           
030900     MOVE LAK-KDLARM        TO UT1-ALOG-KDLARM                            
031000     MOVE LAK-IDARTNR       TO UT1-ALOG-IDARTNR                           
031100     MOVE LAK-IDANSK        TO UT1-ALOG-IDANSK                            
031200     MOVE LAK-IDLEVNR       TO UT1-ALOG-IDLEVNR                           
031300     MOVE ZERO              TO UT1-ALOG-IDDISTR                           
031400     MOVE WS-TIREGDAT       TO UT1-ALOG-TIREGDAT                          
031500     MOVE LAK-TIAAMMDD      TO UT1-ALOG-TIPLANDAT                         
031600     MOVE LAK-KVAVIS        TO UT1-ALOG-KVAVIS                            
031700     MOVE LAK-KVAVROP       TO UT1-ALOG-KVAVROP                           
031800*                                                                         
031900     PERFORM S01-SKRIV-W22105                                             
032000     .                                                                    
032100     EJECT                                                                
032200 Z-FINIT SECTION.                                                         
032300                                                                          
032400     CLOSE W22105                                                         
032500                                                                          
032600     MOVE 'S' TO POSTSUM-OPKOD                                            
032700     CALL POSTSUM USING POSTSUM-PARM                                      
032800     .                                                                    
032900     EJECT                                                                
033000 S01-SKRIV-W22105 SECTION.                                                
033100                                                                          
033200     WRITE UT1-POST FROM UT1-AREA                                         
033300                                                                          
033400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
033500     MOVE 'W22105'   TO POSTSUM-FDNAMN                                    
033600     MOVE 'W22105D1' TO POSTSUM-DDNAMN2                                   
033700     CALL POSTSUM USING POSTSUM-PARM                                      
033800     .                                                                    
033900     SKIP3                                                                
034000 X-TAG-CHECKPOINT   SECTION.                                              
034100                                                                          
034200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
034300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
034400     PERFORM IMS-CHECKPOINT                                               
034500     MOVE ZERO TO CHKP-ANT                                                
034600* --- LÄS OM DATABAS OM DET BEHÖVS                                        
034700     MOVE LAK-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                        
034800     MOVE LAK-TIKLOCK-9KOMPL  TO W-TIKLOCK-9KOMPL                         
034900     EJECT                                                                
035000* --- IMS SEKTIONER ---                                                   
035100     .                                                                    
035200     EJECT                                                                
035300 S99-ABEND SECTION.                                                       
035400                                                                          
035500     SKIP2                                                                
035600     MOVE 'S' TO POSTSUM-OPKOD                                            
035700     CALL POSTSUM USING POSTSUM-PARM                                      
035800     CALL ABEND USING RKOD-ABEND                                          
035900     .                                                                    
036000     EJECT                                                                
036100 IMS-GHU-WDD401 SECTION.                                                  
036200                                                                          
036300     STRING 'WDD401  (WDD401KY>=' W-WDD401KY-MIN-X                        
036400                    '&WDD401KY<=' W-WDD401KY-MAX                          
036500                    '&KDLARM   =' W-KDLARM-X ')'                          
036600          DELIMITED BY SIZE INTO SSA1                                     
036700     MOVE '  GE' TO GODK-STATUSKODER                                      
036800     CALL CBLTDLI USING GHU WDD4-PCB DLI-IO-WDD401 SSA1                   
036900     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
037000     PERFORM IMS-STATUSKONTROLL                                           
037100     .                                                                    
037200     SKIP3                                                                
037300 IMS-GHN-WDD401 SECTION.                                                  
037400                                                                          
037500     STRING 'WDD401  (WDD401KY>=' W-WDD401KY-MIN-X                        
037600                    '&WDD401KY<=' W-WDD401KY-MAX                          
037700                    '&KDLARM   =' W-KDLARM-X ')'                          
037800          DELIMITED BY SIZE INTO SSA1                                     
037900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
038000     CALL CBLTDLI USING GHN WDD4-PCB DLI-IO-WDD401 SSA1                   
038100     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     EJECT                                                                
038500 IMS-REPL-WDD401 SECTION.                                                 
038600                                                                          
038700     MOVE '  ' TO GODK-STATUSKODER                                        
038800     CALL CBLTDLI USING REPL WDD4-PCB DLI-IO-WDD401                       
038900     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
039000     PERFORM IMS-STATUSKONTROLL                                           
039100     .                                                                    
039200     SKIP3                                                                
039300 IMS-DLET-WDD401 SECTION.                                                 
039400                                                                          
039500     MOVE '  ' TO GODK-STATUSKODER                                        
039600     CALL CBLTDLI USING DLET WDD4-PCB DLI-IO-WDD401                       
039700     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040000     EJECT                                                                
040100 IMS-GU-WDD905 SECTION.                                                   
040200                                                                          
040300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
040400          DELIMITED BY SIZE INTO SSA1                                     
040500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
040600          DELIMITED BY SIZE INTO SSA2                                     
040700     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
040800                    '&KDAVROP  =' W-KDAVROP-X ')'                         
040900          DELIMITED BY SIZE INTO SSA3                                     
041000     MOVE '  GE' TO GODK-STATUSKODER                                      
041100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1                    
041200                                                  SSA2                    
041300                                                  SSA3                    
041400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
041500     PERFORM IMS-STATUSKONTROLL                                           
041600     .                                                                    
041700     EJECT                                                                
041800 IMS-GNP-WDD906 SECTION.                                                  
041900                                                                          
042000     MOVE 'WDD906' TO SSA1                                                
042100     MOVE '  GE' TO GODK-STATUSKODER                                      
042200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD906 SSA1                   
042300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
042400     PERFORM IMS-STATUSKONTROLL                                           
042500     .                                                                    
042600     EJECT                                                                
042700 IMS-RESTART SECTION.                                                     
042800     SKIP2                                                                
042900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
043000     MOVE '  ' TO GODK-STATUSKODER                                        
043100     CALL CBLTDLI USING XRST MSG-PCB                                      
043200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
043300                        CHKP-AREA-LENGTH CHKP-AREA                        
043400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700     SKIP3                                                                
043800 IMS-CHECKPOINT SECTION.                                                  
043900     SKIP2                                                                
044000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
044100     MOVE '  XD' TO GODK-STATUSKODER                                      
044200     CALL CBLTDLI USING CHKP MSG-PCB                                      
044300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
044400                        CHKP-AREA-LENGTH CHKP-AREA                        
044500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044600     PERFORM IMS-STATUSKONTROLL                                           
044700                                                                          
044800     IF IMS-EJ-OK                                                         
044900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
045000       DISPLAY FELTEXT                                                    
045100       CALL FELLOG                                                        
045200     END-IF                                                               
045300     .                                                                    
045400     EJECT                                                                
045500 IMS-STATUSKONTROLL SECTION.                                              
045600     SKIP2                                                                
045700     SET STATUS-IX TO 1                                                   
045800     SEARCH GODK-STATUS                                                   
045900       AT END                                                             
046000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046100           DELIMITED BY SIZE INTO FELTEXT                                 
046200         DISPLAY FELTEXT                                                  
046300         CALL FELLOG                                                      
046400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046500         CONTINUE                                                         
046600     END-SEARCH                                                           
046700     .                                                                    
