000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1167400.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   DECEMBER 1999.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR TOTALFIL ERSÄTTNINGAR CANADA                              
001100*        SOM SKICKAS TILL VIPS.                                           
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- ERSATTA ARTIKLAR                                           
002900     SELECT W11635                     ASSIGN TO W11674D1.                
003000*          --- TOTALFIL                                                   
003100     SELECT W11674                     ASSIGN TO W11674D2.                
003200*          --- CANADA ARTIKLAR                                            
003300     SELECT W01184                     ASSIGN TO W11674D3.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W11635                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200*01  POST -COPY W11635 -PRE  IN-      -L.                                 
004300                                                                          
004400 FD  W11674                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700*01  POST -COPY W11636 -PRE  UT-      -L.                                 
004800                                                                          
004900 FD  W01184                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200*01  POST -COPY W01184                -L.                                 
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500     SKIP2                                                                
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W1167400'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
006200 77  WS-FL-TYP1                  PIC X.                                   
006300 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
006400     SKIP2                                                                
006500 01  WS-KDERS-NUM                PIC 9(2).                                
006600 01  WS-KDERS REDEFINES WS-KDERS-NUM.                                     
006700     03 WS-KDERSPOS1             PIC X.                                   
006800     03 WS-KDERSPOS2             PIC X.                                   
006900                                                                          
007000 01  WS-IDARTNR.                                                          
007100     03 WS-IDARTNR-BLANK         PIC X(11) VALUE SPACE.                   
007200     03 WS-IDARTNR-ALFA          PIC X(9).                                
007300                                                                          
007400 01  WS-IDARTNR-TILLK.                                                    
007500     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
007600     03 WS-IDARTNR-TILLK-ALFA    PIC X(9).                                
007700                                                                          
007800*01  -COPY WWPRODSL                                                       
007900*                                                                         
008000 01  FELTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300                                                                          
008400 77  W11635-EOF-SW               PIC X       VALUE 'N'.                   
008500     88  END-OF-W11635                       VALUE 'J'.                   
008600                                                                          
008700 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
008800     88  END-OF-W01184                       VALUE 'J'.                   
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     EJECT                                                                
009700*    ---- PARAMETRAR TILL WDATKONV                                        
009800 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
009900*01  -COPY WDATAREA                                                       
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100     EJECT                                                                
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400 01  IN-AREA-START               PIC X(24)   VALUE                        
010500                                             'IN-AREA-START'.             
010600*01  AREA -COPY W11635     -PRE IN-                                       
010700     EJECT                                                                
010800 01  W01184-AREA-START           PIC X(24)   VALUE                        
010900                                             'SLAG-AREA-START'.           
011000*01  SLAG-AREA -COPY W01184                                               
011100     EJECT                                                                
011200 01  UT-AREA-START               PIC X(24)   VALUE                        
011300                                             'UT-AREA-START'.             
011400*01  AREA -COPY W11636     -PRE UT-                                       
011500     EJECT                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012600     88  IMS-EJ-OK                           VALUE 'XD'.                  
012700     SKIP2                                                                
012800 01  GODK-STATUSKODER.                                                    
012900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     SKIP3                                                                
013100 01  SSA1                        PIC X(64).                               
013200 01  SSA2                        PIC X(64).                               
013300     EJECT                                                                
013400*    --- IMS FUNKTIONSKODER                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800                                                                          
013900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
014000 01  DLI-IO-WLARTC01.                                                     
014100*    03  -COPY WDK601                                                     
014200     EJECT                                                                
014300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA01'.                    
014400 01  DLI-IO-WLERSA01.                                                     
014500*    03  -COPY WDD701  -PRE ERSA-                                         
014600     EJECT                                                                
014700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA11'.                    
014800 01  DLI-IO-WLERSA11.                                                     
014900*    03  -COPY WDD702  -PRE ERSA-                                         
015000     EJECT                                                                
015100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA13'.                    
015200 01  DLI-IO-WLERSA13.                                                     
015300*    03  -COPY WDD704  -PRE ERSA-                                         
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600*                                                                         
015700*01  -COPY W0008  -PRE ARTC-                                              
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000*01  -COPY W0008  -PRE ERSA-                                              
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING ARTC-PCB ERSA-PCB.                             
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING ARTC-PCB ERSA-PCB.                             
016600                                                                          
016700     PERFORM A-INIT                                                       
016800     PERFORM S01-LAES-W11635                                              
016900     PERFORM S04-LAES-W01184                                              
017000                                                                          
017100     PERFORM UNTIL END-OF-W11635 AND                                      
017200                   END-OF-W01184                                          
017300       IF IN-IDARTNR = SLAG-IDARTNR                                       
017400          IF IN-KDERS < 20                                                
017500            PERFORM B-BEARBETA                                            
017600          ELSE                                                            
017700            IF IN-KDERS > 20                                              
017800               IF IN-FLERSATT-CAN = 'J'                                   
017900                  PERFORM B-BEARBETA                                      
018000               ELSE                                                       
018100                  IF IN-KDERS = 21 OR 24 OR 29                            
018200                     ADD -10 TO IN-KDERS                                  
018300                     PERFORM B-BEARBETA                                   
018400                  END-IF                                                  
018500               END-IF                                                     
018600            END-IF                                                        
018700          END-IF                                                          
018800          PERFORM S01-LAES-W11635                                         
018900          PERFORM S04-LAES-W01184                                         
019000       ELSE                                                               
019100          IF IN-IDARTNR > SLAG-IDARTNR                                    
019200             PERFORM S04-LAES-W01184                                      
019300          ELSE                                                            
019400             PERFORM S01-LAES-W11635                                      
019500          END-IF                                                          
019600       END-IF                                                             
019700     END-PERFORM                                                          
019800                                                                          
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 A-INIT SECTION.                                                          
020600                                                                          
020700     OPEN INPUT  W11635                                                   
020800                 W01184                                                   
020900     OPEN OUTPUT W11674                                                   
021000                                                                          
021100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021200     .                                                                    
021300     EJECT                                                                
021400 B-BEARBETA SECTION.                                                      
021500                                                                          
021600     MOVE IN-KDERS        TO WS-KDERS-NUM                                 
021700     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
021800                             W-IDARTNR                                    
021900     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
022000     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
022100     MOVE WS-IDARTNR      TO UT-IDARTNR20                                 
022200                                                                          
022300     PERFORM IMS-GET-ARTC01                                               
022400     IF SEGMENT-FINNS                                                     
022500        MOVE ART-TIERSDAT TO DAT-I-TIDATUM                                
022600        MOVE 'AAVVD'      TO DAT-KDDATFORM                                
022700        PERFORM S02-WDATKONV                                              
022800                                                                          
022900        IF DAT-KDSVAR-OK                                                  
023000           MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                           
023100        ELSE                                                              
023200           MOVE 0            TO UT-TIERSDAT-002                           
023300        END-IF                                                            
023400        MOVE ART-KDPRODSL    TO UT-KDPRODSL                               
023500                                TEST-KDPRODSL                             
023600     ELSE                                                                 
023700        MOVE 0               TO UT-TIERSDAT-002                           
023800        MOVE 0               TO UT-KDPRODSL                               
023810                                TEST-KDPRODSL                             
023900     END-IF                                                               
023910                                                                          
024000     IF NOT KDPRODSL-LYNK                                                 
024100       MOVE 'N'      TO UT-FLDEL                                          
024200       MOVE IN-KDERS TO UT-KDERS                                          
024300       IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR               
024400                         '7' OR '8'                                       
024500          MOVE 1     TO UT-KDARTUTG                                       
024600       ELSE                                                               
024700          IF WS-KDERSPOS2 = '2' OR '5'                                    
024800             MOVE 2  TO UT-KDARTUTG                                       
024900          END-IF                                                          
025000       END-IF                                                             
025100       IF IN-KDERS > 20                                                   
025200          MOVE 'F'      TO UT-KDUTGSTA                                    
025300       ELSE                                                               
025400          MOVE 'P'      TO UT-KDUTGSTA                                    
025500       END-IF                                                             
025600                                                                          
025700       PERFORM IMS-GET-ERSA01                                             
025800       IF SEGMENT-FINNS                                                   
025900         IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                       
026000            IF ERSA-KVKORT = 1                                            
026100               MOVE 'S' TO UT-KDUTGSTR                                    
026200            ELSE                                                          
026300               MOVE 'M' TO UT-KDUTGSTR                                    
026400            END-IF                                                        
026500         ELSE                                                             
026600            IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                    
026700               MOVE 'V' TO UT-KDUTGSTR                                    
026800            END-IF                                                        
026900         END-IF                                                           
027000                                                                          
027100         PERFORM IMS-GET-ERSA13                                           
027200         IF SEGMENT-FINNS                                                 
027300            IF IN-KDERS > 20                                              
027400               CONTINUE                                                   
027500            ELSE                                                          
027600               IF IN-KDERS > 10 AND < 20                                  
027700                  MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM             
027800                  MOVE 'AAVVD' TO DAT-KDDATFORM                           
027900                  PERFORM S02-WDATKONV                                    
028000                  IF DAT-KDSVAR-OK                                        
028100                     MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                 
028200                  ELSE                                                    
028300                     MOVE 0            TO UT-TIERSDAT-002                 
028400                  END-IF                                                  
028500               ELSE                                                       
028600                  IF IN-KDERS > 0 AND < 10                                
028700                     MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM              
028800                     MOVE 'AAVVD'      TO DAT-KDDATFORM                   
028900                     PERFORM S02-WDATKONV                                 
029000                     IF DAT-KDSVAR-OK                                     
029100                        MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002              
029200                     ELSE                                                 
029300                        MOVE 0            TO UT-TIERSDAT-002              
029400                     END-IF                                               
029500                  END-IF                                                  
029600               END-IF                                                     
029700            END-IF                                                        
029800         END-IF                                                           
029900                                                                          
030000         IF IN-KDERS = 29 OR 52 OR 09 OR 19                               
030100            MOVE 0        TO UT-IDKORTNR                                  
030200            MOVE 0        TO UT-KDTEXTGR                                  
030300            MOVE 'N'      TO UT-FLTEXT                                    
030400            MOVE SPACE    TO UT-IDARTNR20-TILLK                           
030500            MOVE 0        TO UT-DIERS-TILLK                               
030600            MOVE SPACE    TO UT-KDUTGSTR                                  
030700            PERFORM S03-SKRIV-W11674                                      
030800         ELSE                                                             
030900*******     PERFORM IMS-GET-ERSA11                                        
031000            PERFORM IMS-GET-ERSA11-FIRST                                  
031100            IF SEGMENT-FINNS                                              
031200               MOVE 1   TO WS-KDTEXTGR-RAKNARE                            
031300               MOVE 'N' TO WS-FL-TYP1                                     
031400                                                                          
031500               PERFORM UNTIL SEGMENT-SAKNAS                               
031600                                                                          
031700                 MOVE ERSA-IDKORTNR TO UT-IDKORTNR                        
031800                                                                          
031900                 IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'        
032000                    MOVE 0        TO UT-KDTEXTGR                          
032100                 ELSE                                                     
032200                   IF ERSA-FLTEXT = 'N'                                   
032300                      MOVE 'J'  TO WS-FL-TYP1                             
032400                   END-IF                                                 
032500                   IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'              
032600                      ADD  +1   TO WS-KDTEXTGR-RAKNARE                    
032700                      MOVE 'N'  TO WS-FL-TYP1                             
032800                   END-IF                                                 
032900                   MOVE WS-KDTEXTGR-RAKNARE TO UT-KDTEXTGR                
033000                 END-IF                                                   
033100                                                                          
033200                 MOVE ERSA-FLTEXT     TO UT-FLTEXT                        
033300                 IF ERSA-FLTEXT = 'N'                                     
033400                    MOVE ERSA-IDARTNR-TILLK TO WS-IDARTNR-NUM             
033500                    MOVE WS-IDARTNR-NUM                                   
033600                                TO WS-IDARTNR-TILLK-ALFA                  
033700                    INSPECT WS-IDARTNR-TILLK-ALFA                         
033800                                REPLACING LEADING ZERO BY SPACE           
033900                    MOVE WS-IDARTNR-TILLK TO UT-IDARTNR20-TILLK           
034000                    MOVE ERSA-DIERS-TILLK TO UT-DIERS-TILLK               
034100                 ELSE                                                     
034200                    IF ERSA-FLTEXT = 'J'                                  
034300                       MOVE ERSA-BEERS  TO UT-IDARTNR20-TILLK             
034400                       MOVE  0     TO UT-DIERS-TILLK                      
034500                     END-IF                                               
034600                 END-IF                                                   
034700                                                                          
034800                 PERFORM S03-SKRIV-W11674                                 
034900                 PERFORM IMS-GET-ERSA11                                   
035000              END-PERFORM                                                 
035100           END-IF                                                         
035200         END-IF                                                           
035300       ELSE                                                               
035400          IF IN-KDERS = 29 OR 52                                          
035500             MOVE 0        TO UT-IDKORTNR                                 
035600             MOVE 0        TO UT-KDTEXTGR                                 
035700             MOVE 'N'      TO UT-FLTEXT                                   
035800             MOVE SPACE    TO UT-IDARTNR20-TILLK                          
035900             MOVE 0        TO UT-DIERS-TILLK                              
036000             MOVE SPACE    TO UT-KDUTGSTR                                 
036100                                                                          
036200             PERFORM S03-SKRIV-W11674                                     
036300          END-IF                                                          
036400       END-IF                                                             
036410     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 Z-FINIT SECTION.                                                         
036800                                                                          
036900     CLOSE W11635                                                         
037000           W01184                                                         
037100           W11674                                                         
037200     MOVE 'S'     TO POSTSUM-OPKOD                                        
037300     CALL POSTSUM USING POSTSUM-PARM                                      
037400     .                                                                    
037500     EJECT                                                                
037600 S01-LAES-W11635 SECTION.                                                 
037700                                                                          
037800     READ W11635 INTO IN-AREA                                             
037900     AT END                                                               
038000        MOVE 999999999 TO IN-IDARTNR                                      
038100        SET END-OF-W11635 TO TRUE                                         
038200                                                                          
038300     NOT AT END                                                           
038400        MOVE 'W11635'     TO    POSTSUM-FDNAMN                            
038500        MOVE 'W11674D1'   TO    POSTSUM-DDNAMN2                           
038600        CALL POSTSUM      USING POSTSUM-PARM                              
038700     END-READ                                                             
038800     .                                                                    
038900     EJECT                                                                
039000 S02-WDATKONV SECTION.                                                    
039100                                                                          
039200     CALL WDATKONV USING DAT-KDDATFORM                                    
039300                         DAT-I-TIDATUM                                    
039400                         DAT-O-TIDATUM                                    
039500                         DAT-KDSVAR                                       
039600     .                                                                    
039700     EJECT                                                                
039800 S03-SKRIV-W11674 SECTION.                                                
039900                                                                          
040000     MOVE 'WB1'        TO UT-IDPTYP                                       
040100     WRITE UT-POST     FROM UT-AREA                                       
040200     MOVE 'W11674 '    TO POSTSUM-FDNAMN                                  
040300     MOVE 'W11674D2'   TO POSTSUM-DDNAMN2                                 
040400     CALL POSTSUM      USING POSTSUM-PARM                                 
040500     .                                                                    
040600     EJECT                                                                
040700 S04-LAES-W01184 SECTION.                                                 
040800                                                                          
040900     READ W01184 INTO SLAG-AREA                                           
041000     AT END                                                               
041100        MOVE 999999999 TO SLAG-IDARTNR                                    
041200        SET END-OF-W01184 TO TRUE                                         
041300                                                                          
041400     NOT AT END                                                           
041500        MOVE 'W01184'     TO POSTSUM-FDNAMN                               
041600        MOVE 'W11674D3'   TO POSTSUM-DDNAMN2                              
041700        CALL POSTSUM USING POSTSUM-PARM                                   
041800     END-READ                                                             
041900     .                                                                    
042000     EJECT                                                                
042100* --- IMS SEKTIONER ---                                                   
042200     SKIP3                                                                
042300 IMS-GET-ARTC01 SECTION.                                                  
042400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
042500          DELIMITED BY SIZE INTO SSA1                                     
042600     MOVE '  GE' TO GODK-STATUSKODER                                      
042700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
042800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
042900     PERFORM IMS-STATUSKONTROLL                                           
043000     .                                                                    
043100     SKIP3                                                                
043200 IMS-GET-ERSA01 SECTION.                                                  
043300     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
043400          DELIMITED BY SIZE INTO SSA1                                     
043500     MOVE '  GE' TO GODK-STATUSKODER                                      
043600     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-WLERSA01 SSA1                  
043700     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
043800     PERFORM IMS-STATUSKONTROLL                                           
043900     .                                                                    
044000     SKIP3                                                                
044100 IMS-GET-ERSA11 SECTION.                                                  
044200     STRING 'WLERSA11 '                                                   
044300          DELIMITED BY SIZE INTO SSA1                                     
044400     MOVE '  GE' TO GODK-STATUSKODER                                      
044500     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
044600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     EJECT                                                                
045000 IMS-GET-ERSA11-FIRST SECTION.                                            
045100     MOVE 'WLERSA11*F' TO SSA1                                            
045200     MOVE '  GE' TO GODK-STATUSKODER                                      
045300     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
045400     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
045500     PERFORM IMS-STATUSKONTROLL                                           
045600     .                                                                    
045700     EJECT                                                                
045800 IMS-GET-ERSA13 SECTION.                                                  
045900     STRING 'WLERSA13 '                                                   
046000          DELIMITED BY SIZE INTO SSA1                                     
046100     MOVE '  GE' TO GODK-STATUSKODER                                      
046200     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA13 SSA1                 
046300     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
046400     PERFORM IMS-STATUSKONTROLL                                           
046500     .                                                                    
046600     SKIP3                                                                
046700 IMS-STATUSKONTROLL SECTION.                                              
046800     SKIP2                                                                
046900     SET STATUS-IX TO 1                                                   
047000     SEARCH GODK-STATUS                                                   
047100       AT END                                                             
047200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047300           DELIMITED BY SIZE INTO FELTEXT                                 
047400         DISPLAY FELTEXT                                                  
047500         CALL FELLOG                                                      
047600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047700         CONTINUE                                                         
047800     END-SEARCH                                                           
047900     .                                                                    
