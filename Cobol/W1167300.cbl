000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1167300.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   DECEMBER 1999.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR TOTALFIL ERSÄTTNINGAR USA                                 
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
002900     SELECT W11635                     ASSIGN TO W11673D1.                
003000*          --- TOTALFIL                                                   
003100     SELECT W11673                     ASSIGN TO W11673D2.                
003200*          --- USA ARTIKLAR                                               
003300     SELECT W01184                     ASSIGN TO W11673D3.                
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
004400 FD  W11673                                                               
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
005800 77  IDPGM                       PIC X(8)    VALUE 'W1167300'.            
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
007710*01  -COPY WWPRODSL                                                       
007720*                                                                         
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100                                                                          
008200 77  W11635-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W11635                       VALUE 'J'.                   
008400                                                                          
008500 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W01184                       VALUE 'J'.                   
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009400     EJECT                                                                
009500*    ---- PARAMETRAR TILL WDATKONV                                        
009600 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
009700*01  -COPY WDATAREA                                                       
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*                                                                         
010000*01  -COPY W0005   -PRE  POSTSUM-                                         
010100     EJECT                                                                
010200 01  IN-AREA-START               PIC X(24)   VALUE                        
010300                                             'IN-AREA-START'.             
010400*01  AREA -COPY W11635     -PRE IN-                                       
010500     EJECT                                                                
010600 01  SLAG-AREA-START               PIC X(24)   VALUE                      
010700                                             'SLAG-AREA-START'.           
010800*01  SLAG-AREA -COPY W01184                                               
010900     EJECT                                                                
011000 01  UT-AREA-START               PIC X(24)   VALUE                        
011100                                             'UT-AREA-START'.             
011200*01  AREA -COPY W11636     -PRE UT-                                       
011300     EJECT                                                                
011400 01  NYCKLAR-TILL-DLI.                                                    
011500     03  W-IDARTNR-X.                                                     
011600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012400     88  IMS-EJ-OK                           VALUE 'XD'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(64).                               
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
013800 01  DLI-IO-WLARTC01.                                                     
013900*    03  -COPY WDK601                                                     
014000     EJECT                                                                
014100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA01'.                    
014200 01  DLI-IO-WLERSA01.                                                     
014300*    03  -COPY WDD701  -PRE ERSA-                                         
014400     EJECT                                                                
014500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA11'.                    
014600 01  DLI-IO-WLERSA11.                                                     
014700*    03  -COPY WDD702  -PRE ERSA-                                         
014800     EJECT                                                                
014900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA13'.                    
015000 01  DLI-IO-WLERSA13.                                                     
015100*    03  -COPY WDD704  -PRE ERSA-                                         
015200     EJECT                                                                
015300 LINKAGE SECTION.                                                         
015400*                                                                         
015500*01  -COPY W0008  -PRE ARTC-                                              
015600     05  FILLER                  PIC X.                                   
015700     EJECT                                                                
015800*01  -COPY W0008  -PRE ERSA-                                              
015900     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016100 PROCEDURE DIVISION  USING ARTC-PCB ERSA-PCB.                             
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING ARTC-PCB ERSA-PCB.                             
016400                                                                          
016500     SKIP2                                                                
016600     PERFORM A-INIT                                                       
016700     PERFORM S01-LAES-W11635                                              
016800     PERFORM S04-LAES-W01184                                              
016900                                                                          
017000     PERFORM UNTIL END-OF-W11635 AND                                      
017100                   END-OF-W01184                                          
017200       IF IN-IDARTNR = SLAG-IDARTNR                                       
017300          IF IN-KDERS < 20                                                
017400             PERFORM B-BEARBETA                                           
017500          ELSE                                                            
017600            IF IN-KDERS > 20                                              
017700               IF IN-FLERSATT-USA = 'J'                                   
017800                  PERFORM B-BEARBETA                                      
017900               ELSE                                                       
018000                  IF IN-KDERS = 21 OR 24 OR 29                            
018100                     ADD -10 TO IN-KDERS                                  
018200                     PERFORM B-BEARBETA                                   
018300                  END-IF                                                  
018400               END-IF                                                     
018500            END-IF                                                        
018600          END-IF                                                          
018700          PERFORM S01-LAES-W11635                                         
018800          PERFORM S04-LAES-W01184                                         
018900       ELSE                                                               
019000          IF IN-IDARTNR > SLAG-IDARTNR                                    
019100             PERFORM S04-LAES-W01184                                      
019200          ELSE                                                            
019300             PERFORM S01-LAES-W11635                                      
019400          END-IF                                                          
019500       END-IF                                                             
019600     END-PERFORM                                                          
019700                                                                          
019800     PERFORM Z-FINIT                                                      
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT SECTION.                                                          
020400                                                                          
020500     OPEN INPUT  W11635                                                   
020600                 W01184                                                   
020700     OPEN OUTPUT W11673                                                   
020800                                                                          
020900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021000     .                                                                    
021100     EJECT                                                                
021200 B-BEARBETA SECTION.                                                      
021300                                                                          
021400     MOVE IN-KDERS        TO WS-KDERS-NUM                                 
021500     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
021600                             W-IDARTNR                                    
021700     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
021800     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
021900     MOVE WS-IDARTNR      TO UT-IDARTNR20                                 
022000                                                                          
022100     PERFORM IMS-GET-ARTC01                                               
022200     IF SEGMENT-FINNS                                                     
022300        MOVE ART-TIERSDAT TO DAT-I-TIDATUM                                
022400        MOVE 'AAVVD'      TO DAT-KDDATFORM                                
022500        PERFORM S02-WDATKONV                                              
022600                                                                          
022700        IF DAT-KDSVAR-OK                                                  
022800           MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                           
022900        ELSE                                                              
023000           MOVE 0            TO UT-TIERSDAT-002                           
023100        END-IF                                                            
023200        MOVE ART-KDPRODSL    TO UT-KDPRODSL                               
023300                                TEST-KDPRODSL                             
023400     ELSE                                                                 
023500        MOVE 0               TO UT-TIERSDAT-002                           
023600        MOVE 0               TO UT-KDPRODSL                               
023610                                TEST-KDPRODSL                             
023700     END-IF                                                               
023800                                                                          
023810     IF NOT KDPRODSL-LYNK                                                 
023900       MOVE 'N'      TO UT-FLDEL                                          
024000       MOVE IN-KDERS TO UT-KDERS                                          
024100       IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR               
024200                         '7' OR '8'                                       
024300          MOVE 1     TO UT-KDARTUTG                                       
024400       ELSE                                                               
024500          IF WS-KDERSPOS2 = '2' OR '5'                                    
024600             MOVE 2  TO UT-KDARTUTG                                       
024700          END-IF                                                          
024800       END-IF                                                             
024900       IF IN-KDERS > 20                                                   
025000         MOVE 'F'      TO UT-KDUTGSTA                                     
025100       ELSE                                                               
025200         MOVE 'P'      TO UT-KDUTGSTA                                     
025300       END-IF                                                             
025400                                                                          
025500       PERFORM IMS-GET-ERSA01                                             
025600       IF SEGMENT-FINNS                                                   
025700          IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                      
025800             IF ERSA-KVKORT = 1                                           
025900                MOVE 'S' TO UT-KDUTGSTR                                   
026000             ELSE                                                         
026100                MOVE 'M' TO UT-KDUTGSTR                                   
026200             END-IF                                                       
026300          ELSE                                                            
026400             IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                   
026500                MOVE 'V' TO UT-KDUTGSTR                                   
026600             END-IF                                                       
026700          END-IF                                                          
026800                                                                          
026900          PERFORM IMS-GET-ERSA13                                          
027000          IF SEGMENT-FINNS                                                
027100             IF IN-KDERS > 20                                             
027200                CONTINUE                                                  
027300             ELSE                                                         
027400                IF IN-KDERS > 10 AND < 20                                 
027500                   MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM            
027600                   MOVE 'AAVVD' TO DAT-KDDATFORM                          
027700                   PERFORM S02-WDATKONV                                   
027800                   IF DAT-KDSVAR-OK                                       
027900                      MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                
028000                   ELSE                                                   
028100                      MOVE 0            TO UT-TIERSDAT-002                
028200                   END-IF                                                 
028300                ELSE                                                      
028400                   IF IN-KDERS > 0 AND < 10                               
028500                      MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM             
028600                      MOVE 'AAVVD'      TO DAT-KDDATFORM                  
028700                      PERFORM S02-WDATKONV                                
028800                      IF DAT-KDSVAR-OK                                    
028900                         MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002             
029000                      ELSE                                                
029100                         MOVE 0            TO UT-TIERSDAT-002             
029200                      END-IF                                              
029300                   END-IF                                                 
029400                END-IF                                                    
029500             END-IF                                                       
029600          END-IF                                                          
029700                                                                          
029800          IF IN-KDERS = 29 OR 52 OR 09 OR 19                              
029900             MOVE 0        TO UT-IDKORTNR                                 
030000             MOVE 0        TO UT-KDTEXTGR                                 
030100             MOVE 'N'      TO UT-FLTEXT                                   
030200             MOVE SPACE    TO UT-IDARTNR20-TILLK                          
030300             MOVE 0        TO UT-DIERS-TILLK                              
030400             MOVE SPACE    TO UT-KDUTGSTR                                 
030500             PERFORM S03-SKRIV-W11673                                     
030600          ELSE                                                            
030700*****       PERFORM IMS-GET-ERSA11                                        
030800            PERFORM IMS-GET-ERSA11-FIRST                                  
030900            IF SEGMENT-FINNS                                              
031000              MOVE 1   TO WS-KDTEXTGR-RAKNARE                             
031100              MOVE 'N' TO WS-FL-TYP1                                      
031200                                                                          
031300              PERFORM UNTIL SEGMENT-SAKNAS                                
031400                                                                          
031500                MOVE ERSA-IDKORTNR TO UT-IDKORTNR                         
031600                                                                          
031700                IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'         
031800                   MOVE 0        TO UT-KDTEXTGR                           
031900                ELSE                                                      
032000                  IF ERSA-FLTEXT = 'N'                                    
032100                     MOVE 'J'  TO WS-FL-TYP1                              
032200                  END-IF                                                  
032300                  IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'               
032400                     ADD  +1   TO WS-KDTEXTGR-RAKNARE                     
032500                     MOVE 'N'  TO WS-FL-TYP1                              
032600                  END-IF                                                  
032700                  MOVE WS-KDTEXTGR-RAKNARE TO UT-KDTEXTGR                 
032800                END-IF                                                    
032900                                                                          
033000                MOVE ERSA-FLTEXT     TO UT-FLTEXT                         
033100                IF ERSA-FLTEXT = 'N'                                      
033200                   MOVE ERSA-IDARTNR-TILLK TO WS-IDARTNR-NUM              
033300                   MOVE WS-IDARTNR-NUM                                    
033400                              TO WS-IDARTNR-TILLK-ALFA                    
033500                   INSPECT WS-IDARTNR-TILLK-ALFA                          
033600                              REPLACING LEADING ZERO BY SPACE             
033700                   MOVE WS-IDARTNR-TILLK TO UT-IDARTNR20-TILLK            
033800                   MOVE ERSA-DIERS-TILLK TO UT-DIERS-TILLK                
033900                ELSE                                                      
034000                   IF ERSA-FLTEXT = 'J'                                   
034100                      MOVE ERSA-BEERS  TO UT-IDARTNR20-TILLK              
034200                      MOVE  0     TO UT-DIERS-TILLK                       
034300                    END-IF                                                
034400                END-IF                                                    
034500                                                                          
034600                PERFORM S03-SKRIV-W11673                                  
034700                PERFORM IMS-GET-ERSA11                                    
034800              END-PERFORM                                                 
034900            END-IF                                                        
035000          END-IF                                                          
035100       ELSE                                                               
035200          IF IN-KDERS = 29 OR 52                                          
035300             MOVE 0        TO UT-IDKORTNR                                 
035400             MOVE 0        TO UT-KDTEXTGR                                 
035500             MOVE 'N'      TO UT-FLTEXT                                   
035600             MOVE SPACE    TO UT-IDARTNR20-TILLK                          
035700             MOVE 0        TO UT-DIERS-TILLK                              
035800             MOVE SPACE    TO UT-KDUTGSTR                                 
035900                                                                          
036000             PERFORM S03-SKRIV-W11673                                     
036100          END-IF                                                          
036200       END-IF                                                             
036210     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 Z-FINIT SECTION.                                                         
036600                                                                          
036700     CLOSE W11635                                                         
036800           W11673                                                         
036900           W01184                                                         
037000     MOVE 'S'     TO POSTSUM-OPKOD                                        
037100     CALL POSTSUM USING POSTSUM-PARM                                      
037200     .                                                                    
037300     EJECT                                                                
037400 S01-LAES-W11635 SECTION.                                                 
037500                                                                          
037600     READ W11635 INTO IN-AREA                                             
037700     AT END                                                               
037800        SET END-OF-W11635 TO TRUE                                         
037900        MOVE 999999999 TO IN-IDARTNR                                      
038000                                                                          
038100     NOT AT END                                                           
038200        MOVE 'W11635'     TO    POSTSUM-FDNAMN                            
038300        MOVE 'W11673D1'   TO    POSTSUM-DDNAMN2                           
038400        CALL POSTSUM      USING POSTSUM-PARM                              
038500     END-READ                                                             
038600     .                                                                    
038700     EJECT                                                                
038800 S02-WDATKONV SECTION.                                                    
038900     SKIP2                                                                
039000     CALL WDATKONV USING DAT-KDDATFORM                                    
039100                         DAT-I-TIDATUM                                    
039200                         DAT-O-TIDATUM                                    
039300                         DAT-KDSVAR                                       
039400     .                                                                    
039500     EJECT                                                                
039600 S03-SKRIV-W11673 SECTION.                                                
039700                                                                          
039800     MOVE 'WB1'        TO UT-IDPTYP                                       
039900     WRITE UT-POST     FROM UT-AREA                                       
040000     MOVE 'W11673 '    TO POSTSUM-FDNAMN                                  
040100     MOVE 'W11673D2'   TO POSTSUM-DDNAMN2                                 
040200     CALL POSTSUM      USING POSTSUM-PARM                                 
040300     .                                                                    
040400     SKIP3                                                                
040500 S04-LAES-W01184 SECTION.                                                 
040600                                                                          
040700     READ W01184 INTO SLAG-AREA                                           
040800     AT END                                                               
040900        MOVE 999999999 TO SLAG-IDARTNR                                    
041000        SET END-OF-W01184 TO TRUE                                         
041100                                                                          
041200     NOT AT END                                                           
041300        MOVE 'W01184'     TO POSTSUM-FDNAMN                               
041400        MOVE 'W11673D3'   TO POSTSUM-DDNAMN2                              
041500        CALL POSTSUM USING POSTSUM-PARM                                   
041600     END-READ                                                             
041700     .                                                                    
041800     EJECT                                                                
041900* --- IMS SEKTIONER ---                                                   
042000     SKIP3                                                                
042100 IMS-GET-ARTC01 SECTION.                                                  
042200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
042300          DELIMITED BY SIZE INTO SSA1                                     
042400     MOVE '  GE' TO GODK-STATUSKODER                                      
042500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
042600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
042700     PERFORM IMS-STATUSKONTROLL                                           
042800     .                                                                    
042900     SKIP3                                                                
043000 IMS-GET-ERSA01 SECTION.                                                  
043100     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
043200          DELIMITED BY SIZE INTO SSA1                                     
043300     MOVE '  GE' TO GODK-STATUSKODER                                      
043400     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-WLERSA01 SSA1                  
043500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     SKIP3                                                                
043900 IMS-GET-ERSA11 SECTION.                                                  
044000     STRING 'WLERSA11 '                                                   
044100          DELIMITED BY SIZE INTO SSA1                                     
044200     MOVE '  GE' TO GODK-STATUSKODER                                      
044300     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
044400     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
044500     PERFORM IMS-STATUSKONTROLL                                           
044600     .                                                                    
044700     EJECT                                                                
044800 IMS-GET-ERSA13 SECTION.                                                  
044900     STRING 'WLERSA13 '                                                   
045000          DELIMITED BY SIZE INTO SSA1                                     
045100     MOVE '  GE' TO GODK-STATUSKODER                                      
045200     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA13 SSA1                 
045300     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     SKIP3                                                                
045700 IMS-GET-ERSA11-FIRST SECTION.                                            
045800     MOVE 'WLERSA11*F' TO SSA1                                            
045900     MOVE '  GE' TO GODK-STATUSKODER                                      
046000     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
046100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-STATUSKONTROLL SECTION.                                              
046600     SKIP2                                                                
046700     SET STATUS-IX TO 1                                                   
046800     SEARCH GODK-STATUS                                                   
046900       AT END                                                             
047000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047100           DELIMITED BY SIZE INTO FELTEXT                                 
047200         DISPLAY FELTEXT                                                  
047300         CALL FELLOG                                                      
047400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047500         CONTINUE                                                         
047600     END-SEARCH                                                           
047700     .                                                                    
