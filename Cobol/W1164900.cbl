000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1164900.                                                
000400 AUTHOR.         KENT JEBSEN.                                             
000500 DATE-WRITTEN.   97/10/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*    HÄMTAR ERSÄTTNINGSINFO FRÅN WDD7 OCH SKAPAR LAYOUT TILL VIPS.        
001100*    FÖR TOTALFILER ÖVRIGA VÄRLDEN. (EJ NDC).                             
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- ERSATTA ARTIKLAR                                           
002800     SELECT W11648                     ASSIGN TO W11649D1.                
002900     SKIP2                                                                
003000*          --- ERSATTA ARTIKLAR FÖR BEVAKNING AV ERS-MEDDELANDEN          
003100     SELECT W11649                     ASSIGN TO W11649D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W11648                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W11648      -L.                                                
004200     SKIP3                                                                
004300 FD  W11649                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W11646 -PRE  W11649-  -L.                                 
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W1164900'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
005700 77  WS-FL-TYP1                  PIC X.                                   
005800 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
005900     SKIP2                                                                
006000 01 WS-KDERS-NUM                PIC 9(2).                                 
006100 01 WS-KDERS                             REDEFINES WS-KDERS-NUM.          
006200     03 WS-KDERSPOS1            PIC X.                                    
006300     03 WS-KDERSPOS2            PIC X.                                    
006400                                                                          
006500 01  WS-IDARTNR.                                                          
006600     03 WS-IDARTNR-BLANK         PIC X(11) VALUE SPACE.                   
006700     03 WS-IDARTNR-ALFA          PIC X(9).                                
006800                                                                          
006900 01  WS-IDARTNR-TILLK.                                                    
007000     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
007100     03 WS-IDARTNR-TILLK-ALFA    PIC X(9).                                
007200                                                                          
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600                                                                          
007700 77  W11648-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W11648                       VALUE 'J'.                   
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     EJECT                                                                
008700*    ---- PARAMETRAR TILL WDATKONV                                        
008800 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
008900*01  -COPY WDATAREA                                                       
009000*    --- PARAMETRAR TILL POSTSUM                                          
009100*                                                                         
009200*01  -COPY W0005   -PRE  POSTSUM-                                         
009300     EJECT                                                                
009400 01  IN-AREA-START               PIC X(24)   VALUE                        
009500                                             'IN-AREA-START'.             
009600     SKIP2                                                                
009700                                                                          
009800*01  AREA -COPY W11648     -PRE IN-                                       
009900     EJECT                                                                
010000 01  UT-AREA-START              PIC X(24)   VALUE                         
010100                                             'UT-AREA-START'.             
010200     SKIP2                                                                
010300                                                                          
010400*01  AREA -COPY W11646     -PRE UT-                                       
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011800     88  IMS-EJ-OK                           VALUE 'XD'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000                                                                          
013100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA01'.                    
013200 01  DLI-IO-WLERSA01.                                                     
013300*    03  -COPY WDD701  -PRE ERSA-                                         
013400     EJECT                                                                
013500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA11'.                    
013600 01  DLI-IO-WLERSA11.                                                     
013700*    03  -COPY WDD702  -PRE ERSA-                                         
013800     EJECT                                                                
013900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA13'.                    
014000 01  DLI-IO-WLERSA13.                                                     
014100*    03  -COPY WDD704  -PRE ERSA-                                         
014200     EJECT                                                                
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500*01  -COPY W0008   -PRE ERSA-                                             
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING  ERSA-PCB.                                     
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING  ERSA-PCB.                                     
015100                                                                          
015200     SKIP2                                                                
015300     PERFORM A-INIT                                                       
015400     PERFORM S01-LAES-W11648                                              
015500                                                                          
015600     PERFORM UNTIL END-OF-W11648                                          
015700       PERFORM B-BEARBETA                                                 
015800       PERFORM S01-LAES-W11648                                            
015900     END-PERFORM                                                          
016000                                                                          
016100     PERFORM Z-FINIT                                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     OPEN INPUT  W11648                                                   
017000                                                                          
017100     OPEN OUTPUT W11649                                                   
017200                                                                          
017300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017400     .                                                                    
017500     EJECT                                                                
017600 B-BEARBETA SECTION.                                                      
017700                                                                          
017800     MOVE IN-IDARTNR      TO W-IDARTNR                                    
017900     MOVE IN-KDERS        TO WS-KDERS-NUM                                 
018000                                                                          
018100     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
018200     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
018300     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
018400     MOVE WS-IDARTNR      TO UT-IDARTNR20                                 
018500                                                                          
018600     MOVE IN-TIERSDAT     TO DAT-I-TIDATUM                                
018700     MOVE 'AAVVD'         TO DAT-KDDATFORM                                
018800                                                                          
018900     PERFORM S02-WDATKONV                                                 
019000                                                                          
019100     IF DAT-KDSVAR-OK                                                     
019200       MOVE DAT-TIAAMMDD  TO UT-TIERSDAT-002                              
019300     ELSE                                                                 
019400       MOVE 0             TO UT-TIERSDAT-002                              
019500     END-IF                                                               
019600                                                                          
019700     MOVE IN-KDPRODSL     TO UT-KDPRODSL                                  
019800                                                                          
019900     MOVE 'N'             TO UT-FLDEL                                     
020000     MOVE IN-KDERS        TO UT-KDERS                                     
020100     IF WS-KDERSPOS2 = '2' OR '5'                                         
020200       MOVE 2  TO UT-KDARTUTG                                             
020300     ELSE                                                                 
020400       MOVE 1  TO UT-KDARTUTG                                             
020500     END-IF                                                               
020600                                                                          
020610     IF  UT-KDERS > 20                                                    
020700     AND IN-FLERSATT = 'N'                                                
020710       SUBTRACT 10        FROM UT-KDERS                                   
020720     END-IF                                                               
020800     IF UT-KDERS > 20                                                     
020900       MOVE 'F'      TO UT-KDUTGSTA                                       
021000     ELSE                                                                 
021100       MOVE 'P'      TO UT-KDUTGSTA                                       
021200     END-IF                                                               
021300                                                                          
021400     PERFORM IMS-GU-ERSA-WLERSA                                           
021500     IF SEGMENT-FINNS                                                     
021600       IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                         
021700         IF ERSA-KVKORT = 1                                               
021800           MOVE 'S'   TO UT-KDUTGSTR                                      
021900         ELSE                                                             
022000           MOVE 'M'   TO UT-KDUTGSTR                                      
022100         END-IF                                                           
022200       ELSE                                                               
022300         IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                       
022400           MOVE 'V'   TO UT-KDUTGSTR                                      
022500         END-IF                                                           
022600       END-IF                                                             
022700                                                                          
022800       PERFORM IMS-GET-ERSA-ERSA13                                        
022900       IF IN-KDERS > 20                                                   
023000          CONTINUE                                                        
023100       ELSE                                                               
023200          IF IN-KDERS > 10 AND < 20                                       
023300             MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM                  
023400             MOVE 'AAVVD' TO DAT-KDDATFORM                                
023500             PERFORM S02-WDATKONV                                         
023600             IF DAT-KDSVAR-OK                                             
023700                MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                      
023800             ELSE                                                         
023900                MOVE 0            TO UT-TIERSDAT-002                      
024000             END-IF                                                       
024100          ELSE                                                            
024200             IF IN-KDERS > 0 AND < 10                                     
024300                MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM                   
024400                MOVE 'AAVVD'      TO DAT-KDDATFORM                        
024500                PERFORM S02-WDATKONV                                      
024600                IF DAT-KDSVAR-OK                                          
024700                   MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                   
024800                ELSE                                                      
024900                   MOVE 0            TO UT-TIERSDAT-002                   
025000                END-IF                                                    
025100             END-IF                                                       
025200          END-IF                                                          
025300       END-IF                                                             
025400                                                                          
025500       IF IN-KDERS = 29 OR 52 OR 09 OR 19                                 
025600          MOVE 0        TO UT-IDKORTNR                                    
025700          MOVE 0        TO UT-KDTEXTGR                                    
025800          MOVE 'N'      TO UT-FLTEXT                                      
025900          MOVE SPACE    TO UT-IDARTNR20-TILLK                             
026000          MOVE 0        TO UT-DIERS-TILLK                                 
026100          MOVE SPACE    TO UT-KDUTGSTR                                    
026200          PERFORM S12-SKRIV-W11649                                        
026300       END-IF                                                             
026400                                                                          
026500       PERFORM IMS-GET-ERSA-ERSA11-FIRST                                  
026600                                                                          
026700       IF SEGMENT-FINNS                                                   
026800         MOVE 1     TO WS-KDTEXTGR-RAKNARE                                
026900         MOVE 'N'   TO WS-FL-TYP1                                         
027000                                                                          
027100         PERFORM UNTIL SEGMENT-SAKNAS                                     
027200                                                                          
027300           MOVE ERSA-IDKORTNR   TO UT-IDKORTNR                            
027400                                                                          
027500           IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'              
027600             MOVE 0        TO UT-KDTEXTGR                                 
027700           ELSE                                                           
027800             IF ERSA-FLTEXT = 'N'                                         
027900               MOVE 'J'  TO WS-FL-TYP1                                    
028000             END-IF                                                       
028100             IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'                    
028200               ADD  +1   TO WS-KDTEXTGR-RAKNARE                           
028300               MOVE 'N'  TO WS-FL-TYP1                                    
028400             END-IF                                                       
028500             MOVE WS-KDTEXTGR-RAKNARE TO UT-KDTEXTGR                      
028600           END-IF                                                         
028700                                                                          
028800           MOVE ERSA-FLTEXT     TO UT-FLTEXT                              
028900           IF ERSA-FLTEXT = 'N'                                           
029000             MOVE ERSA-IDARTNR-TILLK  TO WS-IDARTNR-NUM                   
029100             MOVE WS-IDARTNR-NUM      TO WS-IDARTNR-TILLK-ALFA            
029200             INSPECT WS-IDARTNR-TILLK-ALFA                                
029300                                 REPLACING LEADING ZERO BY SPACE          
029400             MOVE WS-IDARTNR-TILLK     TO UT-IDARTNR20-TILLK              
029500             MOVE ERSA-DIERS-TILLK     TO UT-DIERS-TILLK                  
029600           ELSE                                                           
029700             IF ERSA-FLTEXT = 'J'                                         
029800               MOVE ERSA-BEERS  TO UT-IDARTNR20-TILLK                     
029900               MOVE  0          TO UT-DIERS-TILLK                         
030000             END-IF                                                       
030100           END-IF                                                         
030200                                                                          
030300           PERFORM S12-SKRIV-W11649                                       
030400                                                                          
030500           PERFORM IMS-GNP-ERSA-ERSA11                                    
030600                                                                          
030700         END-PERFORM                                                      
030800       END-IF                                                             
030900     ELSE                                                                 
031000                                                                          
031100       IF IN-KDERS = 29 OR 52                                             
031200         MOVE 0        TO UT-IDKORTNR                                     
031300         MOVE 0        TO UT-KDTEXTGR                                     
031400         MOVE 'N'      TO UT-FLTEXT                                       
031500         MOVE SPACE    TO UT-IDARTNR20-TILLK                              
031600         MOVE 0        TO UT-DIERS-TILLK                                  
031700         MOVE SPACE    TO UT-KDUTGSTR                                     
031800                                                                          
031900         PERFORM S12-SKRIV-W11649                                         
032000                                                                          
032100       END-IF                                                             
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 Z-FINIT SECTION.                                                         
032600                                                                          
032700     CLOSE W11648                                                         
032800           W11649                                                         
032900     MOVE 'S'     TO POSTSUM-OPKOD                                        
033000     CALL POSTSUM USING POSTSUM-PARM                                      
033100     .                                                                    
033200     EJECT                                                                
033300 S01-LAES-W11648  SECTION.                                                
033400                                                                          
033500     READ W11648          INTO IN-AREA                                    
033600     AT END                                                               
033700        SET END-OF-W11648 TO TRUE                                         
033800                                                                          
033900     NOT AT END                                                           
034000        MOVE 'W11648'     TO    POSTSUM-FDNAMN                            
034100        MOVE 'W11649D1'   TO    POSTSUM-DDNAMN2                           
034200        CALL POSTSUM      USING POSTSUM-PARM                              
034300     END-READ                                                             
034400     .                                                                    
034500     EJECT                                                                
034600 S02-WDATKONV SECTION.                                                    
034700     SKIP2                                                                
034800     CALL WDATKONV USING DAT-KDDATFORM                                    
034900                         DAT-I-TIDATUM                                    
035000                         DAT-O-TIDATUM                                    
035100                         DAT-KDSVAR                                       
035200     .                                                                    
035300     EJECT                                                                
035400 S12-SKRIV-W11649 SECTION.                                                
035500                                                                          
035600     MOVE 'WB1'           TO UT-IDPTYP                                    
035700     WRITE W11649-POST    FROM UT-AREA                                    
035800                                                                          
035900     MOVE 'W11649 '       TO POSTSUM-FDNAMN                               
036000     MOVE 'W11649D2'      TO POSTSUM-DDNAMN2                              
036100     CALL POSTSUM         USING POSTSUM-PARM                              
036200     .                                                                    
036300     EJECT                                                                
036400* --- IMS SEKTIONER ---                                                   
036500 IMS-GU-ERSA-WLERSA SECTION.                                              
036600                                                                          
036700     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
036800          DELIMITED BY SIZE INTO SSA1                                     
036900     MOVE '  GE' TO GODK-STATUSKODER                                      
037000     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-WLERSA01 SSA1                  
037100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
037200     PERFORM IMS-STATUSKONTROLL                                           
037300     .                                                                    
037400     SKIP3                                                                
037500 IMS-GNP-ERSA-ERSA11 SECTION.                                             
037600                                                                          
037700     STRING 'WLERSA11 '                                                   
037800          DELIMITED BY SIZE INTO SSA1                                     
037900     MOVE '  GE' TO GODK-STATUSKODER                                      
038000     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
038100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     EJECT                                                                
038500 IMS-GET-ERSA-ERSA11-FIRST SECTION.                                       
038600                                                                          
038700     MOVE 'WLERSA11*F' TO SSA1                                            
038800     MOVE '  GE' TO GODK-STATUSKODER                                      
038900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
039000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
039100     PERFORM IMS-STATUSKONTROLL                                           
039200     .                                                                    
039300     SKIP3                                                                
039400 IMS-GET-ERSA-ERSA13 SECTION.                                             
039500                                                                          
039600     STRING 'WLERSA13 '                                                   
039700          DELIMITED BY SIZE INTO SSA1                                     
039800     MOVE '  GE' TO GODK-STATUSKODER                                      
039900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA13 SSA1                 
040000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
040100     PERFORM IMS-STATUSKONTROLL                                           
040200     .                                                                    
040300     SKIP3                                                                
040400 IMS-STATUSKONTROLL SECTION.                                              
040500     SKIP2                                                                
040600     SET STATUS-IX TO 1                                                   
040700     SEARCH GODK-STATUS                                                   
040800       AT END                                                             
040900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041000           DELIMITED BY SIZE INTO FELTEXT                                 
041100         DISPLAY FELTEXT                                                  
041200         CALL FELLOG                                                      
041300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041400         CONTINUE                                                         
041500     END-SEARCH                                                           
041600     .                                                                    
