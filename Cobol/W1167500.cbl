000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1167500.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   DECEMBER 1999.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR TOTALFIL ERSÄTTNINGAR NDC                                 
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
002700*          --- SYSIN COUNTRY CODE FROM VCOMPARM                           
002800     SELECT SYSIN                      ASSIGN TO SYSINPUT.                
002900*          --- ERSATTA ARTIKLAR                                           
003000     SELECT W11665                     ASSIGN TO W11675D1.                
003100*          --- TOTALFIL                                                   
003200     SELECT W11675                     ASSIGN TO W11675D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  SYSIN                                                                
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  PARM-VCOM                   PIC X(80).                               
004300                                                                          
004400 FD  W11665                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700*01  POST -COPY W11665A -PRE  IN-      -L.                                
004800                                                                          
004900 FD  W11675                                                               
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200*01  POST -COPY W11636 -PRE  UT-      -L.                                 
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500     SKIP2                                                                
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W1167500'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  WS-NDC-FINNS                PIC X       VALUE 'J'.                   
006200 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
006300 77  WS-FL-TYP1                  PIC X.                                   
006400 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
006500     SKIP2                                                                
006600 01  WS-KDERS-NUM                PIC 9(2).                                
006700 01  WS-KDERS REDEFINES WS-KDERS-NUM.                                     
006800     03 WS-KDERSPOS1             PIC X.                                   
006900     03 WS-KDERSPOS2             PIC X.                                   
007000                                                                          
007400 01  WS-IDARTNR.                                                          
007500     03 WS-IDARTNR-BLANK         PIC X(11) VALUE SPACE.                   
007600     03 WS-IDARTNR-ALFA          PIC X(9).                                
007700                                                                          
007800 01  WS-IDARTNR-TILLK.                                                    
007900     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
008000     03 WS-IDARTNR-TILLK-ALFA    PIC X(9).                                
008100                                                                          
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500                                                                          
008600 77  W11665-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W11665                       VALUE 'J'.                   
008800     EJECT                                                                
008900                                                                          
009200 01  WS-DAP-LINE1.                                                        
009300     03  FILLER                  PIC X(133)  VALUE                        
009400                                 '¤DAPSSLVIPS'.                           
009500 01  WS-DAP-LINE2.                                                        
009600     03  FILLER                  PIC X(4)    VALUE '¤DAP'.                
009700     03  WS-DAP-IDDC             PIC X(2)    VALUE SPACE.                 
009800     03  FILLER                  PIC X(127)  VALUE SPACE.                 
009900                                                                          
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100*                                                                         
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600     EJECT                                                                
010700*    ---- PARAMETRAR TILL WDATKONV                                        
010800 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
010900*01  -COPY WDATAREA                                                       
011000*    --- PARAMETRAR TILL POSTSUM                                          
011100*                                                                         
011200*01  -COPY W0005   -PRE  POSTSUM-                                         
011300     EJECT                                                                
011400*    --- VALID IDDC CODES                                                 
011500*01    -COPY WWDCKONS                                                     
011600       EJECT                                                              
011700*                                                                         
011800*01    -COPY WWPRODSL                                                     
011900                                                                          
011910*01    -COPY WWDCLAND                                                     
011920*                                                                         
011950     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'VCOM-PARM'.           
012100 01  PARM-SYSINPUT.                                                       
012200     05 FILLER                   PIC X(06).                               
012300     05 PARM-BESTLAND            PIC X(2).                                
012400     05 FILLER                   PIC X(72).                               
012500                                                                          
012600 01  IN-AREA-START               PIC X(24)   VALUE                        
012700                                             'IN-AREA-START'.             
012800*01  AREA -COPY W11665A     -PRE IN-                                      
012900     EJECT                                                                
013000 01  UT-AREA-START               PIC X(24)   VALUE                        
013100                                             'UT-AREA-START'.             
013200*01  AREA -COPY W11636     -PRE UT-                                       
013300     EJECT                                                                
013400 01  NYCKLAR-TILL-DLI.                                                    
013500     03  W-IDARTNR-X.                                                     
013600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013700     03  W-IDDC-X.                                                        
013800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013900     SKIP2                                                                
014000*    --- STATUS-KOD FRÅN IMS                                              
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FINNS                       VALUE '  '.                  
014300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014600     88  IMS-EJ-OK                           VALUE 'XD'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(64).                               
015200 01  SSA2                        PIC X(64).                               
015300     EJECT                                                                
015400*    --- IMS FUNKTIONSKODER                                               
015500*01  -COPY W0003                                                          
015600     EJECT                                                                
015700*    ---  DLI INPUT-OUTPUT AREA                                           
015800                                                                          
015900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
016000 01  DLI-IO-WLARTC01.                                                     
016100*    03  -COPY WDK601                                                     
016200     EJECT                                                                
016300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA01'.                    
016400 01  DLI-IO-WLERSA01.                                                     
016500*    03  -COPY WDD701  -PRE ERSA-                                         
016600     EJECT                                                                
016700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA11'.                    
016800 01  DLI-IO-WLERSA11.                                                     
016900*    03  -COPY WDD702  -PRE ERSA-                                         
017000     EJECT                                                                
017100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA13'.                    
017200 01  DLI-IO-WLERSA13.                                                     
017300*    03  -COPY WDD704  -PRE ERSA-                                         
017400     EJECT                                                                
017500 01  DLI-IO-WLARTS01.                                                     
017600*    03  -COPY WDK701  -PRE ARTS-                                         
017700     EJECT                                                                
017800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
017900 01  DLI-IO-WLARTS11.                                                     
018000*    03  -COPY WDK711  -PRE ARTS-                                         
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300*                                                                         
018400*01  -COPY W0008  -PRE ARTC-                                              
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700*01  -COPY W0008  -PRE ERSA-                                              
018800     05  FILLER                  PIC X.                                   
018900     EJECT                                                                
019000*01  -COPY W0008  -PRE ARTS-                                              
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300 PROCEDURE DIVISION  USING ARTC-PCB ERSA-PCB ARTS-PCB.                    
019400 MAIN SECTION.                                                            
019500     ENTRY 'DLITCBL' USING ARTC-PCB ERSA-PCB ARTS-PCB.                    
019600                                                                          
019800     PERFORM A-INIT                                                       
019810                                                                          
019900     PERFORM S05-READ-VCOM-PARM                                           
020000     PERFORM S01-LAES-W11665                                              
020100                                                                          
020200     PERFORM UNTIL END-OF-W11665                                          
020310       IF IN-IDLANDX2 = PARM-BESTLAND                                     
020400          IF IN-KDERS < 20                                                
020500             PERFORM B-BEARBETA                                           
020600          ELSE                                                            
020700             IF IN-KDERS > 20                                             
020800                IF IN-FLERSATT = 'J'                                      
020900                   PERFORM B-BEARBETA                                     
021000                ELSE                                                      
021100                   IF IN-KDERS = 21 OR 24 OR 29                           
021200                      ADD  -10 TO IN-KDERS                                
021300                      PERFORM B-BEARBETA                                  
021400                   END-IF                                                 
021500                END-IF                                                    
021600             END-IF                                                       
021700          END-IF                                                          
021800       END-IF                                                             
021900       PERFORM S01-LAES-W11665                                            
022000     END-PERFORM                                                          
022100                                                                          
022200     PERFORM Z-FINIT                                                      
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     OPEN INPUT  SYSIN                                                    
023100                 W11665                                                   
023200     OPEN OUTPUT W11675                                                   
023300                                                                          
023400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023500     .                                                                    
023600     EJECT                                                                
023700 B-BEARBETA SECTION.                                                      
023800                                                                          
023900     MOVE IN-KDERS        TO WS-KDERS-NUM                                 
024000     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
024100                             W-IDARTNR                                    
024200     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
024300     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
024400     MOVE WS-IDARTNR      TO UT-IDARTNR20                                 
024500                                                                          
024600     PERFORM IMS-GET-ARTC01                                               
024700     IF SEGMENT-FINNS                                                     
024800        MOVE ART-TIERSDAT TO DAT-I-TIDATUM                                
024900        MOVE 'AAVVD'      TO DAT-KDDATFORM                                
025000        PERFORM S02-WDATKONV                                              
025100                                                                          
025200        IF DAT-KDSVAR-OK                                                  
025300           MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                           
025400        ELSE                                                              
025500           MOVE 0            TO UT-TIERSDAT-002                           
025600        END-IF                                                            
025700        MOVE ART-KDPRODSL    TO UT-KDPRODSL                               
025800                                TEST-KDPRODSL                             
025900     ELSE                                                                 
026000        MOVE 0               TO UT-TIERSDAT-002                           
026100        MOVE 0               TO UT-KDPRODSL                               
026200                                TEST-KDPRODSL                             
026300     END-IF                                                               
026400                                                                          
026500     IF NOT KDPRODSL-LYNK                                                 
026600       MOVE 'J' TO WS-NDC-FINNS                                           
026700       MOVE ART-KDPRODSL           TO TEST-KDPRODSL                       
026800       IF KDPRODSL-LOCAL                                                  
026900          IF IN-FLERSATT = 'N'                                            
027000             PERFORM S04-KOLLA-FINNS                                      
027100          END-IF                                                          
027200       END-IF                                                             
027300                                                                          
027400       MOVE 'N'      TO UT-FLDEL                                          
027500       MOVE IN-KDERS TO UT-KDERS                                          
027600       IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR               
027700                         '7' OR '8'                                       
027800          MOVE 1     TO UT-KDARTUTG                                       
027900       ELSE                                                               
028000          IF WS-KDERSPOS2 = '2' OR '5'                                    
028100             MOVE 2  TO UT-KDARTUTG                                       
028200          END-IF                                                          
028300       END-IF                                                             
028400                                                                          
028500       IF IN-KDERS > 20                                                   
028600          MOVE 'F'      TO UT-KDUTGSTA                                    
028700       ELSE                                                               
028800          MOVE 'P'      TO UT-KDUTGSTA                                    
028900       END-IF                                                             
029000                                                                          
029100       PERFORM IMS-GET-ERSA01                                             
029200       IF SEGMENT-FINNS                                                   
029300         IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                       
029400            IF ERSA-KVKORT = 1                                            
029500               MOVE 'S' TO UT-KDUTGSTR                                    
029600            ELSE                                                          
029700               MOVE 'M' TO UT-KDUTGSTR                                    
029800            END-IF                                                        
029900         ELSE                                                             
030000            IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                    
030100               MOVE 'V' TO UT-KDUTGSTR                                    
030200            END-IF                                                        
030300         END-IF                                                           
030400                                                                          
030500         PERFORM IMS-GET-ERSA13                                           
030600         IF SEGMENT-FINNS                                                 
030700            IF IN-KDERS > 20                                              
030800               CONTINUE                                                   
030900            ELSE                                                          
031000               IF IN-KDERS > 10 AND < 20                                  
031100                  MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM             
031200                  MOVE 'AAVVD' TO DAT-KDDATFORM                           
031300                  PERFORM S02-WDATKONV                                    
031400                  IF DAT-KDSVAR-OK                                        
031500                     MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                 
031600                  ELSE                                                    
031700                     MOVE 0            TO UT-TIERSDAT-002                 
031800                  END-IF                                                  
031900               ELSE                                                       
032000                  IF IN-KDERS > 0 AND < 10                                
032100                     MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM              
032200                     MOVE 'AAVVD'      TO DAT-KDDATFORM                   
032300                     PERFORM S02-WDATKONV                                 
032400                     IF DAT-KDSVAR-OK                                     
032500                        MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002              
032600                     ELSE                                                 
032700                        MOVE 0            TO UT-TIERSDAT-002              
032800                     END-IF                                               
032900                  END-IF                                                  
033000               END-IF                                                     
033100            END-IF                                                        
033200         END-IF                                                           
033300                                                                          
033400         IF IN-KDERS = 29 OR 52 OR 09 OR 19                               
033500            MOVE 0        TO UT-IDKORTNR                                  
033600            MOVE 0        TO UT-KDTEXTGR                                  
033700            MOVE 'N'      TO UT-FLTEXT                                    
033800            MOVE SPACE    TO UT-IDARTNR20-TILLK                           
033900            MOVE 0        TO UT-DIERS-TILLK                               
034000            MOVE SPACE    TO UT-KDUTGSTR                                  
034100            IF WS-NDC-FINNS = JA                                          
034200               PERFORM S03-SKRIV-W11675                                   
034300            END-IF                                                        
034400         ELSE                                                             
034500            PERFORM IMS-GET-ERSA11-FIRST                                  
034600            IF SEGMENT-FINNS                                              
034700               MOVE 1   TO WS-KDTEXTGR-RAKNARE                            
034800               MOVE 'N' TO WS-FL-TYP1                                     
034900                                                                          
035000               PERFORM UNTIL SEGMENT-SAKNAS                               
035100                                                                          
035200                 MOVE ERSA-IDKORTNR TO UT-IDKORTNR                        
035300                                                                          
035400                 IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'        
035500                    MOVE 0        TO UT-KDTEXTGR                          
035600                 ELSE                                                     
035700                   IF ERSA-FLTEXT = 'N'                                   
035800                      MOVE 'J'  TO WS-FL-TYP1                             
035900                   END-IF                                                 
036000                   IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'              
036100                      ADD  +1   TO WS-KDTEXTGR-RAKNARE                    
036200                      MOVE 'N'  TO WS-FL-TYP1                             
036300                   END-IF                                                 
036400                   MOVE WS-KDTEXTGR-RAKNARE TO UT-KDTEXTGR                
036500                 END-IF                                                   
036600                                                                          
036700                 MOVE ERSA-FLTEXT     TO UT-FLTEXT                        
036800                 IF ERSA-FLTEXT = 'N'                                     
036900                    MOVE ERSA-IDARTNR-TILLK TO WS-IDARTNR-NUM             
037000                    MOVE WS-IDARTNR-NUM                                   
037100                                TO WS-IDARTNR-TILLK-ALFA                  
037200                    INSPECT WS-IDARTNR-TILLK-ALFA                         
037300                                REPLACING LEADING ZERO BY SPACE           
037400                    MOVE WS-IDARTNR-TILLK TO UT-IDARTNR20-TILLK           
037500                    MOVE ERSA-DIERS-TILLK TO UT-DIERS-TILLK               
037600                 ELSE                                                     
037700                    IF ERSA-FLTEXT = 'J'                                  
037800                       MOVE ERSA-BEERS  TO UT-IDARTNR20-TILLK             
037900                       MOVE  0     TO UT-DIERS-TILLK                      
038000                     END-IF                                               
038100                 END-IF                                                   
038200                                                                          
038300                 IF WS-NDC-FINNS = 'J'                                    
038400                    PERFORM S03-SKRIV-W11675                              
038500                 END-IF                                                   
038600                 PERFORM IMS-GET-ERSA11                                   
038700              END-PERFORM                                                 
038800           END-IF                                                         
038900         END-IF                                                           
039000       ELSE                                                               
039100          IF IN-KDERS = 29 OR 52                                          
039200             MOVE 0        TO UT-IDKORTNR                                 
039300             MOVE 0        TO UT-KDTEXTGR                                 
039400             MOVE 'N'      TO UT-FLTEXT                                   
039500             MOVE SPACE    TO UT-IDARTNR20-TILLK                          
039600             MOVE 0        TO UT-DIERS-TILLK                              
039700             MOVE SPACE    TO UT-KDUTGSTR                                 
039800                                                                          
039900             IF WS-NDC-FINNS = 'J'                                        
040000                PERFORM S03-SKRIV-W11675                                  
040100             END-IF                                                       
040200          END-IF                                                          
040300       END-IF                                                             
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 Z-FINIT SECTION.                                                         
040800                                                                          
040900     CLOSE SYSIN                                                          
041000           W11665                                                         
041100           W11675                                                         
041200     MOVE 'S'     TO POSTSUM-OPKOD                                        
041300     CALL POSTSUM USING POSTSUM-PARM                                      
041400     .                                                                    
041500     EJECT                                                                
041600 S01-LAES-W11665 SECTION.                                                 
041700                                                                          
041800     READ W11665 INTO IN-AREA                                             
041900     AT END                                                               
042000        SET END-OF-W11665 TO TRUE                                         
042100                                                                          
042200     NOT AT END                                                           
042300        MOVE 'W11665'     TO    POSTSUM-FDNAMN                            
042400        MOVE 'W11675D1'   TO    POSTSUM-DDNAMN2                           
042500        CALL POSTSUM      USING POSTSUM-PARM                              
042600     END-READ                                                             
042700     .                                                                    
042800     EJECT                                                                
042900 S02-WDATKONV SECTION.                                                    
043000     SKIP2                                                                
043100     CALL WDATKONV USING DAT-KDDATFORM                                    
043200                         DAT-I-TIDATUM                                    
043300                         DAT-O-TIDATUM                                    
043400                         DAT-KDSVAR                                       
043500     .                                                                    
043600     EJECT                                                                
043700 S03-SKRIV-W11675 SECTION.                                                
043800                                                                          
043900     IF IN-IDDC NOT = WS-DAP-IDDC                                         
044000       WRITE UT-POST FROM WS-DAP-LINE1                                    
044100       MOVE SPACE      TO UT-POST                                         
044200                                                                          
044300       MOVE IN-IDDC    TO WS-DAP-IDDC                                     
044400       WRITE UT-POST FROM WS-DAP-LINE2                                    
044500       MOVE SPACE      TO UT-POST                                         
044600     END-IF                                                               
044700                                                                          
044800     MOVE 'WB1'        TO UT-IDPTYP                                       
044900     WRITE UT-POST     FROM UT-AREA                                       
045000     MOVE 'W11675 '    TO POSTSUM-FDNAMN                                  
045100     MOVE 'W11675D2'   TO POSTSUM-DDNAMN2                                 
045200     CALL POSTSUM      USING POSTSUM-PARM                                 
045300     .                                                                    
045400     EJECT                                                                
045500 S04-KOLLA-FINNS SECTION.                                                 
045600                                                                          
045700     PERFORM IMS-GET-ARTS-ARTS                                            
045800     IF SEGMENT-FINNS                                                     
045900        MOVE IN-IDDC   TO W-IDDC                                          
046000        PERFORM IMS-GET-ARTS-SLAG                                         
046100        IF SEGMENT-FINNS                                                  
046200           CONTINUE                                                       
046300        ELSE                                                              
046400           MOVE 'N'    TO WS-NDC-FINNS                                    
046500        END-IF                                                            
046600     ELSE                                                                 
046700        MOVE 'N'       TO WS-NDC-FINNS                                    
046800     END-IF                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 S05-READ-VCOM-PARM SECTION.                                              
047200                                                                          
047300     READ SYSIN                INTO PARM-SYSINPUT                         
048300     .                                                                    
048400     EJECT                                                                
048500* --- IMS SEKTIONER ---                                                   
048600     SKIP3                                                                
048700 IMS-GET-ARTC01 SECTION.                                                  
048800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
048900          DELIMITED BY SIZE INTO SSA1                                     
049000     MOVE '  GE' TO GODK-STATUSKODER                                      
049100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
049200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     .                                                                    
049500     SKIP3                                                                
049600 IMS-GET-ERSA01 SECTION.                                                  
049700     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
049800          DELIMITED BY SIZE INTO SSA1                                     
049900     MOVE '  GE' TO GODK-STATUSKODER                                      
050000     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-WLERSA01 SSA1                  
050100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400     SKIP3                                                                
050500 IMS-GET-ERSA11 SECTION.                                                  
050600     STRING 'WLERSA11 '                                                   
050700          DELIMITED BY SIZE INTO SSA1                                     
050800     MOVE '  GE' TO GODK-STATUSKODER                                      
050900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
051000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     .                                                                    
051300     EJECT                                                                
051400 IMS-GET-ERSA11-FIRST SECTION.                                            
051500     MOVE 'WLERSA11*F' TO SSA1                                            
051600     MOVE '  GE' TO GODK-STATUSKODER                                      
051700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
051800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
051900     PERFORM IMS-STATUSKONTROLL                                           
052000     .                                                                    
052100     SKIP3                                                                
052200 IMS-GET-ERSA13 SECTION.                                                  
052300     STRING 'WLERSA13 '                                                   
052400          DELIMITED BY SIZE INTO SSA1                                     
052500     MOVE '  GE' TO GODK-STATUSKODER                                      
052600     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA13 SSA1                 
052700     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
052800     PERFORM IMS-STATUSKONTROLL                                           
052900     .                                                                    
053000     SKIP3                                                                
053100 IMS-GET-ARTS-ARTS SECTION.                                               
053200     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
053300          DELIMITED BY SIZE INTO SSA1                                     
053400     MOVE '  GE' TO GODK-STATUSKODER                                      
053500     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS01 SSA1                  
053600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
053700     PERFORM IMS-STATUSKONTROLL                                           
053800     .                                                                    
053900     EJECT                                                                
054000 IMS-GET-ARTS-SLAG SECTION.                                               
054100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
054200          DELIMITED BY SIZE INTO SSA1                                     
054300     MOVE '  GE' TO GODK-STATUSKODER                                      
054400     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WLARTS11 SSA1                 
054500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
054600     PERFORM IMS-STATUSKONTROLL                                           
054700     .                                                                    
054800     SKIP3                                                                
054900 IMS-STATUSKONTROLL SECTION.                                              
055000     SKIP2                                                                
055100     SET STATUS-IX TO 1                                                   
055200     SEARCH GODK-STATUS                                                   
055300       AT END                                                             
055400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055500           DELIMITED BY SIZE INTO FELTEXT                                 
055600         DISPLAY FELTEXT                                                  
055700         CALL FELLOG                                                      
055800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055900         CONTINUE                                                         
056000     END-SEARCH                                                           
056100     .                                                                    
