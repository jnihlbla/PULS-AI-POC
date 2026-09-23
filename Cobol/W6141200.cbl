000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6141200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/02/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER URVAL FRÅN BILD 6216.                                      
001000*        SKAPAR LISTFIL MED KVALITETS-SPÄRRADE ARTIKLAR                   
001100*                                                                         
001200*        PROGRAM READS    WDP3                                            
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 CONFIGURATION SECTION.                                                   
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- PARMFIL                                                    
002800     SELECT W614PP                     ASSIGN TO W61412D1.                
002900     SKIP2                                                                
003000*          --- ARTINFO                                                    
003100     SELECT W61410                     ASSIGN TO W61412D2.                
003200     SKIP2                                                                
003300*          --- LISTFIL                                                    
003400     SELECT W61412                     ASSIGN TO W61412D3.                
003500     SKIP2                                                                
003600*          --- SORTERINGSFIL                                              
003700     SELECT SORTFIL                    ASSIGN TO W61412DS.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W614PP                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600 01  PARM                  PIC X(80).                                     
004700     SKIP3                                                                
004800 FD  W61410                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100*01  -COPY W61410      -L.                                                
005200     SKIP3                                                                
005300 FD  W61412                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600*01  POST -COPY W61410 -PRE  UT-  -L.                                     
005700     SKIP2                                                                
005800 SD  SORTFIL.                                                             
005900*01  POST -COPY W61410      -PRE SORT-                                    
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200     SKIP2                                                                
006300*    -COPY WY2000W1                                                       
006400     EJECT                                                                
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(8)    VALUE 'W6141200'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900 77  WS-URVAL                    PIC X       VALUE 'N'.                   
007000                                                                          
007100 01  WS-IDFKNGRP                 PIC 9(4)    VALUE ZERO.                  
007200 01  FILLER REDEFINES WS-IDFKNGRP.                                        
007300     03 WS-GRP-00                PIC 9(2).                                
007400     03 WS-RESTEN-00             PIC 9(2).                                
007500 01  FILLER REDEFINES WS-IDFKNGRP.                                        
007600     03 WS-GRP-000               PIC 9(1).                                
007700     03 WS-RESTEN-000            PIC 9(3).                                
007800                                                                          
007900 01  WS-PARM-IDFKNGRP            PIC 9(4)    VALUE ZERO.                  
008000 01  FILLER REDEFINES WS-PARM-IDFKNGRP.                                   
008100     03 WS-PARM-GRP-00           PIC 9(2).                                
008200     03 WS-PARM-RESTEN-00        PIC 9(2).                                
008300 01  FILLER REDEFINES WS-PARM-IDFKNGRP.                                   
008400     03 WS-PARM-GRP-000          PIC 9(1).                                
008500     03 WS-PARM-RESTEN-000       PIC 9(3).                                
008600                                                                          
008700 77  W614PP-EOF-SW               PIC X       VALUE 'N'.                   
008800     88  END-OF-W614PP                       VALUE 'J'.                   
008900                                                                          
009000 77  W61410-EOF-SW               PIC X       VALUE 'N'.                   
009100     88  END-OF-W61410                       VALUE 'J'.                   
009200                                                                          
009300 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
009400     88  END-OF-SORTFIL                      VALUE 'J'.                   
009500                                                                          
009600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009700 01  FILLER REDEFINES DAGENS-DATUM.                                       
009800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010100                                                                          
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300*                                                                         
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800     SKIP2                                                                
010900*    --- PARAMETRAR TILL ABEND                                            
011000     EJECT                                                                
011100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011400     SKIP2                                                                
011500 01  FELTEXT.                                                             
011600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011800     EJECT                                                                
011900*    --- GODKÄNDA IDDC                                                    
012000*01  -COPY WWDC99                                                         
012100     EJECT                                                                
012200*    --- PARAMETRAR TILL POSTSUM                                          
012300*01  -COPY W0005   -PRE  POSTSUM-                                         
012400     EJECT                                                                
012500 01  PARM-AREA-START             PIC X(24)   VALUE                        
012600                                 'PARM-AREA-START '.                      
012700 01  PARM-AREA                   PIC X(23).                               
012800 01  FILLER REDEFINES PARM-AREA.                                          
012900     03 PARM-KDSORT1             PIC 9.                                   
013000     03 PARM-IDDC-BEST           PIC X(2).                                
013100     03 PARM-KDLEVSP             PIC 9(2).                                
013200     03 PARM-IDFKNGRP            PIC 9(4).                                
013300     03 PARM-FLKVANT             PIC X.                                   
013400     03 PARM-IDDC                PIC X(2).                                
013500     03 PARM-IDUSER              PIC X(8).                                
013600     03 PARM-IDPERSON            PIC X(3).                                
013700     EJECT                                                                
013800 01  IN-AREA-START               PIC X(24)   VALUE                        
013900                                 'IN-AREA-START  '.                       
014000*01  AREA -COPY W61410     -PRE IN-                                       
014100     EJECT                                                                
014200 01  UT-AREA-START               PIC X(24)   VALUE                        
014300                                 'UT-AREA-START  '.                       
014400*01  AREA -COPY W61410     -PRE UT-                                       
014500     EJECT                                                                
014600 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
014700                                  'SORTWS-AREA-START  '.                  
014800*01  AREA -COPY W61410      -PRE SORTWS-                                  
014900 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
015000     EJECT                                                                
015100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015300     SKIP3                                                                
015400 01  NYCKLAR-TILL-DLI.                                                    
015500                                                                          
015600     03  W-KDARBTYP-X.                                                    
015700         05  W-KDARBTYP          PIC X(8)    VALUE 'QUAL    '.            
015800     03  W-IDPERSON-X.                                                    
015900         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
016000     03  W-WDP321KY-MIN-X.                                                
016100         05  W-IDLANDX2-P321-MIN PIC X(2)    VALUE 'SE'.                  
016200         05  W-IDARTNR-FOM-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
016300         05  W-IDARTNR-TOM-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
016400     03  W-WDP321KY-MAX-X.                                                
016500         05  W-IDLANDX2-P321-MAX PIC X(2)    VALUE 'SE'.                  
016600         05  W-IDARTNR-FOM-MAX   PIC S9(9) VALUE 999999999 COMP-3.        
016700         05  W-IDARTNR-TOM-MAX   PIC S9(9) VALUE 999999999 COMP-3.        
016800     03  W-WDP322KY-X.                                                    
016900         05  W-IDLANDX2-P322     PIC X(2)    VALUE 'SE'.                  
017000         05  W-IDLEVNR           PIC X(5)    VALUE LOW-VALUE.             
017100     03  W-WDP323KY-MIN-X.                                                
017200         05  W-IDLANDX2-P321-MIN PIC X(2)    VALUE 'SE'.                  
017300         05  W-IDFKNGRP-FOM-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
017400         05  W-IDFKNGRP-TOM-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
017500     03  W-WDP323KY-MAX-X.                                                
017600         05  W-IDLANDX2-P321-MAX PIC X(2)    VALUE 'SE'.                  
017700         05  W-IDFKNGRP-FOM-MAX  PIC S9(5)   VALUE 99999 COMP-3.          
017800         05  W-IDFKNGRP-TOM-MAX  PIC S9(5)   VALUE 99999 COMP-3.          
017900     03  W-IDARTNR-X.                                                     
018000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018100     03  W-IDFKNGRP-X.                                                    
018200         05  W-IDFKNGRP          PIC S9(5)   VALUE ZERO COMP-3.           
018300                                                                          
018400     EJECT                                                                
018500*    --- STATUS-KOD FRÅN IMS                                              
018600 01  STATUS-WS                   PIC XX.                                  
018700     88  SEGMENT-FINNS                       VALUE '  '.                  
018800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019000     88  BASEN-SLUT                          VALUE 'GB'.                  
019100     SKIP2                                                                
019200 01  GODK-STATUSKODER.                                                    
019300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019400     SKIP3                                                                
019500 01  SSA1                        PIC X(224).                              
019600 01  SSA2                        PIC X(224).                              
019700     EJECT                                                                
019800*    --- IMS FUNKTIONSKODER                                               
019900*01  -COPY W0003                                                          
020000     EJECT                                                                
020100*    ---  DLI INPUT-OUTPUT AREA                                           
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
020300 01  DLI-IO-WDP301.                                                       
020400*    03  -COPY WDP301                                                     
020500     EJECT                                                                
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
020700 01  DLI-IO-WDP311.                                                       
020800*    03  -COPY WDP311                                                     
020900     EJECT                                                                
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP321'.                      
021100 01  DLI-IO-WDP321.                                                       
021200*    03  -COPY WDP321                                                     
021300     EJECT                                                                
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP322'.                      
021500 01  DLI-IO-WDP322.                                                       
021600*    03  -COPY WDP322                                                     
021700     EJECT                                                                
021800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP323'.                      
021900 01  DLI-IO-WDP323.                                                       
022000*    03  -COPY WDP323                                                     
022100     EJECT                                                                
022200 LINKAGE SECTION.                                                         
022300*01  -COPY W0008   -PRE WDP3-                                             
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600 PROCEDURE DIVISION  USING WDP3-PCB.                                      
022700 MAIN SECTION.                                                            
022800     ENTRY 'DLITCBL' USING WDP3-PCB.                                      
022900                                                                          
023000     PERFORM A-INIT                                                       
023100     PERFORM S01-LAS-W614PP                                               
023200                                                                          
023300     IF PARM-KDSORT1 = 0 OR 1                                             
023400        SORT SORTFIL ASCENDING SORT-IDARTNR                               
023500                               SORT-IDDC                                  
023600        INPUT PROCEDURE B-SORT-INPUT                                      
023700        OUTPUT PROCEDURE C-SORT-OUTPUT                                    
023800     ELSE                                                                 
023900        IF PARM-KDSORT1 = 2                                               
024000           SORT SORTFIL ASCENDING SORT-TISPARR-KVAL                       
024100                                  SORT-IDARTNR                            
024200                                  SORT-IDDC                               
024300           INPUT PROCEDURE B-SORT-INPUT                                   
024400           OUTPUT PROCEDURE C-SORT-OUTPUT                                 
024500        ELSE                                                              
024600           SORT SORTFIL ASCENDING SORT-IDFKNGRP                           
024700                                  SORT-IDARTNR                            
024800                                  SORT-IDDC                               
024900           INPUT PROCEDURE B-SORT-INPUT                                   
025000           OUTPUT PROCEDURE C-SORT-OUTPUT                                 
025100        END-IF                                                            
025200     END-IF                                                               
025300                                                                          
025400     IF SORT-RETURN NOT = 0                                               
025500        MOVE SORT-RETURN TO SORT-RETURN-X                                 
025600        STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                     
025700        DELIMITED BY SIZE INTO FELTEXT-STR                                
025800        DISPLAY FELTEXT                                                   
025900        MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                           
026000        PERFORM S99-ABEND                                                 
026100     ELSE                                                                 
026200        PERFORM Z-FINIT                                                   
026300        MOVE ZERO TO RETURN-CODE                                          
026400        GOBACK                                                            
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900                                                                          
027000     OPEN INPUT  W614PP                                                   
027100                 W61410                                                   
027200     OPEN OUTPUT W61412                                                   
027300                                                                          
027400     ACCEPT DAGENS-DATUM  FROM DATE                                       
027500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027600     .                                                                    
027700     EJECT                                                                
027800 B-SORT-INPUT SECTION.                                                    
027900                                                                          
028000     PERFORM S02-LAES-W61410                                              
028100     PERFORM UNTIL END-OF-W61410                                          
028200       PERFORM BA-URVAL                                                   
028300       IF WS-URVAL = JA                                                   
028400         MOVE IN-AREA TO SORTWS-AREA                                      
028500         IF PARM-KDSORT1 = 2                                              
028600            MOVE SORTWS-TISPARR-KVAL TO TMP1-YYMMDD                       
028700            PERFORM WY2000P1                                              
028800            MOVE TMP1-YYMMDD         TO SORTWS-TISPARR-KVAL               
028900         END-IF                                                           
029000         PERFORM S31-SORT-RELEASE                                         
029100       END-IF                                                             
029200       PERFORM S02-LAES-W61410                                            
029300     END-PERFORM                                                          
029400     .                                                                    
029500     EJECT                                                                
029600 BA-URVAL SECTION.                                                        
029700                                                                          
029800     MOVE NEJ         TO WS-URVAL                                         
029900     MOVE IN-IDDC     TO WS-IDDC                                          
030000     MOVE IN-IDFKNGRP TO WS-IDFKNGRP                                      
030100                                                                          
030200     IF PARM-IDDC = SPACE                                                 
030300        MOVE JA TO WS-URVAL                                               
030400     ELSE                                                                 
030500        IF PARM-IDDC = '20' AND SDC                                       
030600           MOVE JA TO WS-URVAL                                            
030700        ELSE                                                              
030810           IF PARM-IDDC = '40' OR '92' AND NDC-US                         
030900              MOVE JA TO WS-URVAL                                         
031000           ELSE                                                           
031100              IF PARM-IDDC = '60' AND NDC-PACIFIC                         
031200                 MOVE JA TO WS-URVAL                                      
031300              ELSE                                                        
031400                 IF PARM-IDDC = IN-IDDC                                   
031500                    MOVE JA TO WS-URVAL                                   
031600                 END-IF                                                   
031700              END-IF                                                      
031800           END-IF                                                         
031900        END-IF                                                            
032000     END-IF                                                               
032100                                                                          
032200     IF WS-URVAL = JA                                                     
032300        IF PARM-IDUSER = SPACE                                            
032400           CONTINUE                                                       
032500        ELSE                                                              
032600           IF PARM-IDUSER = IN-IDUSER-SPKVAL                              
032700              CONTINUE                                                    
032800           ELSE                                                           
032900              MOVE NEJ TO WS-URVAL                                        
033000           END-IF                                                         
033100        END-IF                                                            
033200     END-IF                                                               
033300                                                                          
033400     IF WS-URVAL = JA                                                     
033500        IF PARM-FLKVANT = SPACE                                           
033600           CONTINUE                                                       
033700        ELSE                                                              
033800           IF PARM-FLKVANT = JA                                           
033900              IF IN-KVSPARR-KVAL > 0                                      
034000                 CONTINUE                                                 
034100              ELSE                                                        
034200                 MOVE NEJ TO WS-URVAL                                     
034300              END-IF                                                      
034400           ELSE                                                           
034500              IF PARM-FLKVANT = NEJ                                       
034600                 IF IN-KVSPARR-KVAL = 0                                   
034700                    CONTINUE                                              
034800                 ELSE                                                     
034900                    MOVE NEJ TO WS-URVAL                                  
035000                 END-IF                                                   
035100              END-IF                                                      
035200           END-IF                                                         
035300        END-IF                                                            
035400     END-IF                                                               
035500                                                                          
035600     IF WS-URVAL = JA                                                     
035700        IF PARM-KDLEVSP = 0                                               
035800           CONTINUE                                                       
035900        ELSE                                                              
036000           IF PARM-KDLEVSP = IN-KDLEVSP                                   
036100              CONTINUE                                                    
036200           ELSE                                                           
036300              MOVE NEJ TO WS-URVAL                                        
036400           END-IF                                                         
036500        END-IF                                                            
036600     END-IF                                                               
036700                                                                          
036800***  IF WS-URVAL = JA                                                     
036900***     IF PARM-IDFKNGRP = ZERO                                           
037000***        CONTINUE                                                       
037100***     ELSE                                                              
037200***        IF WS-PARM-GRP = WS-GRP                                        
037300***           CONTINUE                                                    
037400***        ELSE                                                           
037500***           MOVE NEJ TO WS-URVAL                                        
037600***        END-IF                                                         
037700***     END-IF                                                            
037800***  END-IF                                                               
037900                                                                          
038000                                                                          
038100     IF WS-URVAL = JA                                                     
038200        IF PARM-IDFKNGRP = ZERO                                           
038300           CONTINUE                                                       
038400        ELSE                                                              
038500           IF WS-PARM-RESTEN-000 = ZERO                                   
038600              IF WS-PARM-GRP-000 = WS-GRP-000                             
038700                 CONTINUE                                                 
038800              ELSE                                                        
038900                 MOVE NEJ TO WS-URVAL                                     
039000              END-IF                                                      
039100           ELSE                                                           
039200              IF WS-PARM-RESTEN-00 = ZERO                                 
039300                 IF WS-PARM-GRP-00 = WS-GRP-00                            
039400                    CONTINUE                                              
039500                 ELSE                                                     
039600                    MOVE NEJ TO WS-URVAL                                  
039700                 END-IF                                                   
039800              ELSE                                                        
039900                 IF IN-IDFKNGRP = PARM-IDFKNGRP                           
040000                    CONTINUE                                              
040100                 ELSE                                                     
040200                    MOVE NEJ TO WS-URVAL                                  
040300                 END-IF                                                   
040400              END-IF                                                      
040500           END-IF                                                         
040600        END-IF                                                            
040700     END-IF                                                               
040800                                                                          
040900     IF WS-URVAL = JA                                                     
041000       IF PARM-IDPERSON = ZERO                                            
041100         CONTINUE                                                         
041200       ELSE                                                               
041300         MOVE PARM-IDPERSON      TO W-IDPERSON                            
041400         MOVE IN-IDARTNR         TO W-IDARTNR                             
041500         MOVE IN-IDLEVNR         TO W-IDLEVNR                             
041600         MOVE IN-IDFKNGRP        TO W-IDFKNGRP                            
041700         PERFORM IMS-GU-WDP311                                            
041800         IF SEGMENT-FINNS                                                 
041900           PERFORM IMS-GNP-WDP321                                         
042000           IF SEGMENT-FINNS                                               
042100             CONTINUE                                                     
042200           ELSE                                                           
042300             PERFORM IMS-GNP-WDP322                                       
042400             IF SEGMENT-FINNS                                             
042500               CONTINUE                                                   
042600             ELSE                                                         
042700               PERFORM IMS-GNP-WDP323                                     
042800               IF SEGMENT-FINNS                                           
042900                 CONTINUE                                                 
043000               ELSE                                                       
043100                 MOVE NEJ        TO WS-URVAL                              
043200               END-IF                                                     
043300             END-IF                                                       
043400           END-IF                                                         
043500         ELSE                                                             
043600           MOVE NEJ              TO WS-URVAL                              
043700         END-IF                                                           
043800       END-IF                                                             
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 C-SORT-OUTPUT SECTION.                                                   
044300                                                                          
044400     PERFORM S32-SORT-RETURN                                              
044500     PERFORM UNTIL END-OF-SORTFIL                                         
044600       IF PARM-KDSORT1 = 2                                                
044700          MOVE SORTWS-TISPARR-KVAL TO TMP1-YYMMDD                         
044800          PERFORM WY2000P1                                                
044900          MOVE TMP1-YYMMDD         TO SORTWS-TISPARR-KVAL                 
045000       END-IF                                                             
045100       MOVE SORTWS-AREA    TO UT-AREA                                     
045200       MOVE PARM-KDSORT1   TO UT-KDSORT1                                  
045300       MOVE PARM-IDDC-BEST TO UT-IDDC-BEST                                
045400       IF PARM-IDDC = SPACE                                               
045500       AND (PARM-KDLEVSP = ZERO OR 20)                                    
045600          MOVE SORTWS-IDDC TO WS-IDDC                                     
045700          IF SORTWS-KDLEVSP = 20                                          
045800          AND NOT CDC-SE                                                  
045900              CONTINUE                                                    
046000          ELSE                                                            
046100             PERFORM S11-SKRIV-W61412                                     
046200          END-IF                                                          
046300       ELSE                                                               
046400          PERFORM S11-SKRIV-W61412                                        
046500       END-IF                                                             
046600       PERFORM S32-SORT-RETURN                                            
046700     END-PERFORM                                                          
046800     .                                                                    
046900     EJECT                                                                
047000 Z-FINIT SECTION.                                                         
047100                                                                          
047200     CLOSE W614PP                                                         
047300           W61410                                                         
047400           W61412                                                         
047500                                                                          
047600     MOVE 'S' TO POSTSUM-OPKOD                                            
047700     CALL POSTSUM USING POSTSUM-PARM                                      
047800     .                                                                    
047900     EJECT                                                                
048000 S01-LAS-W614PP SECTION.                                                  
048100                                                                          
048200     READ W614PP INTO PARM-AREA                                           
048300     MOVE PARM-IDFKNGRP TO WS-PARM-IDFKNGRP                               
048400     .                                                                    
048500     EJECT                                                                
048600 S02-LAES-W61410  SECTION.                                                
048700                                                                          
048800     READ W61410 INTO IN-AREA                                             
048900     AT END                                                               
049000        SET END-OF-W61410 TO TRUE                                         
049100                                                                          
049200     NOT AT END                                                           
049300        MOVE 'W61410'   TO POSTSUM-FDNAMN                                 
049400        MOVE 'W61412D2' TO POSTSUM-DDNAMN2                                
049500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
049600        CALL POSTSUM USING POSTSUM-PARM                                   
049700     END-READ                                                             
049800     .                                                                    
049900     EJECT                                                                
050000 S11-SKRIV-W61412 SECTION.                                                
050100                                                                          
050200     WRITE UT-POST FROM UT-AREA                                           
050300                                                                          
050400     MOVE 'W61412'   TO POSTSUM-FDNAMN                                    
050500     MOVE 'W61412D3' TO POSTSUM-DDNAMN2                                   
050600     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
050700     CALL POSTSUM USING POSTSUM-PARM                                      
050800     .                                                                    
050900     EJECT                                                                
051000 S31-SORT-RELEASE  SECTION.                                               
051100                                                                          
051200     RELEASE SORT-POST FROM SORTWS-AREA                                   
051300     .                                                                    
051400     EJECT                                                                
051500 S32-SORT-RETURN  SECTION.                                                
051600                                                                          
051700     RETURN SORTFIL INTO SORTWS-AREA                                      
051800     AT END                                                               
051900         SET END-OF-SORTFIL TO TRUE                                       
052000     .                                                                    
052100     EJECT                                                                
052200 S99-ABEND SECTION.                                                       
052300                                                                          
052400     MOVE 'S' TO POSTSUM-OPKOD                                            
052500     CALL POSTSUM USING POSTSUM-PARM                                      
052600     CALL ABEND USING RKOD-ABEND                                          
052700     .                                                                    
052800     EJECT                                                                
052900* --- IMS SEKTIONER ---                                                   
053000     SKIP3                                                                
053100 IMS-GU-WDP311    SECTION.                                                
053200     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
053300          DELIMITED BY SIZE INTO SSA1                                     
053400     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
053500          DELIMITED BY SIZE INTO SSA2                                     
053600     MOVE '  GE' TO GODK-STATUSKODER                                      
053700     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
053800     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
053900     PERFORM IMS-STATUSKONTROLL                                           
054000     .                                                                    
054100 IMS-GNP-WDP321    SECTION.                                               
054200     STRING 'WDP321  (WDP321KY=>' W-WDP321KY-MIN-X                        
054300                    '&WDP321KY=<' W-WDP321KY-MAX-X                        
054400                    '&IDARTNRF=<' W-IDARTNR-X                             
054500                    '&IDARTNRT=>' W-IDARTNR-X      ')'                    
054600          DELIMITED BY SIZE INTO SSA1                                     
054700     MOVE '  GE' TO GODK-STATUSKODER                                      
054800     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP321 SSA1                   
054900     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
055000     PERFORM IMS-STATUSKONTROLL                                           
055100     .                                                                    
055200 IMS-GNP-WDP322    SECTION.                                               
055300     STRING 'WDP322  (WDP322KY =' W-WDP322KY-X ')'                        
055400          DELIMITED BY SIZE INTO SSA1                                     
055500     MOVE '  GE' TO GODK-STATUSKODER                                      
055600     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP322 SSA1                   
055700     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
055800     PERFORM IMS-STATUSKONTROLL                                           
055900     .                                                                    
056000 IMS-GNP-WDP323    SECTION.                                               
056100     STRING 'WDP323  (WDP323KY=>' W-WDP323KY-MIN-X                        
056200                    '&WDP323KY=<' W-WDP323KY-MAX-X                        
056300                    '&IDFKNGRF=<' W-IDFKNGRP-X                            
056400                    '&IDFKNGRT=>' W-IDFKNGRP-X     ')'                    
056500          DELIMITED BY SIZE INTO SSA1                                     
056600     MOVE '  GE' TO GODK-STATUSKODER                                      
056700     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP323 SSA1                   
056800     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
056900     PERFORM IMS-STATUSKONTROLL                                           
057000     .                                                                    
057100 IMS-STATUSKONTROLL SECTION.                                              
057200     SET STATUS-IX TO 1                                                   
057300     SEARCH GODK-STATUS                                                   
057400       AT END                                                             
057500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
057600         DELIMITED BY SIZE INTO FELTEXT                                   
057700         CALL FELLOG                                                      
057800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
057900         CONTINUE                                                         
058000     END-SEARCH                                                           
058100     .                                                                    
058200*    -COPY WY2000P1                                                       
