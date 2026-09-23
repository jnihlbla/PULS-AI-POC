000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W9103900.                                               
000400 AUTHOR.          36300 ÅKE FORSLUND 1743.                                
000500 DATE-WRITTEN.    SEPT 1982.                                              
000600                                                                          
001800*                                                                         
001900*    FUNKTION:                                                            
002000*                                                                         
002100*        PROGRAMET LÄSER IGENOM ORDERREGISTRET (WDE4).                    
002300*        FÖR ORDERRADER SOM LIGGER I ÖPPEN KÖ I VR SKAPAS                 
002400*        POSTER Å UT FILEN FÖR SENARE VEDERBÖRLIG BEHANDLING              
002500*        I VR.                                                            
002600*                                                                         
002700*    VISSA RO-DISTRIKT SELEKTERAS BORT                                    
002800*                                                                         
003100*                                                                         
003200                                                                          
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP3                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000     SELECT W91039-B02    ASSIGN TO UT-S-W91039D1.                        
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP1                                                                
004600 FD  W91039-B02                                                           
004700     LABEL RECORD   STANDARD                                              
004800     RECORDING      F                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005100*01  POST  -COPY W910B02    -PRE W91039- -L.                              
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005401                                                                          
005410*    -- CHECKED BY WY2000                                                 
005500 77  PROGRAM-NAMN                PIC X(8) VALUE 'W9103900'.               
005600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005700     SKIP2                                                                
005800*- - - - - - - - - - - - - - GENERELLA KONSTANTER                         
005900                                                                          
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200     SKIP2                                                                
006300 01  WS-ANTAL                    PIC S9(7)   COMP-3 VALUE ZERO.           
006400     EJECT                                                                
006500 01  W009KSIF-PARAM.                                                      
006600*                                                                         
006700   03  K-IDARTNR                 PIC 9(9)    VALUE ZERO.                  
006800   03  K-IDDISTR                 PIC 9(4)    VALUE ZERO.                  
006900   03  K-LGD-9                   PIC 9(1)    VALUE 9.                     
007000   03  K-LGD-4                   PIC 9(1)    VALUE 4.                     
007100   03  K-REKSIFFR                PIC 9(1)    VALUE ZERO.                  
007200     SKIP3                                                                
007300                                                                          
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
008400   03  W009KSIF                  PIC X(8)    VALUE 'W009KSIF'.            
008510   03  W460DIS1                  PIC X(8)    VALUE 'W460DIS1'.            
008600   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008700   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008710   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
008800     SKIP2                                                                
008900 01  SPAR.                                                                
009100   03  SPAR-KORD-IDKUNDRF-X.                                              
009200     05  SPAR-KORD-IDKUNDRF-N    PIC 9(5)    VALUE ZERO.                  
009300     05  FILLER                  PIC X(5)    VALUE SPACE.                 
009400   03  SPAR-ORAD-IDKUNDRF-RO-X.                                           
009500     05  SPAR-ORAD-IDKUNDRF-RO-N PIC 9(5)    VALUE ZERO.                  
009600     05  FILLER                  PIC X(5)    VALUE SPACE.                 
010000     SKIP2                                                                
010100 01  SWITCHAR.                                                            
010200   03  KORD-GODKAEND-SW          PIC X(1)    VALUE 'N'.                   
010300     88  KORD-GODKAEND                       VALUE 'J'.                   
010400     SKIP2                                                                
010500*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010600                                                                          
010700 01  RETURKODER.                                                          
010800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +33  COMP SYNC.        
010900     EJECT                                                                
011000 01  FILLER                      PIC X(24)  VALUE                         
011100                                            'B02-AREA'.                   
011200     SKIP2                                                                
011300*01  AREA  -COPY W910B02    -PRE B02-                                     
011500     EJECT                                                                
011600*- - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                      
011700                                                                          
011800*    -COPY W0005       -PRE POSTSUM-                                      
012500     EJECT                                                                
012510*- - - - - - - - - - - - - - PARAMETRAR TILL W460DIS1                     
012520                                                                          
012530*01  -COPY W460DIS1                                                       
012540     EJECT                                                                
012550*01  -COPY W460LISO                                                       
012551                                                                          
012552 01    TEST-IDDISTR              PIC 9(5)              COMP-3.            
012553 01    FILLER REDEFINES TEST-IDDISTR.                                     
012555*  03   -COPY WWDIST85.                                                   
012560     EJECT                                                                
012600 01  W-DLI-NYCKLAR.                                                       
012700                                                                          
012800   03  W-IDPURAD-X.                                                       
012900       05  W-IDPURAD             PIC S9(5)   COMP-3 VALUE ZERO.           
013000                                                                          
013100   03  W-IDPRODNR-X.                                                      
013200       05  W-IDPRODNR            PIC S9(7)   COMP-3 VALUE ZERO.           
013210                                                                          
013220   03  W-IDKOLLI-X.                                                       
013230       05  W-IDKOLLI             PIC S9(5)   COMP-3 VALUE ZERO.           
014000     EJECT                                                                
014100*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
014200*                                                                         
014300 01  IMS-WS.                                                              
014400   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
014500     SKIP3                                                                
014600*                            *** STATUSKOD FRÅN IMS                       
014700   03  STATUS-WS                 PIC XX.                                  
014800     88  SEGMENT-FINNS                       VALUE '  '.                  
014900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015100     SKIP3                                                                
015200   03  GODK-STATUSKODER.                                                  
015300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015400     SKIP3                                                                
015500   03  SSA1                      PIC X(96).                               
015600   03  SSA2                      PIC X(96).                               
015700     EJECT                                                                
015800*01  -COPY W0003                                                          
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E401'.           
016200 01  DLI-IO-E401.                                                         
016300*  03  -COPY WDE401                                                       
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E411'.           
016700 01  DLI-IO-E411.                                                         
016800*    02  -COPY WDE411                                                     
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E421'.           
017200 01  DLI-IO-E421.                                                         
017300*    02  -COPY WDE421                                                     
017310     EJECT                                                                
017400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E611'.           
017410 01  DLI-IO-E611.                                                         
017500*    02  -COPY WDE611                                                     
017700     EJECT                                                                
018800 LINKAGE SECTION.                                                         
018900     SKIP3                                                                
019000*01  -COPY W0008 -PRE WDE4-.                                              
019200     05  FILLER              PIC X.                                       
019300     EJECT                                                                
019310*01  -COPY W0008 -PRE WDE6-.                                              
019320     05  FILLER              PIC X.                                       
019330     EJECT                                                                
020200 PROCEDURE DIVISION  USING WDE4-PCB WDE6-PCB.                             
020300                                                                          
020400     ENTRY 'DLITCBL' USING WDE4-PCB WDE6-PCB.                             
020500     SKIP3                                                                
020600     PERFORM A-INIT                                                       
020700                                                                          
020800     PERFORM IMS-GET-WDE401                                               
020900                                                                          
021000     PERFORM UNTIL (NOT SEGMENT-FINNS)                                    
021100                                                                          
021200       MOVE KORD-IDDISTR TO DIS1-IDDISTR                                  
021210                            TEST-IDDISTR                                  
021300                                                                          
021310       CALL W460DIS1 USING DIS1-W460DIS1                                  
021320                                                                          
021482       IF  DIS1-IDLANDX2 = ISO-SPANIEN                                    
022400          CONTINUE                                                        
022500       ELSE                                                               
022600*----------------------------------------- VR-VILLKOR UPPFYLLT            
022700         PERFORM B-BEHANDLA-KORD                                          
022800         IF KORD-GODKAEND                                                 
022900           PERFORM IMS-GNP-WDE411                                         
023000                                                                          
023100           PERFORM UNTIL (NOT SEGMENT-FINNS)                              
023200*----------------------------------------- VR-VILLKOR UPPFYLLT            
023400             IF ORAD-KDRADSTA  <= 3                                       
023401             OR (ORAD-KDRADSTA > 3 AND KORD-KVORDRAD-PACK = ZERO)         
023500               SUBTRACT ORAD-KVLEVART FROM ORAD-KVBEART                   
023600                                        GIVING WS-ANTAL                   
023620               IF WS-ANTAL > 0                                            
023700                 PERFORM C-BEHANDLA-ORAD                                  
023800               END-IF                                                     
023810             END-IF                                                       
023900                                                                          
024000             MOVE ORAD-IDPURAD TO W-IDPURAD                               
024100             PERFORM IMS-GNP-WDE421                                       
024101             IF SEGMENT-FINNS                                             
024102               MOVE KKOLLI-IDPRODNR TO W-IDPRODNR                         
024103               MOVE KKOLLI-IDKOLLI  TO W-IDKOLLI                          
024110               PERFORM IMS-GU-WDE611                                      
024130             END-IF                                                       
024200                                                                          
024300             PERFORM UNTIL (NOT SEGMENT-FINNS)                            
024310               IF DIST85-PU-VIA-VR                                        
024381                  IF (KOLLI-TIFAKT = ZERO                                 
024382                  AND KOLLI-TIPACKN = ZERO)                               
024383                    MOVE KKOLLI-KVLEVART TO WS-ANTAL                      
024384                    PERFORM C-BEHANDLA-ORAD                               
024385                  END-IF                                                  
024386               ELSE                                                       
024400                  IF KOLLI-TIFAKT = ZERO                                  
024500                     MOVE KKOLLI-KVLEVART TO WS-ANTAL                     
024600                     PERFORM C-BEHANDLA-ORAD                              
024700                  END-IF                                                  
024710               END-IF                                                     
024800                                                                          
024810               PERFORM IMS-GNP-WDE421                                     
024811               IF SEGMENT-FINNS                                           
024812                 MOVE KKOLLI-IDPRODNR TO W-IDPRODNR                       
024813                 MOVE KKOLLI-IDKOLLI  TO W-IDKOLLI                        
024814                 PERFORM IMS-GU-WDE611                                    
024819               END-IF                                                     
025000             END-PERFORM                                                  
025300             PERFORM IMS-GNP-WDE411                                       
025400           END-PERFORM                                                    
025500         END-IF                                                           
025600       END-IF                                                             
025700       PERFORM IMS-GET-WDE401                                             
025800     END-PERFORM                                                          
025900                                                                          
026000     PERFORM Z-FINIT                                                      
026100     MOVE ZERO TO RETURN-CODE                                             
026200     GOBACK.                                                              
026300     EJECT                                                                
026400 A-INIT SECTION.                                                          
026500     SKIP2                                                                
026600     OPEN OUTPUT W91039-B02                                               
026700                                                                          
026800     MOVE PROGRAM-NAMN          TO POSTSUM-PROGNAMN                       
026900                                                                          
027000     MOVE 'B02'                 TO B02-IDPTYP.                            
027100     EJECT                                                                
027200 B-BEHANDLA-KORD SECTION.                                                 
027300     SKIP2                                                                
027400     IF KORD-KDORDKL            < 5                                       
027500         AND (KORD-FLOVRLEV     = NEJ)                                    
027510         AND (KORD-KDFAKTYP     = 'R' OR 'N' OR 'K')                      
027520                                                                          
029100         MOVE KORD-IDDISTR      TO B02-IDDISTR                            
029101         MOVE KORD-IDKUNDNR     TO B02-IDKUNDNR                           
029110         MOVE KORD-TIORDREG     TO B02-TIAAMMDD                           
029200                                                                          
029400         MOVE KORD-IDKUNDRF     TO SPAR-KORD-IDKUNDRF-X                   
029500                                                                          
029600         MOVE JA                TO KORD-GODKAEND-SW                       
029700     ELSE                                                                 
029800         MOVE NEJ               TO KORD-GODKAEND-SW                       
029900     END-IF.                                                              
030000     EJECT                                                                
030100 C-BEHANDLA-ORAD SECTION.                                                 
030200     SKIP2                                                                
030600     MOVE 71                TO B02-IDSUPPL                                
030700     MOVE 1                 TO B02-REKSUPPL                               
032100                                                                          
032200     MOVE ORAD-IDKUNDRF-RO      TO SPAR-ORAD-IDKUNDRF-RO-X                
032400                                                                          
032900     IF SPAR-ORAD-IDKUNDRF-RO-N > ZERO                                    
033000        MOVE SPAR-ORAD-IDKUNDRF-RO-N                                      
033100                                TO B02-IDORDNR7                           
033101     ELSE                                                                 
033300        MOVE SPAR-KORD-IDKUNDRF-N                                         
033400                                TO B02-IDORDNR7                           
033510     END-IF                                                               
033600                                                                          
034100     MOVE ORAD-IDARTNR        TO B02-IDARTNR                              
034200                                 K-IDARTNR                                
034300     CALL  W009KSIF   USING      K-IDARTNR                                
034400                                 K-LGD-9                                  
034500                                 K-REKSIFFR                               
034600     MOVE K-REKSIFFR          TO B02-REKSIFFR                             
034700                                                                          
034800     MOVE WS-ANTAL            TO B02-KVBEART                              
035300     MOVE ORAD-KDORDKL        TO B02-KDORDER                              
035500                                                                          
036000     IF  SPAR-ORAD-IDKUNDRF-RO-N > ZERO                                   
036100       MOVE 1                 TO B02-KDRO                                 
036200     ELSE                                                                 
036300       MOVE ZERO              TO B02-KDRO                                 
038300     END-IF                                                               
038400                                                                          
038410     MOVE ORAD-KDVRINFO       TO B02-KDVRINFO                             
038500     PERFORM S01-SKRIV-W91039                                             
038700     .                                                                    
038800     EJECT                                                                
038970 S01-SKRIV-W91039 SECTION.                                                
039000     SKIP2                                                                
039100     WRITE W91039-POST          FROM B02-AREA                             
039200                                                                          
039300     MOVE B02-IDPTYP            TO POSTSUM-TRANSTYP                       
039400     MOVE 'W91039'              TO POSTSUM-FDNAMN                         
039500     MOVE 'W91039D1'            TO POSTSUM-DDNAMN2                        
039600     CALL  POSTSUM   USING         POSTSUM-PARM.                          
040500     EJECT                                                                
040600 Z-FINIT SECTION.                                                         
040700     SKIP2                                                                
040800     CLOSE W91039-B02                                                     
040900                                                                          
041000     MOVE   'S'   TO    POSTSUM-OPKOD                                     
041100     CALL POSTSUM USING POSTSUM-PARM.                                     
041200     EJECT                                                                
041300 IMS-GET-WDE401 SECTION.                                                  
041400     SKIP1                                                                
041500     MOVE 'WDE401 ' TO SSA1                                               
041600     MOVE '  GB' TO GODK-STATUSKODER                                      
041700     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
041800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
041900     PERFORM IMS-STATUSKONTROLL.                                          
042000     SKIP3                                                                
042100 IMS-GNP-WDE411 SECTION.                                                  
042200     SKIP1                                                                
042300     MOVE 'WDE411 ' TO SSA1                                               
042400     MOVE '  GE' TO GODK-STATUSKODER                                      
042500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E411 SSA1                     
042600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
042700     PERFORM IMS-STATUSKONTROLL.                                          
042800     SKIP3                                                                
042900 IMS-GNP-WDE421 SECTION.                                                  
043000     SKIP1                                                                
043100     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
043200             DELIMITED BY SIZE INTO SSA1                                  
043300     MOVE 'WDE421 ' TO SSA2                                               
043400     MOVE '  GE' TO GODK-STATUSKODER                                      
043500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E421 SSA1 SSA2                
043600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
043700     PERFORM IMS-STATUSKONTROLL.                                          
043800     EJECT                                                                
043900 IMS-GU-WDE611 SECTION.                                                   
044000     SKIP1                                                                
044100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
044200             DELIMITED BY SIZE INTO SSA1                                  
044210     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
044220             DELIMITED BY SIZE INTO SSA2                                  
044400     MOVE '    ' TO GODK-STATUSKODER                                      
044500     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
044600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
044700     PERFORM IMS-STATUSKONTROLL.                                          
044800     SKIP3                                                                
046010 IMS-STATUSKONTROLL SECTION.                                              
046100                                                                          
046200     SET STATUS-IX TO 1                                                   
046300     SEARCH GODK-STATUS                                                   
046400       AT  END                                                            
046500         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
046600           DELIMITED BY SIZE INTO FELTEXT                                 
046700         CALL FELLOG                                                      
046800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
046900     END-SEARCH.                                                          
