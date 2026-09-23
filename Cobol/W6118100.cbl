000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6118100.                                                
000400*AUTHOR.         BERT ANDERSSON.                                          
000500*DATE-WRITTEN.   92/04/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAMMET ÄR ETT SB SOM LÄSER IGENOM W6D1 OCH                   
001100*        SKRIVER UT EN FIL. FILEN INNEHÅLLER SAMTLIGA                     
001200*        RENSNINGSBARA FÖLJESEDLAR PÅ W6D1. FÖLJESEDELN                   
001300*        ÄR RENSNINGSBAR OM ALLA PARTIER HAR FLKLAR = J,                  
001400*        OCH MOTTAGNINGSDATUM - TIINLMOT - ÄR ÄLDRE ÄN                    
001500*        340 KALENDERDAGAR (CA 11 MÅN RENSNING FÖR W6D1 O W6L2)           
001600*                                                                         
001700*        SKAPAR ÄVEN FIL FÖR VARNING OM EJ AVSLUTADE PARTIER              
001800*        ÄLDRE ÄN 300 DAGAR.                                              
001900*                                                                         
002000*        PROGRAMMET LÄSER     W6INLA (W6D1)                               
002100*                                                                         
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- UTFIL W61181                                               
003200     SELECT W61181                     ASSIGN TO W61181D1.                
003300     SELECT W6118A                     ASSIGN TO W61181D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W61181                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  POST -COPY W6118101  -PRE  UT-  -L.                                  
004400     EJECT                                                                
004500 FD  W6118A                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900*01  POST -COPY W6118A01  -PRE  UT2-  -L.                                 
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200*  -- CHECKED BY WY2000                                                   
005300     SKIP2                                                                
005400 77  IDPGM                       PIC X(8)    VALUE 'W6118100'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700*                                                                         
005800 77  ART-SW                      PIC X       VALUE 'N'.                   
005900     88  SAMTL-ARTIKL-HAR-FLKLAR-JA          VALUE 'J'.                   
006000*                                                                         
006100 77  BORTTAG-SW                  PIC X       VALUE 'N'.                   
006200     88  BORTTAG-OK                          VALUE 'J'.                   
006300*                                                                         
006400*                                                                         
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  GENERAL-SUBPROGRAM.                                                  
007200*                                                                         
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007800     SKIP2                                                                
007900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008000                                                                          
008100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600*    --- PARAMETRAR TILL POSTSUM                                          
008700*                                                                         
008800*01  -COPY W0005   -PRE  POSTSUM-                                         
008900     SKIP2                                                                
009000 01  UT-AREA-START               PIC X(24)   VALUE                        
009100                                 'UT-AREA-START  '.                       
009200     SKIP2                                                                
009300                                                                          
009400*01  AREA -COPY W6118101   -PRE UT-                                       
009500     EJECT                                                                
009600 01  UT2-AREA-START             PIC X(24)   VALUE                         
009700                                 'UT2-AREA-START  '.                      
009800     SKIP2                                                                
009900                                                                          
010000*01  AREA -COPY W6118A01   -PRE UT2-                                      
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL WDAGKONV                                         
010300     EJECT                                                                
010400*01  -COPY WDAGAREA                                                       
010500*    --- AREAS FOR IMS-SECTIONS                                           
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000*    --- STATUS-KOD FRÅN IMS                                              
011100 01  STATUS-WS                   PIC XX.                                  
011300     88  SEGMENT-FINNS                       VALUE ' ' 'GA' 'GK'.         
011400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011500     88  END-OF-DATA                         VALUE 'GB'.                  
011600     SKIP2                                                                
011700 01  GODK-STATUSCODES.                                                    
011800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900     SKIP3                                                                
012000 01  SSA1                        PIC X(64).                               
012100 01  SSA2                        PIC X(64).                               
012200     EJECT                                                                
012300*    --- IMS FUNCTION CODES                                               
012400*01  -COPY W0003                                                          
012500     EJECT                                                                
012600*    ---  DLI INPUT-OUTPUT AREA                                           
012700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012800     SKIP3                                                                
012900 01  DLI-IO-AREA.                                                         
013000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013100     SKIP3                                                                
013200     03  W6INLA01 REDEFINES IO-AREA.                                      
013300*        05  -COPY W6D101                                                 
013400     SKIP3                                                                
013500     03  W6INLA11 REDEFINES IO-AREA.                                      
013600*        05  -COPY W6D111                                                 
013700     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900                                                                          
014000     EJECT                                                                
014100*01  -COPY W0008  -PRE INLA-                                              
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400 PROCEDURE DIVISION  USING INLA-PCB.                                      
014500     ENTRY 'DLITCBL' USING INLA-PCB.                                      
014600                                                                          
014700     PERFORM A-INIT                                                       
014800     PERFORM IMS-GN-INLA                                                  
014900     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                          
015000                                                                          
015100       EVALUATE INLA-SEG-NAME-FB                                          
015200         WHEN  'W6D101'                                                   
015300           PERFORM B-SPAR-DATA                                            
015400                                                                          
015500         WHEN  'W6D111'                                                   
015600           PERFORM C-KONTROLL                                             
015700           PERFORM D-KOLLA-GAMLA-EJ-AVSLUTADE                             
015800                                                                          
015900         WHEN  OTHER                                                      
016000           CONTINUE                                                       
016100       END-EVALUATE                                                       
016200                                                                          
016300       PERFORM IMS-GN-INLA                                                
016400     END-PERFORM                                                          
016401                                                                          
016410     IF SAMTL-ARTIKL-HAR-FLKLAR-JA                                        
016420       PERFORM S11-SKRIV-W61181                                           
016430     END-IF                                                               
016500                                                                          
016600     PERFORM Z-FINIT                                                      
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     OPEN OUTPUT W61181                                                   
017500                 W6118A                                                   
017600     SKIP2                                                                
017700     MOVE IDPGM                     TO POSTSUM-PROGNAMN                   
017800     MOVE NEJ                       TO ART-SW                             
017900                                                                          
018000     ACCEPT DAGENS-DATUM FROM DATE                                        
018100     .                                                                    
018200     SKIP2                                                                
018300 B-SPAR-DATA  SECTION.                                                    
018400                                                                          
018500     PERFORM BA-EV-SKRIV-POST                                             
018600     IF SEGMENT-FINNS                                                     
018700       MOVE INL-IDDC                TO UT-IDDC                            
018800       MOVE INL-IDLEVNR             TO UT-IDLEVNR                         
018900       MOVE INL-IDFS                TO UT-IDFS                            
019000       MOVE INL-TIAVIDAT            TO UT-TIAVIDAT                        
019100       PERFORM BB-KOLLA-DATUM                                             
019200     END-IF                                                               
019300     .                                                                    
019400     SKIP2                                                                
019500 BA-EV-SKRIV-POST     SECTION.                                            
019600                                                                          
019700     IF SAMTL-ARTIKL-HAR-FLKLAR-JA                                        
019800       PERFORM S11-SKRIV-W61181                                           
019900     END-IF                                                               
020000     MOVE JA                     TO ART-SW                                
020100     .                                                                    
020200     SKIP2                                                                
020300 BB-KOLLA-DATUM SECTION.                                                  
020400                                                                          
020500     IF INL-KDINL = '310'                                                 
020600       IF INL-TIAVIDAT NOT = ZERO                                         
020700         MOVE +001                 TO DAG-KDCALL                          
020800         MOVE INL-TIAVIDAT         TO DAG-TIAAMMDD-FOM                    
020900         MOVE DAGENS-DATUM         TO DAG-TIAAMMDD-TOM                    
021000                                                                          
021100         CALL WDAGKONV USING DAG-KDCALL                                   
021200                   DAG-DATUM-AREA DAG-KDSVAR                              
021300                                                                          
021400         IF DAG-KDSVAR = SPACE                                            
021500             IF DAG-KVKALDAG       > 340                                  
021600                 MOVE JA TO BORTTAG-SW                                    
021700             ELSE                                                         
021800                 MOVE NEJ TO BORTTAG-SW                                   
021900             END-IF                                                       
022000         ELSE                                                             
022100             MOVE 'FEL UR DAGKONV' TO FELTEXT                             
022200             DISPLAY FELTEXT                                              
022300             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
022400         END-IF                                                           
022500       ELSE                                                               
022600         IF INL-TIINLMOT NOT = ZERO                                       
022700           MOVE +001                 TO DAG-KDCALL                        
022800           MOVE INL-TIINLMOT         TO DAG-TIAAMMDD-FOM                  
022900           MOVE DAGENS-DATUM         TO DAG-TIAAMMDD-TOM                  
023000                                                                          
023100           CALL WDAGKONV USING DAG-KDCALL                                 
023200                     DAG-DATUM-AREA DAG-KDSVAR                            
023300                                                                          
023400           IF DAG-KDSVAR = SPACE                                          
023500               IF DAG-KVKALDAG       > 340                                
023600                   MOVE JA TO BORTTAG-SW                                  
023700               ELSE                                                       
023800                   MOVE NEJ TO BORTTAG-SW                                 
023900               END-IF                                                     
024000           ELSE                                                           
024100               MOVE 'FEL UR DAGKONV' TO FELTEXT                           
024200               DISPLAY FELTEXT                                            
024300               CALL ABEND USING RKOD-ABEND-MED-DUMP                       
024400           END-IF                                                         
024500         ELSE                                                             
024600           MOVE NEJ TO BORTTAG-SW                                         
024700         END-IF                                                           
024800       END-IF                                                             
024900     ELSE                                                                 
025000       IF INL-TIINLMOT NOT = ZERO                                         
025100         MOVE +001                 TO DAG-KDCALL                          
025200         MOVE INL-TIINLMOT         TO DAG-TIAAMMDD-FOM                    
025300         MOVE DAGENS-DATUM         TO DAG-TIAAMMDD-TOM                    
025400                                                                          
025500         CALL WDAGKONV USING DAG-KDCALL                                   
025600                   DAG-DATUM-AREA DAG-KDSVAR                              
025700                                                                          
025800         IF DAG-KDSVAR = SPACE                                            
025900             IF DAG-KVKALDAG       > 340                                  
026000                 MOVE JA TO BORTTAG-SW                                    
026100             ELSE                                                         
026200                 MOVE NEJ TO BORTTAG-SW                                   
026300             END-IF                                                       
026400         ELSE                                                             
026500             MOVE 'FEL UR DAGKONV' TO FELTEXT                             
026600             DISPLAY FELTEXT                                              
026700             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
026800         END-IF                                                           
026900       ELSE                                                               
027000         MOVE NEJ TO BORTTAG-SW                                           
027100       END-IF                                                             
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 C-KONTROLL           SECTION.                                            
027600                                                                          
027700     IF ART-FLKLAR = JA AND BORTTAG-OK                                    
027800       CONTINUE                                                           
027900     ELSE                                                                 
028000       MOVE NEJ                     TO ART-SW                             
028100     END-IF                                                               
028200     .                                                                    
028300     SKIP2                                                                
028400 D-KOLLA-GAMLA-EJ-AVSLUTADE SECTION.                                      
028500                                                                          
028600     IF ART-FLKLAR = NEJ AND DAG-KVKALDAG > 300                           
028700       IF ART-IDLOPNRM > ZERO                                             
028800         MOVE ART-IDLOPNRM  TO UT2-IDLOPNRM                               
028900         MOVE ART-IDDC      TO UT2-IDDC                                   
029000         MOVE ART-IDARTNR   TO UT2-IDARTNR                                
029100         PERFORM S12-SKRIV-W6118A                                         
029200       END-IF                                                             
029300     END-IF                                                               
029400     .                                                                    
029500     SKIP2                                                                
029600 Z-FINIT SECTION.                                                         
029700                                                                          
029800     CLOSE W61181                                                         
029900           W6118A                                                         
030000     SKIP2                                                                
030100     MOVE 'S' TO POSTSUM-OPKOD                                            
030200     CALL POSTSUM USING POSTSUM-PARM                                      
030300     .                                                                    
030400     SKIP2                                                                
030500 S11-SKRIV-W61181 SECTION.                                                
030600                                                                          
030700     SKIP2                                                                
030800     WRITE UT-POST FROM UT-AREA                                           
030900                                                                          
031000     MOVE 'W61181' TO POSTSUM-FDNAMN                                      
031100     MOVE 'W61181D1' TO POSTSUM-DDNAMN2                                   
031200     CALL POSTSUM USING POSTSUM-PARM                                      
031300     .                                                                    
031400     SKIP2                                                                
031500 S12-SKRIV-W6118A SECTION.                                                
031600                                                                          
031700     SKIP2                                                                
031800     WRITE UT2-POST FROM UT2-AREA                                         
031900                                                                          
032000     MOVE 'W6118A'   TO POSTSUM-FDNAMN                                    
032100     MOVE 'W61181D2' TO POSTSUM-DDNAMN2                                   
032200     CALL POSTSUM USING POSTSUM-PARM                                      
032300     .                                                                    
032400     SKIP2                                                                
032500* --- IMS SECTIONS  ---                                                   
032600     SKIP3                                                                
032700 IMS-GN-INLA   SECTION.                                                   
032800     SKIP2                                                                
032900     CALL CBLTDLI USING GN INLA-PCB DLI-IO-AREA                           
033000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
033100     MOVE '  GAGKGB' TO GODK-STATUSCODES                                  
033200     PERFORM IMS-STATUSCHECK                                              
033300     .                                                                    
033400     SKIP2                                                                
033500 IMS-STATUSCHECK SECTION.                                                 
033600     SKIP2                                                                
033700     SET STATUS-IX TO 1                                                   
033800     SEARCH GODK-STATUS                                                   
033900       AT END                                                             
034000         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
034100         DISPLAY FELTEXT                                                  
034200         CALL FELLOG                                                      
034300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034400         CONTINUE                                                         
034500     END-SEARCH                                                           
034600     .                                                                    
