000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W611ADR.                                                 
000500*AUTHOR.         LARS THELL.                                              
000600*DATE-WRITTEN.   92/04/24.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET ÄR ETT SUBPROGRAM SOM RÄKNAR UT MÖJLIGA NÄSTA         
001200*        ADRESSER FÖR ETT PARTI.                                          
001300*                                                                         
001400*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001500*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U1000 -  FEL IFRÅN SUBPROGRAM W611STYR                           
001900*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800     SKIP2                                                                
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(8)    VALUE 'W611ADR'.             
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  W-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                  
003500 77  W-ADINLOMR-NXT              PIC X(4)    VALUE SPACE.                 
003600 77  W-KVINLART-KIT              PIC S9(7)   VALUE ZERO COMP-3.           
003700 01  FILLER                      PIC X(16)   VALUE 'W-SPAR-IDDC '.        
003800 01  W-SPAR-IDDC                 PIC X(2)    VALUE SPACE.                 
003900                                                                          
004000 77  FLKVAANT-SW                 PIC X       VALUE 'N'.                   
004100  88 FLKVAANT                                VALUE 'J'.                   
004200                                                                          
004300 77  FLBEFT-SW                   PIC X       VALUE 'N'.                   
004400  88 FLBEFT                                  VALUE 'J'.                   
004500                                                                          
004600 77  FLPRIO-SW                   PIC X       VALUE 'N'.                   
004700  88 FLPRIO                                  VALUE 'J'.                   
004800                                                                          
004900 77  FLSATS-SW                   PIC X       VALUE 'N'.                   
005000  88 FLSATS                                  VALUE 'J'.                   
005100                                                                          
005200 77  NXT-FINNS-SW                PIC X       VALUE 'N'.                   
005300  88 NXT-FINNS                               VALUE 'J'.                   
005400                                                                          
005500 77  WS-FLEJBUFF                 PIC X       VALUE 'N'.                   
005510 77  WS-ADINLOMR-BOA             PIC X(4)    VALUE SPACE.                 
005600     EJECT                                                                
005700*      --- VALID IDDC CODES                                               
005800*                                                                         
005900*01    -COPY WWDC99                                                       
006000       EJECT                                                              
006100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES DAGENS-DATUM.                                       
006300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006600     EJECT                                                                
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
007300     SKIP2                                                                
007400*    --- PARAMETRAR TILL ABEND                                            
007500                                                                          
007600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007800     SKIP2                                                                
007900 01  FELTEXT.                                                             
008000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*01  -COPY W611STYR                                                       
008400     EJECT                                                                
008500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000                                                                          
009100     03  W-W6D101KY-X.                                                    
009200         05  W-D101KY-IDDC       PIC X(2)     VALUE SPACE.                
009300         05  W-D101KY-IDLEVNR    PIC  X(5)    VALUE SPACE.                
009400         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
009500         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
009600                                                                          
009700     03  W-IDLOPNRM-X.                                                    
009800         05  W-IDLOPNRM              PIC S9(9) COMP-3 VALUE ZERO.         
009900     03  W-W6GXKEY-6005-X.                                                
010000         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
010100         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
010200         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
010300                                                                          
010400     03  W-W6GXKEY-6006-X.                                                
010500         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
010600         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
010700                                                                          
010800     03  W-IDRADNR-INL-X.                                                 
010900         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
011000                                                                          
011100     03  W-IDARTNR-X.                                                     
011200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011300                                                                          
011400     03  W-KDSEGKEY-X.                                                    
011500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011600                                                                          
011700     03  W-KDCLAGER-X.                                                    
011800         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(128).                              
013000 01  SSA2                        PIC X(64).                               
013100 01  SSA3                        PIC X(64).                               
013200     EJECT                                                                
013300*    --- IMS FUNKTIONSKODER                                               
013400*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
013800     SKIP3                                                                
013900 01  DLI-IO-AREA1.                                                        
014000     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
014100     SKIP3                                                                
014200     03  W6INLA01 REDEFINES IO-AREA1.                                     
014300*        05  -COPY W6D101                                                 
014400     EJECT                                                                
014500     03  W6INLC01 REDEFINES IO-AREA1.                                     
014600*        05  -COPY W6D1B1                                                 
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
014900     SKIP3                                                                
015000 01  DLI-IO-AREA2.                                                        
015100     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
015200     SKIP3                                                                
015300     03  W6INLA11 REDEFINES IO-AREA2.                                     
015400*        05  -COPY W6D111                                                 
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
015700     SKIP3                                                                
015800 01  DLI-IO-AREA3.                                                        
015900     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
016000     03  W6INLA21 REDEFINES IO-AREA3.                                     
016100*        05  -COPY W6D121                                                 
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
016400     SKIP3                                                                
016500 01  DLI-IO-AREA4.                                                        
016600     03  IO-AREA4                PIC X(150)  VALUE SPACE.                 
016700     03  W6PLAA11 REDEFINES IO-AREA4.                                     
016800*        05  -COPY W6GX6006  -PRE PLAA-                                   
016900     EJECT                                                                
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
017100 01  DLI-IO-WDK6-AREA.                                                    
017200     03  DLI-IO-WDK6 PIC X(900).                                          
017300     03  IO-WDK601 REDEFINES DLI-IO-WDK6.                                 
017400*          05  -COPY WDK601 -PRE K6-                                      
017500     EJECT                                                                
017600     03  IO-WDK611 REDEFINES DLI-IO-WDK6.                                 
017700*          05  -COPY WDK611                                               
017800     EJECT                                                                
017900 LINKAGE SECTION.                                                         
018000                                                                          
018100*01  -COPY W611ADR                                                        
018200     EJECT                                                                
018300*01  -COPY W0008  -PRE INLA-                                              
018400     05  FILLER                  PIC X.                                   
018500*01  -COPY W0008  -PRE INLC-                                              
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800*01  -COPY W0008  -PRE PLAA-                                              
018900     05  FILLER                  PIC X.                                   
019000     SKIP3                                                                
019100*01  -COPY W0008  -PRE WDK6-                                              
019200     05  FILLER                  PIC X.                                   
019300     SKIP3                                                                
019400 01  STYR-HANA-PCB               PIC X.                                   
019500     SKIP3                                                                
019600 01  STYR-PLAA-PCB               PIC X.                                   
019700     EJECT                                                                
019800 PROCEDURE DIVISION  USING ADR-W611ADR INLA-PCB INLC-PCB PLAA-PCB         
019900                           WDK6-PCB STYR-HANA-PCB STYR-PLAA-PCB.          
020000     PERFORM A-INIT                                                       
020100                                                                          
020200     PERFORM B-LAES-IN-INLA01-INLA11                                      
020300                                                                          
020400     PERFORM C-KOLLA-ALLA-INLA21                                          
020500                                                                          
020600     PERFORM D-FYLL-I-ADINLOMR-NXT                                        
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300     SKIP2                                                                
021400     ACCEPT DAGENS-DATUM  FROM DATE                                       
021500     MOVE SPACE                TO ADR-ADINLOMR-NXT1                       
021600                                  ADR-ADINLOMR-NXT2                       
021700                                  ADR-ADINLOMR-NXT3                       
021800                                  ADR-ADINLOMR-NXT4                       
021900                                  ADR-ADINLOMR-NXT5                       
022000                                  ADR-ADINLOMR-NXT6                       
022100     .                                                                    
022200     EJECT                                                                
022300 B-LAES-IN-INLA01-INLA11   SECTION.                                       
022400                                                                          
022500     MOVE ADR-IDLOPNRM         TO W-IDLOPNRM                              
022600     PERFORM IMS-GU-INLC-INLC01                                           
022700                                                                          
022800     MOVE SEQB-IDDC            TO W-D101KY-IDDC                           
022900                                  W-SPAR-IDDC                             
023000                                  W-6005-IDDC                             
023100                                  WS-IDDC                                 
023200     MOVE SEQB-IDLEVNR         TO W-D101KY-IDLEVNR                        
023300     MOVE SEQB-IDFS            TO W-D101KY-IDFS                           
023400     MOVE SEQB-TIAVIDAT        TO W-D101KY-TIAVIDAT                       
023500     MOVE SEQB-IDRADNR-INL     TO W-IDRADNR-INL                           
023600     PERFORM IMS-GU-INLA-INLA01                                           
023700                                                                          
023800     PERFORM IMS-GNP-INLA-INLA11                                          
023900     .                                                                    
024000     EJECT                                                                
024100 C-KOLLA-ALLA-INLA21      SECTION.                                        
024200                                                                          
024300     MOVE NEJ                  TO FLKVAANT-SW                             
024400                                  FLBEFT-SW                               
024500                                  FLPRIO-SW                               
024600                                  FLSATS-SW                               
024700     MOVE ZERO                 TO W-KVINLART-KIT                          
024800     PERFORM IMS-GNP-INLA-INLA21                                          
024900     PERFORM UNTIL SEGMENT-SAKNAS                                         
025000         IF ART-KDKVAANT       > ZERO   AND                               
025100            RAD-FLKVAANT       = NEJ    AND                               
025200           (RAD-KDINLSTA       = SPACE OR 'FPK ' OR 'SAK')                
025300             MOVE JA           TO FLKVAANT-SW                             
025400         END-IF                                                           
025500                                                                          
025600         IF ART-BEFT           > ZERO AND                                 
025700            RAD-KDINLSTA       = SPACE OR 'SAK'                           
025800             MOVE JA           TO FLBEFT-SW                               
025900         END-IF                                                           
026000                                                                          
026100         IF RAD-KDINLPRIO      < 30 AND                                   
026200           (RAD-KDINLSTA       = SPACE OR 'FPK' OR 'SAK')                 
026300             MOVE JA           TO FLPRIO-SW                               
026400         END-IF                                                           
026500                                                                          
026600         IF ART-KVAVIS-KIT     > ZERO AND                                 
026700            RAD-FLSATS         = JA   AND                                 
026800            RAD-KDINLSTA       = SPACE  OR 'SAK'                          
026900             MOVE JA           TO FLSATS-SW                               
027000         END-IF                                                           
027100                                                                          
027200         IF RAD-FLSATS         =  JA                                      
027300             COMPUTE W-KVINLART-KIT =                                     
027400                     W-KVINLART-KIT + RAD-KVINLART                        
027500         END-IF                                                           
027600         PERFORM IMS-GNP-INLA-INLA21                                      
027700     END-PERFORM                                                          
027800                                                                          
027900     IF W-KVINLART-KIT         < ART-KVAVIS-KIT                           
028000         MOVE JA               TO FLSATS-SW                               
028100     END-IF                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 D-FYLL-I-ADINLOMR-NXT    SECTION.                                        
028500                                                                          
028600     IF ART-KVKVASEK-VER       < ART-KVKVASEK-BER   OR                    
028700        ART-KVKVAPRIM-VER      < ART-KVKVAPRIM-BER  OR                    
028800        FLKVAANT                                    OR                    
028900        FLBEFT                                                            
029000         PERFORM DA-CALL-W611STYR                                         
029100         IF ART-KVKVASEK-VER       < ART-KVKVASEK-BER   OR                
029200            ART-KVKVAPRIM-VER      < ART-KVKVAPRIM-BER                    
029300             IF STYR-ADINLOMR-FB        NOT = SPACE                       
029400                 MOVE STYR-ADINLOMR-FB  TO W-ADINLOMR-NXT                 
029500                 PERFORM S01-FLYTTA-ADINLOMR-NXT                          
029600             END-IF                                                       
029700                                                                          
029800             IF STYR-ADINLOMR-FP        NOT = SPACE                       
029900                 MOVE STYR-ADINLOMR-FP  TO W-ADINLOMR-NXT                 
030000                 PERFORM S01-FLYTTA-ADINLOMR-NXT                          
030100             END-IF                                                       
030200         ELSE                                                             
030300             IF STYR-ADINLOMR-FP        NOT = SPACE                       
030400                 MOVE STYR-ADINLOMR-FP  TO W-ADINLOMR-NXT                 
030500                 PERFORM S01-FLYTTA-ADINLOMR-NXT                          
030600             ELSE                                                         
030700                 IF FLKVAANT                                              
030800                     IF STYR-ADINLOMR-FB        NOT = SPACE               
030900                         MOVE STYR-ADINLOMR-FB  TO W-ADINLOMR-NXT         
031000                         PERFORM S01-FLYTTA-ADINLOMR-NXT                  
031100                     END-IF                                               
031200                 END-IF                                                   
031300             END-IF                                                       
031400         END-IF                                                           
031500     END-IF                                                               
031600                                                                          
031700                                                                          
031800     IF FLPRIO AND (CDC-SE OR NDC)                                        
031900         MOVE ART-ADLAGOMR     TO W-ADLAGOMR                              
032000         MOVE W-ADLAGOMR       TO W-ADINLOMR-NXT                          
032100         PERFORM S01-FLYTTA-ADINLOMR-NXT                                  
032200     END-IF                                                               
032300                                                                          
032400                                                                          
032500     IF FLSATS AND CDC-SE                                                 
032600         MOVE 'HL  '           TO W-ADINLOMR-NXT                          
032700         PERFORM S01-FLYTTA-ADINLOMR-NXT                                  
032800     END-IF                                                               
032900                                                                          
033000                                                                          
033100     IF CDC-SE OR NDC                                                     
033200                                                                          
033300       IF ART-KVAVIS            =  ART-KVAVIS-PRIO OR                     
033400         (ART-KVAVIS            =  ART-KVAVIS-KIT AND FLSATS)             
033500           CONTINUE                                                       
033600       ELSE                                                               
033700           IF CDC-SE                                                      
033800             MOVE ART-IDARTNR   TO W-IDARTNR                              
033900             PERFORM IMS-GU-WDK611                                        
034000             MOVE CLAG-FLEJBUFF     TO WS-FLEJBUFF                        
034100             MOVE CLAG-ADINLOMR-BOA TO WS-ADINLOMR-BOA                    
034200           ELSE                                                           
034300             MOVE NEJ           TO WS-FLEJBUFF                            
034310             MOVE SPACE         TO WS-ADINLOMR-BOA                        
034400           END-IF                                                         
034500           MOVE ART-ADLAGOMR          TO W-ADLAGOMR                       
034600           IF WS-FLEJBUFF = JA                                            
034700             MOVE W-ADLAGOMR          TO W-ADINLOMR-NXT                   
034800             PERFORM S01-FLYTTA-ADINLOMR-NXT                              
034900           ELSE                                                           
034910             IF WS-ADINLOMR-BOA NOT = SPACE AND                           
034911                ART-ADTRDEST(1:2) NOT = 'CD'                              
034912* SÄTTER INTE ALTERNATIV BUFFERT PÅ INLEVERANSER SOM STYR MOT CD          
034913               MOVE WS-ADINLOMR-BOA   TO W-ADINLOMR-NXT                   
034914               PERFORM S01-FLYTTA-ADINLOMR-NXT                            
034920             ELSE                                                         
035000               MOVE W-ADLAGOMR          TO W-6006-ADINLOMR                
035100               MOVE W-SPAR-IDDC         TO W-6005-IDDC                    
035200               PERFORM IMS-GU-PLAA-PLAA11                                 
035300**** FÖR ATT KLARA SAKNADE LO I TEST                                      
035400               IF SEGMENT-FINNS                                           
035500                 IF PLAA-6006-ADINLOMR-BO = '    '                        
035600                    MOVE W-ADLAGOMR            TO W-ADINLOMR-NXT          
035700                 ELSE                                                     
035800                    MOVE PLAA-6006-ADINLOMR-BO TO W-ADINLOMR-NXT          
035900                 END-IF                                                   
036000                 PERFORM S01-FLYTTA-ADINLOMR-NXT                          
036100               END-IF                                                     
036110             END-IF                                                       
036200           END-IF                                                         
036300       END-IF                                                             
036400     ELSE                                                                 
036500                                                                          
036600       MOVE 'CDC'      TO W-ADINLOMR-NXT                                  
036700       PERFORM S01-FLYTTA-ADINLOMR-NXT                                    
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 DA-CALL-W611STYR SECTION.                                                
037200                                                                          
037300     MOVE W-6005-IDDC          TO STYR-IDDC                               
037400     MOVE INL-IDLEVNR          TO STYR-IDLEVNR                            
037500     MOVE ART-IDARTNR          TO STYR-IDARTNR                            
037600     MOVE ART-IDFKNGRP         TO STYR-IDFKNGRP                           
037700     MOVE ART-BEFT             TO STYR-BEFT                               
037800     CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                      
037900                                       STYR-PLAA-PCB                      
038000     .                                                                    
038100     EJECT                                                                
038200 S01-FLYTTA-ADINLOMR-NXT    SECTION.                                      
038300                                                                          
038400     PERFORM S01A-KOLLA-OM-NXT-FINNS                                      
038500     IF NXT-FINNS                                                         
038600         CONTINUE                                                         
038700      ELSE                                                                
038800         EVALUATE TRUE                                                    
038900           WHEN ADR-ADINLOMR-NXT1 = SPACE                                 
039000             MOVE W-ADINLOMR-NXT TO ADR-ADINLOMR-NXT1                     
039100                                                                          
039200           WHEN ADR-ADINLOMR-NXT2 = SPACE                                 
039300             MOVE W-ADINLOMR-NXT TO ADR-ADINLOMR-NXT2                     
039400                                                                          
039500           WHEN ADR-ADINLOMR-NXT3 = SPACE                                 
039600             MOVE W-ADINLOMR-NXT TO ADR-ADINLOMR-NXT3                     
039700                                                                          
039800           WHEN ADR-ADINLOMR-NXT4 = SPACE                                 
039900             MOVE W-ADINLOMR-NXT TO ADR-ADINLOMR-NXT4                     
040000                                                                          
040100            WHEN ADR-ADINLOMR-NXT5 = SPACE                                
040200              MOVE W-ADINLOMR-NXT TO ADR-ADINLOMR-NXT5                    
040300                                                                          
040400            WHEN ADR-ADINLOMR-NXT6 = SPACE                                
040500              MOVE W-ADINLOMR-NXT TO ADR-ADINLOMR-NXT6                    
040600         END-EVALUATE                                                     
040700     END-IF                                                               
040800     .                                                                    
040900     EJECT                                                                
041000 S01A-KOLLA-OM-NXT-FINNS     SECTION.                                     
041100                                                                          
041200     MOVE NEJ                  TO NXT-FINNS-SW                            
041300     IF W-ADINLOMR-NXT         =  ADR-ADINLOMR-NXT1 OR                    
041400                                  ADR-ADINLOMR-NXT2 OR                    
041500                                  ADR-ADINLOMR-NXT3 OR                    
041600                                  ADR-ADINLOMR-NXT4 OR                    
041700                                  ADR-ADINLOMR-NXT5 OR                    
041800                                  ADR-ADINLOMR-NXT6                       
041900         MOVE JA               TO NXT-FINNS-SW                            
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300* --- IMS SEKTIONER ---                                                   
042400     SKIP3                                                                
042500 IMS-GU-INLC-INLC01 SECTION.                                              
042600     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
042700          DELIMITED BY SIZE INTO SSA1                                     
042800     MOVE '    ' TO GODK-STATUSKODER                                      
042900     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA1 SSA1                     
043000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300     SKIP3                                                                
043400 IMS-GU-INLA-INLA01 SECTION.                                              
043500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     MOVE '    ' TO GODK-STATUSKODER                                      
043800     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA1 SSA1                     
043900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200 IMS-GNP-INLA-INLA11 SECTION.                                             
044300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
044400          DELIMITED BY SIZE INTO SSA1                                     
044500     MOVE '  GE' TO GODK-STATUSKODER                                      
044600     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA2 SSA1                    
044700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
044800     PERFORM IMS-STATUSKONTROLL                                           
044900     .                                                                    
045000     SKIP3                                                                
045100 IMS-GNP-INLA-INLA21 SECTION.                                             
045200     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
045300          DELIMITED BY SIZE INTO SSA1                                     
045400     MOVE  'W6INLA21'          TO SSA2                                    
045500     MOVE '  GE' TO GODK-STATUSKODER                                      
045600     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA3 SSA1 SSA2               
045700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     .                                                                    
046000     SKIP3                                                                
046100 IMS-GU-PLAA-PLAA11 SECTION.                                              
046200     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
046300          DELIMITED BY SIZE INTO SSA1                                     
046400     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
046500          DELIMITED BY SIZE INTO SSA2                                     
046600     MOVE '  GE' TO GODK-STATUSKODER                                      
046700     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
046800     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
046900     PERFORM IMS-STATUSKONTROLL                                           
047000     .                                                                    
047100     EJECT                                                                
047200 IMS-GU-WDK611 SECTION.                                                   
047300                                                                          
047400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
047500          DELIMITED BY SIZE INTO SSA1                                     
047600     MOVE 'WDK611   ' TO SSA2                                             
047700     MOVE '  GE' TO GODK-STATUSKODER                                      
047800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK6 SSA1 SSA2                 
047900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200     EJECT                                                                
048300 IMS-STATUSKONTROLL SECTION.                                              
048400     SKIP2                                                                
048500     SET STATUS-IX TO 1                                                   
048600     SEARCH GODK-STATUS                                                   
048700       AT END                                                             
048800         MOVE 'FEL VID DL1 CALL'  TO FELTEXT-STR                          
048900         CALL FELLOG                                                      
049000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
049100         CONTINUE                                                         
049200     END-SEARCH                                                           
049300     .                                                                    
