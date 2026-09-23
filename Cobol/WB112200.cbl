000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WB112200.                                                
000300 AUTHOR.         EGHOLT CONNY.                                            
000400 DATE-WRITTEN.   05/03/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900         LÄSER WDK6                                                       
001000*                                                                         
001200*    ÄNDRING:                                                             
001300*        URVALSNYCKEL FÖR IDENTIFIKATION AV ARTIKELPOST FRÅN KDP          
001400*        SKALL ÄNDRAS FRÅN ENBART ARTIKELNUMMER TILL ATT VARA EN          
001500*        KOMBINATION AV ARTIKEL + SU   (IDARTNR + IDUPPDSU).              
001600*        D.V.S.                                                           
001700*        PÅ DETTA VIS KOMMER ALDRIG EN ARTIKEL TILLHÖRANDE BÅDE           
001800*        ETT NUVARANDE OCH FRAMTIDA BILPROJEKT INTE ATT ANNULLERAS        
001900*        DÅ DET NUVARANDE BILPROJEKTETS ARTIKEL FÖRSVINNER FRÅN           
002000*        KDP-FILEN.                                                       
002100*                                                                         
002200*    2007-11-16  PROD ABEND FIX. ETRACKER 5934364                         
002300*                                                                         
002310*    2015-12     ETRACKER 10219962                                        
002320*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*          --- NY KDPFIL PC0234F2                                         
003700     SELECT WB0120N                    ASSIGN TO WB1122D1.                
003800     SKIP2                                                                
003900*          --- FÖREGÅENDE KDPFIL PC0234F2                                 
004000     SELECT WB0120G                    ASSIGN TO WB1122D2.                
004100     SKIP2                                                                
004200*          --- KOMPLETTERAD KDP-FIL MED KDANNULL UPPDATERAT               
004300     SELECT WB1122                     ASSIGN TO WB1122D3.                
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP3                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  WB0120N                                                              
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  -COPY PC0234F2      -L.                                              
005400     SKIP3                                                                
005500 FD  WB0120G                                                              
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  -COPY PC0234F2      -L.                                              
006000     SKIP3                                                                
006100 FD  WB1122                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500 01  WB1122-POST.                                                         
006600*    03 -COPY PC0234F2    -L.                                             
006700     03 WB1122-KDANNULL         PIC X.                                    
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000                                                                          
007100 77  IDPGM                       PIC X(8)    VALUE 'WB112200'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007400                                                                          
007500 77  WB0120N-EOF-SW               PIC X       VALUE 'N'.                  
007600     88  END-OF-WB0120N                       VALUE 'J'.                  
007700                                                                          
007800 77  WB0120G-EOF-SW               PIC X       VALUE 'N'.                  
007900     88  END-OF-WB0120G                       VALUE 'J'.                  
008000     EJECT                                                                
008100 01  SPAR-AREA.                                                           
008200     03 TB1ACCE-KEY-G.                                                    
008300       05 WS-IDARTNR-KEY-G       PIC 9(8)    VALUE ZERO.                  
008400       05 WS-UPDNR-SU-KEY-G      PIC X(8)    VALUE ZERO.                  
008500                                                                          
008600     03 TB1ACCE-KEY-N.                                                    
008700       05 WS-IDARTNR-KEY-N       PIC 9(8)    VALUE ZERO.                  
008800       05 WS-UPDNR-SU-KEY-N      PIC X(8)    VALUE ZERO.                  
008801                                                                          
008810*- - - - - - - - - - - - - - NYCKLAR TILL DLI.                            
008820 01  NYCKLAR-TILL-DLI.                                                    
008830     03   W-IDARTNR-X.                                                    
008840       05 W-IDARTNR              PIC S9(9)   COMP-3.                      
008850*- - - - - - - - -STATUSKOD FRÅN IMS                                      
008860 01  STATUS-WS                   PIC X(2).                                
008870     88  SEGMENT-FINNS           VALUE '  '.                              
008891                                                                          
008892 01  GODK-STATUSKODER.                                                    
008893     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008894 01  SSA1                        PIC X(32).                               
008900                                                                          
009000     EJECT                                                                
009010                                                                          
009100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009200 01  FILLER REDEFINES DAGENS-DATUM.                                       
009300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009501                                                                          
009510 01  WS-TIFINLV.                                                          
009511     03  WS-AAVV                 PIC X(4).                                
009512     03  FILLER                  PIC X(1).                                
009520 01  WS-TIFINLV-DISP             PIC 9(5).                                
009530 01  WS-ACTUAL-WEEK              PIC X(4).                                
009530 01  WS-AOINFTID-T.                                                       
009530     03  FILLER                  PIC X(2).                                
009530     03  WS-AOINFTID4-T          PIC X(4).                                
009600     EJECT                                                                
009610                                                                          
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800*                                                                         
009900     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
010000     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM'.              
010010     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
010020     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
010030     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
010100     SKIP2                                                                
010200*    --- PARAMETRAR TILL ABEND                                            
010300                                                                          
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010700     SKIP2                                                                
010800 01  FELTEXT.                                                             
010900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL POSTSUM                                          
011300*                                                                         
011400*01  -COPY W0005   -PRE  POSTSUM-                                         
011500     EJECT                                                                
011510*    --- PARAMETRAR TILL WDATKONV                                         
011520*                                                                         
011530*01  -COPY WDATAREA                                                       
011540     EJECT                                                                
011600 01  WB0120N-AREA-START PIC X(24) VALUE 'WB0120N-AREA-START  '.           
011700*01  AREA -COPY PC0234F2     -PRE WB0120N-                                
011800     EJECT                                                                
011900                                                                          
012000 01  WB0120G-AREA-START PIC X(24) VALUE 'WB0120G-AREA-START  '.           
012100*01  AREA -COPY PC0234F2     -PRE WB0120G-                                
012200     EJECT                                                                
012300                                                                          
012400 01  WB1122-AREA-START  PIC X(24) VALUE 'WB1122-AREA-START  '.            
012500 01  UPD-AREA.                                                            
012600*    03  -COPY PC0234F2   -PRE UPD-                                       
012700     03  UPD-KDANNULL         PIC X.                                      
012800     EJECT                                                                
012801                                                                          
012802*    --- IMS DLI FUNCTION CODES                                           
012810*01   -COPY W0003                                                         
012820     EJECT                                                                
012821                                                                          
012822*    ---  DLI INPUT-OUTPUT AREA                                           
012830 01  FILLER                     PIC X(16)    VALUE                        
012840                                            'DLI-IO-AREA'.                
012871 01  DLI-IO-WDK601.                                                       
012872*    03  -COPY WDK601  -PRE WDK6-                                         
012880     EJECT                                                                
012900                                                                          
012910 LINKAGE SECTION.                                                         
012940*01  -COPY W0008       -PRE WDK6-                                         
012950     05  FILLER                 PIC X.                                    
012960     EJECT                                                                
012993                                                                          
012994 PROCEDURE DIVISION  USING WDK6-PCB.                                      
012995 MAIN SECTION.                                                            
012996     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
013200     SKIP2                                                                
013300                                                                          
013400     PERFORM A-INIT                                                       
013500     PERFORM S01-LAES-WB0120N                                             
013600     PERFORM S02-LAES-WB0120G                                             
013700     PERFORM UNTIL END-OF-WB0120G  AND END-OF-WB0120N                     
013800                                                                          
013900       IF TB1ACCE-KEY-N  = TB1ACCE-KEY-G                                  
014000*        IF WB0120N-AREA = WB0120G-AREA                                   
014100*          -- ONÖDIGT ATT BEHANDLA                                        
014200*          CONTINUE                                                       
014300*        ELSE                                                             
014400*          -- VANLIG REPLACE-POST, SKRIV DEN NYA FILENS POST              
014500           MOVE WB0120N-AREA      TO UPD-PC0234F2                         
014601*          MOVE UPD-ARTNR         TO W-IDARTNR                            
014602*          PERFORM IMS-GU-WDK601                                          
014603*          IF SEGMENT-FINNS                                               
014605*            MOVE WDK6-ART-TIFINLV  TO WS-TIFINLV-DISP                    
014606*            MOVE WS-TIFINLV-DISP   TO WS-TIFINLV                         
014607*            IF WS-AAVV     < WS-ACTUAL-WEEK                              
014607             MOVE UPD-AOINFTID6-T   TO WS-AOINFTID-T                      
014607             IF WS-AOINFTID4-T < WS-ACTUAL-WEEK                           
014608               MOVE 'I'             TO UPD-KDANNULL                       
014609             ELSE                                                         
014610               MOVE ' '             TO UPD-KDANNULL                       
014611             END-IF                                                       
014612*          ELSE                                                           
014613*            MOVE '-'               TO UPD-KDANNULL                       
014614*          END-IF                                                         
014615           IF UPD-MDSMARK = 'J'                                           
014620             MOVE 'Y' TO UPD-MDSMARK                                      
014630           END-IF                                                         
014640*          DISPLAY UPD-ARTNR ' REPL ' WS-AAVV ' ' WS-ACTUAL-WEEK          
014700           PERFORM S11-SKRIV-WB1122                                       
014800*        END-IF                                                           
014900         IF NOT END-OF-WB0120N  PERFORM S01-LAES-WB0120N  END-IF          
015000         IF NOT END-OF-WB0120G  PERFORM S02-LAES-WB0120G  END-IF          
015100       ELSE                                                               
015200         IF TB1ACCE-KEY-N < TB1ACCE-KEY-G                                 
015300*          -- LÄGRE NYCKEL PÅ NYA, SKRIV UT NY INSERT-POST                
015400           MOVE WB0120N-AREA      TO UPD-PC0234F2                         
015502*          MOVE UPD-ARTNR         TO W-IDARTNR                            
015504*          PERFORM IMS-GU-WDK601                                          
015505*          IF SEGMENT-FINNS                                               
015507*            MOVE WDK6-ART-TIFINLV  TO WS-TIFINLV-DISP                    
015508*            MOVE WS-TIFINLV-DISP   TO WS-TIFINLV                         
015509*            IF WS-AAVV     < WS-ACTUAL-WEEK                              
014607             MOVE UPD-AOINFTID6-T   TO WS-AOINFTID-T                      
015509             IF WS-AOINFTID4-T < WS-ACTUAL-WEEK                           
015510               MOVE 'I'             TO UPD-KDANNULL                       
015511             ELSE                                                         
015512               MOVE ' '             TO UPD-KDANNULL                       
015513             END-IF                                                       
015514*          ELSE                                                           
015515*            MOVE '-'               TO UPD-KDANNULL                       
015516*          END-IF                                                         
015517           IF UPD-MDSMARK = 'J'                                           
015518             MOVE 'Y' TO UPD-MDSMARK                                      
015520           END-IF                                                         
015540*          DISPLAY UPD-ARTNR ' INS ' WS-AAVV ' ' WS-ACTUAL-WEEK           
015600           PERFORM S11-SKRIV-WB1122                                       
015700           IF NOT END-OF-WB0120N PERFORM S01-LAES-WB0120N END-IF          
015800         ELSE                                                             
015900           IF TB1ACCE-KEY-N > TB1ACCE-KEY-G                               
016000*            -- LÄGRE NYCKEL PÅ GAMLA, SKRIV ANNULLATION                  
016100*            -- SKRIV UT EN ANNULL-POST AV DEN GAMLA ARTIKELN             
016300             MOVE WB0120G-AREA      TO UPD-PC0234F2                       
016302*            MOVE UPD-ARTNR         TO W-IDARTNR                          
016303*            PERFORM IMS-GU-WDK601                                        
016304*            IF SEGMENT-FINNS                                             
016306*              MOVE WDK6-ART-TIFINLV  TO WS-TIFINLV-DISP                  
016307*              MOVE WS-TIFINLV-DISP   TO WS-TIFINLV                       
016308*              IF WS-AAVV     < WS-ACTUAL-WEEK                            
014607               MOVE UPD-AOINFTID6-T   TO WS-AOINFTID-T                    
016308               IF WS-AOINFTID4-T < WS-ACTUAL-WEEK                         
016309                 MOVE 'I'             TO UPD-KDANNULL                     
016310               ELSE                                                       
016311                 MOVE 'A'             TO UPD-KDANNULL                     
016312               END-IF                                                     
016313*            ELSE                                                         
016314*              MOVE 'A'               TO UPD-KDANNULL                     
016315*            END-IF                                                       
016316             IF UPD-MDSMARK = 'J'                                         
016317               MOVE 'Y' TO UPD-MDSMARK                                    
016318             END-IF                                                       
016319*          DISPLAY UPD-ARTNR ' DEL ' WS-AAVV ' ' WS-ACTUAL-WEEK           
016400             PERFORM S11-SKRIV-WB1122                                     
016500             IF NOT END-OF-WB0120G PERFORM S02-LAES-WB0120G END-IF        
016600           END-IF                                                         
016700         END-IF                                                           
016800       END-IF                                                             
016900     END-PERFORM                                                          
017000                                                                          
017100                                                                          
017200     PERFORM Z-FINIT                                                      
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT SECTION.                                                          
017900                                                                          
018000     OPEN INPUT  WB0120N                                                  
018100                 WB0120G                                                  
018200                                                                          
018300     OPEN OUTPUT WB1122                                                   
018400     SKIP2                                                                
018500     ACCEPT DAGENS-DATUM  FROM DATE                                       
018510                                                                          
018600     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
018601                                                                          
018610     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
018620     MOVE DAGENS-DATUM     TO DAT-I-TIDATUM                               
018630                                                                          
018640     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
018650                         DAT-O-TIDATUM DAT-KDSVAR                         
018660                                                                          
018670     IF DAT-KDSVAR-OK                                                     
018680       MOVE DAT-TIAAVV-GRP TO WS-ACTUAL-WEEK                              
018700     ELSE                                                                 
018701       STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                  
018702       DELIMITED BY SIZE INTO FELTEXT                                     
018703       CALL FELLOG                                                        
018704     END-IF                                                               
018710     .                                                                    
018800     EJECT                                                                
018810                                                                          
018900 Z-FINIT SECTION.                                                         
019000     CLOSE WB0120N                                                        
019100           WB0120G                                                        
019200           WB1122                                                         
019300     SKIP2                                                                
019400     MOVE 'S' TO POSTSUM-OPKOD                                            
019500     CALL POSTSUM USING POSTSUM-PARM                                      
019600     .                                                                    
019700     EJECT                                                                
019710                                                                          
019800 S01-LAES-WB0120N  SECTION.                                               
019900     READ WB0120N INTO WB0120N-AREA                                       
020000     AT END                                                               
020100        MOVE HIGH-VALUE TO WB0120N-AREA                                   
020200        SET END-OF-WB0120N TO TRUE                                        
020400     NOT AT END                                                           
020900        MOVE 'WB0120N' TO POSTSUM-FDNAMN                                  
021000        MOVE 'WB1122D1' TO POSTSUM-DDNAMN2                                
021100        MOVE 'NYA' TO POSTSUM-TRANSTYP                                    
021200        CALL POSTSUM USING POSTSUM-PARM                                   
021300     END-READ                                                             
021310                                                                          
021400*    --- SPARA ALLTID KOMBINERAD KONTROLL-NYCKEL (ID PÅ TB1ACCE)          
021410     MOVE WB0120N-ARTNR    TO WS-IDARTNR-KEY-N                            
021420     MOVE WB0120N-UPDNR-SU TO WS-UPDNR-SU-KEY-N                           
021500     .                                                                    
021600     EJECT                                                                
021610                                                                          
021700 S02-LAES-WB0120G  SECTION.                                               
021800     READ WB0120G INTO WB0120G-AREA                                       
021900     AT END                                                               
022000        MOVE HIGH-VALUE TO WB0120G-AREA                                   
022100        SET END-OF-WB0120G TO TRUE                                        
022300     NOT AT END                                                           
022800        MOVE 'WB0120G' TO POSTSUM-FDNAMN                                  
022900        MOVE 'WB1122D2' TO POSTSUM-DDNAMN2                                
023000        MOVE 'GAM'    TO POSTSUM-TRANSTYP                                 
023100        CALL POSTSUM USING POSTSUM-PARM                                   
023200     END-READ                                                             
023201                                                                          
023210*    --- SPARA ALLTID KOMBINERAD KONTROLL-NYCKELID PÅ TB1ACCE             
023220     MOVE WB0120G-ARTNR    TO WS-IDARTNR-KEY-G                            
023230     MOVE WB0120G-UPDNR-SU TO WS-UPDNR-SU-KEY-G                           
023300     .                                                                    
023400     EJECT                                                                
023500 S11-SKRIV-WB1122 SECTION.                                                
023600                                                                          
023700     WRITE WB1122-POST FROM UPD-AREA                                      
023800                                                                          
023900     MOVE 'UPD'    TO POSTSUM-TRANSTYP                                    
024000     MOVE 'WB1122' TO POSTSUM-FDNAMN                                      
024100     MOVE 'WB1122D3' TO POSTSUM-DDNAMN2                                   
024200     CALL POSTSUM USING POSTSUM-PARM                                      
024300     .                                                                    
024400     EJECT                                                                
024410 IMS-GU-WDK601 SECTION.                                                   
024420                                                                          
024430     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
024440            DELIMITED BY SIZE INTO SSA1                                   
024450     MOVE '  GE' TO GODK-STATUSKODER                                      
024460     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
024470     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024480     PERFORM DLI-STATUS-CONTROL                                           
024490     .                                                                    
024491 EJECT                                                                    
024492                                                                          
024493 DLI-STATUS-CONTROL SECTION.                                              
024494                                                                          
024495     SET STATUS-IX TO 1                                                   
024496     SEARCH GODK-STATUS                                                   
024497       AT END                                                             
024498         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024499         DELIMITED BY SIZE INTO FELTEXT                                   
024500         CALL FELLOG                                                      
024510       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024600         CONTINUE                                                         
024700     END-SEARCH                                                           
024800     .                                                                    
