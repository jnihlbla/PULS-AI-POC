000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2719700.                                                
000400 AUTHOR.         JOHAN NIHLBLAD.                                          
000500 DATE-WRITTEN.   10/11/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        READS DB WDD7 WITH SB.                                           
001000*        GETS SUPERSEDING PARTNO TIFINLV(IF MORE THAN ONE                 
001100*        WE USE THE ON CLOSEST IN TIME.                                   
001200*        WE DO THIS FOR X2,X3,X5 AND X6 SUPERSESSIONS                     
001210*        (TIEM DEPENDENT SUPERSESSIONS)                                   
001220*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500     SELECT W27197                     ASSIGN TO W27197D1.                
002600     SKIP2                                                                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W27197                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W27197 -PRE  UT-  -L.                                     
003700     SKIP3                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W2719700'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  SW-BEHANDLA                 PIC X       VALUE 'N'.                   
004510 77  SW-TILLK                    PIC X       VALUE 'N'.                   
004600 77  WS-TIFINLV                  PIC 9(7)    VALUE ZERO.                  
004700 77  WS-KDERS                    PIC 9(2)    VALUE ZERO.                  
004800                                                                          
004900                                                                          
005000 01  ARBETSAREOR.                                                         
005100     03  WS-SPAR-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.           
005200     03  WS-JAMF-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.           
005300     03  IX                      PIC S9(1)   VALUE ZERO COMP-3.           
005400                                                                          
005500                                                                          
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200                                                                          
006210 01  WS-ART-TIFINLV              PIC 9(7)    VALUE ZERO.                  
006220 01  FILLER REDEFINES WS-ART-TIFINLV.                                     
006230     03  WS-ART-TIFINLV-SEKEL    PIC 9(2).                                
006240     03  WS-ART-TIFINLV-AAVVD    PIC 9(5).                                
006300                                                                          
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     SKIP2                                                                
007200*    --- PARAMETRAR TILL ABEND                                            
007300                                                                          
007400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007600     SKIP2                                                                
007700 01  FELTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000     EJECT                                                                
008100                                                                          
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300                                                                          
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  UT-AREA-START           PIC X(24)   VALUE                            
008700                                 'UT-AREA-START  '.                       
008800     SKIP2                                                                
008900                                                                          
009000*01  AREA -COPY W27197     -PRE UT-                                       
009100                                                                          
009200                                                                          
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  NYCKLAR-TILL-DLI.                                                    
010000     03  W-IDARTNR-X.                                                     
010100        05 W-IDARTNR             PIC S9(9)  COMP-3 VALUE ZERO.            
010200                                                                          
010300     03  W-IDARTNR-TILLK.                                                 
010400        05 W-IDARTNR-TILL        PIC S9(9)  COMP-3.                       
010500                                                                          
010600     03  W-KDSEGKEY-X.                                                    
010700         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
010800                                                                          
010900     EJECT                                                                
011000                                                                          
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012600     EJECT                                                                
012700*    ---  DLI INPUT-OUTPUT AREA                                           
012800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012900     SKIP3                                                                
013000 01  DLI-IO-AREA.                                                         
013100     03  IO-AREA                 PIC X(800)  VALUE SPACE.                 
013200                                                                          
013300     03  WDD701    REDEFINES  IO-AREA.                                    
013400*        05  -COPY WDD701                                                 
013500                                                                          
013600     03  WDD702    REDEFINES  IO-AREA.                                    
013700*        05  -COPY WDD702                                                 
013800                                                                          
013900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
014000 01  DLI-IO-WDK601.                                                       
014100*    03  -COPY WDK601                                                     
014200     EJECT                                                                
014300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
014400 01  DLI-IO-WDK611.                                                       
014500*    03  -COPY WDK611                                                     
014600     EJECT                                                                
014700                                                                          
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000                                                                          
015100     EJECT                                                                
015200*01  -COPY W0008  -PRE WDD7-                                              
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500*01  -COPY W0008  -PRE WDK6-                                              
015600     05  FILLER                  PIC X.                                   
015700     EJECT                                                                
015800 PROCEDURE DIVISION  USING WDD7-PCB WDK6-PCB.                             
015900     ENTRY 'DLITCBL' USING WDD7-PCB WDK6-PCB.                             
016000                                                                          
016100     PERFORM A-INIT                                                       
016200     PERFORM IMS-GET-WDD7                                                 
016300     PERFORM UNTIL SEGMENT-SLUT                                           
016400        EVALUATE WDD7-SEG-NAME-FB                                         
016500           WHEN 'WDD701  '                                                
016502              IF IDARTNR NOT = W-IDARTNR                                  
016503              AND W-IDARTNR NOT = ZERO                                    
016504                IF WS-TIFINLV < 9999999                                   
016505                  PERFORM S11-SKRIV-W27197                                
016506                END-IF                                                    
016507              END-IF                                                      
016600              MOVE IDARTNR      TO W-IDARTNR                              
016700                                   UT-IDARTNR                             
016710              MOVE 9999999      TO WS-TIFINLV                             
016800              MOVE NEJ          TO SW-BEHANDLA                            
016900              PERFORM IMS-GU-WDK611                                       
017000              IF SEGMENT-FINNS                                            
017100                MOVE JA         TO SW-BEHANDLA                            
017300                MOVE CLAG-KDERS TO WS-KDERS                               
017310                                   UT-KDERS                               
017400              END-IF                                                      
017500           WHEN 'WDD702  '                                                
017510              IF SW-BEHANDLA = JA                                         
017600                IF FLTEXT = 'N'                                           
017700                  PERFORM C-BEHANDLA-POST                                 
017710                END-IF                                                    
017720              END-IF                                                      
017800         END-EVALUATE                                                     
018200         PERFORM IMS-GET-WDD7                                             
018300     END-PERFORM                                                          
018301     IF W-IDARTNR > ZERO                                                  
018302     AND SEGMENT-SLUT                                                     
018303       IF WS-TIFINLV < 9999999                                            
018310         PERFORM S11-SKRIV-W27197                                         
018320       END-IF                                                             
018330     END-IF                                                               
018400     PERFORM Z-FINIT                                                      
018500     MOVE ZERO TO RETURN-CODE                                             
018600     GOBACK                                                               
018700     .                                                                    
018800     EJECT                                                                
018900                                                                          
019000 A-INIT SECTION.                                                          
019100                                                                          
019200     OPEN OUTPUT W27197                                                   
019300                                                                          
019400     ACCEPT DAGENS-DATUM  FROM DATE                                       
019500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019600     .                                                                    
019700     EJECT                                                                
019800                                                                          
019900 C-BEHANDLA-POST SECTION.                                                 
020000                                                                          
020200       IF WS-KDERS(2:1) = 2 OR 3 OR 5 OR 6                                
020300         MOVE IDARTNR-TILLK       TO W-IDARTNR-TILL                       
020400         PERFORM IMS-GU-WDK601-TILLK                                      
020500         IF SEGMENT-FINNS                                                 
020510           IF ART-TIFINLV > 50000                                         
020520             MOVE 19           TO WS-ART-TIFINLV-SEKEL                    
020530             MOVE ART-TIFINLV  TO WS-ART-TIFINLV-AAVVD                    
020540           ELSE                                                           
020550             MOVE 20           TO WS-ART-TIFINLV-SEKEL                    
020560             MOVE ART-TIFINLV  TO WS-ART-TIFINLV-AAVVD                    
020570           END-IF                                                         
020600           IF WS-ART-TIFINLV < WS-TIFINLV                                 
020700             MOVE WS-ART-TIFINLV TO WS-TIFINLV                            
020710           END-IF                                                         
020900         END-IF                                                           
021000       END-IF                                                             
021200     .                                                                    
021300     EJECT                                                                
021400                                                                          
021500                                                                          
021600                                                                          
021700 Z-FINIT SECTION.                                                         
021800     CLOSE W27197                                                         
021900     SKIP2                                                                
022000     MOVE 'S' TO POSTSUM-OPKOD                                            
022100     CALL POSTSUM USING POSTSUM-PARM                                      
022200     .                                                                    
022300     EJECT                                                                
022400                                                                          
022500                                                                          
022600 S11-SKRIV-W27197 SECTION.                                                
022700                                                                          
022800     MOVE WS-TIFINLV(3:5)   TO UT-TISTOREF                                
022900     WRITE UT-POST FROM UT-AREA                                           
023000                                                                          
023100     MOVE 'W27197' TO POSTSUM-FDNAMN                                      
023200     MOVE 'W27197D1' TO POSTSUM-DDNAMN2                                   
023300     CALL POSTSUM USING POSTSUM-PARM                                      
023400     .                                                                    
023500     EJECT                                                                
023600                                                                          
023700                                                                          
023800* --- IMS SEKTIONER ---                                                   
023900     SKIP3                                                                
024000     EJECT                                                                
024100                                                                          
024200                                                                          
024300 IMS-GET-WDD7   SECTION.                                                  
024400                                                                          
024500     CALL CBLTDLI USING GN WDD7-PCB DLI-IO-AREA                           
024600     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
024700     MOVE '  GAGKGBGE' TO GODK-STATUSKODER                                
024800     PERFORM IMS-STATUSKONTROLL                                           
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-GU-WDK611 SECTION.                                                   
025200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
025300          DELIMITED BY SIZE INTO SSA1                                     
025400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
025500          DELIMITED BY SIZE INTO SSA2                                     
025600     MOVE '  GE' TO GODK-STATUSKODER                                      
025700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
025800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025900     PERFORM IMS-STATUSKONTROLL                                           
026000     .                                                                    
026100     EJECT                                                                
026200 IMS-GU-WDK601-TILLK SECTION.                                             
026300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-TILLK ')'                     
026400          DELIMITED BY SIZE INTO SSA1                                     
026500     MOVE '  GE' TO GODK-STATUSKODER                                      
026600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
026700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026800     PERFORM IMS-STATUSKONTROLL                                           
026900     .                                                                    
027000     EJECT                                                                
027100 IMS-STATUSKONTROLL SECTION.                                              
027200                                                                          
027300     SET STATUS-IX TO 1                                                   
027400     SEARCH GODK-STATUS                                                   
027500       AT END                                                             
027600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
027700         DISPLAY FELTEXT                                                  
027800         CALL FELLOG                                                      
027900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028000         CONTINUE                                                         
028100     END-SEARCH                                                           
028200     .                                                                    
028300     EJECT                                                                
