000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3355800.                                                
000300 AUTHOR.         KIHLBERG STEFAN.                                         
000400 DATE-WRITTEN.   06/01/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPAR FIL MED FÖRSÄLJNINGSPRIS FÖR SAMTLIGA ARTIKLAR FÖR        
000900*        VSG-ESDIC                                                        
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- FIL MED ARTIKELUPPGIFTER                                   
002500     SELECT W91042                     ASSIGN TO W33558D1.                
002600     SKIP2                                                                
002700*          --- FIL MED ARTIKLAR OCH PRISER TILL VSG-ESDIC                 
002800     SELECT W33559                     ASSIGN TO W33558D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W91042                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W91042      -L.                                                
003900     EJECT                                                                
004000 FD  W33559                                                               
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS 0.                                                    
004300     SKIP2                                                                
004400 01  UT-POST  PIC X(80).                                                  
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W3355800'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  W91042-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W91042                       VALUE 'J'.                   
005400     EJECT                                                                
005500 01  DAGENS-DATUM-VECKA-DAG      PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM-VECKA-DAG.                             
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-VECKA      PIC 9(2).                                
005900     03  DAGENS-DATUM-DAGNR      PIC 9(1).                                
006000     03  DAGENS-DATUM-DAGNR-2    PIC 9(1).                                
006100     EJECT                                                                
006200 01  ARBETSAREOR.                                                         
006300                                                                          
006400     03 WS-IDDISTR               PIC S9(5)  VALUE 01291.                  
006500     03 WS-IDKUNDNR              PIC S9(7)  VALUE ZERO.                   
006600     03 WS-IDDC                  PIC X(2)   VALUE '11'.                   
006700     03 WS-KDORDKL               PIC S9     VALUE 1.                      
006800     03 WS-ANTAL-POSTER          PIC 9(7)   VALUE ZERO.                   
006900                                                                          
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007700     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
007800     SKIP2                                                                
007900*    --- PARAMETRAR TILL ABEND                                            
008000                                                                          
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008400     SKIP2                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL POSTSUM                                          
009000*                                                                         
009100*01  -COPY W0005   -PRE  POSTSUM-                                         
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL DATKORT                                          
009400*                                                                         
009500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27130'.              
009600     SKIP2                                                                
009700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009800     SKIP2                                                                
009900*01  -COPY WDATKORT                                                       
010000*01  -COPY W335PRIS                                                       
010100     EJECT                                                                
010200 01  IN-AREA-START               PIC X(24)   VALUE                        
010300                                 'IN-AREA-START  '.                       
010400     SKIP2                                                                
010500                                                                          
010600*01  AREA -COPY W91042     -PRE IN-                                       
010700     EJECT                                                                
010800 01  FILLER            PIC X(16)   VALUE 'UT-AREA-DATA  '.                
010900 01  UT-AREA-DATA.                                                        
011000                                                                          
011100*    03  FILLER -COPY W33559 -PRE UT-    REDEFINES UT-AREA-DATA           
011200                                                                          
011300 01  FILLER            PIC X(16)   VALUE 'UT-AREA-START '.                
011400                                                                          
011500 01  UT-AREA-START.                                                       
011600*    03  FILLER -COPY W461RI0N     REDEFINES UT-AREA-START                
011700                                                                          
011800 01  FILLER            PIC X(16)   VALUE 'UT-AREA-END   ' .               
011900                                                                          
012000 01  UT-AREA-END.                                                         
012100*    03  FILLER -COPY W461RI9N     REDEFINES UT-AREA-END                  
012200                                                                          
012300     EJECT                                                                
012400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012500*                                                                         
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012800     SKIP3                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700                                                                          
014800 01  PRIS-ARTC-PCB               PIC X.                                   
014810 01  PRIS-WDK7-PCB               PIC X.                                   
014900 01  PRIS-GMTA-PCB               PIC X.                                   
015000 01  PRIS-BETA-PCB               PIC X.                                   
015100 01  PRIS-GPRIA-PCB              PIC X.                                   
015200 01  PRIS-GPRIB-PCB              PIC X.                                   
015300 01  PRIS-COST-WDK6-PCB          PIC X.                                   
015400 01  PRIS-COST-WDK7-PCB          PIC X.                                   
015500 01  PRIS-COST-WDF1-PCB          PIC X.                                   
015600 01  PRIS-COST-9305-PCB          PIC X.                                   
015700 01  PRIS-COST-WDK72-PCB         PIC X.                                   
015710 01  PRIS-COST-WDB6-PCB          PIC X.                                   
015800                                                                          
015900     EJECT                                                                
016000 PROCEDURE DIVISION  USING                                                
016100      PRIS-ARTC-PCB PRIS-WDK7-PCB                                         
016110      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
016200      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
016300      PRIS-COST-WDK6-PCB                                                  
016400      PRIS-COST-WDK7-PCB                                                  
016500      PRIS-COST-WDF1-PCB                                                  
016600      PRIS-COST-9305-PCB                                                  
016610      PRIS-COST-WDK72-PCB                                                 
016620      PRIS-COST-WDB6-PCB.                                                 
016700 MAIN SECTION.                                                            
016800     ENTRY 'DLITCBL' USING                                                
016900      PRIS-ARTC-PCB PRIS-WDK7-PCB                                         
016910      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
017000      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
017100      PRIS-COST-WDK6-PCB                                                  
017200      PRIS-COST-WDK7-PCB                                                  
017300      PRIS-COST-WDF1-PCB                                                  
017400      PRIS-COST-9305-PCB                                                  
017410      PRIS-COST-WDK72-PCB                                                 
017420      PRIS-COST-WDB6-PCB.                                                 
017600                                                                          
017700     PERFORM A-INIT                                                       
017800                                                                          
017900     PERFORM S20-SKAPA-STARTPOST                                          
018000     INITIALIZE UT-POST                                                   
018100     PERFORM S01-LAES-W91042                                              
018200     PERFORM UNTIL END-OF-W91042                                          
018300        IF IN-KDERS-UTG = ZERO AND IN-KDERS < 20                          
018400           MOVE 1            TO PRIS-KDCALL                               
018410           MOVE IDPGM        TO PRIS-IDPGM                                
018500           MOVE IN-IDARTNR   TO PRIS-IDARTNR                              
018600           MOVE WS-IDDISTR   TO PRIS-IDDISTR                              
018700           MOVE WS-IDKUNDNR  TO PRIS-IDKUNDNR                             
018800           MOVE WS-IDDC      TO PRIS-IDDC                                 
018900           MOVE WS-KDORDKL   TO PRIS-KDORDKL                              
019000           MOVE NEJ          TO PRIS-FLINVEST                             
019100                                                                          
019200           CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                
019210                               PRIS-WDK7-PCB                              
019300                               PRIS-GMTA-PCB PRIS-BETA-PCB                
019400                               PRIS-GPRIA-PCB PRIS-GPRIB-PCB              
019500                               PRIS-COST-WDK6-PCB                         
019600                               PRIS-COST-WDK7-PCB                         
019700                               PRIS-COST-WDF1-PCB                         
019800                               PRIS-COST-9305-PCB                         
019810                               PRIS-COST-WDK72-PCB                        
019820                               PRIS-COST-WDB6-PCB                         
019900                                                                          
020000           IF PRIS-KDSVAR = '2'                                           
020100              MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'          
020200                                    TO FELTEXT                            
020300              CALL ABEND USING RKOD-ABEND                                 
020400           END-IF                                                         
020500                                                                          
020600           MOVE IN-IDARTNR     TO UT-IDARTNR                              
020700           MOVE IN-BEART(4)    TO UT-BEART                                
020800           MOVE PRIS-PRARTNTO  TO UT-PRARTNTO                             
020900           PERFORM S11-SKRIV-W33559                                       
021000           COMPUTE WS-ANTAL-POSTER =                                      
021100                   WS-ANTAL-POSTER + 1                                    
021200        END-IF                                                            
021300                                                                          
021400        PERFORM S01-LAES-W91042                                           
021500     END-PERFORM                                                          
021600     PERFORM S21-SKAPA-END-POST                                           
021700                                                                          
021800     PERFORM Z-FINIT                                                      
021900                                                                          
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500                                                                          
022600     OPEN INPUT  W91042                                                   
022700                                                                          
022800     OPEN OUTPUT W33559                                                   
022900                                                                          
023000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
023100     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
023200     MOVE D-VECKA     TO DAGENS-DATUM-VECKA                               
023300     MOVE D-DAGNR     TO DAGENS-DATUM-DAGNR                               
023400     MOVE ZERO        TO DAGENS-DATUM-DAGNR-2                             
023500                                                                          
023600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023700     MOVE ZERO               TO WS-ANTAL-POSTER                           
023800     .                                                                    
023900     EJECT                                                                
024000 Z-FINIT SECTION.                                                         
024100     CLOSE W91042                                                         
024200           W33559                                                         
024300     SKIP2                                                                
024400     MOVE 'S' TO POSTSUM-OPKOD                                            
024500     CALL POSTSUM USING POSTSUM-PARM                                      
024600     .                                                                    
024700     EJECT                                                                
024800 S01-LAES-W91042  SECTION.                                                
024900     READ W91042 INTO IN-AREA                                             
025000     AT END                                                               
025100        MOVE HIGH-VALUE TO IN-AREA                                        
025200        SET END-OF-W91042 TO TRUE                                         
025300                                                                          
025400     NOT AT END                                                           
025500        MOVE 'W91042' TO POSTSUM-FDNAMN                                   
025600        MOVE 'W33558D1' TO POSTSUM-DDNAMN2                                
025700        MOVE SPACE     TO POSTSUM-TRANSTYP                                
025800        CALL POSTSUM USING POSTSUM-PARM                                   
025900     END-READ                                                             
026000     .                                                                    
026100     EJECT                                                                
026200 S11-SKRIV-W33559 SECTION.                                                
026300                                                                          
026400     WRITE UT-POST FROM UT-AREA-DATA                                      
026500                                                                          
026600     MOVE 'W33559' TO POSTSUM-FDNAMN                                      
026700     MOVE 'W33558D2' TO POSTSUM-DDNAMN2                                   
026800     CALL POSTSUM USING POSTSUM-PARM                                      
026900     .                                                                    
027000     EJECT                                                                
027100                                                                          
027200 S20-SKAPA-STARTPOST SECTION.                                             
027300                                                                          
027400     INITIALIZE                         UT-POST                           
027500     MOVE 'STA'                      TO START-IDPTYP                      
027600     MOVE WS-IDDISTR                 TO START-IDDISTR                     
027700     MOVE WS-IDDC                    TO START-IDDC                        
027800     MOVE DAGENS-DATUM-VECKA-DAG     TO START-TIFILDAT                    
027900     MOVE FUNCTION CURRENT-DATE(9:6) TO START-TIHHMMSS                    
028000     MOVE SPACE                      TO START-FILLERX59                   
028100     WRITE UT-POST FROM UT-AREA-START                                     
028200                                                                          
028300     MOVE 'RI0'                 TO POSTSUM-FDNAMN                         
028400     CALL POSTSUM      USING POSTSUM-PARM                                 
028500                                                                          
028600     .                                                                    
028700     EJECT                                                                
028800                                                                          
028900                                                                          
029000 S21-SKAPA-END-POST SECTION.                                              
029100                                                                          
029200     INITIALIZE                     UT-POST                               
029300     MOVE 'END'                  TO SLUT-IDPTYP                           
029400     MOVE WS-ANTAL-POSTER        TO SLUT-KVTRANS-002                      
029500     WRITE UT-POST FROM UT-AREA-END                                       
029600                                                                          
029700     MOVE 'RI9'                 TO POSTSUM-FDNAMN                         
029800     CALL POSTSUM      USING POSTSUM-PARM                                 
029900     .                                                                    
030000     EJECT                                                                
