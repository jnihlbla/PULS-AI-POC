000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9102500.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   11/11/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOMPLETTERAR LISTA ERSÄTTNINGAR TILL ERC                         
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDK6                                       
001100*        PROGRAMMET LÄSER      WDD7                                       
001200*                                                                         
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
002500*          --- ERSÄTTNINGAR                                               
002600     SELECT W91025                     ASSIGN TO W91025D1.                
002700     SKIP2                                                                
002800*          --- LISTFIL ERC ERSÄTTNINGAR                                   
002900     SELECT W91026                     ASSIGN TO W91025D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W91025                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W91024      -L.                                                
004000     SKIP3                                                                
004100 FD  W91026                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W91026 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W9102500'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W91025-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W91025                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  IN-AREA-START               PIC X(24)   VALUE                        
008400                                 'IN-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W91024     -PRE IN-                                       
008800     EJECT                                                                
008900 01  UT-AREA-START               PIC X(24)   VALUE                        
009000                                 'UT-AREA-START  '.                       
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W91026     -PRE UT-                                       
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600*                                                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-IDARTNR-X.                                                     
010200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010300     03  W-KDSEGKEY-X.                                                    
010400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
010500     03  W-IDKORTNR-X.                                                    
010600         05  W-IDKORTNR          PIC S9(3)   VALUE ZERO COMP-3.           
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012500 01  DLI-IO-WDK601.                                                       
012600*    03  -COPY WDK601                                                     
012700     EJECT                                                                
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012900 01  DLI-IO-WDK611.                                                       
013000*    03  -COPY WDK611                                                     
013100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD701'.                      
013200 01  DLI-IO-WDD701.                                                       
013300*    03  -COPY WDD701                                                     
013400     EJECT                                                                
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD702'.                      
013600 01  DLI-IO-WDD702.                                                       
013700*    03  -COPY WDD702                                                     
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100                                                                          
014200*01  -COPY W0008  -PRE WDK6-                                              
014300     05  FILLER                  PIC X.                                   
014400                                                                          
014500*01  -COPY W0008  -PRE WDD7-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING WDK6-PCB WDD7-PCB.                             
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING WDK6-PCB WDD7-PCB.                             
015100                                                                          
015200                                                                          
015300     PERFORM A-INIT                                                       
015400                                                                          
015500     PERFORM S01-LAES-W91025                                              
015600     PERFORM UNTIL END-OF-W91025                                          
015700       MOVE IN-IDARTNR   TO W-IDARTNR                                     
015800       PERFORM IMS-GET-WDK601                                             
015900       IF SEGMENT-FINNS                                                   
016000          IF ART-KDERS-UTG = ZERO                                         
016100             PERFORM IMS-GET-WDK611                                       
016200             IF SEGMENT-FINNS                                             
016300*************IF CLAG-KDERS < 20 OR ART-TIERSDAT = ZERO                    
016400                IF ART-TIERSDAT = ZERO                                    
016500                   PERFORM B-BORT-ERS                                     
016600                ELSE                                                      
016700                   PERFORM C-ERS-INFO                                     
016800                END-IF                                                    
016900             END-IF                                                       
016901          ELSE                                                            
016902             PERFORM C-ERS-INFO                                           
016910          END-IF                                                          
017000       END-IF                                                             
017100       PERFORM S01-LAES-W91025                                            
017200     END-PERFORM                                                          
017300                                                                          
017400                                                                          
017500     PERFORM Z-FINIT                                                      
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 A-INIT SECTION.                                                          
018200                                                                          
018300     OPEN INPUT  W91025                                                   
018400                                                                          
018500     OPEN OUTPUT W91026                                                   
018600                                                                          
018700     ACCEPT DAGENS-DATUM  FROM DATE                                       
018800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018900     .                                                                    
019000     EJECT                                                                
019100 B-BORT-ERS SECTION.                                                      
019200                                                                          
019300*    SKRIV UTFIL MED BLANKT  ERS INFO                                     
019400     MOVE ART-IDARTNR    TO UT-IDARTNR                                    
019500     MOVE ZERO           TO UT-IDARTNR-TILLK                              
019600     MOVE ZERO           TO UT-DIERS-TILLK                                
019700     MOVE CLAG-KDERS     TO UT-KDERS                                      
019800     MOVE ZERO           TO UT-TIERSDAT                                   
019900     MOVE ART-FLERS      TO UT-FLERS                                      
020000     PERFORM S11-SKRIV-W91026                                             
020100     .                                                                    
020200     EJECT                                                                
020300 C-ERS-INFO SECTION.                                                      
020400                                                                          
020500     PERFORM IMS-GET-WDD701                                               
020600     IF SEGMENT-FINNS                                                     
020700        PERFORM IMS-GET-WDD702                                            
020800        PERFORM UNTIL SEGMENT-SAKNAS                                      
020900           MOVE ART-IDARTNR      TO UT-IDARTNR                            
021000           MOVE IDARTNR-TILLK    TO UT-IDARTNR-TILLK                      
021100           MOVE DIERS-TILLK      TO UT-DIERS-TILLK                        
021110           IF ART-KDERS-UTG = ZERO                                        
021200              MOVE CLAG-KDERS    TO UT-KDERS                              
021210           ELSE                                                           
021211              MOVE ART-KDERS-UTG TO UT-KDERS                              
021220           END-IF                                                         
021300           MOVE ART-TIERSDAT     TO UT-TIERSDAT                           
021400           MOVE ART-FLERS        TO UT-FLERS                              
021500           PERFORM S11-SKRIV-W91026                                       
021600           PERFORM IMS-GET-WDD702                                         
021700        END-PERFORM                                                       
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 Z-FINIT SECTION.                                                         
022200     CLOSE W91025                                                         
022300           W91026                                                         
022400     SKIP2                                                                
022500     MOVE 'S' TO POSTSUM-OPKOD                                            
022600     CALL POSTSUM USING POSTSUM-PARM                                      
022700     .                                                                    
022800     EJECT                                                                
022900 S01-LAES-W91025  SECTION.                                                
023000     READ W91025 INTO IN-AREA                                             
023100     AT END                                                               
023200        MOVE HIGH-VALUE TO IN-AREA                                        
023300        SET END-OF-W91025 TO TRUE                                         
023400                                                                          
023500     NOT AT END                                                           
023600        MOVE 'W91025'   TO POSTSUM-FDNAMN                                 
023700        MOVE 'W91025D1' TO POSTSUM-DDNAMN2                                
023800        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
023900        CALL POSTSUM USING POSTSUM-PARM                                   
024000     END-READ                                                             
024100     .                                                                    
024200     EJECT                                                                
024300 S11-SKRIV-W91026 SECTION.                                                
024400                                                                          
024500     WRITE UT-POST FROM UT-AREA                                           
024600                                                                          
024700     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
024800     MOVE 'W91026'   TO POSTSUM-FDNAMN                                    
024900     MOVE 'W91025D2' TO POSTSUM-DDNAMN2                                   
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300 S99-ABEND SECTION.                                                       
025400                                                                          
025500     SKIP2                                                                
025600     MOVE 'S' TO POSTSUM-OPKOD                                            
025700     CALL POSTSUM USING POSTSUM-PARM                                      
025800     CALL ABEND USING RKOD-ABEND                                          
025900     .                                                                    
026000     EJECT                                                                
026100* --- IMS SEKTIONER ---                                                   
026200                                                                          
026300 IMS-GET-WDK601 SECTION.                                                  
026400                                                                          
026500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
026600          DELIMITED BY SIZE INTO SSA1                                     
026700     MOVE '  GE' TO GODK-STATUSKODER                                      
026800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
026900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     .                                                                    
027200     EJECT                                                                
027300 IMS-GET-WDK611 SECTION.                                                  
027400                                                                          
027500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
027600          DELIMITED BY SIZE INTO SSA1                                     
027700     MOVE '  GE' TO GODK-STATUSKODER                                      
027800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
027900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028000     PERFORM IMS-STATUSKONTROLL                                           
028100     .                                                                    
028200     EJECT                                                                
028300 IMS-GET-WDD701 SECTION.                                                  
028400                                                                          
028500     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
028600          DELIMITED BY SIZE INTO SSA1                                     
028700     MOVE '  GE' TO GODK-STATUSKODER                                      
028800     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
028900     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200     EJECT                                                                
029300 IMS-GET-WDD702 SECTION.                                                  
029400                                                                          
029500*****STRING 'WDD702  (IDKORTNR =' W-IDKORTNR-X ')'                        
029600     STRING 'WDD702     '                                                 
029700          DELIMITED BY SIZE INTO SSA1                                     
029800     MOVE '  GE' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
030000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     EJECT                                                                
030400 IMS-STATUSKONTROLL SECTION.                                              
030500                                                                          
030600     SET STATUS-IX TO 1                                                   
030700     SEARCH GODK-STATUS                                                   
030800       AT END                                                             
030900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031000           DELIMITED BY SIZE INTO FELTEXT                                 
031100         DISPLAY FELTEXT                                                  
031200         CALL FELLOG                                                      
031300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031400         CONTINUE                                                         
031500     END-SEARCH                                                           
031600     .                                                                    
