000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2712B00.                                                
000400 AUTHOR.         JOHAN NIHLBLAD.                                          
000500 DATE-WRITTEN.   10/11/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*                                                                         
001100*        PROGRAMMET KOMPLETTERAR PASSIVERINGSFIL                          
001200*        MED DATA FRÅN WDK7.                                              
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLART7 (WDK7)                              
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          ---                                                            
003100     SELECT W2712A                     ASSIGN TO W2712BD1.                
003200     SKIP2                                                                
003300*          ---                                                            
003400     SELECT W2712B                     ASSIGN TO W2712BD2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W2712A                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  -COPY W27136      -L.                                                
004400                                                                          
004500                                                                          
004600 FD  W2712B                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W27136 -PRE UT-    -L.                                    
005100                                                                          
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500*    -CHECKED BY WY2000                                                   
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(8)    VALUE 'W2712B00'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  W2712A-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W2712A                       VALUE 'J'.                   
006300                                                                          
006400     EJECT                                                                
006500 01  ERROR-TEXT.                                                          
006600     03  FILLER                  PIC X(8)    VALUE 'ERR-TXT'.             
006700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006800                                                                          
006900 01  ARBETSAREOR.                                                         
007000                                                                          
007100     03 FILLER                   PIC X(8)   VALUE 'IMS-CALL'.             
007200     03 WS-IMS-CALL              PIC X(16)  VALUE SPACE.                  
007300                                                                          
007400     EJECT                                                                
007500                                                                          
007600******************************************************************        
007700*      TABELLER                                                           
007800******************************************************************        
007900                                                                          
008000*                                                                         
008100 01  WS-IDDC-TABELL.                                                      
008200     03 WS-VALID-IDDC  OCCURS 100 INDEXED BY WS-IDDC-IX.                  
008300       05 WS-IDDC             PIC X(2).                                   
008400       05 WS-KDDC             PIC X(2).                                   
009300*                                                                         
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500*                                                                         
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010000     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
010100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010200     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
010300     SKIP2                                                                
010400*    --- PARAMETRAR TILL ABEND                                            
010500                                                                          
010600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010800     SKIP2                                                                
010900 01  FELTEXT.                                                             
011000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL DATKORT                                          
011400*                                                                         
011500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2712B'.              
011600     SKIP2                                                                
011700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011800     SKIP2                                                                
011900*01  -COPY WDATKORT                                                       
012000     EJECT                                                                
012100*******                      PARAMETRAR TILL WORKDAY                      
012200*                                                                         
012300*01  -COPY WORKAREA                                                       
012400     EJECT                                                                
012500*01  -COPY WDATAREA                                                       
012600     EJECT                                                                
012700 01  IN-AREA-START           PIC X(24)   VALUE                            
012800                                 'IN-AREA-START  '.                       
012900     SKIP2                                                                
013000                                                                          
013100*01  AREA -COPY W27136     -PRE IN-                                       
013200     EJECT                                                                
013300 01  UT-AREA-START               PIC X(24)   VALUE                        
013400                                 'UT-AREA-START  '.                       
013500     SKIP2                                                                
013600                                                                          
013700*01  AREA -COPY W27136     -PRE UT-                                       
013800     EJECT                                                                
013900     EJECT                                                                
014000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014100*                                                                         
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014400     SKIP3                                                                
014500 01  NYCKLAR-TILL-DLI.                                                    
014600     03  W-IDARTNR-X.                                                     
014700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014800     03  W-IDDC-X.                                                        
014900         05  W-IDDC              PIC X(02)    VALUE SPACE.                
015000     SKIP2                                                                
015100*    --- STATUS-KOD FRÅN IMS                                              
015200 01  STATUS-WS                   PIC XX.                                  
015300     88  SEGMENT-FINNS                       VALUE '  '.                  
015400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700******************************************************************        
016800*          DLI INPUT - OUTPUT AREA                                        
016900******************************************************************        
017000                                                                          
017100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTS11'.             
017200     SKIP3                                                                
017300 01  DLI-IO-AREA-ARTS11.                                                  
017400*        05  -COPY WDK711                                                 
017500     SKIP3                                                                
017600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDB601'.              
017700 01   DLI-IO-AREA-B601.                                                   
017800*     03  -COPY WDB601                                                    
017900     EJECT                                                                
018000                                                                          
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0009   -PRE MSG-                                              
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE  ARTS-                                             
018700     05  FILLER                  PIC X.                                   
018800*01  -COPY W0008  -PRE  WDB6-                                             
018900     05  FILLER                  PIC X.                                   
019000     EJECT                                                                
019100 PROCEDURE DIVISION  USING MSG-PCB ARTS-PCB WDB6-PCB.                     
019200     ENTRY 'DLITCBL' USING MSG-PCB ARTS-PCB WDB6-PCB.                     
019300                                                                          
019400     PERFORM A-INIT                                                       
019500     PERFORM B-SKAPA-POSTER                                               
019600     PERFORM Z-FINIT                                                      
019700                                                                          
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK                                                               
020000     .                                                                    
020100     EJECT                                                                
020200                                                                          
020300                                                                          
020400 A-INIT SECTION.                                                          
020500                                                                          
020600     OPEN INPUT  W2712A                                                   
020700                                                                          
020800     OPEN OUTPUT W2712B                                                   
020900     PERFORM AA-LADDA-DCTAB                                               
021000     .                                                                    
021100                                                                          
021200 AA-LADDA-DCTAB SECTION.                                                  
021300                                                                          
021400     INITIALIZE WS-IDDC-TABELL                                            
021500     SET WS-IDDC-IX TO +1                                                 
021600     PERFORM IMS-GN-WDB601                                                
021700     PERFORM UNTIL SEGMENT-SLUT                                           
021900        MOVE DCS-IDDC       TO WS-IDDC(WS-IDDC-IX)                        
022000        SET WS-IDDC-IX UP BY +1                                           
022100        IF WS-IDDC-IX > 100                                               
022200          MOVE 'DC-TABELLEN FULL' TO ERROR-TEXT-STR                       
022300          DISPLAY ERROR-TEXT                                              
022400          CALL FELLOG                                                     
022500        END-IF                                                            
022700        PERFORM IMS-GN-WDB601                                             
022800     END-PERFORM                                                          
022900     .                                                                    
023000     EJECT                                                                
023100 B-SKAPA-POSTER SECTION.                                                  
023200     PERFORM S01-LAES-W2712A                                              
023300                                                                          
023400     PERFORM UNTIL END-OF-W2712A                                          
023500       SET WS-IDDC-IX TO +1                                               
023600       MOVE IN-IDARTNR    TO W-IDARTNR                                    
023700       PERFORM UNTIL WS-IDDC(WS-IDDC-IX) = SPACE OR                       
023800                     WS-IDDC-IX > 100                                     
023900         MOVE WS-IDDC(WS-IDDC-IX) TO W-IDDC                               
024000                                     UT-IDDC                              
024100         PERFORM IMS-GU-WDK7-ARTS11                                       
024200         IF SEGMENT-FINNS                                                 
024300           IF SLAG-FLPB-FLYTT = 'N'                                       
024400             MOVE IN-IDARTNR       TO UT-IDARTNR                          
024500             MOVE SLAG-IDPERSON-BUY TO UT-IDPERSON-BUY                    
024600             MOVE IN-KDERS         TO UT-KDERS                            
024700             MOVE SLAG-KDREFSTA    TO UT-KDREFSTA                         
024800             MOVE IN-TIREFSTO-CLAG TO UT-TIREFSTO-CLAG                    
024900             MOVE IN-TIREFSTO-SLAG TO UT-TIREFSTO-SLAG                    
025000             MOVE IN-FLPB-FLYTT    TO UT-FLPB-FLYTT                       
025100             PERFORM S02-SKRIV-W2712B                                     
025200           END-IF                                                         
025300         END-IF                                                           
025400         SET WS-IDDC-IX UP BY +1                                          
025500       END-PERFORM                                                        
025600       PERFORM S01-LAES-W2712A                                            
025700     END-PERFORM                                                          
025800     .                                                                    
025900     EJECT                                                                
026000 Z-FINIT SECTION.                                                         
026100     CLOSE W2712A                                                         
026200           W2712B                                                         
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600                                                                          
026700 S01-LAES-W2712A  SECTION.                                                
026800     READ W2712A INTO IN-AREA                                             
026900     AT END                                                               
027000        SET END-OF-W2712A TO TRUE                                         
027100                                                                          
027200     END-READ                                                             
027300     .                                                                    
027400     EJECT                                                                
027500                                                                          
027600                                                                          
027700 S02-SKRIV-W2712B SECTION.                                                
027800                                                                          
027900     WRITE UT-POST   FROM UT-AREA                                         
028000     .                                                                    
028100     EJECT                                                                
028200                                                                          
028300                                                                          
028400* --- IMS SEKTIONER --   &&&                                              
028500     SKIP3                                                                
028600                                                                          
028700                                                                          
028800 IMS-GU-WDK7-ARTS11 SECTION.                                              
028900                                                                          
029000     MOVE 'GU-ARTS11'        TO WS-IMS-CALL                               
029100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
029200          DELIMITED BY SIZE INTO SSA1                                     
029300     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
029400          DELIMITED BY SIZE INTO SSA2                                     
029500     MOVE '  GE' TO GODK-STATUSKODER                                      
029600     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS11 SSA1 SSA2          
029700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     .                                                                    
030000     SKIP3                                                                
030100                                                                          
030200 IMS-GN-WDB601    SECTION.                                                
030300     MOVE 'GN-WDB601'        TO WS-IMS-CALL                               
030400     MOVE 'WDB601  ' TO SSA1                                              
030500     MOVE '  GB'     TO GODK-STATUSKODER                                  
030600     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
030700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000     EJECT                                                                
031100 IMS-STATUSKONTROLL SECTION.                                              
031200                                                                          
031300     SET STATUS-IX TO 1                                                   
031400     SEARCH GODK-STATUS                                                   
031500       AT END                                                             
031600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
031700         DISPLAY FELTEXT                                                  
031800         CALL FELLOG                                                      
031900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032000         CONTINUE                                                         
032100     END-SEARCH                                                           
032200     .                                                                    
