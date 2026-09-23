000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6127000.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   97/06/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOMMENTAR                                                        
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001100*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001200*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INLEVERANSHISTORIK                                         
002700     SELECT W61263                     ASSIGN TO W61270D1.                
002800     SKIP2                                                                
002900*          --- PASSED ETA'S                                               
003000     SELECT W61267                     ASSIGN TO W61270D2.                
003100     SKIP2                                                                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W61263                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W61263      -L.                                                
004200     SKIP3                                                                
004300 FD  W61267                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W61267  -PRE UT-    -L.                                   
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W6127000'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500                                                                          
005600 77  W61263-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W61263                       VALUE 'J'.                   
005800                                                                          
005900 77  SKRIV-SW                    PIC X       VALUE 'J'.                   
006000     88  SKRIV-EJ                            VALUE 'N'.                   
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700     EJECT                                                                
006800*      --- VALID IDDC CODES                                               
006900*                                                                         
007000*01    -COPY WWDC99                                                       
007100       EJECT                                                              
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
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
009300 01  IN-AREA-START               PIC X(24)   VALUE                        
009400                                 'IN-AREA-START  '.                       
009500*01  AREA -COPY W61263     -PRE IN-                                       
009600     EJECT                                                                
009700 01  UT-AREA-START               PIC X(24)   VALUE                        
009800                                 'UT-AREA-START  '.                       
009900*01  AREA -COPY W61267     -PRE UT-                                       
010000     EJECT                                                                
010100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010200*                                                                         
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  NYCKLAR-TILL-DLI.                                                    
010700     03  W-IDSKYLT-X.                                                     
010800         05  W-IDSKYLT           PIC X(3)    VALUE 'USA'.                 
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011100     03  W-IDDC-X.                                                        
011200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011300     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
013100 01  DLI-IO-WLBENA11.                                                     
013200*    03  -COPY WDD311  -PRE BENA-                                         
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
013400 01  DLI-IO-WLARTC11.                                                     
013500*    03  -COPY WDK611  -PRE ARTC-                                         
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
013700 01  DLI-IO-WLARTS11.                                                     
013800*    03  -COPY WDK711  -PRE ARTS-                                         
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200     EJECT                                                                
014300*01  -COPY W0008  -PRE BENA-                                              
014400     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014600*01  -COPY W0008  -PRE ARTC-                                              
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900*01  -COPY W0008  -PRE ARTS-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING BENA-PCB ARTC-PCB                              
015300     ARTS-PCB.                                                            
015400 MAIN SECTION.                                                            
015500     ENTRY 'DLITCBL' USING BENA-PCB ARTC-PCB                              
015600     ARTS-PCB.                                                            
015700                                                                          
015800                                                                          
015900     PERFORM A-INIT                                                       
016000                                                                          
016100     PERFORM S01-LAES-W61263                                              
016200     PERFORM UNTIL END-OF-W61263                                          
016300                                                                          
016400*      TILLSÄTTNING AV DB-NYCKLAR                                         
016500       MOVE IN-IDARTNR       TO W-IDARTNR                                 
016600       MOVE IN-IDDC          TO W-IDDC                                    
016700                                                                          
016800*      HÄMTA HEM KOMPLETTERANDE UPPGIFTER FRÅN DATABASERNA                
016900       PERFORM IMS-GET-ARTS-SLAG                                          
017000       IF SEGMENT-SAKNAS                                                  
017100         MOVE 'N' TO SKRIV-SW                                             
017200       END-IF                                                             
017300       PERFORM IMS-GET-ARTC-CLAG                                          
017400       IF SEGMENT-SAKNAS                                                  
017500         MOVE 'N' TO SKRIV-SW                                             
017600       END-IF                                                             
017700       PERFORM IMS-GET-BENA-TEXT                                          
017800                                                                          
017900*      TILLDELNING SAMT SKRIFT TILL FIL                                   
018000       IF SKRIV-SW = 'J'                                                  
018100         PERFORM S21-TILLDELNING                                          
018200         PERFORM S11-SKRIV-W61267                                         
018300       END-IF                                                             
018400                                                                          
018500       PERFORM S01-LAES-W61263                                            
018600     END-PERFORM                                                          
018700                                                                          
018800     PERFORM Z-FINIT                                                      
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500                                                                          
019600     OPEN INPUT  W61263                                                   
019700                                                                          
019800     OPEN OUTPUT W61267                                                   
019900                                                                          
020000     ACCEPT DAGENS-DATUM  FROM DATE                                       
020100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020200     .                                                                    
020300     EJECT                                                                
020400 Z-FINIT SECTION.                                                         
020500     CLOSE W61263                                                         
020600           W61267                                                         
020700     SKIP2                                                                
020800     MOVE 'S' TO POSTSUM-OPKOD                                            
020900     CALL POSTSUM USING POSTSUM-PARM                                      
021000     .                                                                    
021100     EJECT                                                                
021200 S01-LAES-W61263 SECTION.                                                 
021300     READ W61263 INTO IN-AREA                                             
021400     AT END                                                               
021500        MOVE HIGH-VALUE TO IN-AREA                                        
021600        SET END-OF-W61263 TO TRUE                                         
021700                                                                          
021800     NOT AT END                                                           
021900        MOVE 'W61263' TO POSTSUM-FDNAMN                                   
022000        MOVE 'W61270D1' TO POSTSUM-DDNAMN2                                
022100        MOVE IN-IDDC TO POSTSUM-TRANSTYP                                  
022200        CALL POSTSUM USING POSTSUM-PARM                                   
022300     END-READ                                                             
022400     .                                                                    
022500     EJECT                                                                
022600 S11-SKRIV-W61267 SECTION.                                                
022700                                                                          
022800     WRITE UT-POST FROM UT-AREA                                           
022900                                                                          
023000     MOVE SPACE TO POSTSUM-TRANSTYP                                       
023100     MOVE 'W61267' TO POSTSUM-FDNAMN                                      
023200     MOVE 'W61270D2' TO POSTSUM-DDNAMN2                                   
023300     CALL POSTSUM USING POSTSUM-PARM                                      
023400     .                                                                    
023500     EJECT                                                                
023600 S21-TILLDELNING SECTION.                                                 
023700                                                                          
023800     SKIP2                                                                
023900     MOVE IN-IDDC           TO UT-IDDC                                    
024000     MOVE IN-IDARTNR        TO UT-IDARTNR                                 
024100     MOVE IN-IDKUNDRF       TO UT-IDKUNDRF                                
024200     MOVE IN-IDLEVNR        TO UT-IDLEVNR                                 
024300     MOVE IN-KVAVIS         TO UT-KVAVIS                                  
024400     MOVE IN-TIBERANK       TO UT-TIBERANK                                
024500                                                                          
024600     MOVE ARTS-SLAG-KVROS-DAG     TO UT-KVROS-DAG                         
024700     MOVE ARTS-SLAG-KVROS-BULK    TO UT-KVROS-BULK                        
024800     MOVE BENA-TEXT-BEART         TO UT-BEART                             
024900                                                                          
025000     MOVE IN-IDDC  TO WS-IDDC                                             
025100     MOVE ARTS-SLAG-IDPERSON-BUY  TO UT-IDPERSON-BUY                      
025200     .                                                                    
025300     EJECT                                                                
025400 S99-ABEND SECTION.                                                       
025500                                                                          
025600     SKIP2                                                                
025700     MOVE 'S' TO POSTSUM-OPKOD                                            
025800     CALL POSTSUM USING POSTSUM-PARM                                      
025900     CALL ABEND USING RKOD-ABEND                                          
026000     .                                                                    
026100     EJECT                                                                
026200* --- IMS SEKTIONER ---                                                   
026300 IMS-GET-BENA-TEXT SECTION.                                               
026400                                                                          
026500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
026600          DELIMITED BY SIZE INTO SSA1                                     
026700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
026800            DELIMITED BY SIZE INTO SSA2                                   
026900     MOVE SPACE TO GODK-STATUSKODER                                       
027000     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2            
027100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
027200     PERFORM IMS-STATUSKONTROLL                                           
027300     .                                                                    
027400     EJECT                                                                
027500 IMS-GET-ARTC-CLAG SECTION.                                               
027600                                                                          
027700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
027800          DELIMITED BY SIZE INTO SSA1                                     
027900     MOVE 'WLARTC11 '         TO SSA2                                     
028000     MOVE '  GE' TO GODK-STATUSKODER                                      
028100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
028200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
028300     PERFORM IMS-STATUSKONTROLL                                           
028400     .                                                                    
028500     EJECT                                                                
028600 IMS-GET-ARTS-SLAG SECTION.                                               
028700                                                                          
028800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
028900          DELIMITED BY SIZE INTO SSA1                                     
029000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
029100          DELIMITED BY SIZE INTO SSA2                                     
029200     MOVE '  GE' TO GODK-STATUSKODER                                      
029300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
029400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSKONTROLL                                           
029600                                                                          
029700     .                                                                    
029800     EJECT                                                                
029900 IMS-STATUSKONTROLL SECTION.                                              
030000                                                                          
030100     SET STATUS-IX TO 1                                                   
030200     SEARCH GODK-STATUS                                                   
030300       AT END                                                             
030400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030500           DELIMITED BY SIZE INTO FELTEXT                                 
030600         DISPLAY FELTEXT                                                  
030700         DISPLAY 'FELAKTIGT ARTNR: ' IN-IDARTNR                           
030800         CALL FELLOG                                                      
030900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031000         CONTINUE                                                         
031100     END-SEARCH                                                           
031200     .                                                                    
