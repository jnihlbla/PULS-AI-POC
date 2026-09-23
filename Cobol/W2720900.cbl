000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2720900.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   15/11/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPAR REFILLFÖRSLAG VID ÄNDRING AV VISSA                        
001000*        ERSÄTTNINGSKODER, SKAPAR FIL TILL PGM W2711100                   
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FIL MED ÄNDRADE ERSKODER                                   
002100     SELECT W27208                     ASSIGN TO W27209D1.                
002200     EJECT                                                                
002300*          --- FIL MED REFILLFÖRSLAG TILL W271P811                        
002400     SELECT W27209                     ASSIGN TO W27209D2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W27208                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W27208      -PRE  IN-  -L.                                     
003500     EJECT                                                                
003600 FD  W27209                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W27111 -PRE  UT-  -L.                                     
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2720900'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900     SKIP2                                                                
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400 77  W27208-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W27208                       VALUE 'J'.                   
005600                                                                          
005700     EJECT                                                                
005800 01  ARBETSAREOR.                                                         
005900                                                                          
006000     03 WS-ANTAL-W27209          PIC 9(8)   VALUE ZERO.                   
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800 01  IN-AREA-START               PIC X(24)   VALUE                        
007900                                             'IN-AREA-START'.             
008000     SKIP2                                                                
008100                                                                          
008200*01  AREA -COPY W27208     -PRE IN-                                       
008300*                                                                         
008400     EJECT                                                                
008500 01  UT-AREA-START           PIC X(24)   VALUE                            
008600                                 'UT-AREA-START  '.                       
008700                                                                          
008800                                                                          
008900*01  AREA -COPY W27111     -PRE UT-                                       
009000                                                                          
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  NYCKLAR-TILL-DLI.                                                    
009400     03  W-IDARTNR-X.                                                     
009500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009600     03  W-KDSEGKEY-X.                                                    
009700         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
009800     03  W-IDDC-B6-X.                                                     
009900         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
010000                                                                          
010100     03  W-IDDC-X.                                                        
010200         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
010300                                                                          
010400     03  W-IDDC-B616-X.                                                   
010500         05  W-IDDC-B616         PIC X(2)   VALUE SPACE.                  
010600     SKIP2                                                                
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FINNS                       VALUE '  '.                  
011000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011300     88  IMS-EJ-OK                           VALUE 'XD'.                  
011400     SKIP2                                                                
011500 01  GODK-STATUSKODER.                                                    
011600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01  SSA1                        PIC X(64).                               
011900 01  SSA2                        PIC X(64).                               
012100     EJECT                                                                
012200*    --- IMS FUNKTIONSKODER                                               
012300*01  -COPY W0003                                                          
012400     EJECT                                                                
012500*    ---  DLI INPUT-OUTPUT AREA                                           
012600                                                                          
012700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
012800 01  DLI-IO-WDK601.                                                       
012900*    03  -COPY WDK601                                                     
013000     EJECT                                                                
013100                                                                          
013200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
013300 01  DLI-IO-WDK611.                                                       
013400*    03  -COPY WDK611                                                     
013500     EJECT                                                                
013600                                                                          
013700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
013800 01  DLI-IO-WDK629.                                                       
013900*    03  -COPY WDK629                                                     
014000     EJECT                                                                
014100                                                                          
014200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
014300 01  DLI-IO-WDK711.                                                       
014400*    03  -COPY WDK711                                                     
014500     EJECT                                                                
014600                                                                          
014700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB616'.                      
014800 01  DLI-IO-WDB616.                                                       
014900*    03  -COPY WDB616                                                     
015000     EJECT                                                                
015100                                                                          
015200     EJECT                                                                
015300 LINKAGE SECTION.                                                         
015400                                                                          
015500*01  -COPY W0009   -PRE MSG-                                              
015600                                                                          
015700*01  -COPY W0008  -PRE WDK6-                                              
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000*01  -COPY W0008  -PRE WDK7-                                              
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300*01  -COPY W0008  -PRE WDB6-                                              
016400     05  FILLER                  PIC X.                                   
016500     EJECT                                                                
016600 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDK7-PCB WDB6-PCB.            
016700 MAIN SECTION.                                                            
016800     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDK7-PCB WDB6-PCB.            
016900                                                                          
017000     SKIP2                                                                
017100     PERFORM A-INIT                                                       
017200     PERFORM S01-LAES-W27208                                              
017300     PERFORM UNTIL END-OF-W27208                                          
017400       MOVE IN-IDARTNR TO W-IDARTNR                                       
017500       PERFORM IMS-GU-WDK601                                              
017510       IF SEGMENT-FINNS                                                   
017600         PERFORM IMS-GNP-WDK611                                           
017700         IF SEGMENT-FINNS                                                 
017800           PERFORM IMS-GNP-WDK629                                         
017900           IF SEGMENT-FINNS                                               
018000             PERFORM B-BEHANDLA-POSTER                                    
018100           END-IF                                                         
018200         END-IF                                                           
018210       END-IF                                                             
018300*                                                                         
018400       PERFORM S01-LAES-W27208                                            
018500     END-PERFORM                                                          
018600                                                                          
018700                                                                          
018800     PERFORM Z-FINIT                                                      
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500     SKIP2                                                                
019600                                                                          
019700     OPEN INPUT W27208                                                    
019800     OPEN OUTPUT W27209                                                   
019900     .                                                                    
020000     EJECT                                                                
020100 B-BEHANDLA-POSTER SECTION.                                               
020200                                                                          
020300     MOVE '11'                          TO UT-IDDC                        
020400                                           W-IDDC-B6                      
020500     MOVE CREF-IDDC-REF                 TO W-IDDC-B616                    
020600                                           W-IDDC                         
020700     PERFORM IMS-GU-WDB616                                                
020800     IF SEGMENT-FINNS                                                     
020900       PERFORM IMS-GU-WDK711                                              
021000       IF SEGMENT-FINNS                                                   
021100         MOVE SLAG-ADLAGOMR             TO UT-ADLAGOMR-CDC                
021200         MOVE SLAG-ADGANG               TO UT-ADGANG-CDC                  
021300         MOVE SLAG-ADPLATS              TO UT-ADPLATS-CDC                 
021400         MOVE CLAG-ADLAGOMR             TO UT-ADLAGOMR-SDC                
021500         MOVE CLAG-ADGANG               TO UT-ADGANG-SDC                  
021600         MOVE CLAG-ADPLATS              TO UT-ADPLATS-SDC                 
021700         MOVE IN-IDARTNR                TO UT-IDARTNR                     
021800         MOVE REF-IDDISTR-REFILL        TO UT-IDDISTR                     
021900         MOVE CREF-IDPERSON-BUY         TO UT-IDPERSON-BUY                
022000         MOVE ZERO                      TO UT-KVBEART                     
022100         MOVE 'P'                       TO UT-KDREFORS                    
022200         MOVE ART-IDLEVNR               TO UT-IDLEVNR                     
022300         MOVE IN-KDREFTXT               TO UT-KDREFTXT                    
022400         MOVE ZERO                      TO UT-KDFRAKT                     
022500                                           UT-KVBEART-CD                  
022600                                           UT-ADLAGOMR-CD                 
022700                                           UT-ADGANG-CD                   
022800                                           UT-ADPLATS-CD                  
022900                                           UT-IDKUNDNR                    
023000         MOVE CLAG-IDDC-REF             TO UT-IDDC-REF                    
023100         IF CREF-FLFLYG = 'J' OR 'Y'                                      
023200           MOVE 'A'                     TO UT-KDREFTYP                    
023300         ELSE                                                             
023400           MOVE 'B'                     TO UT-KDREFTYP                    
023500         END-IF                                                           
023510         PERFORM S03-SKRIV-W27209                                         
023600       END-IF                                                             
023700     END-IF                                                               
023800                                                                          
023900     .                                                                    
024000     EJECT                                                                
024100 Z-FINIT SECTION.                                                         
024200                                                                          
024300     CLOSE W27208                                                         
024400           W27209                                                         
024500     .                                                                    
024600     EJECT                                                                
024700 S01-LAES-W27208  SECTION.                                                
024800     SKIP2                                                                
024900     READ W27208 INTO IN-AREA                                             
025000     AT END                                                               
025100        SET END-OF-W27208 TO TRUE                                         
025200                                                                          
025300     NOT AT END                                                           
025400        ADD 1                TO WS-ANTAL-W27209                           
025500     END-READ                                                             
025600     .                                                                    
025700     EJECT                                                                
025800                                                                          
025900 S03-SKRIV-W27209     SECTION.                                            
026000                                                                          
026100     WRITE UT-POST FROM UT-AREA                                           
026200     .                                                                    
026300     EJECT                                                                
026400 IMS-GU-WDK601 SECTION.                                                   
026500                                                                          
026600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
026700          DELIMITED BY SIZE INTO SSA1                                     
026900     MOVE '  GE' TO GODK-STATUSKODER                                      
027000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
027100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027200     PERFORM IMS-STATUSKONTROLL                                           
027300     .                                                                    
027400     EJECT                                                                
027500 IMS-GNP-WDK611 SECTION.                                                  
027600                                                                          
027700     MOVE 'WDK611  '         TO SSA1                                      
027800     MOVE '  ' TO GODK-STATUSKODER                                        
027900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
028000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028100     PERFORM IMS-STATUSKONTROLL                                           
028200     .                                                                    
028300     EJECT                                                                
028310 IMS-GNP-WDK629 SECTION.                                                  
028320                                                                          
028330     MOVE 'WDK629  '         TO SSA1                                      
028340     MOVE '  GE' TO GODK-STATUSKODER                                      
028350     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
028360     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028370     PERFORM IMS-STATUSKONTROLL                                           
028380     .                                                                    
028390     EJECT                                                                
028400 IMS-GU-WDK711 SECTION.                                                   
028500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
028600          DELIMITED BY SIZE INTO SSA1                                     
028700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
028800          DELIMITED BY SIZE INTO SSA2                                     
028900     MOVE '  GE' TO GODK-STATUSKODER                                      
029000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
029100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     .                                                                    
029400     SKIP3                                                                
029500                                                                          
029600 IMS-GU-WDB616    SECTION.                                                
029700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
029800          DELIMITED BY SIZE INTO SSA1                                     
029900     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
030000          DELIMITED BY SIZE INTO SSA2                                     
030100     MOVE '  GE' TO GODK-STATUSKODER                                      
030200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
030300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
030400     PERFORM IMS-STATUSKONTROLL                                           
030500     .                                                                    
030600     EJECT                                                                
030700                                                                          
030800 IMS-STATUSKONTROLL SECTION.                                              
030900     SKIP2                                                                
031000     SET STATUS-IX TO 1                                                   
031100     SEARCH GODK-STATUS                                                   
031200       AT END                                                             
031300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031400           DELIMITED BY SIZE INTO FELTEXT                                 
031500         DISPLAY FELTEXT                                                  
031600         CALL FELLOG                                                      
031700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031800         CONTINUE                                                         
031900     END-SEARCH                                                           
032000     .                                                                    
