000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W5606500.                                               
000400 AUTHOR.          INGVAR SKJELBRED                                        
000500 DATE-WRITTEN.    AUGUSTI  1997.                                          
000600                                                                          
000700*    REMARKS.                                                             
000800*       PROGRAMET LÄSER IGENOM WDL6 (INLEVERANSHISTORIK FÖR SDC-          
000900*       LAGER) OCH SKRIVER EN FIL MED DATA TILL LIFO SYSTEMET.            
001000*       FÖR BYTES OCH BYTES-RADIO-ARTIKLAR SKRIVS EN EXTRA TRANS          
001100*       PÅ UTFILEN MED OBJEKTETS ARTIKELNUMMER OCH LEVERANTÖRENS          
001200*       PRIS SOM HÄMTAS FRÅN ARTIKELREGISTRETS PRISSEGMENT.               
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000     SELECT W56065                     ASSIGN TO W56065D1.                
002100                                                                          
002200 DATA DIVISION.                                                           
002300                                                                          
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W56065                                                               
002700     RECORDING       F                                                    
002800     BLOCK CONTAINS  0.                                                   
002900                                                                          
003000*01  POST -COPY W56065 -PRE UT-  -L.                                      
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77   IDPGM                      PIC X(8)    VALUE 'W5606500'.            
003600                                                                          
003700 01  JA                          PIC X       VALUE 'J'.                   
003800 01  NEJ                         PIC X       VALUE 'N'.                   
003900 01  W-DAINLEV                   PIC 9(16)   VALUE ZERO.                  
004000 01  W-DAINLEV-NIOR              PIC 9(16) VALUE 9999999999999999.        
004100                                                                          
004200 01  TEST-IDDISTR             PIC 9(5)  COMP-3.                           
004300*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
004400                                                                          
004500 01  TEST-IDARTNR              PIC 9(9) COMP-3.                           
004600*01  FILLER -COPY WWBYT19    -RED TEST-IDARTNR                            
004700     EJECT                                                                
004800*    --- VALID IDDC CODES                                                 
004900*                                                                         
005000*01  -COPY WWDC99                                                         
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005600     EJECT                                                                
005700 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
005800 01  NYCKLAR-TILL-DLI.                                                    
005900     03  W-IDARTNR-X.                                                     
006000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
006100     03  W-IDLEVNR-X.                                                     
006200         05 W-IDLEVNR            PIC X(5)    VALUE SPACE.                 
006300                                                                          
006400 01  IMS-WS.                                                              
006500                                                                          
006600   03  STATUS-WS                 PIC X(2).                                
006700      88  SEGMENT-FINNS                      VALUE '  '.                  
006800      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
006900      88  SEGMENT-SLUT                       VALUE 'GB'.                  
007000      88  SEGMENT-OK                         VALUE 'GA'.                  
007100                                                                          
007200   03 GODK-STATUSKODER.                                                   
007300      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
007400                                                                          
007500     03  SSA1                       PIC X(64)  VALUE SPACE.               
007600     03  SSA2                       PIC X(64)  VALUE SPACE.               
007700     03  SSA3                       PIC X(64)  VALUE SPACE.               
007800     EJECT                                                                
007900*01        -COPY W0003                                                    
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16) VALUE 'POSTSUM'.               
008200                                                                          
008300*    -COPY W0005       -PRE POSTSUM-                                      
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)  VALUE 'UT-AREA-START'.        
008600                                                                          
008700*01  AREA  -COPY W56065   -PRE UT-                                        
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'IO-AREA'.              
009000 01  IO-AREA.                                                             
009100   03  IO-AREA1                  PIC X(250).                              
009200                                                                          
009300*  03  ART-AREA   -COPY WDL601     -RED IO-AREA1                          
009400                                                                          
009500*  03  INL-AREA   -COPY WDL611     -RED IO-AREA1                          
009600     EJECT                                                                
009700                                                                          
009800 01  FILLER                      PIC X(16)  VALUE 'WDK601'.               
009900 01  DLI-IO-WDK601.                                                       
010000*  03   -COPY WDK601 -PRE WDK6-                                           
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)  VALUE 'WDK621'.               
010300 01  DLI-IO-WDK621.                                                       
010400*  03   -COPY WDK621 -PRE WDK6-                                           
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010700                                                                          
010800*01  -COPY W0008       -PRE INLC-                                         
010900       05 FILLER                 PIC X(1).                                
011000                                                                          
011100*01  -COPY W0008       -PRE WDK6-                                         
011200       05 FILLER                 PIC X(1).                                
011300     EJECT                                                                
011400 PROCEDURE DIVISION USING INLC-PCB WDK6-PCB.                              
011500     ENTRY 'DLITCBL' USING INLC-PCB WDK6-PCB.                             
011600                                                                          
011700     PERFORM A-INIT                                                       
011800     PERFORM IMS-GET-WDL6                                                 
011900                                                                          
012000     PERFORM UNTIL SEGMENT-SLUT                                           
012100                                                                          
012200      EVALUATE INLC-SEG-NAME-FB                                           
012300        WHEN  'WDL601'                                                    
012400          MOVE ART-IDARTNR      TO UT-IDARTNR                             
012500        WHEN  'WDL611'                                                    
012600          PERFORM B-BEARBETA                                              
012700       END-EVALUATE                                                       
012800                                                                          
012900       PERFORM IMS-GET-WDL6                                               
013000     END-PERFORM                                                          
013100                                                                          
013200     PERFORM Z-FINIT                                                      
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600     SKIP3                                                                
013700 A-INIT SECTION.                                                          
013800                                                                          
013900     OPEN OUTPUT W56065                                                   
014000                                                                          
014100     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
014200                                                                          
014300     .                                                                    
014400     EJECT                                                                
014500 B-BEARBETA SECTION.                                                      
014600                                                                          
014700     IF INL-IDPTYP = 'R30' OR '310'                                       
014800       MOVE INL-IDDISTR TO TEST-IDDISTR                                   
014900       MOVE INL-IDDC    TO WS-IDDC                                        
015000       IF DIST35-CDC-NDC41-REFILL                                         
015200       OR DIST35-CDC-NDC43-REFILL                                         
015300       OR DIST35-CDC-NDC44-REFILL                                         
015310       OR DIST35-CDC-NDC45-REFILL                                         
015320       OR DIST35-CDC-NDC46-REFILL                                         
015330       OR DIST35-CDC-NDC47-REFILL                                         
015400       OR DIST35-JAP-NDC41-REFILL                                         
015600       OR DIST35-JAP-NDC43-REFILL                                         
015610       OR DIST35-JAP-NDC44-REFILL                                         
015700       OR DIST35-US-US-TRANSFER                                           
015800       OR NDC-US                                                          
015900          PERFORM BA-SKAPA-FIL                                            
016000       END-IF                                                             
016100     END-IF                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 BA-SKAPA-FIL SECTION.                                                    
016500                                                                          
016600     IF INL-PRKURS = ZERO                                                 
016700     AND INL-KDVALISO = '  '                                              
016800     AND INL-PRARTNTO = ZERO                                              
016900         CONTINUE                                                         
017000     ELSE                                                                 
017100         COMPUTE W-DAINLEV = W-DAINLEV-NIOR - INL-DAINLEV                 
017200         MOVE W-DAINLEV(1:8)              TO UT-DAFAKT                    
017300         MOVE INL-IDDC                    TO UT-IDDC                      
017400         MOVE INL-IDFAKT                  TO UT-IDFAKT                    
017500         COMPUTE UT-PRARTBEL-PR =                                         
017600                          INL-PRARTNTO / INL-PRKURS                       
017700         COMPUTE UT-KVAKS = INL-KVAVIS - INL-KVANTMOT                     
017800                                - INL-KVART-SKROT                         
017900         MOVE INL-IDDISTR                 TO UT-IDDISTR                   
018000         MOVE INL-PRKURS                  TO UT-PRKURS                    
018100                                                                          
018200         MOVE UT-IDARTNR                TO W-IDARTNR                      
018300         PERFORM IMS-GET-WDK601                                           
018400         IF SEGMENT-FINNS                                                 
018500           MOVE WDK6-ART-IDFKNGRP       TO UT-IDFKNGRP                    
018600         ELSE                                                             
018700           MOVE ZERO                    TO UT-IDFKNGRP                    
018800         END-IF                                                           
018900                                                                          
019000         PERFORM S01-SKRIV-W56065                                         
019100         MOVE UT-IDARTNR                  TO TEST-IDARTNR                 
019200         IF BYT19-BYTES OR BYT19-RADIO                                    
019300           IF BYT19-BYTES                                                 
019400             ADD 6000                     TO UT-IDARTNR                   
019500           ELSE                                                           
019600             ADD 1000                     TO UT-IDARTNR                   
019700           END-IF                                                         
019800           MOVE UT-IDARTNR                TO W-IDARTNR                    
019900           MOVE '9993 '                   TO W-IDLEVNR                    
020000                                                                          
020100           PERFORM IMS-GET-WDK601                                         
020200           IF SEGMENT-FINNS                                               
020300             MOVE WDK6-ART-IDFKNGRP       TO UT-IDFKNGRP                  
020400           ELSE                                                           
020500             MOVE ZERO                    TO UT-IDFKNGRP                  
020600           END-IF                                                         
020700                                                                          
020800           PERFORM IMS-GET-WDK621                                         
020900           IF SEGMENT-FINNS                                               
021000             MOVE WDK6-PRL-PRARTBEL-PR    TO UT-PRARTBEL-PR               
021100             MOVE 1                       TO UT-PRKURS                    
021200           ELSE                                                           
021300             MOVE ZERO                    TO UT-PRARTBEL-PR               
021400           END-IF                                                         
021500           PERFORM S01-SKRIV-W56065                                       
021600         END-IF                                                           
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 Z-FINIT SECTION.                                                         
022100                                                                          
022200     CLOSE W56065                                                         
022300                                                                          
022400     MOVE 'S' TO POSTSUM-OPKOD                                            
022500     CALL POSTSUM USING POSTSUM-PARM                                      
022600     .                                                                    
022700     SKIP2                                                                
022800 S01-SKRIV-W56065       SECTION.                                          
022900                                                                          
023000     WRITE UT-POST FROM UT-AREA                                           
023100                                                                          
023200     MOVE 'W56065'     TO POSTSUM-FDNAMN                                  
023300     MOVE 'W56065D1'   TO POSTSUM-DDNAMN2                                 
023400     MOVE 'UT  '       TO POSTSUM-TRANSTYP                                
023500     CALL POSTSUM USING POSTSUM-PARM                                      
023600     .                                                                    
023700     EJECT                                                                
023800 IMS-GET-WDL6 SECTION.                                                    
023900                                                                          
024000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
024100     CALL CBLTDLI USING GN INLC-PCB IO-AREA                               
024200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
024300     PERFORM IMS-STATUSKONTROLL                                           
024400     .                                                                    
024500     SKIP2                                                                
024600 IMS-GET-WDK601 SECTION.                                                  
024700                                                                          
024800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
024900          DELIMITED BY SIZE INTO SSA1                                     
025000     MOVE '  GE' TO GODK-STATUSKODER                                      
025100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
025200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025300     PERFORM IMS-STATUSKONTROLL                                           
025400     .                                                                    
025500     EJECT                                                                
025600 IMS-GET-WDK621 SECTION.                                                  
025700                                                                          
025800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
025900          DELIMITED BY SIZE INTO SSA1                                     
026000     STRING 'WDK611  (KDSEGKEY =1)'                                       
026100          DELIMITED BY SIZE INTO SSA2                                     
026200     STRING 'WDK621  (IDLEVNR  =' W-IDLEVNR-X ')'                         
026300          DELIMITED BY SIZE INTO SSA3                                     
026400     MOVE '  GE' TO GODK-STATUSKODER                                      
026500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK621 SSA1 SSA2 SSA3          
026600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026700     PERFORM IMS-STATUSKONTROLL                                           
026800     .                                                                    
026900     EJECT                                                                
027000 IMS-STATUSKONTROLL SECTION.                                              
027100                                                                          
027200     SET STATUS-IX TO 1                                                   
027300     SEARCH GODK-STATUS AT END CALL FELLOG                                
027400     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
027500     END-SEARCH                                                           
027600     .                                                                    
