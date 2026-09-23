000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4184200.                                                 
000300*AUTHOR.        THOMAS NILSSON.                                           
000400*DATE-WRITTEN.  SEPT 1987.                                                
000500*REMARKS.                                                                 
000600                                                                          
000700*    FUNKTION:                                                            
000800                                                                          
000900*                                                                         
001000*        PROGRAMMET LÄSER WDA2 MED SB.                                    
001100*        OCH SKAPAR EN FIL MED 01,11 OCH 21 SEGMENT.                      
001200*            SKAPAR EN FIL MED 01,11        SEGMENT.                      
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700*                                                                         
001800     SELECT W41842    ASSIGN TO W41842D1.                                 
001900*                                                                         
002000     SELECT W41844    ASSIGN TO W41842D2.                                 
002100*                                                                         
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP2                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W41842                                                               
002800     RECORDING       V                                                    
002900     BLOCK CONTAINS 0.                                                    
003000                                                                          
003100 01  WDA201-AREA.                                                         
003200   03  FILLER                    PIC X(3).                                
003300*03  A01-AREA  -COPY WDA201  -L                                           
003400                                                                          
003500 01  WDA211-AREA.                                                         
003600   03  FILLER                    PIC X(3).                                
003700*03  A11-AREA  -COPY WDA211  -L                                           
003800                                                                          
003900 01  WDA221-AREA.                                                         
004000   03  FILLER                    PIC X(3).                                
004100*03  A21-AREA  -COPY WDA221  -L                                           
004200     EJECT                                                                
004300     SKIP3                                                                
004400 FD  W41844                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800 01  W4184401-AREA.                                                       
004900*03  A01-AREA2 -COPY W4184401  -L                                         
005000                                                                          
005100 01  W4184411-AREA.                                                       
005200*03  A11-AREA2 -COPY W4184411  -L                                         
005300                                                                          
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600*    -- CHECKED BY WY2000                                                 
005700 77  PROGRAM-NAMN                PIC X(6)    VALUE 'W41842'.              
005800 77  SPAR-KDVALISO               PIC X(3)    VALUE SPACE.                 
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  SW-SKRIV-UT2                PIC X       VALUE 'N'.                   
006200     88  SKRIV-UT2-JA                        VALUE 'J'.                   
006300     88  SKRIV-UT2-NEJ                       VALUE 'N'.                   
006400 77  WS-KDLEVANM                 PIC X       VALUE ' '.                   
006500     88  KDLEVANM-5                          VALUE '5'.                   
006600     88  KDLEVANM-6                          VALUE '6'.                   
006700     88  KDLEVANM-7                          VALUE '7'.                   
006800     88  KDLEVANM-8                          VALUE '8'.                   
006900     88  KDLEVANM-9                          VALUE '9'.                   
007000 77  WS-IDFTG                    PIC 99      VALUE 0.                     
007100     88  IDFTG-57                            VALUE 57.                    
007200 77  WS-IDDC-RET                 PIC X(2)    VALUE '  '.                  
007300     88  IDDC-RET-11                         VALUE '11'.                  
007400 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
007500 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
007600*                                                                         
007700 01  FILLER              PIC X(18) VALUE 'SPAR-ANM-IDLEVANM '.            
007800 01  SPAR-ANM-IDLEVANM.                                                   
007900     03 SPAR-ANM-IDDISTR        PIC S9(5)    COMP-3.                      
008000     03 SPAR-ANM-IDKUNDNR       PIC S9(7)    COMP-3.                      
008100     03 SPAR-ANM-IDRAPPNR       PIC 9(7).                                 
008200     EJECT                                                                
008300 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
008400                                                                          
008500 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
008600*01  FILLER   -COPY WWDIST79   -RED TEST-IDDISTR.                         
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
009200                                                                          
009300*    --- PARAMETRAR TILL W510CURR                                         
009400*01  -COPY W510CURR                                                       
009500     EJECT                                                                
009600 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
009700 01  IMS-WS.                                                              
009800     03  STATUS-WS               PIC X(2).                                
009900        88  SEGMENT-FINNS                    VALUE '  '.                  
010000        88  SEGMENT-SLUT                     VALUE 'GB'.                  
010100                                                                          
010200     03  GODK-STATUSKODER.                                                
010300         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(128).                              
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800                                                                          
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
011400 01  DLI-IO-AREA.                                                         
011500     03  IO-AREA             PIC X(1200).                                 
011600*    03  WDA201  -COPY WDA201 -RED IO-AREA.                               
011700     EJECT                                                                
011800*    03  WDA211  -COPY WDA211 -RED IO-AREA.                               
011900     EJECT                                                                
012000*    03  WDA221  -COPY WDA221 -RED IO-AREA.                               
012100     EJECT                                                                
012200                                                                          
012300 01  FILLER                  PIC X(16) VALUE 'UT-AREA'.                   
012400 01  UT-AREAN.                                                            
012500     03  UT-AREA             PIC X(1200).                                 
012600     03  ANM-AREA REDEFINES UT-AREA.                                      
012700         05  UT-ANM-IDPTYP   PIC X(3).                                    
012800*    05  -COPY WDA201 -PRE UT-.                                           
012900     EJECT                                                                
013000     03  LEV-AREA REDEFINES UT-AREA.                                      
013100         05  UT-LEV-IDPTYP   PIC X(3).                                    
013200*    05  -COPY WDA211 -PRE UT-.                                           
013300     EJECT                                                                
013400     03  TXT-AREA REDEFINES UT-AREA.                                      
013500         05  UT-TXT-IDPTYP   PIC X(3).                                    
013600*    05  -COPY WDA221 -PRE UT-.                                           
013700                                                                          
013800 01  FILLER                  PIC X(16) VALUE 'UT2-AREA'.                  
013900 01  UT2-AREAN.                                                           
014000     03  UT2-AREA            PIC X(1200).                                 
014100*    03  -COPY W4184401 -PRE UT2-ANM-  -RED UT2-AREA.                     
014200     EJECT                                                                
014300*    03  -COPY W4184411 -PRE UT2-LEV-  -RED UT2-AREA.                     
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600     SKIP3                                                                
014700*01  -COPY W0008   -PRE WDA2-                                             
014800         05  FILLER          PIC X(1).                                    
014900     EJECT                                                                
015000*01  -COPY W0008  -PRE WDG2-                                              
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING WDA2-PCB WDG2-PCB.                             
015400     ENTRY 'DLITCBL' USING WDA2-PCB WDG2-PCB.                             
015500                                                                          
015600     OPEN OUTPUT W41842                                                   
015700     OPEN OUTPUT W41844                                                   
015800                                                                          
015900     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
016000     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
016100     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
016200     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
016300     MOVE 'M'                         TO CURR-KDVALTYP                    
016400                                                                          
016500     PERFORM IMS-GET-WDA201                                               
016600                                                                          
016700     PERFORM UNTIL SEGMENT-SLUT                                           
016800       EVALUATE WDA2-SEG-NAME-FB                                          
016900       WHEN 'WDA201'                                                      
017000         MOVE '201' TO UT-ANM-IDPTYP                                      
017100         MOVE ANM-IDDISTR  TO TEST-IDDISTR                                
017200         MOVE ANM-KDVALISO TO SPAR-KDVALISO                               
017300         MOVE DLI-IO-AREA TO UT-ANM-WDA201                                
017400         WRITE WDA201-AREA FROM UT-AREAN                                  
017500                                                                          
017600         MOVE ANM-KDLEVANM     TO WS-KDLEVANM                             
017700         MOVE ANM-IDFTG        TO WS-IDFTG                                
017800         MOVE ANM-IDDC-RET     TO WS-IDDC-RET                             
017900         MOVE NEJ              TO SW-SKRIV-UT2                            
018000         IF (KDLEVANM-5 OR                                                
018100             KDLEVANM-6 OR                                                
018200             KDLEVANM-7 OR                                                
018300             KDLEVANM-8 OR                                                
018400             KDLEVANM-9)    AND                                           
018500             IDFTG-57       AND                                           
018600             IDDC-RET-11                                                  
018700           MOVE JA             TO SW-SKRIV-UT2                            
018800           MOVE ANM-IDLEVANM   TO SPAR-ANM-IDLEVANM                       
018900           MOVE '201'          TO UT2-ANM-IDPTYP                          
019000           MOVE DLI-IO-AREA    TO UT2-ANM-WDA201                          
019100           WRITE W4184401-AREA FROM UT2-AREAN                             
019200         END-IF                                                           
019300       WHEN 'WDA211'                                                      
019400         MOVE '211' TO UT-LEV-IDPTYP                                      
019500         MOVE DLI-IO-AREA TO UT-LEV-WDA211                                
019600         IF DIST79-DEALER-PRICE                                           
019800           PERFORM B-BEHANDLA-LOKAL-VALUTA                                
019900         END-IF                                                           
020000         WRITE WDA211-AREA FROM UT-AREAN                                  
020100                                                                          
020200         IF SKRIV-UT2-JA                                                  
020300           MOVE '211'               TO UT2-LEV-IDPTYP                     
020400           MOVE SPAR-ANM-IDLEVANM   TO UT2-LEV-IDLEVANM                   
020500***        FLYTTAR BEARBETAT SEGMENT 211           ***                    
020600           MOVE UT-LEV-WDA211       TO UT2-LEV-WDA211                     
020700           WRITE W4184411-AREA      FROM UT2-AREAN                        
020800         END-IF                                                           
020900       WHEN 'WDA221'                                                      
021000         MOVE '221' TO UT-TXT-IDPTYP                                      
021100         MOVE DLI-IO-AREA TO UT-TXT-WDA221                                
021200         WRITE WDA221-AREA FROM UT-AREAN                                  
021300       END-EVALUATE                                                       
021400       PERFORM IMS-GET-WDA201                                             
021500     END-PERFORM                                                          
021600                                                                          
021700     CLOSE W41842                                                         
021800     CLOSE W41844                                                         
021900     MOVE ZERO TO RETURN-CODE                                             
022000     GOBACK                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 B-BEHANDLA-LOKAL-VALUTA SECTION.                                         
022400                                                                          
022500*- PRISET SKALL VARA I SEK.MAN HÄMTAR KURSEN OCH RÄKNAR OM.               
022600                                                                          
022700*SO-FIX FÖR ATT KLARA GAMLA POSTER SOM REDAN HAR SEK.                     
022800     IF SPAR-KDVALISO = SPACE                                             
022900       CONTINUE                                                           
023000     ELSE                                                                 
023100       IF DIST79-DEALER-PRICE                                             
023300**** FÖR DIST. 778 SVERIGE ÄR JU DEALER-NET-PRICE = SEK REDAN.            
023400         IF SPAR-KDVALISO = 'SEK'                                         
023500           MOVE LEV-PRARTBTO-LOC TO UT-LEV-PRARTBTO                       
023600         ELSE                                                             
023700           MOVE SPAR-KDVALISO   TO CURR-KDVALISO-ROW                      
023800                                                                          
023900**** LÄS PRKURS PÅ WDG2                   *****                           
024000                                                                          
024100           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
024200           IF CURR-KDSVAR = ' '                                           
024300              CONTINUE                                                    
024400           ELSE                                                           
024500              MOVE 1      TO CURR-PRKURS-NEW                              
024600           END-IF                                                         
024700           COMPUTE UT-LEV-PRARTBTO ROUNDED =                              
024800              UT-LEV-PRARTBTO-LOC  * CURR-PRKURS-NEW                      
024900                                                                          
025000           END-COMPUTE                                                    
025100         END-IF                                                           
025200       ELSE                                                               
025300         PERFORM S01-BEHANDLA-CENTRAL-PRICING                             
025400       END-IF                                                             
025500     END-IF                                                               
025600     .                                                                    
025700     EJECT                                                                
025800 S01-BEHANDLA-CENTRAL-PRICING SECTION.                                    
025900                                                                          
026000**- GAMLA LA PÅ CP-DISTRIKT KAN HA KDVALISO = SPACE ( FÖRE DNI )          
026100**- OCH KDVALISO = BET-KDVALISO ( EFTER DNI ).                            
026200**- NYA LA HAR PRARTBTO=0/GAMLA>0 SEK                                     
026300**- DÄRFÖR BEHÖVER EJ GAMLA RÄKNAS OM. SEK-PRISET GÄLLDE HÄR.             
026400                                                                          
026500     IF LEV-PRARTBTO = ZERO                                               
026600       IF LEV-PRARTBTO-LOC NOT = ZERO                                     
026700         IF SPAR-KDVALISO = 'SEK'                                         
026800           MOVE LEV-PRARTBTO-LOC TO UT-LEV-PRARTBTO                       
026900         ELSE                                                             
027000           MOVE SPAR-KDVALISO   TO CURR-KDVALISO-ROW                      
027100                                                                          
027200**** LÄS PRKURS PÅ WDG2                 *****                             
027300                                                                          
027400           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
027500           IF CURR-KDSVAR = ' '                                           
027600              CONTINUE                                                    
027700           ELSE                                                           
027800              MOVE 1      TO CURR-PRKURS-NEW                              
027900           END-IF                                                         
028000           COMPUTE UT-LEV-PRARTBTO ROUNDED =                              
028100              UT-LEV-PRARTBTO-LOC  * CURR-PRKURS-NEW                      
028200                                                                          
028300           END-COMPUTE                                                    
028400         END-IF                                                           
028500       END-IF                                                             
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 IMS-GET-WDA201         SECTION.                                          
029000                                                                          
029100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
029200     CALL CBLTDLI USING GN WDA2-PCB IO-AREA                               
029300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
029400     PERFORM IMS-STATUSKONTROLL                                           
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029800 IMS-STATUSKONTROLL       SECTION.                                        
029900                                                                          
030000     SET STATUS-IX TO 1                                                   
030100     SEARCH GODK-STATUS                                                   
030200       AT END CALL FELLOG                                                 
030300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
030400     END-SEARCH                                                           
030500     .                                                                    
