000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6121500.                                                
000300 AUTHOR.         EVA LUNDELL.                                             
000400 DATE-WRITTEN.   96/08/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAM FÖR RENSNING AV WDL6.                                    
001000*        FÖR VARJE ARTIKEL/DC SKALL RADER YNGRE ÄN 1 ÅR LIGGA KVAR        
001100*        . OM ANTALET RADER PER ARTIKEL/DC ÄR 5 ELLER FÄRRE SKALL         
001200*        MAN INTE RENSA. ÅLDERN GÄLLER INLÄGGNINGSDATUM.                  
001300*        RENSNINGSBARA POSTR SKRIVS PÅ FIL FÖR RENSNING I SENARE          
001400*        PROGRAM                                                          
001500*                                                                         
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SELECT W61215               ASSIGN TO W61215D1.                      
002700                                                                          
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200 FD  W61215                                                               
003300     RECORDING F                                                          
003400     BLOCK CONTAINS 0.                                                    
003500     SKIP2                                                                
003600*01  POST -COPY W61215 -L.                                                
003700                                                                          
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6121500'.            
004400 01  WS-STRINGRAD                PIC X(80).                               
004410 01  FELTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
004901 77  WS-W61215-ANT               PIC S9(7) COMP-3   VALUE ZERO.           
004902 77  MAX-ANT                     PIC S9(5) COMP-3   VALUE +5.             
004903 77  DC-IX                       PIC  9(3)          VALUE ZERO.           
004904 77  DC-IX-MAX                   PIC  9(3)          VALUE 100.            
004910                                                                          
004920 01  WS-DC-TABELL.                                                        
004921     03  WS-DC-RAD OCCURS 100.                                            
004940         05 WS-DC                PIC  X(2)          VALUE SPACE.          
004950         05 WS-DC-ANT            PIC  9(5)          VALUE ZERO.           
004960         05 WS-DC-TOT            PIC  9(5)          VALUE ZERO.           
007200                                                                          
007300     EJECT                                                                
007400 01  WS-RENS-DATUM               PIC 9(8)    VALUE ZERO.                  
007500                                                                          
007600 01  WS-DAINLINL                 PIC 9(8)    VALUE ZERO.                  
007700 01  FILLER REDEFINES WS-DAINLINL.                                        
007800   03  WS-DAINLINL-SEKEL         PIC 9(2).                                
007900   03  WS-TIINLINL               PIC 9(6).                                
008000                                                                          
008001                                                                          
008002 01  WS-DAINLEV                  PIC 9(16)   VALUE ZERO.                  
008003 01  FILLER REDEFINES WS-DAINLEV.                                         
008004   03  WS-DAINLEV-8POS           PIC 9(8).                                
008005   03  WS-DAINLEV-REST           PIC 9(8).                                
008006                                                                          
008200                                                                          
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600                                                                          
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
008900                                                                          
009000 01  AREA -COPY W61215 -PRE UT-                                           
009100                                                                          
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400                                                                          
009500 01  NYCKLAR-TILL-DLI.                                                    
009600     03  W-IDARTNR-X.                                                     
009700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009800                                                                          
009900                                                                          
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FINNS                       VALUE '  '.                  
010300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010500                                                                          
010600                                                                          
010700 01  GODK-STATUSKODER.                                                    
010800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900                                                                          
011000                                                                          
011100 01  SSA1                        PIC X(64).                               
011200                                                                          
011300     EJECT                                                                
011400*    --- IMS FUNKTIONSKODER                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011900                                                                          
012000 01  DLI-IO-AREA.                                                         
012100   03  IO-AREA                   PIC X(200)  VALUE SPACE.                 
012200   03  WLINLC01  REDEFINES IO-AREA.                                       
012300*    05  -COPY WDL601                                                     
012400                                                                          
012500     EJECT                                                                
012600   03  WLINLC11  REDEFINES IO-AREA.                                       
012700*    05  -COPY WDL611                                                     
012800                                                                          
012810 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
012820 01   DLI-IO-AREA-B601.                                                   
012830*     03  -COPY WDB601                                                    
012900     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100*01  -COPY W0008  -PRE INLC-                                              
013200     05  FILLER                  PIC X.                                   
013210*01  -COPY W0008  -PRE WDB6-                                              
013220     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING INLC-PCB WDB6-PCB.                             
013500 MAIN SECTION.                                                            
013600     ENTRY 'DLITCBL' USING INLC-PCB WDB6-PCB.                             
013700                                                                          
013800     PERFORM A-INIT                                                       
013900     PERFORM IMS-GET-INLC                                                 
014000     PERFORM UNTIL SEGMENT-SLUT                                           
014100       IF INLC-SEG-NAME-FB = 'WLINLC01'                                   
014200         MOVE ART-IDARTNR TO UT-IDARTNR                                   
014300         PERFORM S01-NOLLA-RAEKNARE                                       
014400       ELSE                                                               
014500         IF INL-IDPTYP = 'R32' OR 'R34' OR 'W33'                          
014510           IF INL-IDPTYP = 'R32'                                          
014520           AND INL-TIINLINL = ZERO                                        
014600              MOVE INL-TIINLMOT TO WS-TIINLINL                            
014700              IF WS-TIINLINL < 500000                                     
014800                MOVE 20 TO WS-DAINLINL-SEKEL                              
014900              ELSE                                                        
015000                MOVE 19 TO WS-DAINLINL-SEKEL                              
015100              END-IF                                                      
015101           ELSE                                                           
015102             IF INL-IDPTYP = 'W33'                                        
015103               MOVE INL-DAINLEV TO WS-DAINLEV                             
015104               COMPUTE WS-DAINLINL = 99999999 - WS-DAINLEV-8POS           
015105             ELSE                                                         
015110               MOVE INL-TIINLINL TO WS-TIINLINL                           
015120               IF WS-TIINLINL < 500000                                    
015130                 MOVE 20 TO WS-DAINLINL-SEKEL                             
015140               ELSE                                                       
015150                 MOVE 19 TO WS-DAINLINL-SEKEL                             
015160               END-IF                                                     
015170             END-IF                                                       
015210           END-IF                                                         
015220           PERFORM B-KOLLA-SKRIV                                          
015300         END-IF                                                           
015400       END-IF                                                             
015500       PERFORM IMS-GET-INLC                                               
015600     END-PERFORM                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500                                                                          
016600     OPEN OUTPUT W61215                                                   
016700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-RENS-DATUM                    
016800     SUBTRACT 10000 FROM WS-RENS-DATUM                                    
016810                                                                          
016820     INITIALIZE WS-DC-TABELL                                              
016830                                                                          
016831     MOVE 1  TO DC-IX                                                     
016840     PERFORM IMS-GN-WDB601                                                
016850     PERFORM UNTIL SEGMENT-SLUT OR DC-IX > DC-IX-MAX                      
016860        IF NOT DCS-DDC                                                    
016870           MOVE DCS-IDDC  TO  WS-DC(DC-IX)                                
016880           ADD  1 TO DC-IX                                                
016890        END-IF                                                            
016891        PERFORM IMS-GN-WDB601                                             
016892     END-PERFORM                                                          
016900     .                                                                    
017000                                                                          
017100     EJECT                                                                
017200 B-KOLLA-SKRIV SECTION.                                                   
017300                                                                          
017320     MOVE 1 TO DC-IX                                                      
017330     PERFORM UNTIL WS-DC(DC-IX) = SPACE                                   
017340                OR DC-IX > DC-IX-MAX                                      
017350                OR WS-DC(DC-IX) = INL-IDDC                                
017360                                                                          
017393        ADD 1 TO DC-IX                                                    
017394     END-PERFORM                                                          
017395                                                                          
017396     IF DC-IX NOT > DC-IX-MAX AND                                         
017397        WS-DC(DC-IX) = INL-IDDC                                           
017398                                                                          
017399        ADD 1 TO WS-DC-ANT(DC-IX)                                         
017400        IF WS-DC-ANT(DC-IX) > MAX-ANT                                     
017401           IF WS-DAINLINL < WS-RENS-DATUM                                 
017405              PERFORM S02-SKAPA-SKRIV-W61215                              
017408              ADD 1 TO WS-DC-TOT(DC-IX)                                   
017409           END-IF                                                         
017410        END-IF                                                            
017420     END-IF                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 Z-FINIT SECTION.                                                         
026100                                                                          
026200     CLOSE W61215                                                         
026300                                                                          
026400     DISPLAY '*** ALLT SOM ÄR ÄLDRE ÄN ' WS-RENS-DATUM                    
026500     DISPLAY '*** ÄR SKRIVET PÅ FIL FÖR SENARE RENSNING '                 
026600     DISPLAY '***     '                                                   
026610     IF WS-DC(DC-IX-MAX) NOT = SPACE                                      
026611        DISPLAY '*** OBS OBS OBS OBS OBS OBS OBS OBS OBS ***'             
026612        DISPLAY '*** DC-TABELLEN ÄR FULL                 ***'             
026613        DISPLAY '*** OBS OBS OBS OBS OBS OBS OBS OBS OBS ***'             
026614        DISPLAY '                                        ***'             
026620     END-IF                                                               
026630                                                                          
026640     MOVE 1  TO DC-IX                                                     
026650     PERFORM UNTIL DC-IX > DC-IX-MAX                                      
026660                OR WS-DC(DC-IX) = SPACE                                   
026670                                                                          
026671        MOVE SPACE TO WS-STRINGRAD                                        
026672                                                                          
026673        STRING 'ANTAL DC ' WS-DC(DC-IX) ' = ' WS-DC-TOT(DC-IX)            
026674        DELIMITED BY SIZE  INTO WS-STRINGRAD                              
026675                                                                          
026676        DISPLAY WS-STRINGRAD                                              
026677        ADD 1 TO DC-IX                                                    
026678                                                                          
026692     END-PERFORM                                                          
026700                                                                          
027800     DISPLAY 'TOTAL       = '  WS-W61215-ANT                              
027900     .                                                                    
028000                                                                          
028100     EJECT                                                                
028200 S01-NOLLA-RAEKNARE SECTION.                                              
028300                                                                          
028310                                                                          
028320     MOVE 1  TO DC-IX                                                     
028340     PERFORM UNTIL DC-IX > DC-IX-MAX                                      
028341                OR WS-DC(DC-IX) = SPACE                                   
028350                                                                          
028360        MOVE ZERO  TO  WS-DC-ANT(DC-IX)                                   
028361        MOVE ZERO  TO  WS-DC-TOT(DC-IX)                                   
028370        ADD  1 TO DC-IX                                                   
028391     END-PERFORM                                                          
029400     .                                                                    
029500     EJECT                                                                
029600 S02-SKAPA-SKRIV-W61215 SECTION.                                          
029700                                                                          
029800     MOVE INL-WDL611 TO UT-WDL611                                         
029900     WRITE POST FROM UT-AREA                                              
030000     ADD +1 TO WS-W61215-ANT                                              
030100     .                                                                    
030200                                                                          
030300     EJECT                                                                
030400* --- IMS SEKTIONER ---                                                   
030500                                                                          
030600 IMS-GET-INLC SECTION.                                                    
030700                                                                          
030800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
030900     CALL CBLTDLI USING GN INLC-PCB DLI-IO-AREA                           
031000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300                                                                          
031400                                                                          
031410 IMS-GN-WDB601    SECTION.                                                
031420     MOVE 'WDB601  ' TO SSA1                                              
031440     MOVE '  GB' TO GODK-STATUSKODER                                      
031450     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
031460     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031470     PERFORM IMS-STATUSKONTROLL                                           
031480     IF SEGMENT-SLUT                                                      
031490         MOVE SPACE TO DCS-IDDC                                           
031491     END-IF                                                               
031492     .                                                                    
031500 IMS-STATUSKONTROLL SECTION.                                              
031600                                                                          
031700     SET STATUS-IX TO 1                                                   
031800     SEARCH GODK-STATUS                                                   
031900       AT END                                                             
032000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032100           DELIMITED BY SIZE INTO FELTEXT                                 
032200         DISPLAY FELTEXT                                                  
032300         CALL FELLOG                                                      
032400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032500         CONTINUE                                                         
032600     END-SEARCH                                                           
032700     .                                                                    
