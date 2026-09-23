000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.    W9101200.                                                 
000400 AUTHOR.        INGRID DANIELSSON.                                        
000500*    DATE-WRITTEN.  AUG 1989.                                             
000600*    UPDATED BY J. NIHLBLAD DEC. 2007                                     
000700                                                                          
000800     REMARKS.                                                             
000900*               PROGRAMMET LÄSER CONCAT INFIL W092PP/W111PP               
001000*               SOM SORTERAS FÖR ATT SKAPA UTFIL W91012.                  
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP3                                                                
001400 CONFIGURATION SECTION.                                                   
001500 SPECIAL-NAMES.                                                           
001600     ALPHABET Y2000 IS X'05' THRU  X'09' X'00' THRU X'04'.                
001700 INPUT-OUTPUT SECTION.                                                    
001800     SKIP3                                                                
001900 FILE-CONTROL.                                                            
002000     SKIP3                                                                
002100     SELECT INFIL      ASSIGN TO UT-S-W91012D1.                           
002200     SELECT UTFIL      ASSIGN TO UT-S-W91012D2.                           
002300     SELECT SORTFIL    ASSIGN TO UT-S-W91012DS.                           
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  INFIL                                                                
003000     LABEL RECORDS ARE STANDARD                                           
003100     RECORDING MODE IS F                                                  
003200     BLOCK CONTAINS 0.                                                    
003300     SKIP3                                                                
003400*01  POST    -COPY W111PPMS    -PRE IN-    -L.                            
003500                                                                          
003600     EJECT                                                                
003700 FD  UTFIL                                                                
003800     LABEL RECORDS ARE STANDARD                                           
003900     RECORDING MODE IS V                                                  
004000     BLOCK CONTAINS 0.                                                    
004100     SKIP3                                                                
004200*01  POST    -COPY W91012      -PRE UT-  -L.                              
004300                                                                          
004400     SKIP3                                                                
004500*01          -COPY PPMS135.                                               
004600                                                                          
004700     EJECT                                                                
004800 SD  SORTFIL                                                              
004900     RECORDING MODE IS F.                                                 
005000     SKIP3                                                                
005100*01  POST    -COPY W111PPMS    -PRE SORT-                                 
005200*Y2K-SORT                                                                 
005300     03 FILLER REDEFINES SORT-W111PPMS.                                   
005400        05  FILLER           PIC X(8).                                    
005500        05  SORT-DECADE      PIC X.                                       
005600        05  SORT-SMALL-DATE  PIC S9(5) COMP-3.                            
005700                                                                          
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000     SKIP3                                                                
006100                                                                          
006200*    -- CHECKED BY WY2000                                                 
006300 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W9101200'.                
006400     SKIP3                                                                
006500 01  GENERELLA-KONSTANTER.                                                
006600     03  JA                  PIC X       VALUE 'J'.                       
006700     03  NEJ                 PIC X       VALUE 'N'.                       
006800     SKIP3                                                                
006900*01  -COPY WWPRODSL                                                       
007000     SKIP3                                                                
007100 01  SWITCHAR.                                                            
007200                                                                          
007300     03  EOF-SW              PIC X       VALUE 'N'.                       
007400         88  EOF                         VALUE 'J'.                       
007500                                                                          
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
007900     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM '.             
008000     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     SKIP3                                                                
008400 01  RETURKODER.                                                          
008500     03  RKOD                  PIC S9(4)  COMP SYNC  VALUE ZERO.          
008600     03  RKOD-ABEND-UTAN-DUMP  PIC S9(4)  COMP SYNC  VALUE +16.           
008700     03  RKOD-ABEND-MED-DUMP   PIC S9(4)  COMP SYNC  VALUE +1000.         
008800     EJECT                                                                
008900*01  -COPY W0005            -PRE POSTSUM-                                 
009000     EJECT                                                                
009100*01  WDATAREA    -COPY WDATAREA.                                          
009200 01  UT-TRANSID.                                                          
009300     03  FILLER              PIC X(6)     VALUE 'W91012'.                 
009400     03  FILLER              PIC X(8)     VALUE 'W91012D2'.               
009500     03  FILLER              PIC X(4)     VALUE 'POST'.                   
009600     EJECT                                                                
009700*01  AREA   -COPY W111PPMS     -PRE S-                                    
009800     EJECT                                                                
009900 01  FELTEXT.                                                             
010000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010200     EJECT                                                                
010300 01  W-AREA.                                                              
010400     03  WS-IDARTNR      PIC S9(9)  VALUE ZERO  COMP-3.                   
010500     03  W-TIAAMMDD      PIC 9(6).                                        
010600     03  W-TIKLOCK       PIC 9(8).                                        
010700     03  W-KDERS         PIC 9(2).                                        
010800     03  W-ANMARKNING    PIC X(8).                                        
010900     EJECT                                                                
011000*01  AREA   -COPY PPMS135      -PRE 135-                                  
011100     EJECT                                                                
011200*01  AREA   -COPY W91012       -PRE W910-                                 
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900                                                                          
012000     03  W-IDSKYLT-X.                                                     
012100         05  W-IDSKYLT           PIC XXX     VALUE 'GB '.                 
012200     SKIP2                                                                
012300*    --- STATUS-KOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FINNS                       VALUE '  '.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012800     SKIP2                                                                
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013400     EJECT                                                                
013500*    --- IMS FUNKTIONSKODER                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
014000 01  DLI-IO-WDK601.                                                       
014100*    03  -COPY WDK601                                                     
014200 01  DLI-IO-WDK611.                                                       
014300*    03  -COPY WDK611                                                     
014400     EJECT                                                                
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD7'.                        
014600 01  DLI-IO-WDD701.                                                       
014700*    03  -COPY WDD701                                                     
014800 01  DLI-IO-WDD702.                                                       
014900*    03  -COPY WDD702                                                     
015000     EJECT                                                                
015100 LINKAGE SECTION.                                                         
015200                                                                          
015300*01  -COPY W0008  -PRE WDK6-                                              
015400     05  FILLER                  PIC X.                                   
015500*01  -COPY W0008  -PRE WDD7-                                              
015600     05  FILLER                  PIC X.                                   
015700     EJECT                                                                
015800                                                                          
015900 PROCEDURE DIVISION  USING WDK6-PCB WDD7-PCB.                             
016000 MAIN SECTION.                                                            
016100     ENTRY 'DLITCBL' USING WDK6-PCB WDD7-PCB.                             
016200                                                                          
016300     PERFORM A-INIT                                                       
016400                                                                          
016500*    SORT SORTFIL ASCENDING                                               
016600     SORT SORTFIL DESCENDING                                              
016700                  SORT-IDARTNR                                            
016800                  SORT-DECADE                                             
016900                  SORT-SMALL-DATE                                         
017000                  SORT-TIKLOCK                                            
017100*                 COLLATING SEQUENCE Y2000                                
017200     USING INFIL                                                          
017300     OUTPUT PROCEDURE IS B-BEHANDLA-OCH-SKRIV                             
017400                                                                          
017500     PERFORM Z-FINIT                                                      
017600                                                                          
017700     IF SORT-RETURN > 0                                                   
017800         MOVE RKOD-ABEND-MED-DUMP TO RKOD                                 
017900         CALL ABEND USING RKOD                                            
018000     ELSE                                                                 
018100         MOVE ZERO TO RETURN-CODE                                         
018200         GOBACK                                                           
018300     END-IF                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 A-INIT     SECTION.                                                      
018700     SKIP2                                                                
018800     OPEN OUTPUT UTFIL                                                    
018900                                                                          
019000*    MOVE    'IDAG'   TO DAT-KDDATFORM                                    
019100*    MOVE     ZERO    TO DAT-I-TIDATUM                                    
019200*    CALL WDATKONV USING DAT-KDDATFORM                                    
019300*                        DAT-I-TIDATUM                                    
019400*                        DAT-O-TIDATUM                                    
019500*                        DAT-KDSVAR                                       
019600*    IF DAT-KDSVAR-OK                                                     
019700*      MOVE DAT-TIAAMMDD  TO  135-DATUM                                   
019800*      MOVE     '135'     TO  135-POSTTYP                                 
019900*                                                                         
020000*      WRITE UT-POST FROM 135-AREA                                        
020100*      MOVE UT-TRANSID TO POSTSUM-TRANSID                                 
020200*      CALL POSTSUM USING POSTSUM-PARM                                    
020300*    ELSE                                                                 
020400*      DISPLAY  'DATUMHÄMTNINGEN HAR GÅTT SNETT'                          
020500*      MOVE   +16  TO   RKOD                                              
020600*      CALL ABEND USING RKOD                                              
020700*    END-IF                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 B-BEHANDLA-OCH-SKRIV          SECTION.                                   
021100 SKIP3                                                                    
021200     RETURN SORTFIL INTO S-AREA                                           
021300                           AT END   MOVE JA TO EOF-SW                     
021400     END-RETURN                                                           
021500                                                                          
021600     PERFORM UNTIL EOF                                                    
021700       IF SORT-IDARTNR NOT = WS-IDARTNR                                   
021800         MOVE SORT-IDARTNR TO W-IDARTNR                                   
021900                              WS-IDARTNR                                  
022000         PERFORM IMS-GU-WDK601                                            
022100         IF SEGMENT-FINNS                                                 
022200           PERFORM IMS-GN-WDK611                                          
022300           MOVE ART-KDPRODSL          TO TEST-KDPRODSL                    
022400           IF KDPRODSL-VOLVO-ALL                                          
022500             PERFORM IMS-GU-WDD701                                        
022600             IF SEGMENT-FINNS                                             
022700               PERFORM IMS-GN-WDD702                                      
022800               PERFORM UNTIL SEGMENT-SAKNAS                               
022900                 MOVE SORT-IDARTNR    TO W910-IDARTNR                     
023000                 MOVE IDKORTNR        TO W910-IDKORTNR                    
023100                 IF CLAG-KDERS > 10                                       
023200                   MOVE CLAG-KDERS    TO W910-KDERS                       
023300                 ELSE                                                     
023400                   MOVE ZERO          TO W910-KDERS                       
023500                 END-IF                                                   
023600                 IF FLTEXT = JA                                           
023700                   MOVE BEERS         TO W910-BEERS                       
023800                 ELSE                                                     
023900                   MOVE IDARTNR-TILLK TO W910-BEERS                       
024000                 END-IF                                                   
024100*****************SKRIV UT-POST**************                              
024200                 WRITE UT-POST FROM W910-AREA                             
024300                 MOVE UT-TRANSID TO POSTSUM-TRANSID                       
024400                 CALL POSTSUM USING POSTSUM-PARM                          
024500                 PERFORM IMS-GN-WDD702                                    
024600               END-PERFORM                                                
024700             END-IF                                                       
024800           END-IF                                                         
024900         END-IF                                                           
025000                                                                          
025100       END-IF                                                             
025200       RETURN SORTFIL INTO S-AREA                                         
025300                             AT END   MOVE JA TO EOF-SW                   
025400       END-RETURN                                                         
025500     END-PERFORM                                                          
025600     .                                                                    
025700     EJECT                                                                
025800 Z-FINIT    SECTION.                                                      
025900     SKIP2                                                                
026000     CLOSE UTFIL                                                          
026100                                                                          
026200     MOVE 'S' TO POSTSUM-OPKOD                                            
026300     CALL POSTSUM USING POSTSUM-PARM                                      
026400     .                                                                    
026500 IMS-GU-WDK601 SECTION.                                                   
026600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
026700     DELIMITED BY SIZE INTO SSA1                                          
026800     MOVE '  GE' TO GODK-STATUSKODER                                      
026900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
027000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027100     PERFORM IMS-STATUSKONTROLL                                           
027200     .                                                                    
027300     SKIP3                                                                
027400 IMS-GN-WDK611     SECTION.                                               
027500     MOVE 'WDK611  ' TO SSA1                                              
027600     MOVE '  GE' TO GODK-STATUSKODER                                      
027700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
027800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027900     PERFORM IMS-STATUSKONTROLL                                           
028000     .                                                                    
028100     SKIP3                                                                
028200 IMS-GU-WDD701 SECTION.                                                   
028300     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
028400     DELIMITED BY SIZE INTO SSA1                                          
028500     MOVE '  GE' TO GODK-STATUSKODER                                      
028600     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
028700     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     .                                                                    
029000     SKIP3                                                                
029100 IMS-GN-WDD702     SECTION.                                               
029200     MOVE 'WDD702  ' TO SSA1                                              
029300     MOVE '  GE' TO GODK-STATUSKODER                                      
029400     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
029500     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
029600     PERFORM IMS-STATUSKONTROLL                                           
029700     .                                                                    
029800     SKIP3                                                                
029900 IMS-STATUSKONTROLL SECTION.                                              
030000                                                                          
030100     SET STATUS-IX TO 1                                                   
030200     SEARCH GODK-STATUS                                                   
030300       AT END                                                             
030400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030500           DELIMITED BY SIZE INTO FELTEXT                                 
030600         DISPLAY FELTEXT                                                  
030700         CALL FELLOG                                                      
030800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030900         CONTINUE                                                         
031000     END-SEARCH                                                           
031100     .                                                                    
