000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.      W4781500.                                               
000500 AUTHOR.          ANNA-LENA ANDERSSON/BRA                                 
000600 DATE-WRITTEN.    JUNI 1980                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001300*    FUNKTION:                                                            
001400*                 ********  SB - PROGRAM    *********                     
001500*                 PROGRAMMET LÄSER KOLLIREGISTRET                         
001600*                 (WDE601,WDE611,WDE4E) OCH SKAPAR FILEN                  
001700*                 W47815, SOM ÄR UNDERLAG FÖR LISTAN:                     
001800*                 'DAGLIG UPPFÖLJNING TILL RA'                            
001900*                                                                         
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP3                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*- - - - - - - - - - - - - - UTFIL:                                       
002800*                      - - UTDRAG UR KOLLIREGISTER FÖR DAGLIG             
002900*                                UPPFÖLJNINGSLISTA TILL RA                
003000     SELECT W47815-UTUPPF                ASSIGN TO UT-S-W47815D1.         
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800 FD  W47815-UTUPPF                                                        
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      F                                                     
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300*01  POST  -COPY W47815     -PRE UTUPPF- -L.                              
004500     SKIP3                                                                
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005401*    -COPY WY2000W1                                                       
005410     SKIP3                                                                
005500*- - - - - - - - - - - - - - GENERERAT PROGRAMNAMN                        
005600 77  PROGRAM-NAMN                PIC X(8)  VALUE 'W4781500'.              
005700                                                                          
005800 77  JA                          PIC X(1)        VALUE 'J'.               
005900 77  NEJ                         PIC X(1)        VALUE 'N'.               
006000     SKIP3                                                                
006100 77  SPAR-VORD-FLDIRLEV          PIC X(1).                                
006200     SKIP3                                                                
006300 01  SWITCHAR.                                                            
006400     03  ORAD-FINNS-SW               PIC X(1)    VALUE 'N'.               
006500         88  ORAD-FINNS                          VALUE 'J'.               
006600     03  AKTUELL-SW                  PIC X(1).                            
006700         88  AKTUELL-POST                        VALUE 'J'.               
006800         88  EJ-AKTUELL-POST                     VALUE 'N'.               
006900     03  AKTUELL-TYP6-SW             PIC X(1).                            
007000         88  AKTUELL-TYP6-POST                   VALUE 'J'.               
007100     03  AKTUELL-UTUPPF-SW           PIC X(1).                            
007200         88  AKTUELL-UTUPPF-POST                 VALUE 'J'.               
007300     EJECT                                                                
007400 01  TEST-IDDISTR                    PIC 9(5) COMP-3.                     
007500*01  FILLER -COPY WWDIST03   -RED TEST-IDDISTR.                           
007700*01  FILLER -COPY WWDIST20   -RED TEST-IDDISTR.                           
007900*01  FILLER -COPY WWDIST93   -RED TEST-IDDISTR.                           
008100 01  DAGENS-DATUM.                                                        
008200                                                                          
008300     03    DAGENS-DAT.                                                    
008400           05    DAGENS-DATUM-AA     PIC 99.                              
008500           05    DAGENS-DATUM-MM     PIC 99.                              
008600           05    DAGENS-DATUM-DD     PIC 99.                              
008700     03    DAGENS-DATUM-AAMMDD       REDEFINES DAGENS-DAT                 
008800                                     PIC 9(6).                            
008900     03    DAGENS-DATUM-COMP3        PIC S9(7)        COMP-3.             
009000     03    DAGENS-DATUM-DAGNR        PIC 9(1).                            
009100                                                                          
009200 01  IDTTYP-VAERDEN.                                                      
009300     03    IDTTYP-1                  PIC X(3)         VALUE '001'.        
009400     03    IDTTYP-2                  PIC X(3)         VALUE '002'.        
009500     03    IDTTYP-3                  PIC X(3)         VALUE '003'.        
009600     03    IDTTYP-4                  PIC X(3)         VALUE '004'.        
009700     03    IDTTYP-6                  PIC X(3)         VALUE '006'.        
009800     03    IDTTYP-F1                 PIC X(3)         VALUE 'F1 '.        
009900     03    IDTTYP-F2                 PIC X(3)         VALUE 'F2 '.        
010000                                                                          
010100 01  LAGRA-IDENTITETER.                                                   
010200     03    LAG-UTUPPF-IDDISTR        PIC S9(5)        COMP-3              
010300                                     VALUE +0.                            
010400     03    LAG-UTUPPF-IDKUNDNR       PIC S9(7)        COMP-3              
010500                                     VALUE +0.                            
010600     03    LAG-UTUPPF-IDTTYP         PIC X(3)                             
010700                                     VALUE '000'.                         
010800     03    LAG-UTUPPF-TIBEGPAC       PIC S9(7)        COMP-3              
010900                                     VALUE +0.                            
011600                                                                          
011700 01  DYNAMISKA-SUBPROGRAM.                                                
011800     03  POSTSUM                     PIC X(8)    VALUE 'POSTSUM '.        
011900     03  DATKORT                     PIC X(8)    VALUE 'DATKORT '.        
012000     03  CBLTDLI                     PIC X(8)    VALUE 'CBLTDLI '.        
012100     03  FELLOG                      PIC X(8)    VALUE 'FELLOG  '.        
012200     EJECT                                                                
012300*- - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                      
012400                                                                          
012500*    -COPY W0005       -PRE POSTSUM-                                      
012700     EJECT                                                                
012800*- - - - - - - - - - - - - - PARAMETRAR TILL DATKORT                      
012900 01  DATUMKORT-ID                PIC X(6)   VALUE 'WDATUM'.               
013000*    -COPY WDATKORT                                                       
013200     EJECT                                                                
013300 01  FILLER                      PIC X(24)  VALUE                         
013400                                            'UTUPPF-AREA-START'.          
013500     SKIP2                                                                
013600*01  AREA  -COPY W47815     -PRE UTUPPF-                                  
014600     EJECT                                                                
014700 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
014710                                                                          
014800 01  W-WDE4E1KY-MIN-X.                                                    
014900     03  W-IDPRODNR-MIN          PIC S9(7) VALUE +0 COMP-3.               
014901     03  FILLER                  PIC X(19) VALUE LOW-VALUE.               
014902                                                                          
014903 01  W-WDE4E1KY-MAX-X.                                                    
014904     03  W-IDPRODNR-MAX          PIC S9(7) VALUE +0 COMP-3.               
014905     03  FILLER                  PIC X(19) VALUE HIGH-VALUE.              
014906                                                                          
014910 01  IMS-WS.                                                              
014920     03  STATUS-WS               PIC X(2).                                
015000        88 SEGMENT-FINNS                    VALUE '  '.                   
015100        88 SEGMENT-SAKNAS                   VALUE 'GE'.                   
015200        88 BASEN-SLUT                       VALUE 'GB'.                   
015300     03  GODK-STATUSKODER.                                                
015400        05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).           
015500                                                                          
015510     03  SSA1                    PIC X(96).                               
015600*                     IMS FUNKTIONSKODER                                  
015700*01    -COPY W0003                                                        
015900     EJECT                                                                
016000 01  DLI-IO-AREA.                                                         
016100                                                                          
016200   03  IO-AREA1                  PIC X(500).                              
016300                                                                          
016400*  03  VORD-AREA  -COPY WDE601       -RED IO-AREA1                        
016600     EJECT                                                                
016700*  03  KOLLI-AREA -COPY WDE611       -RED IO-AREA1                        
016900     EJECT                                                                
016910 01  DLI-IO-E4E1.                                                         
017000*  03  -COPY WDE4E1                                                       
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017400 01  PREFIX                      PIC X.                                   
017500*01  -COPY W0008     -PRE WDE6-                                           
017700     05 FILLER                   PIC X.                                   
017800     EJECT                                                                
017810*01  -COPY W0008     -PRE WDE4E-                                          
017820     05 FILLER                   PIC X.                                   
017830     EJECT                                                                
017900 PROCEDURE DIVISION USING WDE6-PCB WDE4E-PCB.                             
018000     ENTRY 'DLITCBL' USING WDE6-PCB WDE4E-PCB.                            
018100                                                                          
018200     PERFORM A-INIT                                                       
018300     PERFORM IMS-LAES-WDE6                                                
018400     PERFORM UNTIL BASEN-SLUT                                             
018500         EVALUATE WDE6-SEG-NAME-FB                                        
018600           WHEN 'WDE601  '                                                
018610              IF ORAD-FINNS                                               
018630                PERFORM F-UTUPPF-BEHANDLA-E4-POST                         
018640              END-IF                                                      
018700              MOVE VORD-IDDISTR TO TEST-IDDISTR                           
018800              IF VORD-KVORDRAD  > ZERO  AND NOT                           
018900                 DIST20-EMBALLAGE       AND NOT                           
019100                 DIST93-BYTESRADIO-C2                                     
019200                MOVE JA    TO ORAD-FINNS-SW                               
019300                PERFORM B-UTUPPF-BEHANDLA-01-POST                         
019400                MOVE VORD-IDPRODNR  TO W-IDPRODNR-MIN                     
019410                                       W-IDPRODNR-MAX                     
019500              ELSE                                                        
019600                MOVE NEJ   TO ORAD-FINNS-SW                               
019700              END-IF                                                      
019800           WHEN 'WDE611  '                                                
019900              IF ORAD-FINNS                                               
020000                PERFORM D-UTUPPF-BEHANDLA-10-POST                         
020700              END-IF                                                      
020800         END-EVALUATE                                                     
020900         PERFORM IMS-LAES-WDE6                                            
021000     END-PERFORM                                                          
021010     IF ORAD-FINNS                                                        
021020       PERFORM F-UTUPPF-BEHANDLA-E4-POST                                  
021030     END-IF                                                               
021100                                                                          
021200     PERFORM Z-FINIT                                                      
021300     MOVE ZERO TO RETURN-CODE                                             
021400     GOBACK.                                                              
021500     EJECT                                                                
021600 A-INIT SECTION.                                                          
021700     SKIP2                                                                
021800     OPEN OUTPUT W47815-UTUPPF                                            
022000                                                                          
022100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
022200                                                                          
022300*                       DATUMINFORMATION HÄMTAS FRÅN DATUMKORT            
022400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT.              
022500                                                                          
022600     MOVE D-AAR               TO DAGENS-DATUM-AA                          
022700     MOVE D-MAANAD            TO DAGENS-DATUM-MM                          
022800     MOVE D-DAG               TO DAGENS-DATUM-DD                          
022900     MOVE DAGENS-DATUM-AAMMDD TO DAGENS-DATUM-COMP3                       
023000     MOVE D-DAGNR             TO DAGENS-DATUM-DAGNR                       
023100     .                                                                    
023200     EJECT                                                                
023300 B-UTUPPF-BEHANDLA-01-POST SECTION.                                       
023400                                                                          
023500*    KONTROLLERA OM POST SKALL ANVÄNDAS. OM VILLKOREN ÄR UPPFYLLDA        
023600*    FÖR IDTTYP = 1 SKRIVS POSTEN UT. ANNARS SPARAS UPPGIFTER             
023700*    I UTUPPF-AREA.                                                       
023800                                                                          
023900     MOVE +0                 TO LAG-UTUPPF-IDDISTR                        
024000     MOVE +0                 TO LAG-UTUPPF-IDKUNDNR                       
024100     MOVE VORD-FLDIRLEV TO SPAR-VORD-FLDIRLEV                             
024101     MOVE VORD-DABEGPAC (3:6)  TO TMP1-YYMMDD                             
024102     MOVE DAGENS-DATUM-AAMMDD  TO TMP2-YYMMDD                             
024110     PERFORM WY2000P1                                                     
024200     IF TMP1-YYMMDD <= TMP2-YYMMDD                                        
024400        IF VORD-KDORDKL = 2 OR 3                                          
024500           IF VORD-KDORDSTA = 1                                           
024600              MOVE IDTTYP-1  TO UTUPPF-IDTTYP                             
024700              MOVE IDTTYP-1  TO LAG-UTUPPF-IDTTYP                         
024800              PERFORM BAA-FYLL-I-UPPGIFTER-UTUPPF                         
024900              PERFORM S01-SKRIV-UTUPPF-POST                               
025000           END-IF                                                         
025100                                                                          
025200        ELSE                                                              
025300                                                                          
025400              IF VORD-KDORDKL = +0                                        
025500                                                                          
025600                 MOVE IDTTYP-2  TO UTUPPF-IDTTYP                          
025700                 MOVE IDTTYP-2  TO LAG-UTUPPF-IDTTYP                      
025800                 PERFORM BA-KDORDKL-0-1                                   
025900                                                                          
026000               ELSE                                                       
026100                                                                          
026200                 IF VORD-KDORDKL = +1                                     
026300                                                                          
026400                    MOVE IDTTYP-3 TO UTUPPF-IDTTYP                        
026500                    MOVE IDTTYP-3 TO LAG-UTUPPF-IDTTYP                    
026600                    PERFORM BA-KDORDKL-0-1                                
026700                                                                          
026800                 END-IF                                                   
026900               END-IF                                                     
027000       END-IF                                                             
027100     END-IF                                                               
027110     .                                                                    
027200     EJECT                                                                
027300 BA-KDORDKL-0-1 SECTION.                                                  
027400                                                                          
027500     MOVE NEJ TO AKTUELL-UTUPPF-SW                                        
027600     IF VORD-KDORDSTA = 1                                                 
027700        PERFORM BAA-FYLL-I-UPPGIFTER-UTUPPF                               
027800        PERFORM S01-SKRIV-UTUPPF-POST                                     
027900        MOVE JA TO AKTUELL-UTUPPF-SW                                      
028000     END-IF                                                               
028010     .                                                                    
028100     EJECT                                                                
028200 BAA-FYLL-I-UPPGIFTER-UTUPPF SECTION.                                     
028300                                                                          
028400     MOVE VORD-IDDISTR       TO UTUPPF-IDDISTR                            
028500     MOVE VORD-IDKUNDNR      TO UTUPPF-IDKUNDNR                           
028600     MOVE VORD-IDPRODNR      TO UTUPPF-IDPRODNR                           
028700     MOVE SPACE              TO UTUPPF-IDKUNDRF                           
028800     MOVE VORD-IDDC          TO UTUPPF-IDDC                               
028900     MOVE +0                 TO UTUPPF-IDKOLLI                            
029000     MOVE VORD-KDFRAKT       TO UTUPPF-KDFRAKT                            
029100     MOVE VORD-VKORDNTO      TO UTUPPF-VKORDNTO                           
029200     MOVE VORD-VLORDNTO      TO UTUPPF-VLORDNTO                           
029300     MOVE VORD-VKORDBTO      TO UTUPPF-VKORDBTO                           
029400     MOVE VORD-VLORDBTO      TO UTUPPF-VLORDBTO                           
029500     MOVE VORD-IDLOTNR       TO UTUPPF-IDLOTNR                            
029600     MOVE VORD-KDORDLOT      TO UTUPPF-KDORDLOT                           
029700     MOVE VORD-ADLEVPL       TO UTUPPF-ADLEVPL                            
029800     MOVE VORD-DABEGPAC (3:6) TO UTUPPF-TIBEGPAC                          
029900                                LAG-UTUPPF-TIBEGPAC                       
030000*                               TIBEGPAC ANVÄNDS SOM SORTERINGARG         
030010     .                                                                    
030100     EJECT                                                                
040200 D-UTUPPF-BEHANDLA-10-POST SECTION.                                       
040300                                                                          
040400*    OM VILLKOREN ÄR UPPFYLLDA FÖR IDTTYP = 2 ELLER 3                     
040500*    SKRIVS POSTEN UT                                                     
040600     SKIP2                                                                
040700     IF AKTUELL-UTUPPF-POST                                               
040800       IF UTUPPF-IDTTYP = IDTTYP-2 OR IDTTYP-3                            
040900          MOVE UTUPPF-IDDISTR TO TEST-IDDISTR                             
041000          IF KOLLI-TILASTN = +0                                           
041100             AND NOT DIST03-SVERIGE                                       
041200                                                                          
041300             MOVE KOLLI-IDKOLLI TO UTUPPF-IDKOLLI                         
041400             MOVE KOLLI-VKORDBTO-KOLLI TO UTUPPF-VKORDBTO                 
041500             MOVE KOLLI-VLORDBTO-KOLLI TO UTUPPF-VLORDBTO                 
041600             MOVE +0            TO UTUPPF-VKORDNTO                        
041700             MOVE +0            TO UTUPPF-VLORDNTO                        
041800             MOVE LAG-UTUPPF-TIBEGPAC TO UTUPPF-TIBEGPAC                  
041900             PERFORM S01-SKRIV-UTUPPF-POST                                
042000                                                                          
042100          END-IF                                                          
042200       END-IF                                                             
042300     END-IF                                                               
042310     .                                                                    
042400     EJECT                                                                
048400 F-UTUPPF-BEHANDLA-E4-POST SECTION.                                       
048500                                                                          
048600*    OM NÅGON IDTTYP ÄR UTSKRIVEN, SKRIVS EN POST MED                     
048700*    ENDAST IDKUNDRF                                                      
048800     SKIP2                                                                
048810     PERFORM IMS-GU-WDE4E1                                                
048820     IF SEGMENT-FINNS                                                     
048900       IF LAG-UTUPPF-IDDISTR = SEQE-IDDISTR                               
049000          AND LAG-UTUPPF-IDKUNDNR = SEQE-IDKUNDNR                         
049100                                                                          
049200        IF LAG-UTUPPF-IDTTYP = IDTTYP-1 OR = IDTTYP-2                     
049300                          OR = IDTTYP-3                                   
049400                                                                          
049500           MOVE LAG-UTUPPF-IDTTYP TO UTUPPF-IDTTYP                        
049600           MOVE SEQE-IDKUNDRF   TO UTUPPF-IDKUNDRF                        
049700           MOVE +0              TO UTUPPF-IDKOLLI                         
049800           MOVE +0              TO UTUPPF-KDFRAKT                         
049900           MOVE +0              TO UTUPPF-VKORDNTO                        
050000           MOVE +0              TO UTUPPF-VLORDNTO                        
050100           MOVE +0              TO UTUPPF-IDLOTNR                         
050200           MOVE SPACE           TO UTUPPF-KDORDLOT                        
050300           MOVE +0              TO UTUPPF-ADLEVPL                         
050400           MOVE LAG-UTUPPF-TIBEGPAC TO UTUPPF-TIBEGPAC                    
050500                                                                          
050600           PERFORM S01-SKRIV-UTUPPF-POST                                  
050700           MOVE +0              TO LAG-UTUPPF-IDDISTR                     
050800           MOVE +0              TO LAG-UTUPPF-IDKUNDNR                    
050900           MOVE +0              TO LAG-UTUPPF-IDTTYP                      
051000                                                                          
051100         END-IF                                                           
051200       END-IF                                                             
051201     END-IF                                                               
051210     .                                                                    
051300     EJECT                                                                
056600 S01-SKRIV-UTUPPF-POST SECTION.                                           
056700                                                                          
056800     IF UTUPPF-IDKUNDRF = SPACE                                           
056900        MOVE UTUPPF-IDDISTR        TO LAG-UTUPPF-IDDISTR                  
057000        MOVE UTUPPF-IDKUNDNR       TO LAG-UTUPPF-IDKUNDNR                 
057100     END-IF                                                               
057200     IF SPAR-VORD-FLDIRLEV NOT = 'J'                                      
057300     WRITE UTUPPF-POST  FROM UTUPPF-AREA                                  
057400     END-IF                                                               
057500                                                                          
057600     MOVE 'W47815-UTUPPF'          TO POSTSUM-FDNAMN                      
057700     MOVE 'W47815D1'               TO POSTSUM-DDNAMN2                     
057800     MOVE UTUPPF-IDTTYP            TO POSTSUM-TRANSTYP                    
057900     CALL POSTSUM  USING POSTSUM-PARM                                     
057910     .                                                                    
058000     EJECT                                                                
064500 Z-FINIT SECTION.                                                         
064600                                                                          
064700     CLOSE W47815-UTUPPF                                                  
064900                                                                          
065000*- - - - - - - - - - - - - - - -  SKRIV UT ANTAL SKRIVNA POSTER           
065100     MOVE 'S' TO POSTSUM-OPKOD                                            
065200     CALL POSTSUM USING POSTSUM-PARM                                      
065210     .                                                                    
065300     EJECT                                                                
065400*- - - - - - - - - - - I M S - SECTION                                    
065500 IMS-LAES-WDE6 SECTION.                                                   
065600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
065700     CALL CBLTDLI USING GN WDE6-PCB IO-AREA1                              
065800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
065900     PERFORM IMS-STATUSKONTROLL                                           
065910     .                                                                    
066000     SKIP3                                                                
066010 IMS-GU-WDE4E1                 SECTION.                                   
066020     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
066030                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
066040            DELIMITED BY SIZE INTO SSA1                                   
066050     MOVE '  GE' TO GODK-STATUSKODER                                      
066060     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-E4E1 SSA1                     
066070     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
066080     PERFORM IMS-STATUSKONTROLL                                           
066090     .                                                                    
066091     SKIP3                                                                
066100 IMS-STATUSKONTROLL SECTION.                                              
066200     SET STATUS-IX TO 1                                                   
066300     SEARCH GODK-STATUS AT END CALL FELLOG                                
066400     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
066500     NEXT SENTENCE                                                        
066600     END-SEARCH                                                           
066700     .                                                                    
066710     EJECT                                                                
066800*    -COPY WY2000P1                                                       
