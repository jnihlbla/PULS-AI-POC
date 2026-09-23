000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3309600.                                                 
000400 AUTHOR.        RONNY STENHOLM.                                           
000500 DATE-WRITTEN.  DECEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SPARAR ÅRETS STATISTIK ENLIGT ART/DISTR/ÅR.                      
001000*        POSTERNA SUMMERAS PÅ SAMTLIGA FÄLT MED UNDANTAG AV               
001100*        PRARTSJK SOM MULTIPLICERAS MED SULEVANT OCH LÄGGS                
001200*        I SUARTSJK.                                                      
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*    --- INFIL PRIMÄWREGISTRET:                                           
002200                                                                          
002300     SELECT W33013                       ASSIGN TO W33096D1.              
002400                                                                          
002500*    --- UTFIL:                                                           
002600*           --- ARTIKELINFORMATION ÅRSSTATISTIK:                          
002700     SELECT W33096                       ASSIGN TO W33096D2.              
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W33013                                                               
003400     LABEL RECORD   STANDARD                                              
003500     RECORDING      V                                                     
003600     BLOCK CONTAINS 0.                                                    
003700     SKIP2                                                                
003800*01  -COPY W330310     -L.                                                
004000     SKIP2                                                                
004100*01  -COPY W330300     -L.                                                
004300     SKIP2                                                                
004400*01  -COPY W330320     -L.                                                
004600     SKIP2                                                                
004700*01  -COPY W330330     -L.                                                
004900     SKIP2                                                                
005000*01  -COPY W330340     -L.                                                
005200     SKIP2                                                                
005300*01  -COPY W330350     -L.                                                
005500     EJECT                                                                
005600 FD  W33096                                                               
005700     LABEL RECORD   STANDARD                                              
005800     RECORDING      F                                                     
005900     BLOCK CONTAINS 0.                                                    
006000     SKIP2                                                                
006100*01  POST -COPY W33096     -PRE UT- -L.                                   
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500     SKIP2                                                                
006501                                                                          
006510*    -- CHECKED BY WY2000                                                 
006600 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3309600'.            
006700 77  JA                          PIC X(1)    VALUE 'J'.                   
006800 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006900 77  INFIL-13-EOF                PIC X(1)    VALUE 'N'.                   
007000 77  RAETT-AR                    PIC X(1)    VALUE 'N'.                   
007100 77  FL-IX-TRAEFF                PIC X(1)    VALUE 'N'.                   
007110 77  FIRST-TIME                  PIC X(3)    VALUE 'YES'.                 
007200 77  SPAR-PRARTSJK               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007300 77  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
007400 77  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO COMP-3.           
007500 77  SOEKT-AA                    PIC 9(2)    VALUE ZERO.                  
007600 77  SOEK-IX                     PIC S9(4)   COMP SYNC.                   
007700 77  SKRIV-IX                    PIC S9(4)   COMP SYNC.                   
007800 77  TOTAL-IX                    PIC S9(4)   COMP SYNC.                   
007900 77  TAB-MAX                     PIC S9(9)   COMP VALUE ZERO.             
008000     SKIP3                                                                
008100 01  TEST-AAVV                   PIC 9(6)    VALUE 005700.                
008200 01  FILLER REDEFINES TEST-AAVV.                                          
008300     03  FILLER                  PIC 9(2).                                
008400     03  TEST-AA                 PIC 9(2).                                
008500     03  FILLER                  PIC 9(2).                                
008600 01  DAGENS-DATUM.                                                        
008700     03  DAGENS-AA               PIC 9(2)    VALUE ZERO.                  
008800     03  DAGENS-MM               PIC 9(2)    VALUE ZERO.                  
008900     03  DAGENS-DD               PIC 9(2)    VALUE ZERO.                  
009000                                                                          
009100     EJECT                                                                
009200 01  DYNAMISKA-SUBPROGRAM.                                                
009300*                                                                         
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
009700     SKIP2                                                                
009800*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
009900*                                                                         
010000 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
010100*01  -COPY W0005      -PRE  POSTSUM-                                      
010300     EJECT                                                                
010400*- - - - - - - - - - - - - - - - PARAMETRAR TILL WINTSOR                  
010500*                                                                         
010600 01  FILLER                       PIC X(16)  VALUE 'WINTSOR'.             
010700 01  TABENTRY-PARM.                                                       
010800     03  STEGLANGD                PIC S9(9) COMP.                         
010900     03  ANTAL                    PIC S9(9) COMP.                         
011000     03  NYCKELLANGD              PIC S9(9) COMP.                         
011100     EJECT                                                                
011200*- - - - - - - - - - - - - - - - UT-AREA                                  
011300*                                                                         
011400 01  FILLER                      PIC X(16)  VALUE 'UTAREA96'.             
011500 01  UT-AREA                     PIC X(82).                               
011600*01  FILLER  -PRE UT96- -COPY W33096      -RED UT-AREA.                   
011800     EJECT                                                                
011900*- - - - - - - - - - - - - - - - IN-AREA                                  
012000*                                                                         
012100 01  FILLER                      PIC X(16)  VALUE 'INAREA13'.             
012200 01  IN-AREA                     PIC X(28).                               
012300*01  FILLER  -PRE IN30- -COPY W330300     -RED IN-AREA.                   
012500     SKIP2                                                                
012600*01  FILLER  -PRE IN31- -COPY W330310     -RED IN-AREA.                   
012800     SKIP2                                                                
012900*01  FILLER  -PRE IN32- -COPY W330320     -RED IN-AREA.                   
013100     SKIP2                                                                
013200*01  FILLER  -PRE IN33- -COPY W330330     -RED IN-AREA.                   
013400     SKIP2                                                                
013500*01  FILLER  -PRE IN34- -COPY W330340     -RED IN-AREA.                   
013700     SKIP2                                                                
013800*01  FILLER  -PRE IN35- -COPY W330350     -RED IN-AREA.                   
014000     SKIP2                                                                
014100     EJECT                                                                
014200*- - - - - - - - - - - - - - - - TABELL-AREA.                             
014300*                                                                         
014400 01  FILLER                       PIC X(16)  VALUE 'TABELL'.              
014500 01  TABELL.                                                              
014600     03  TABELL-POST OCCURS 4000.                                         
014700         05  TAB-INDEX            PIC S9(5)      COMP-3.                  
014800         05  TAB-IDDISTR          PIC S9(5)      COMP-3.                  
014900     EJECT                                                                
015000*- - - - - - - - - - - - - - - - SOEK-AREA                                
015100*                                                                         
015200 01  FILLER                       PIC X(16)  VALUE 'SOEK-AREA'.           
015300 01  SOEK-AREA.                                                           
015400     03  SOEK-RAD OCCURS 4000.                                            
015500         05  SOEK-SUARTSJK        PIC S9(9)V9(2) COMP-3.                  
015600         05  SOEK-SULEVANT        PIC S9(9)      COMP-3.                  
015700         05  SOEK-SUARTFSG        PIC S9(9)V9(2) COMP-3.                  
015800         05  SOEK-SULEVANT-DO     PIC S9(9)      COMP-3.                  
015900         05  SOEK-SUARTFSG-DO     PIC S9(9)V9(2) COMP-3.                  
016000         05  SOEK-SULEVANT-RAB    PIC S9(9)      COMP-3.                  
016100         05  SOEK-SUARTFSG-RAB    PIC S9(9)V9(2) COMP-3.                  
016200         05  SOEK-SULEVANT-SPEC   PIC S9(9)      COMP-3.                  
016300         05  SOEK-SUARTFSG-SPEC   PIC S9(9)V9(2) COMP-3.                  
016400         05  SOEK-SULEVANT-MAN    PIC S9(9)      COMP-3.                  
016500         05  SOEK-SUARTFSG-MAN    PIC S9(9)V9(2) COMP-3.                  
016600         05  SOEK-SULEVANT-KRE    PIC S9(9)      COMP-3.                  
016700         05  SOEK-SUARTFSG-KRE    PIC S9(9)V9(2) COMP-3.                  
016800     EJECT                                                                
016900 PROCEDURE DIVISION.                                                      
017000     SKIP2                                                                
017100 STYR SECTION.                                                            
017110**    DISPLAY  'A-HITTA-RAETT SECT    '                                   
017200     PERFORM A-INIT                                                       
017210**    DISPLAY  'B-HITTA-RAETT SECT    '                                   
017300     PERFORM B-HITTA-RAETT-AAVV                                           
017310**   DISPLAY  'PROGRAM LEFT B-HITTA-RAETT'                                
017400     PERFORM UNTIL INFIL-13-EOF = JA                                      
017500                                                                          
017600        PERFORM UNTIL INFIL-13-EOF = JA          OR                       
017700                      (TEST-AA NOT = SOEKT-AA)   OR                       
017800                      SPAR-IDARTNR < IN30-IDARTNR                         
017900                                                                          
018000           MOVE IN30-PRARTSJK TO SPAR-PRARTSJK                            
018100           PERFORM S01-LAS-13-FIL                                         
018200                                                                          
018300           PERFORM UNTIL IN30-IDPTYP = 300 OR                             
018400                        INFIL-13-EOF = JA                                 
018500              PERFORM C-BERAEKNA-STATISTIK                                
018600              PERFORM S01-LAS-13-FIL                                      
018700           END-PERFORM                                                    
018800                                                                          
018900           IF IN30-IDPTYP = 300                                           
019000             MOVE IN30-DAFSGVV TO TEST-AAVV                               
019100           END-IF                                                         
019200                                                                          
019300        END-PERFORM                                                       
019400                                                                          
019500        PERFORM D-SKRIV-FIL96-FRAN-TABELL                                 
019600        MOVE ZERO TO TOTAL-IX                                             
019700                                                                          
019800        PERFORM B-HITTA-RAETT-AAVV                                        
019900                                                                          
020000     END-PERFORM                                                          
020100     PERFORM Z-FINIT                                                      
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700     SKIP2                                                                
020800     OPEN INPUT W33013                                                    
020900     OPEN OUTPUT W33096                                                   
021000                                                                          
021100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
021200                                                                          
021210* DENNA INITIERING GÖRS DÄRFÖR ATT DET VISAT SIG ATT NOLL-                
021220* STÄLLNINGEN I S03 S04 EJ FUNGERADE RIKTIGT                              
021230* DET BLEV KVAR VÄRDEN I TABELLEN.                                        
021240* JAG VARNAR DÄRFÖR FÖR ATT TA BORT DESSA / RS 970116                     
021300     INITIALIZE SOEK-AREA                                                 
021400     INITIALIZE TABELL                                                    
021500                                                                          
021600     MOVE ZERO      TO  SKRIV-IX                                          
021700     MOVE ZERO      TO  TOTAL-IX                                          
021800                                                                          
021900                                                                          
022000     MOVE +6        TO  STEGLANGD                                         
022100     MOVE +3        TO  NYCKELLANGD                                       
022200                                                                          
022300     ACCEPT DAGENS-DATUM FROM DATE                                        
022310**   DISPLAY 'DAGENS-DATUM  ' DAGENS-DATUM                                
022320     IF DAGENS-AA = 00                                                    
022330       MOVE 99 TO SOEKT-AA                                                
022340     ELSE                                                                 
022400       COMPUTE SOEKT-AA = DAGENS-AA - 1                                   
022410     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 B-HITTA-RAETT-AAVV SECTION.                                              
022900     MOVE NEJ TO RAETT-AR                                                 
022910**   DISPLAY 'IN30-DAFSGVV   ' IN30-DAFSGVV                               
022920**   DISPLAY 'IN30-IDPTYP    ' IN30-IDPTYP                                
022930**   DISPLAY 'RAETT-AR       ' RAETT-AR                                   
022940**   DISPLAY 'TEST-AA       ' TEST-AA                                     
022950**   DISPLAY 'SOEKT-AA      ' SOEKT-AA                                    
023000     PERFORM UNTIL (IN30-IDPTYP = 300 AND RAETT-AR = JA) OR               
023100                    INFIL-13-EOF = JA                                     
023110      IF FIRST-TIME = 'YES'                                               
023120       MOVE 'NO' TO FIRST-TIME                                            
023130      ELSE                                                                
023200       MOVE IN30-DAFSGVV TO TEST-AAVV                                     
023210      END-IF                                                              
023300       IF TEST-AA = SOEKT-AA                                              
023400         MOVE JA TO RAETT-AR                                              
023500       ELSE                                                               
023510**     DISPLAY 'LAES KOLL I B-SEKTIONEN'                                  
023600         PERFORM S01-LAS-13-FIL                                           
023700         PERFORM UNTIL IN30-IDPTYP = 300 OR                               
023800                 INFIL-13-EOF = JA                                        
023900           PERFORM S01-LAS-13-FIL                                         
024000         END-PERFORM                                                      
024100       END-IF                                                             
024200     END-PERFORM                                                          
024300     IF INFIL-13-EOF = NEJ                                                
024400        MOVE IN30-IDARTNR TO SPAR-IDARTNR                                 
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800                                                                          
024900 C-BERAEKNA-STATISTIK SECTION.                                            
024910**    DISPLAY  'C-BERAEKNA-STATS      '                                   
025000                                                                          
025100     IF IN31-IDPTYP = 310                                                 
025200        MOVE IN31-IDDISTR TO SPAR-IDDISTR                                 
025300        PERFORM CA-HITTA-RAETT-INDEX                                      
025400                                                                          
025500        COMPUTE SOEK-SUARTSJK(SOEK-IX) =                                  
025600                 SPAR-PRARTSJK * IN31-SULEVANT +                          
025700                 SOEK-SUARTSJK(SOEK-IX)                                   
025800        ADD IN31-SULEVANT TO SOEK-SULEVANT(SOEK-IX)                       
025900        ADD IN31-SUARTFSG TO SOEK-SUARTFSG(SOEK-IX)                       
026000        ADD IN31-SULEVANT-DO TO SOEK-SULEVANT-DO(SOEK-IX)                 
026100        ADD IN31-SUARTFSG-DO TO SOEK-SUARTFSG-DO(SOEK-IX)                 
026200     END-IF                                                               
026300     IF IN31-IDPTYP = 320                                                 
026400        ADD IN32-SULEVANT-RAB TO                                          
026500                 SOEK-SULEVANT-RAB(SOEK-IX)                               
026600        ADD IN32-SUARTFSG-RAB TO                                          
026700                 SOEK-SUARTFSG-RAB(SOEK-IX)                               
026800     ELSE                                                                 
026900        IF IN31-IDPTYP = 330                                              
027000          ADD IN33-SULEVANT-SPEC TO                                       
027100                   SOEK-SULEVANT-SPEC(SOEK-IX)                            
027200          ADD IN33-SUARTFSG-SPEC TO                                       
027300                   SOEK-SUARTFSG-SPEC(SOEK-IX)                            
027400        ELSE                                                              
027500           IF IN31-IDPTYP = 340                                           
027600              ADD IN34-SULEVANT-MAN TO                                    
027700                       SOEK-SULEVANT-MAN(SOEK-IX)                         
027800              ADD IN34-SUARTFSG-MAN TO                                    
027900                       SOEK-SUARTFSG-MAN(SOEK-IX)                         
028000           ELSE                                                           
028100              IF IN31-IDPTYP = 350                                        
028200                 ADD IN35-SULEVANT-KRE TO                                 
028300                          SOEK-SULEVANT-KRE(SOEK-IX)                      
028400                 ADD IN35-SUARTFSG-KRE TO                                 
028500                          SOEK-SUARTFSG-KRE(SOEK-IX)                      
028600              END-IF                                                      
028700           END-IF                                                         
028800        END-IF                                                            
028900     END-IF                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 CA-HITTA-RAETT-INDEX SECTION.                                            
029300     SKIP2                                                                
029400     MOVE NEJ TO FL-IX-TRAEFF                                             
029500     MOVE +1 TO SOEK-IX                                                   
029600                                                                          
029700     PERFORM UNTIL SOEK-IX > TOTAL-IX OR FL-IX-TRAEFF = JA                
029800                                                                          
029900        IF SPAR-IDDISTR = TAB-IDDISTR(SOEK-IX)                            
030000           MOVE JA TO FL-IX-TRAEFF                                        
030100        END-IF                                                            
030200                                                                          
030300        IF FL-IX-TRAEFF = NEJ                                             
030400           ADD +1 TO SOEK-IX                                              
030500        END-IF                                                            
030600                                                                          
030700     END-PERFORM                                                          
030800                                                                          
030900                                                                          
031000     IF FL-IX-TRAEFF = NEJ                                                
031100        MOVE SPAR-IDDISTR TO TAB-IDDISTR(SOEK-IX)                         
031200        MOVE SOEK-IX      TO TAB-INDEX(SOEK-IX)                           
031300        ADD +1            TO TOTAL-IX                                     
031400     END-IF                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 D-SKRIV-FIL96-FRAN-TABELL SECTION.                                       
031710**    DISPLAY  'D-SKRIV-FIL96-FRÅN SEC'                                   
031800                                                                          
031900     PERFORM DA-SORTERA-INDEX-TABELL                                      
032000     MOVE +1 TO SOEK-IX                                                   
032100     PERFORM UNTIL SOEK-IX > TOTAL-IX                                     
032200        MOVE TAB-INDEX(SOEK-IX) TO SKRIV-IX                               
032300        IF SOEK-SUARTSJK(SKRIV-IX) NOT =  0                               
032400           MOVE SPAR-IDARTNR            TO UT96-IDARTNR                   
032500           MOVE TAB-IDDISTR(SOEK-IX)    TO UT96-IDDISTR                   
032600           MOVE SOEKT-AA                TO UT96-TIAA                      
032700           MOVE SOEK-SUARTSJK(SKRIV-IX) TO UT96-SUARTSJK                  
032800           MOVE SOEK-SULEVANT(SKRIV-IX) TO UT96-SULEVANT                  
032900           MOVE SOEK-SUARTFSG(SKRIV-IX) TO UT96-SUARTFSG                  
033000           MOVE SOEK-SULEVANT-DO(SKRIV-IX) TO                             
033100                     UT96-SULEVANT-DO                                     
033200           MOVE SOEK-SUARTFSG-DO(SKRIV-IX) TO                             
033300                     UT96-SUARTFSG-DO                                     
033400           MOVE SOEK-SULEVANT-RAB(SKRIV-IX) TO                            
033500                     UT96-SULEVANT-RAB                                    
033600           MOVE SOEK-SUARTFSG-RAB(SKRIV-IX) TO                            
033700                     UT96-SUARTFSG-RAB                                    
033800           MOVE SOEK-SULEVANT-SPEC(SKRIV-IX) TO                           
033900                     UT96-SULEVANT-SPEC                                   
034000           MOVE SOEK-SUARTFSG-SPEC(SKRIV-IX) TO                           
034100                     UT96-SUARTFSG-SPEC                                   
034200           MOVE SOEK-SULEVANT-MAN(SKRIV-IX) TO                            
034300                     UT96-SULEVANT-MAN                                    
034400           MOVE SOEK-SUARTFSG-MAN(SKRIV-IX) TO                            
034500                     UT96-SUARTFSG-MAN                                    
034600           MOVE SOEK-SULEVANT-KRE(SKRIV-IX) TO                            
034700                      UT96-SULEVANT-KRE                                   
034800           MOVE SOEK-SUARTFSG-KRE(SKRIV-IX) TO                            
034900                     UT96-SUARTFSG-KRE                                    
035000           PERFORM S02-SKRIV-FIL-96                                       
035100*          PERFORM S03-NOLLSTAELL-SOEK-AREA                               
035200*          PERFORM S04-NOLLSTAELL-TABELL                                  
035300        END-IF                                                            
035400        ADD +1 TO SOEK-IX                                                 
035500     END-PERFORM                                                          
035510     INITIALIZE SOEK-AREA                                                 
035520                TABELL                                                    
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900 DA-SORTERA-INDEX-TABELL SECTION.                                         
036000     SKIP2                                                                
036100     MOVE TOTAL-IX   TO  TAB-MAX                                          
036200     MOVE TAB-MAX    TO  ANTAL                                            
036300                                                                          
036400     CALL WINTSOR  USING TABELL                                           
036500                         STEGLANGD                                        
036600                         ANTAL                                            
036700                         TAB-IDDISTR(1)                                   
036800                         NYCKELLANGD                                      
036900     .                                                                    
037000     EJECT                                                                
037100                                                                          
037200 Z-FINIT SECTION.                                                         
037300     SKIP2                                                                
037400     CLOSE W33013                                                         
037500           W33096                                                         
037600                                                                          
037700     MOVE 'S' TO POSTSUM-OPKOD                                            
037800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
037900     .                                                                    
038000                                                                          
038100 S01-LAS-13-FIL SECTION.                                                  
038200     SKIP2                                                                
038300       READ W33013 INTO IN-AREA                                           
038400          AT END MOVE JA TO INFIL-13-EOF                                  
038500       END-READ                                                           
038600                                                                          
038700     IF INFIL-13-EOF = NEJ                                                
038800        MOVE '13' TO POSTSUM-TRANSTYP                                     
038900        MOVE 'W33013' TO POSTSUM-FDNAMN                                   
039000        MOVE 'W33096D1' TO POSTSUM-DDNAMN2                                
039100        CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                     
039200     END-IF                                                               
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 S02-SKRIV-FIL-96 SECTION.                                                
039700     SKIP2                                                                
039800     WRITE UT-POST FROM UT-AREA                                           
039900     MOVE '96'  TO POSTSUM-TRANSTYP                                       
040000     MOVE 'W33096' TO POSTSUM-FDNAMN                                      
040100     MOVE 'W33096D2' TO POSTSUM-DDNAMN2                                   
040200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
040300     .                                                                    
040400     EJECT                                                                
040500*                                                                         
040600*S03-NOLLSTAELL-SOEK-AREA SECTION.                                        
040700*    SKIP2                                                                
040800*    MOVE ZERO TO SOEK-SUARTSJK(SKRIV-IX)                                 
040900*                 SOEK-SULEVANT(SKRIV-IX)                                 
041000*                 SOEK-SUARTFSG(SKRIV-IX)                                 
041100*                 SOEK-SULEVANT-DO(SKRIV-IX)                              
041200*                 SOEK-SUARTFSG-DO(SKRIV-IX)                              
041300*                 SOEK-SULEVANT-RAB(SKRIV-IX)                             
041400*                 SOEK-SUARTFSG-RAB(SKRIV-IX)                             
041500*                 SOEK-SULEVANT-SPEC(SKRIV-IX)                            
041600*                 SOEK-SUARTFSG-SPEC(SKRIV-IX)                            
041700*                 SOEK-SULEVANT-MAN(SKRIV-IX)                             
041800*                 SOEK-SUARTFSG-MAN(SKRIV-IX)                             
041900*                 SOEK-SULEVANT-KRE(SKRIV-IX)                             
042000*                 SOEK-SUARTFSG-KRE(SKRIV-IX)                             
042100*    .                                                                    
042200*    EJECT                                                                
042300*S04-NOLLSTAELL-TABELL SECTION.                                           
042400*    SKIP2                                                                
042500*    MOVE ZERO TO TAB-IDDISTR(SOEK-IX)                                    
042600*                 TAB-INDEX(SOEK-IX)                                      
042700*    .                                                                    
042800*    EJECT                                                                
