000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2363600.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LISTA LEVERANSPRECISION                                          
001000*        FÖR INLEVERANSER GÅGNA VECKAN                                    
001100*                                                                         
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
002500*          --- LISTUNDERLAG                                               
002600     SELECT W23636                     ASSIGN TO W23636D1.                
002700     SKIP2                                                                
002800*          --- LISTA LEVERANSPRECISION                                    
002900     SELECT W23637                     ASSIGN TO W23636D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W23636                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W23636      -L.                                                
004000     SKIP3                                                                
004100 FD  W23637                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500 01  W23636-001-RAD              PIC X(125).                              
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W2363600'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W23636-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W23636                       VALUE 'J'.                   
005500                                                                          
005600 01  ARBETSAREOR.                                                         
005700     03  IX-DEL                  PIC S9(5)   VALUE ZERO  COMP-3.          
005800     03  OLD-IDLEVNR             PIC X(5)    VALUE SPACE.                 
005900     03  OLD-IDLOPNRM            PIC 9(9)    VALUE ZERO  COMP-3.          
006000     03  WS-KVDAGAR              PIC S9(3)   VALUE ZERO  COMP-3.          
006100     03  WS-DEL                  PIC 99V99   VALUE ZERO.                  
006200     03  WS-ANTAL                PIC 9(5)    VALUE ZERO.                  
006300     03  WS-TOTSUM               PIC 9(8)V99 VALUE ZERO.                  
006400     03  FILLER OCCURS 13.                                                
006500         05  WS-SUM              PIC 9(8)V99 VALUE ZERO.                  
006600     03  FILLER OCCURS 13.                                                
006700         05  WS-PROC             PIC 9(8)V99 VALUE ZERO.                  
006800     03  WS-RATT                 PIC 999V99  VALUE ZERO.                  
006900     03  WS-TIDIGA               PIC 999V99  VALUE ZERO.                  
007000     03  WS-SENA                 PIC 999V99  VALUE ZERO.                  
007100     03  WS-SNITT                PIC 9(8)V99 VALUE ZERO.                  
007200     03  WS-VIKT                 PIC 9(8)V99 VALUE ZERO.                  
007300     03  WS-SUMVIKT              PIC 9(8)V99 VALUE ZERO.                  
007310     03  TOT-ANTAL               PIC 9(5)    VALUE ZERO.                  
007320     03  TOT-TOTSUM              PIC 9(8)V99 VALUE ZERO.                  
007330     03  FILLER OCCURS 13.                                                
007340         05  TOT-SUM             PIC 9(8)V99 VALUE ZERO.                  
007350     03  FILLER OCCURS 13.                                                
007360         05  TOT-PROC            PIC 9(8)V99 VALUE ZERO.                  
007370     03  TOT-RATT                PIC 999V99  VALUE ZERO.                  
007380     03  TOT-TIDIGA              PIC 999V99  VALUE ZERO.                  
007390     03  TOT-SENA                PIC 999V99  VALUE ZERO.                  
007391     03  TOT-SNITT               PIC 9(8)V99 VALUE ZERO.                  
007393     03  TOT-SUMVIKT             PIC 9(8)V99 VALUE ZERO.                  
007400     EJECT                                                                
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000     EJECT                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     SKIP2                                                                
008700*    --- PARAMETRAR TILL ABEND                                            
008800                                                                          
008900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009200     SKIP2                                                                
009300 01  FELTEXT.                                                             
009400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL DATKORT                                          
009800*                                                                         
009900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23636'.              
010000     SKIP2                                                                
010100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010200     SKIP2                                                                
010300*01  -COPY WDATKORT                                                       
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900 01  IN-AREA-START               PIC X(24)   VALUE                        
011000                                 'IN-AREA-START  '.                       
011100     SKIP2                                                                
011200                                                                          
011300*01  AREA -COPY W23636     -PRE IN-                                       
011400     EJECT                                                                
011500 01  W001-AREA-START             PIC X(24)   VALUE                        
011600                                 'W001-AREA-START  '.                     
011700     SKIP2                                                                
011800 01  W001-HJALPAREOR.                                                     
011900*                                                                         
012000     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
012100     03  W001-MAX-RADER-PER-SIDA                                          
012200                                 PIC 9(3)    VALUE 42.                    
012300     03  W001-LISTNR             PIC X(11)   VALUE 'W23636-001'.          
012400     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
012500     03  W001-RADRAKNARE         PIC S9(3)   COMP-3 VALUE 999.            
012600     EJECT                                                                
012700 01  W001-RAD.                                                            
012800*                                                                         
012900     03  FILLER                  PIC X(121)  VALUE SPACE.                 
013000     EJECT                                                                
013100 01  W001-RUBRIK1.                                                        
013200*                                                                         
013300     03  FILLER                  PIC X(3)  VALUE SPACE.                   
013400     03  FILLER                  PIC X(21)                                
013500                                VALUE 'VOLVO CAR CUST SERV  '.            
013600     03  FILLER                  PIC X(12)                                
013700                                 VALUE 'W23636-001'.                      
013800     03  FILLER                  PIC X(54)                                
013900         VALUE 'DELIVERY PRECISION IN % OF PRE ADVICES'.                  
014000     03  W001-DATUM              PIC XXBXXBXX.                            
014100     03  FILLER                  PIC X(3)   VALUE SPACE.                  
014200     03  FILLER                  PIC X(5)                                 
014300                                 VALUE 'PAGE '.                           
014400     03  W001-SID                PIC Z(4)9.                               
014500     SKIP2                                                                
014600 01  W001-RUBRIK2.                                                        
014700     03  FILLER                  PIC X(40)                                
014800                                 VALUE ' SUPPL      '.                    
014900     03  FILLER                  PIC X(78)                                
015000                           VALUE ' DEVIATION DAYS    '.                   
015100     SKIP2                                                                
015200 01  W001-RUBRIK3.                                                        
015300     03  FILLER                  PIC X(1)  VALUE SPACE.                   
015400     03  W001-IDLEVNR            PIC X(5)  VALUE SPACE.                   
015500     03  FILLER                  PIC X(17)                                
015600                                 VALUE '        ARTNR'.                   
015700     03  FILLER                  PIC X(41)                                
015800     VALUE ' <5    -5    -4    -3    -2    -1     0 '.                    
015900     03  FILLER                  PIC X(70)                                
016000     VALUE '  +1    +2    +3    +4    +5    >5 '.                         
016100     SKIP2                                                                
016200 01  W001-RUBRIKS.                                                        
016300     03  FILLER                  PIC X(1)  VALUE SPACE.                   
016400     03  FILLER                  PIC X(40)                                
016500     VALUE 'LEVNR              SNITTAVVIKELSE (DGR)'.                     
016600     03  FILLER                  PIC X(50)                                
016700     VALUE ' RÄTT DAG %      SENA %    TIDIGA %'.                         
016800     SKIP2                                                                
016900 01  W001-RUBRIKA.                                                        
017000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
017100     03  FILLER                  PIC X(38)                                
017200     VALUE 'SUPPL  NO AVER. CORR.  LATE   EARLY  '.                       
017300     03  FILLER                  PIC X(41)                                
017400     VALUE ' <5    -5    -4    -3    -2    -1     0 '.                    
017500     03  FILLER                  PIC X(35)                                
017600     VALUE '  +1    +2    +3    +4    +5    >5 '.                         
017700     SKIP2                                                                
017800 01  W001-RUBRIKB.                                                        
017900     03  FILLER                  PIC X(1)  VALUE SPACE.                   
018000     03  FILLER                  PIC X(37)                                
018100     VALUE '         (DAYS)    %     %      %    '.                       
018200     EJECT                                                                
018300 01  W001-SNITT1.                                                         
018400     03  FILLER                  PIC X(1)   VALUE SPACE.                  
018500     03  W001-IDLEVNR-SNITT-X    PIC X(5)   VALUE SPACE.                  
018600     03  FILLER                  PIC X(23)  VALUE SPACE.                  
018700     03  W001-SNITTAVV-X         PIC ZZ9.9  VALUE ZERO.                   
018800     03  FILLER                  PIC X(11)  VALUE SPACE.                  
018900     03  W001-RATT-X             PIC ZZ9.9  VALUE ZERO.                   
019000     03  FILLER                  PIC X(07)  VALUE SPACE.                  
019100     03  W001-SENA-X             PIC ZZ9.9  VALUE ZERO.                   
019200     03  FILLER                  PIC X(07)  VALUE SPACE.                  
019300     03  W001-TIDIGA-X           PIC ZZ9.9  VALUE ZERO.                   
019400     03  FILLER                  PIC X(07)  VALUE SPACE.                  
019500     SKIP3                                                                
019600 01  W001-SNITTA.                                                         
019700     03  FILLER                  PIC X(1)   VALUE SPACE.                  
019800     03  W001-IDLEVNR-SNITT      PIC X(5)   VALUE SPACE.                  
019801     03  FILLER                  PIC X(1)   VALUE SPACE.                  
019810     03  W001-ANTAL              PIC Z(3)   VALUE ZERO.                   
019900     03  FILLER                  PIC X(1)   VALUE SPACE.                  
020000     03  W001-SNITTAVV           PIC ZZ9.9  VALUE ZERO.                   
020100     03  FILLER                  PIC X(01)  VALUE SPACE.                  
020200     03  W001-RATT               PIC ZZ9.9  VALUE ZERO.                   
020300     03  FILLER                  PIC X(01)  VALUE SPACE.                  
020400     03  W001-SENA               PIC ZZ9.9  VALUE ZERO.                   
020500     03  FILLER                  PIC X(03)  VALUE SPACE.                  
020600     03  W001-TIDIGA             PIC ZZ9.9  VALUE ZERO.                   
020700     03  FILLER                  PIC X(01)  VALUE SPACE.                  
020800     03  FILLER OCCURS 13.                                                
020900         05  W001-PROC           PIC ZZ9.9 BLANK WHEN ZERO.               
021000         05  FILLER              PIC X(01) VALUE SPACE.                   
021010     SKIP3                                                                
021020 01  W001-SNITT-TOT.                                                      
021030     03  FILLER                  PIC X(1)   VALUE SPACE.                  
021040     03  W001-IDLEVNR-TOT        PIC X(5)   VALUE SPACE.                  
021060     03  W001-ANTAL-TOT          PIC Z(4)   VALUE ZERO.                   
021070     03  FILLER                  PIC X(1)   VALUE SPACE.                  
021080     03  W001-SNITTAVV-TOT       PIC ZZ9.9  VALUE ZERO.                   
021090     03  FILLER                  PIC X(01)  VALUE SPACE.                  
021091     03  W001-RATT-TOT           PIC ZZ9.9  VALUE ZERO.                   
021092     03  FILLER                  PIC X(01)  VALUE SPACE.                  
021093     03  W001-SENA-TOT           PIC ZZ9.9  VALUE ZERO.                   
021094     03  FILLER                  PIC X(03)  VALUE SPACE.                  
021095     03  W001-TIDIGA-TOT         PIC ZZ9.9  VALUE ZERO.                   
021096     03  FILLER                  PIC X(01)  VALUE SPACE.                  
021097     03  FILLER OCCURS 13.                                                
021098         05  W001-PROC-TOT       PIC ZZ9.9 BLANK WHEN ZERO.               
021099         05  FILLER              PIC X(01) VALUE SPACE.                   
021100     EJECT                                                                
021200 PROCEDURE DIVISION.                                                      
021300 MAIN SECTION.                                                            
021400     SKIP2                                                                
021500                                                                          
021600     PERFORM A-INIT                                                       
021700     PERFORM S01-LAES-W23636                                              
021800     MOVE IN-IDLEVNR   TO OLD-IDLEVNR                                     
021900                          W001-IDLEVNR                                    
022000     MOVE ZERO         TO OLD-IDLOPNRM                                    
022100     PERFORM UNTIL END-OF-W23636                                          
022200       IF IN-IDLEVNR NOT = OLD-IDLEVNR                                    
022300          PERFORM C-SUMMERA-OLD-LEV                                       
022400       END-IF                                                             
022500                                                                          
022600       PERFORM B-BERAKNA-RAD-ARTNR                                        
022700                                                                          
022800       PERFORM S01-LAES-W23636                                            
022900     END-PERFORM                                                          
023000     PERFORM C-SUMMERA-OLD-LEV                                            
023100                                                                          
023110     PERFORM D-SUMMERA-TOT-LEV                                            
023200                                                                          
023300     PERFORM Z-FINIT                                                      
023400                                                                          
023500     MOVE ZERO TO RETURN-CODE                                             
023600     GOBACK                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 A-INIT SECTION.                                                          
024000                                                                          
024100     OPEN INPUT  W23636                                                   
024200                                                                          
024300     OPEN OUTPUT W23637                                                   
024400     SKIP2                                                                
024500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
024600     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
024700     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
024800     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
024900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025000                                                                          
025100     MOVE +1                TO IX-DEL                                     
025200     PERFORM UNTIL IX-DEL > 13                                            
025300        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
025400        MOVE ZERO           TO WS-PROC (IX-DEL)                           
025500        ADD +1              TO IX-DEL                                     
025600     END-PERFORM                                                          
025700                                                                          
025800     MOVE DAGENS-DATUM      TO W001-DATUM                                 
025900     .                                                                    
026000     EJECT                                                                
026100 B-BERAKNA-RAD-ARTNR SECTION.                                             
026200                                                                          
026300     COMPUTE IX-DEL = IN-KVDAGAR + 7                                      
026400     IF IX-DEL < 1                                                        
026500        MOVE 1            TO IX-DEL                                       
026600     END-IF                                                               
026700     IF IX-DEL > 13                                                       
026800        MOVE 13           TO IX-DEL                                       
026900     END-IF                                                               
027000                                                                          
027100     IF IN-KVAVIS > ZERO                                                  
027200        COMPUTE WS-DEL  ROUNDED =                                         
027300                        IN-KVAVROP-AVB / IN-KVAVIS                        
027400     ELSE                                                                 
027500        MOVE ZERO         TO WS-DEL                                       
027600     END-IF                                                               
027700     ADD  WS-DEL          TO WS-SUM   (IX-DEL)                            
027710     ADD  WS-DEL          TO TOT-SUM  (IX-DEL)                            
027800                                                                          
027900     IF IN-IDLOPNRM NOT = OLD-IDLOPNRM                                    
028000        ADD  +1           TO WS-ANTAL                                     
028010        ADD  +1           TO TOT-ANTAL                                    
028100     END-IF                                                               
028200     MOVE IN-IDLOPNRM     TO OLD-IDLOPNRM                                 
028300                                                                          
028400     PERFORM BA-ADDERA-SNITTBER                                           
028500     .                                                                    
028600     EJECT                                                                
028700 BA-ADDERA-SNITTBER  SECTION.                                             
028800                                                                          
028900     MOVE IN-KVDAGAR TO WS-KVDAGAR                                        
029000     IF WS-KVDAGAR < ZERO                                                 
029100        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
029200     END-IF                                                               
029300     COMPUTE WS-VIKT = WS-KVDAGAR * WS-DEL                                
029400     ADD WS-VIKT    TO WS-SUMVIKT                                         
029410     ADD WS-VIKT    TO TOT-SUMVIKT                                        
029500     .                                                                    
029600     EJECT                                                                
029700 C-SUMMERA-OLD-LEV   SECTION.                                             
029800                                                                          
029900     MOVE ZERO              TO WS-TOTSUM                                  
030000                                                                          
030100     MOVE +1                TO IX-DEL                                     
030200     PERFORM UNTIL IX-DEL > 13                                            
030300        ADD WS-SUM (IX-DEL) TO WS-TOTSUM                                  
030400        ADD +1              TO IX-DEL                                     
030500     END-PERFORM                                                          
030600                                                                          
030700     MOVE +1                TO IX-DEL                                     
030800     PERFORM UNTIL IX-DEL > 13                                            
030900        IF WS-TOTSUM > ZERO                                               
031000           COMPUTE WS-PROC (IX-DEL) ROUNDED =                             
031100                   100 * WS-SUM (IX-DEL) / WS-TOTSUM                      
031200        ELSE                                                              
031300           MOVE ZERO        TO WS-PROC (IX-DEL)                           
031400        END-IF                                                            
031500        MOVE WS-PROC (IX-DEL) TO W001-PROC (IX-DEL)                       
031600        ADD +1              TO IX-DEL                                     
031700     END-PERFORM                                                          
031800                                                                          
031900     PERFORM CA-SKRIV-SNITTAVVIKELSE                                      
032000                                                                          
032100     MOVE +1                TO IX-DEL                                     
032200     PERFORM UNTIL IX-DEL > 13                                            
032300        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
032400        MOVE ZERO           TO WS-PROC (IX-DEL)                           
032500        ADD +1              TO IX-DEL                                     
032600     END-PERFORM                                                          
032700     MOVE IN-IDLEVNR        TO OLD-IDLEVNR                                
032800                               W001-IDLEVNR                               
032900     MOVE ZERO              TO WS-SUMVIKT                                 
033000     MOVE ZERO              TO WS-ANTAL                                   
033100     MOVE ZERO              TO OLD-IDLOPNRM                               
033200     .                                                                    
033300     EJECT                                                                
033400 CA-SKRIV-SNITTAVVIKELSE SECTION.                                         
033500                                                                          
033600     MOVE OLD-IDLEVNR     TO W001-IDLEVNR-SNITT                           
033700                                                                          
033800     IF WS-ANTAL > ZERO                                                   
033900        COMPUTE WS-SNITT ROUNDED =                                        
034000                                WS-SUMVIKT / WS-ANTAL                     
034100     ELSE                                                                 
034200        MOVE ZERO            TO WS-SNITT                                  
034300     END-IF                                                               
034400     MOVE WS-SNITT           TO W001-SNITTAVV                             
034410     MOVE WS-ANTAL           TO W001-ANTAL                                
034500     MOVE WS-PROC (7)        TO W001-RATT                                 
034600     MOVE ZERO               TO WS-SENA WS-TIDIGA                         
034700                                                                          
034800     MOVE +8                 TO IX-DEL                                    
034900     PERFORM UNTIL IX-DEL > 13                                            
035000        ADD WS-PROC (IX-DEL) TO WS-SENA                                   
035100        ADD +1               TO IX-DEL                                    
035200     END-PERFORM                                                          
035300     MOVE WS-SENA            TO W001-SENA                                 
035400                                                                          
035500     MOVE +1                 TO IX-DEL                                    
035600     PERFORM UNTIL IX-DEL > 6                                             
035700        ADD WS-PROC (IX-DEL) TO WS-TIDIGA                                 
035800        ADD +1               TO IX-DEL                                    
035900     END-PERFORM                                                          
036000     MOVE WS-TIDIGA          TO W001-TIDIGA                               
036100     MOVE W001-SNITTA        TO W001-RAD                                  
036200     PERFORM S21-SKRIV-W23636-001                                         
036210     .                                                                    
036220     EJECT                                                                
036230 D-SUMMERA-TOT-LEV   SECTION.                                             
036240                                                                          
036250     MOVE ZERO              TO TOT-TOTSUM                                 
036260                                                                          
036270     MOVE +1                TO IX-DEL                                     
036280     PERFORM UNTIL IX-DEL > 13                                            
036290        ADD TOT-SUM (IX-DEL) TO TOT-TOTSUM                                
036291        ADD +1              TO IX-DEL                                     
036292     END-PERFORM                                                          
036293                                                                          
036294     MOVE +1                TO IX-DEL                                     
036295     PERFORM UNTIL IX-DEL > 13                                            
036296        IF TOT-TOTSUM > ZERO                                              
036297           COMPUTE TOT-PROC (IX-DEL) ROUNDED =                            
036298                   100 * TOT-SUM (IX-DEL) / TOT-TOTSUM                    
036299        ELSE                                                              
036300           MOVE ZERO        TO TOT-PROC (IX-DEL)                          
036301        END-IF                                                            
036302        MOVE TOT-PROC (IX-DEL) TO W001-PROC-TOT (IX-DEL)                  
036303        ADD +1              TO IX-DEL                                     
036304     END-PERFORM                                                          
036305                                                                          
036306     PERFORM DA-SKRIV-SNITTAVVIKELSE                                      
036307                                                                          
036319     .                                                                    
036320     EJECT                                                                
036321 DA-SKRIV-SNITTAVVIKELSE SECTION.                                         
036322                                                                          
036323     MOVE 'TOTAL'         TO W001-IDLEVNR-TOT                             
036324                                                                          
036325     IF TOT-ANTAL > ZERO                                                  
036326        COMPUTE TOT-SNITT ROUNDED =                                       
036327                               TOT-SUMVIKT / TOT-ANTAL                    
036328     ELSE                                                                 
036329        MOVE ZERO            TO TOT-SNITT                                 
036330     END-IF                                                               
036331     MOVE TOT-SNITT          TO W001-SNITTAVV-TOT                         
036332     MOVE TOT-ANTAL          TO W001-ANTAL-TOT                            
036333     MOVE TOT-PROC (7)       TO W001-RATT-TOT                             
036334     MOVE ZERO               TO TOT-SENA TOT-TIDIGA                       
036335                                                                          
036336     MOVE +8                 TO IX-DEL                                    
036337     PERFORM UNTIL IX-DEL > 13                                            
036338        ADD TOT-PROC(IX-DEL) TO TOT-SENA                                  
036339        ADD +1               TO IX-DEL                                    
036340     END-PERFORM                                                          
036341     MOVE TOT-SENA           TO W001-SENA-TOT                             
036342                                                                          
036343     MOVE +1                 TO IX-DEL                                    
036344     PERFORM UNTIL IX-DEL > 6                                             
036345        ADD TOT-PROC(IX-DEL) TO TOT-TIDIGA                                
036346        ADD +1               TO IX-DEL                                    
036347     END-PERFORM                                                          
036348     MOVE TOT-TIDIGA         TO W001-TIDIGA-TOT                           
036349     MOVE W001-SNITT-TOT     TO W001-RAD                                  
036350     MOVE 50                 TO W001-RADRAKNARE                           
036351     PERFORM S21-SKRIV-W23636-001                                         
036360                                                                          
036400     .                                                                    
036500     EJECT                                                                
036600 Z-FINIT SECTION.                                                         
036700                                                                          
036800     CLOSE W23636                                                         
036900           W23637                                                         
037000     SKIP2                                                                
037100     MOVE 'S' TO POSTSUM-OPKOD                                            
037200     CALL POSTSUM USING POSTSUM-PARM                                      
037300     .                                                                    
037400     EJECT                                                                
037500 S01-LAES-W23636  SECTION.                                                
037600     READ W23636 INTO IN-AREA                                             
037700     AT END                                                               
037800        SET END-OF-W23636 TO TRUE                                         
037900                                                                          
038000     NOT AT END                                                           
038100        MOVE 'W23636'   TO POSTSUM-FDNAMN                                 
038200        MOVE 'W23636D1' TO POSTSUM-DDNAMN2                                
038300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
038400        CALL POSTSUM USING POSTSUM-PARM                                   
038500     END-READ                                                             
038600     .                                                                    
038700     EJECT                                                                
038800 S21-SKRIV-W23636-001  SECTION.                                           
038900                                                                          
039000     MOVE 2 TO W001-SKIP                                                  
039100     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
039200       PERFORM S21A-SKRIV-RUBRIKER                                        
039300     END-IF                                                               
039400     SKIP2                                                                
039500     WRITE W23636-001-RAD FROM W001-RAD AFTER W001-SKIP                   
039600     SKIP2                                                                
039700     MOVE SPACE TO W001-RAD                                               
039800     ADD  +2 TO W001-RADRAKNARE                                           
039900     .                                                                    
040000     EJECT                                                                
040100 S21S-SKRIV-W23636-001  SECTION.                                          
040200                                                                          
040300     MOVE 2 TO W001-SKIP                                                  
040400     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
040500       PERFORM S21A-SKRIV-RUBRIKER                                        
040600     END-IF                                                               
040700     SKIP2                                                                
040800     WRITE W23636-001-RAD FROM W001-RAD AFTER W001-SKIP                   
040900     SKIP2                                                                
041000     MOVE SPACE TO W001-RAD                                               
041100     ADD  +2 TO W001-RADRAKNARE                                           
041200     .                                                                    
041300     EJECT                                                                
041400 S21A-SKRIV-RUBRIKER SECTION.                                             
041500                                                                          
041600     ADD +1 TO W001-SIDRAKNARE                                            
041700     MOVE W001-SIDRAKNARE TO W001-SID                                     
041800     WRITE W23636-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
041900     WRITE W23636-001-RAD FROM W001-RUBRIKA AFTER 2                       
042000     WRITE W23636-001-RAD FROM W001-RUBRIKB AFTER 1                       
042100     MOVE +7 TO W001-RADRAKNARE                                           
042200     SKIP2                                                                
042300     MOVE 2 TO W001-SKIP                                                  
042400     .                                                                    
042500     EJECT                                                                
042600 S99-ABEND SECTION.                                                       
042700                                                                          
042800     SKIP2                                                                
042900     MOVE 'S' TO POSTSUM-OPKOD                                            
043000     CALL POSTSUM USING POSTSUM-PARM                                      
043100     CALL ABEND USING RKOD-ABEND                                          
043200     .                                                                    
