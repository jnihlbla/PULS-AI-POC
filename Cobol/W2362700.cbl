000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2362700.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LISTA LEVERANSPRECISION                                          
001000*        FÖR INLEVERANSER GÅGNA PERIODEN                                  
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
002600     SELECT W23626                     ASSIGN TO W23627D1.                
002700     SKIP2                                                                
002800*          --- LISTA LEVERANSPRECISION/STYCK                              
002900     SELECT W23627-001                 ASSIGN TO W23627D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W23626                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W23636      -L.                                                
004000     SKIP3                                                                
004100 FD  W23627-001                                                           
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500 01  W23627-001-RAD              PIC X(121).                              
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W2362700'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W23626-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W23626                       VALUE 'J'.                   
005500                                                                          
005600 01  ARBETSAREOR.                                                         
005700     03  IX-DEL                  PIC S9(5)    VALUE ZERO  COMP-3.         
005800     03  OLD-IDLEVNR             PIC X(5)     VALUE SPACE.                
005900     03  OLD-IDARTNR             PIC 9(9)     VALUE ZERO  COMP-3.         
005910     03  OLD-IDLOPNRM            PIC 9(9)     VALUE ZERO  COMP-3.         
006000     03  WS-KVDAGAR              PIC S9(3)    VALUE ZERO  COMP-3.         
006100     03  WS-STYCK                PIC S9999    VALUE ZERO.                 
006200     03  WS-ANTAL                PIC 9(5)     VALUE ZERO.                 
006210     03  WS-ANTAL-X              PIC 9(5)     VALUE ZERO.                 
006300     03  WS-TOTSUM               PIC 9(8)V99  VALUE ZERO.                 
006400     03  FILLER OCCURS 13.                                                
006500         05  WS-SUM              PIC 9(8)V99  VALUE ZERO.                 
006600     03  FILLER OCCURS 13.                                                
006700         05  WS-PROC             PIC 9(8)V99  VALUE ZERO.                 
006800     03  WS-RATT                 PIC 999V99   VALUE ZERO.                 
006900     03  WS-TIDIGA               PIC 999V99   VALUE ZERO.                 
007000     03  WS-SENA                 PIC 999V99   VALUE ZERO.                 
007100     03  WS-SNITT                PIC 9(8)V99  VALUE ZERO.                 
007200     03  WS-VIKT                 PIC 9(8)V99  VALUE ZERO.                 
007300     03  WS-SUMVIKT              PIC 9(8)V99  VALUE ZERO.                 
007310     03  TOT-ANTAL               PIC 9(9)     VALUE ZERO.                 
007330     03  TOT-TOTSUM              PIC 9(8)V99  VALUE ZERO.                 
007340     03  FILLER OCCURS 13.                                                
007350         05  TOT-SUM             PIC 9(8)V99  VALUE ZERO.                 
007360     03  FILLER OCCURS 13.                                                
007370         05  TOT-PROC            PIC 9(8)V99  VALUE ZERO.                 
007380     03  TOT-RATT                PIC 999V99   VALUE ZERO.                 
007390     03  TOT-TIDIGA              PIC 999V99   VALUE ZERO.                 
007391     03  TOT-SENA                PIC 999V99   VALUE ZERO.                 
007392     03  TOT-SNITT               PIC 9(8)V99  VALUE ZERO.                 
007394     03  TOT-SUMVIKT             PIC 9(8)V99  VALUE ZERO.                 
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
009900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23627'.              
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
012300     03  W001-LISTNR             PIC X(11)   VALUE 'W23627-001'.          
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
013700                                 VALUE 'W23627-001'.                      
013800     03  FILLER                  PIC X(54)                                
013900         VALUE 'PERIOD: DELIVERY PRECISION IN % OF QUANTITY'.             
014000     03  W001-DATUM              PIC XXBXXBXX.                            
014100     03  FILLER                  PIC X(3)   VALUE SPACE.                  
014200     03  FILLER                  PIC X(5)                                 
014300                                 VALUE 'PAGE '.                           
014400     03  W001-SID                PIC Z(4)9.                               
014500     SKIP2                                                                
016900 01  W001-RUBRIKA.                                                        
017000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
017100     03  FILLER                  PIC X(42)                                
017200     VALUE 'SUPPL     TOT AVER. CORR.  LATE   EARLY  '.                   
017300     03  FILLER                  PIC X(41)                                
017400     VALUE ' <5    -5    -4    -3    -2    -1     0 '.                    
017500     03  FILLER                  PIC X(35)                                
017600     VALUE '  +1    +2    +3    +4    +5    >5 '.                         
017700     SKIP2                                                                
017800 01  W001-RUBRIKB.                                                        
017900     03  FILLER                  PIC X(1)  VALUE SPACE.                   
018000     03  FILLER                  PIC X(41)                                
018100     VALUE '             (DAYS)    %     %      %    '.                   
018200     EJECT                                                                
018300 01  W001-DETALJ1.                                                        
018400     03  FILLER                  PIC X(10) VALUE SPACE.                   
018500     03  W001-IDARTNR            PIC Z(9).                                
018600     03  FILLER                  PIC X(03) VALUE SPACE.                   
018700     03  FILLER OCCURS 13.                                                
018800         05  W001-STYCK          PIC ZZZZ BLANK WHEN ZERO.                
018900         05  FILLER              PIC X(02) VALUE SPACE.                   
019000     SKIP3                                                                
019100 01  W001-SUMMA1.                                                         
019200     03  FILLER                  PIC X(10) VALUE ' SUMMA'.                
019300     03  W001-ANTAL-X            PIC Z(9).                                
019400     03  FILLER                  PIC X(02) VALUE SPACE.                   
019500     03  FILLER OCCURS 13.                                                
019600         05  W001-PROC-X         PIC ZZ9.9 BLANK WHEN ZERO.               
019700         05  FILLER              PIC X(01) VALUE SPACE.                   
019800     SKIP3                                                                
019900 01  W001-SNITT1.                                                         
020000     03  FILLER                  PIC X(1)   VALUE SPACE.                  
020100     03  W001-IDLEVNR-SNITT-X    PIC X(5)   VALUE SPACE.                  
020200     03  FILLER                  PIC X(23)  VALUE SPACE.                  
020300     03  W001-SNITTAVV-X         PIC ZZ9.9  VALUE ZERO.                   
020400     03  FILLER                  PIC X(11)  VALUE SPACE.                  
020500     03  W001-RATT-X             PIC ZZ9.9  VALUE ZERO.                   
020600     03  FILLER                  PIC X(07)  VALUE SPACE.                  
020700     03  W001-SENA-X             PIC ZZ9.9  VALUE ZERO.                   
020800     03  FILLER                  PIC X(07)  VALUE SPACE.                  
020900     03  W001-TIDIGA-X           PIC ZZ9.9  VALUE ZERO.                   
021000     03  FILLER                  PIC X(07)  VALUE SPACE.                  
021100     SKIP3                                                                
021200 01  W001-SNITTA.                                                         
021300     03  FILLER                  PIC X(1)   VALUE SPACE.                  
021400     03  W001-IDLEVNR-SNITT      PIC X(5)   VALUE SPACE.                  
021410     03  W001-TOTSUM             PIC Z(8)   VALUE ZERO.                   
021500     03  FILLER                  PIC X(1)   VALUE SPACE.                  
021600     03  W001-SNITTAVV           PIC ZZ9.9  VALUE ZERO.                   
021700     03  FILLER                  PIC X(01)  VALUE SPACE.                  
021800     03  W001-RATT               PIC ZZ9.9  VALUE ZERO.                   
021900     03  FILLER                  PIC X(01)  VALUE SPACE.                  
022000     03  W001-SENA               PIC ZZ9.9  VALUE ZERO.                   
022100     03  FILLER                  PIC X(03)  VALUE SPACE.                  
022200     03  W001-TIDIGA             PIC ZZ9.9  VALUE ZERO.                   
022300     03  FILLER                  PIC X(01)  VALUE SPACE.                  
022400     03  FILLER OCCURS 13.                                                
022500         05  W001-PROC           PIC ZZ9.9 BLANK WHEN ZERO.               
022600         05  FILLER              PIC X(01) VALUE SPACE.                   
022610     EJECT                                                                
022620 01  W001-SNITT-TOT.                                                      
022630     03  FILLER                  PIC X(1)   VALUE SPACE.                  
022640     03  W001-IDLEVNR-TOT        PIC X(3)   VALUE SPACE.                  
022650     03  FILLER                  PIC X(1)   VALUE SPACE.                  
022660     03  W001-TOTSUM-TOT         PIC Z(9)   VALUE ZERO.                   
022670     03  FILLER                  PIC X(1)   VALUE SPACE.                  
022680     03  W001-SNITTAVV-TOT       PIC ZZ9.9  VALUE ZERO.                   
022690     03  FILLER                  PIC X(01)  VALUE SPACE.                  
022691     03  W001-RATT-TOT           PIC ZZ9.9  VALUE ZERO.                   
022692     03  FILLER                  PIC X(01)  VALUE SPACE.                  
022693     03  W001-SENA-TOT           PIC ZZ9.9  VALUE ZERO.                   
022694     03  FILLER                  PIC X(03)  VALUE SPACE.                  
022695     03  W001-TIDIGA-TOT         PIC ZZ9.9  VALUE ZERO.                   
022696     03  FILLER                  PIC X(01)  VALUE SPACE.                  
022697     03  FILLER OCCURS 13.                                                
022698         05  W001-PROC-TOT       PIC ZZ9.9 BLANK WHEN ZERO.               
022699         05  FILLER              PIC X(01) VALUE SPACE.                   
022700     EJECT                                                                
022800 PROCEDURE DIVISION.                                                      
022900 MAIN SECTION.                                                            
023000     SKIP2                                                                
023100                                                                          
023200     PERFORM A-INIT                                                       
023300     PERFORM S01-LAES-W23626                                              
023400     MOVE IN-IDLEVNR   TO OLD-IDLEVNR                                     
023500                                                                          
023600     MOVE ZERO         TO OLD-IDARTNR                                     
023610     MOVE ZERO         TO OLD-IDLOPNRM                                    
023700     PERFORM UNTIL END-OF-W23626                                          
023800       IF IN-IDLEVNR NOT = OLD-IDLEVNR                                    
023900          PERFORM C-SUMMERA-OLD-LEV                                       
024000       END-IF                                                             
024100                                                                          
024200       PERFORM B-BERAKNA-RAD-ARTNR                                        
024400                                                                          
024500       PERFORM S01-LAES-W23626                                            
024600     END-PERFORM                                                          
024700     PERFORM C-SUMMERA-OLD-LEV                                            
024800                                                                          
024810     PERFORM D-SUMMERA-TOT-LEV                                            
024900                                                                          
025000     PERFORM Z-FINIT                                                      
025100                                                                          
025200     MOVE ZERO TO RETURN-CODE                                             
025300     GOBACK                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 A-INIT SECTION.                                                          
025700                                                                          
025800     OPEN INPUT  W23626                                                   
025900                                                                          
026000     OPEN OUTPUT W23627-001                                               
026100     SKIP2                                                                
026200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
026300     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
026400     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
026500     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
026600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026700                                                                          
026800     MOVE +1                TO IX-DEL                                     
026900     PERFORM UNTIL IX-DEL > 13                                            
027000        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
027100        MOVE ZERO           TO WS-PROC (IX-DEL)                           
027200        ADD +1              TO IX-DEL                                     
027300     END-PERFORM                                                          
027400                                                                          
027500     MOVE DAGENS-DATUM      TO W001-DATUM                                 
027600     .                                                                    
027700     EJECT                                                                
027800 B-BERAKNA-RAD-ARTNR SECTION.                                             
027900                                                                          
028000     MOVE SPACE           TO W001-DETALJ1                                 
028100     MOVE IN-IDARTNR      TO W001-IDARTNR                                 
028200     COMPUTE IX-DEL = IN-KVDAGAR + 7                                      
028300     IF IX-DEL < 1                                                        
028400        MOVE 1            TO IX-DEL                                       
028500     END-IF                                                               
028600     IF IX-DEL > 13                                                       
028700        MOVE 13           TO IX-DEL                                       
028800     END-IF                                                               
028900     IF IN-KVAVIS > ZERO                                                  
029000        COMPUTE WS-STYCK ROUNDED =                                        
029100                        IN-KVAVROP-AVB                                    
029300     ELSE                                                                 
029400        MOVE ZERO         TO WS-STYCK                                     
029500     END-IF                                                               
029600     MOVE WS-STYCK        TO W001-STYCK (IX-DEL)                          
029700     ADD  WS-STYCK        TO WS-SUM   (IX-DEL)                            
029800     ADD  WS-STYCK        TO TOT-SUM  (IX-DEL)                            
030200     MOVE IN-IDARTNR      TO OLD-IDARTNR                                  
030210                                                                          
030220     IF IN-IDLOPNRM NOT = OLD-IDLOPNRM                                    
030230        ADD  +1           TO WS-ANTAL                                     
030231        ADD  +1           TO TOT-ANTAL                                    
030240     END-IF                                                               
030250     MOVE IN-IDLOPNRM     TO OLD-IDLOPNRM                                 
030300                                                                          
030400     PERFORM BA-ADDERA-SNITTBER                                           
030500     .                                                                    
030600     EJECT                                                                
030700 BA-ADDERA-SNITTBER  SECTION.                                             
030800                                                                          
030900     MOVE IN-KVDAGAR TO WS-KVDAGAR                                        
031000     IF WS-KVDAGAR < ZERO                                                 
031100        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
031200     END-IF                                                               
031300     COMPUTE WS-VIKT = WS-KVDAGAR * WS-STYCK                              
031400     ADD WS-VIKT    TO WS-SUMVIKT                                         
031410     ADD WS-VIKT    TO TOT-SUMVIKT                                        
031500     .                                                                    
031600     EJECT                                                                
031700 C-SUMMERA-OLD-LEV   SECTION.                                             
031800                                                                          
031900     MOVE ZERO              TO WS-TOTSUM                                  
032000                                                                          
032100     MOVE +1                TO IX-DEL                                     
032200     PERFORM UNTIL IX-DEL > 13                                            
032300        ADD WS-SUM (IX-DEL) TO WS-TOTSUM                                  
032400        ADD +1              TO IX-DEL                                     
032500     END-PERFORM                                                          
032600                                                                          
032700     MOVE +1                TO IX-DEL                                     
032800     PERFORM UNTIL IX-DEL > 13                                            
032900        IF WS-TOTSUM > ZERO                                               
033000           COMPUTE WS-PROC (IX-DEL) ROUNDED =                             
033100                   100 * WS-SUM (IX-DEL) / WS-TOTSUM                      
033200        ELSE                                                              
033300           MOVE ZERO        TO WS-PROC (IX-DEL)                           
033400        END-IF                                                            
033500        MOVE WS-PROC (IX-DEL) TO W001-PROC (IX-DEL)                       
033600        ADD +1              TO IX-DEL                                     
033700     END-PERFORM                                                          
033800     MOVE WS-ANTAL-X        TO W001-ANTAL-X                               
033900     MOVE W001-SUMMA1       TO W001-RAD                                   
034000                                                                          
034200                                                                          
034300     PERFORM CA-SKRIV-SNITTAVVIKELSE                                      
034400                                                                          
034500     MOVE +1                TO IX-DEL                                     
034600     PERFORM UNTIL IX-DEL > 13                                            
034700        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
034800        MOVE ZERO           TO WS-PROC (IX-DEL)                           
034900        ADD +1              TO IX-DEL                                     
035000     END-PERFORM                                                          
035100     MOVE IN-IDLEVNR        TO OLD-IDLEVNR                                
035200                                                                          
035400     MOVE ZERO              TO WS-SUMVIKT                                 
035500     MOVE ZERO              TO WS-ANTAL WS-ANTAL-X                        
035600     MOVE ZERO              TO OLD-IDARTNR                                
035610     MOVE ZERO              TO OLD-IDLOPNRM                               
035700     .                                                                    
035800     EJECT                                                                
035900 CA-SKRIV-SNITTAVVIKELSE SECTION.                                         
036000                                                                          
036100     MOVE OLD-IDLEVNR     TO W001-IDLEVNR-SNITT                           
036200                                                                          
036300     IF WS-TOTSUM > ZERO                                                  
036400        COMPUTE WS-SNITT ROUNDED =                                        
036500                                WS-SUMVIKT / WS-TOTSUM                    
036600     ELSE                                                                 
036700        MOVE ZERO            TO WS-SNITT                                  
036800     END-IF                                                               
036900     MOVE WS-SNITT           TO W001-SNITTAVV                             
036910     MOVE WS-TOTSUM          TO W001-TOTSUM                               
037000     MOVE WS-PROC (7)        TO W001-RATT                                 
037100     MOVE ZERO               TO WS-SENA WS-TIDIGA                         
037200                                                                          
037300     MOVE +8                 TO IX-DEL                                    
037400     PERFORM UNTIL IX-DEL > 13                                            
037500        ADD WS-PROC (IX-DEL) TO WS-SENA                                   
037600        ADD +1               TO IX-DEL                                    
037700     END-PERFORM                                                          
037800     MOVE WS-SENA            TO W001-SENA                                 
037900                                                                          
038000     MOVE +1                 TO IX-DEL                                    
038100     PERFORM UNTIL IX-DEL > 6                                             
038200        ADD WS-PROC (IX-DEL) TO WS-TIDIGA                                 
038300        ADD +1               TO IX-DEL                                    
038400     END-PERFORM                                                          
038500     MOVE WS-TIDIGA          TO W001-TIDIGA                               
038600     MOVE W001-SNITTA        TO W001-RAD                                  
038700     PERFORM S21-SKRIV-W23627-001                                         
039100     .                                                                    
039200     EJECT                                                                
039210 D-SUMMERA-TOT-LEV   SECTION.                                             
039220                                                                          
039230     MOVE ZERO              TO TOT-TOTSUM                                 
039240                                                                          
039250     MOVE +1                TO IX-DEL                                     
039260     PERFORM UNTIL IX-DEL > 13                                            
039270        ADD TOT-SUM (IX-DEL) TO TOT-TOTSUM                                
039280        ADD +1              TO IX-DEL                                     
039290     END-PERFORM                                                          
039291                                                                          
039292     MOVE +1                TO IX-DEL                                     
039293     PERFORM UNTIL IX-DEL > 13                                            
039294        IF TOT-TOTSUM > ZERO                                              
039295           COMPUTE TOT-PROC (IX-DEL) ROUNDED =                            
039296                   100 * TOT-SUM (IX-DEL) / TOT-TOTSUM                    
039297        ELSE                                                              
039298           MOVE ZERO        TO TOT-PROC (IX-DEL)                          
039299        END-IF                                                            
039300        MOVE TOT-PROC (IX-DEL) TO W001-PROC-TOT (IX-DEL)                  
039301        ADD +1              TO IX-DEL                                     
039302     END-PERFORM                                                          
039306                                                                          
039307     PERFORM DA-SKRIV-SNITTAVVIKELSE                                      
039321     .                                                                    
039322     EJECT                                                                
039323 DA-SKRIV-SNITTAVVIKELSE SECTION.                                         
039324                                                                          
039325     MOVE 'TOT'           TO W001-IDLEVNR-TOT                             
039326                                                                          
039327     IF TOT-TOTSUM > ZERO                                                 
039328        COMPUTE TOT-SNITT ROUNDED =                                       
039329                                TOT-SUMVIKT / TOT-TOTSUM                  
039330     ELSE                                                                 
039331        MOVE ZERO            TO TOT-SNITT                                 
039332     END-IF                                                               
039333     MOVE TOT-SNITT          TO W001-SNITTAVV-TOT                         
039334     MOVE TOT-TOTSUM         TO W001-TOTSUM-TOT                           
039335     MOVE TOT-PROC (7)       TO W001-RATT-TOT                             
039336     MOVE ZERO               TO TOT-SENA TOT-TIDIGA                       
039337                                                                          
039338     MOVE +8                 TO IX-DEL                                    
039339     PERFORM UNTIL IX-DEL > 13                                            
039340        ADD TOT-PROC (IX-DEL) TO TOT-SENA                                 
039341        ADD +1               TO IX-DEL                                    
039342     END-PERFORM                                                          
039343     MOVE TOT-SENA           TO W001-SENA-TOT                             
039344                                                                          
039345     MOVE +1                 TO IX-DEL                                    
039346     PERFORM UNTIL IX-DEL > 6                                             
039347        ADD TOT-PROC (IX-DEL) TO TOT-TIDIGA                               
039348        ADD +1               TO IX-DEL                                    
039349     END-PERFORM                                                          
039350     MOVE TOT-TIDIGA         TO W001-TIDIGA-TOT                           
039351     MOVE W001-SNITT-TOT     TO W001-RAD                                  
039352     MOVE 50                 TO W001-RADRAKNARE                           
039353                                                                          
039354     PERFORM S21-SKRIV-W23627-001                                         
039355     .                                                                    
039356     EJECT                                                                
039360 Z-FINIT SECTION.                                                         
039400                                                                          
039500     CLOSE W23626                                                         
039600           W23627-001                                                     
039700     SKIP2                                                                
039800     MOVE 'S' TO POSTSUM-OPKOD                                            
039900     CALL POSTSUM USING POSTSUM-PARM                                      
040000     .                                                                    
040100     EJECT                                                                
040200 S01-LAES-W23626  SECTION.                                                
040300     READ W23626 INTO IN-AREA                                             
040400     AT END                                                               
040500        SET END-OF-W23626 TO TRUE                                         
040600                                                                          
040700     NOT AT END                                                           
040800        MOVE 'W23626'   TO POSTSUM-FDNAMN                                 
040900        MOVE 'W23627D1' TO POSTSUM-DDNAMN2                                
041000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
041100        CALL POSTSUM USING POSTSUM-PARM                                   
041200     END-READ                                                             
041300     .                                                                    
041400     EJECT                                                                
041500 S21-SKRIV-W23627-001  SECTION.                                           
041600                                                                          
041700     MOVE 2 TO W001-SKIP                                                  
041800     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
041900       PERFORM S21A-SKRIV-RUBRIKER                                        
042000     END-IF                                                               
042100     SKIP2                                                                
042200     WRITE W23627-001-RAD FROM W001-RAD AFTER W001-SKIP                   
042300     SKIP2                                                                
042400     MOVE SPACE TO W001-RAD                                               
042500     ADD  +2 TO W001-RADRAKNARE                                           
042600     .                                                                    
042700     EJECT                                                                
042800 S21S-SKRIV-W23627-001  SECTION.                                          
042900                                                                          
043000     MOVE 2 TO W001-SKIP                                                  
043100     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
043200       PERFORM S21A-SKRIV-RUBRIKER                                        
043300     END-IF                                                               
043400     SKIP2                                                                
043500     WRITE W23627-001-RAD FROM W001-RAD AFTER W001-SKIP                   
043600     SKIP2                                                                
043700     MOVE SPACE TO W001-RAD                                               
043800     ADD  +2 TO W001-RADRAKNARE                                           
043900     .                                                                    
044000     EJECT                                                                
044100 S21A-SKRIV-RUBRIKER SECTION.                                             
044200                                                                          
044300     ADD +1 TO W001-SIDRAKNARE                                            
044400     MOVE W001-SIDRAKNARE TO W001-SID                                     
044500     WRITE W23627-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
044600     WRITE W23627-001-RAD FROM W001-RUBRIKA AFTER 2                       
044700     WRITE W23627-001-RAD FROM W001-RUBRIKB AFTER 1                       
044800     MOVE +7 TO W001-RADRAKNARE                                           
044900     SKIP2                                                                
045000     MOVE 2 TO W001-SKIP                                                  
045100     .                                                                    
045200     EJECT                                                                
045300 S99-ABEND SECTION.                                                       
045400                                                                          
045500     SKIP2                                                                
045600     MOVE 'S' TO POSTSUM-OPKOD                                            
045700     CALL POSTSUM USING POSTSUM-PARM                                      
045800     CALL ABEND USING RKOD-ABEND                                          
045900     .                                                                    
