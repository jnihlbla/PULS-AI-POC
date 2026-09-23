000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6125200.                                                
000400 AUTHOR.         JOHAN LINDKVIST.                                         
000500 DATE-WRITTEN.   97/04/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER 310:OR OCH SKAPAR UTLISTOR (AK IN HOUSE) PER DC            
001100*        SKAPAR ÄVEN UTFIL ENLIGT ROCKLEIGH-VAR. TILL FREDAGAR.           
001200*                                                                         
001300*    ÄNDRINGAR:                                                           
001400* 99-03-02    JOHAN L                                                     
001500*        ANDRA SA ATT JPN/AUS FAR "VALUE" I SEK                           
001600* 99-03-10    JOHAN L                                                     
001700*        SPLITTRA LISTAN NORTH AMERICA / JAPAN / AUSTRALIEN               
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- 310:OR                                                     
002800     SELECT W61251                    ASSIGN TO W61252D1.                 
002900     SKIP2                                                                
003000*          --- AK IN HOUSE                                                
003100     SELECT W61252-001                 ASSIGN TO W61252D2.                
003200     SKIP2                                                                
004200*          --- SORTERINGSFIL                                              
004300     SELECT SORTFIL                    ASSIGN TO W61252DS.                
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP3                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  W61251                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  -COPY W61251      -L.                                                
005400     SKIP3                                                                
005500 FD  W61252-001                                                           
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800     SKIP2                                                                
005900 01  W61252-001-RAD              PIC X(121).                              
006000     SKIP3                                                                
007800     SKIP2                                                                
007900 SD  SORTFIL.                                                             
008000                                                                          
008100*01  POST -COPY W61251     -PRE SORT-                                     
008200     EJECT                                                                
008300 WORKING-STORAGE SECTION.                                                 
008400                                                                          
008500 01  TEMP-VALUE                  PIC S9(7)V9(2)  COMP-3.                  
008600 01  PERCENT-PRIOVALUE           PIC 9(1)V9(4)   COMP-3.                  
008700 01  PERCENT-PRIOLINES           PIC 9(1)V9(4)   COMP-3.                  
008800 01  TOTAL-VALUE                 PIC S9(7)V9(2)  COMP-3.                  
008900 01  TOT-ANTAL-RADER             PIC 9(5)        COMP-3.                  
009000                                                                          
009100 01 TABELL.                                                               
009200   03 FAKTDATA OCCURS 8 TIMES.                                            
009300     05 EJPRIO-TOTVALUE          PIC S9(7)V9(2) COMP-3.                   
009400     05 PRIO-TOTVALUE            PIC S9(7)V9(2) COMP-3.                   
009500     05 EJPRIO-ANTAL-RADER       PIC 9(5)       COMP-3.                   
009600     05 PRIO-ANTAL-RADER         PIC 9(5)       COMP-3.                   
009700     05 ANTAL-KOLLI              PIC 9(5)       COMP-3.                   
009800     05 SPAR-FAKTURA             PIC S9(7)      COMP-3.                   
009900     05 SPAR-ORDNR5              PIC 9(5)       COMP-3.                   
010000     05 SPAR-KUNDNR              PIC S9(7)      COMP-3.                   
010100     05 SPAR-KOLLI               PIC S9(5)      COMP-3.                   
010200     05 IDDC                     PIC X(2).                                
010300                                                                          
010400 01  IND                         PIC 99.                                  
010500 01  IND2                        PIC 99.                                  
010600                                                                          
010700 77  IDPGM                       PIC X(8)    VALUE 'W6125200'.            
010800 77  JA                          PIC X       VALUE 'J'.                   
010900 77  NEJ                         PIC X       VALUE 'N'.                   
011000                                                                          
011100 77  W61251-EOF-SW               PIC X       VALUE 'N'.                   
011200     88  END-OF-W61251                       VALUE 'J'.                   
011300                                                                          
011400 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
011500     88  END-OF-SORTFIL                      VALUE 'J'.                   
011600     EJECT                                                                
011700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011800 01  FILLER REDEFINES DAGENS-DATUM.                                       
011900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012200     EJECT                                                                
012300*      --- VALID IDDC CODES                                               
012400*                                                                         
012500*01    -COPY WWDC99                                                       
012600*01    -COPY WWDCKONS                                                     
012700       EJECT                                                              
012800 01  DYNAMISKA-SUBPROGRAM.                                                
012900*                                                                         
013000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
013300     SKIP2                                                                
013400*    --- PARAMETRAR TILL ABEND                                            
013500                                                                          
013600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013900     SKIP2                                                                
014000 01  FELTEXT.                                                             
014100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL DATKORT                                          
014500*                                                                         
014600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61252'.              
014700     SKIP2                                                                
014800 01  DATUMKORT-ID                PIC X(6)    VALUE '000000'.              
014900     SKIP2                                                                
015000*01  -COPY WDATKORT                                                       
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL POSTSUM                                          
015300*                                                                         
015400*01  -COPY W0005   -PRE  POSTSUM-                                         
015500     EJECT                                                                
015600*01  -COPY WDATAREA                                                       
015700     EJECT                                                                
015800 01  IN-AREA-START               PIC X(24)   VALUE                        
015900                                 'IN-AREA-START  '.                       
016000     SKIP2                                                                
016100                                                                          
016200*01  AREA -COPY W61251     -PRE IN-                                       
016300     EJECT                                                                
016400 01  W00N-AREA-START             PIC X(24)   VALUE                        
016500                                 'W00N-AREA-START  '.                     
016600     SKIP2                                                                
016700 01  W00N-RUBRIK1.                                                        
016800*                                                                         
016900     03  FILLER                  PIC X(3) VALUE SPACES.                   
017000     03  FILLER                  PIC X(21)                                
017100                                VALUE 'VOLVO CAR PARTS      '.            
017200     03  W00N-LISTNR             PIC X(12)                                
017300                                 VALUE 'W61252-00X'.                      
017400     03  FILLER                  PIC X(54)                                
017500                      VALUE '       AK IN HOUSE, REFILL.   '.             
017600     03  W001-DATUM              PIC XXBXXBXX.                            
017700     03  FILLER                  PIC X(4) VALUE SPACES.                   
017800     03  FILLER                  PIC X(4)                                 
017900                                 VALUE 'PAGE'.                            
018000     03  W001-SID                PIC Z(4)9.                               
018100     SKIP2                                                                
018200 01  W00N-RUBRIK2.                                                        
018300     03  FILLER                  PIC X(3)  VALUE SPACES.                  
018400     03  FILLER                  PIC X(2)  VALUE 'DC'.                    
018500     03  FILLER                  PIC X(16) VALUE SPACES.                  
018600     03  FILLER                  PIC X(5)  VALUE ' QTY '.                 
018700     03  FILLER                  PIC X(16) VALUE SPACES.                  
018800     03  FILLER                  PIC X(5)  VALUE ' QTY '.                 
018900     03  FILLER                  PIC X(16) VALUE SPACES.                  
019000     03  FILLER                  PIC X(5)  VALUE 'VALUE'.                 
019100     03  FILLER                  PIC X(16) VALUE SPACES.                  
019200     03  FILLER                  PIC X(5)  VALUE '%PRIO'.                 
019300     03  FILLER                  PIC X(16) VALUE SPACES.                  
019400     03  FILLER                  PIC X(5)  VALUE '%PRIO'.                 
019500                                                                          
019600 01  W00N-RUBRIK3.                                                        
019700     03  FILLER                  PIC X(21) VALUE SPACES.                  
019800     03  FILLER                  PIC X(5)  VALUE 'CASES'.                 
019900     03  FILLER                  PIC X(16) VALUE SPACES.                  
020000     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
020100     03  FILLER                  PIC X(16) VALUE SPACES.                  
020200     03  W00N-TECKEN             PIC X(5)  VALUE '  $  '.                 
020300     03  FILLER                  PIC X(16) VALUE SPACES.                  
020400     03  FILLER                  PIC X(5)  VALUE 'VALUE'.                 
020500     03  FILLER                  PIC X(16) VALUE SPACES.                  
020600     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
020700     EJECT                                                                
020800 01  W00B-AREA-START             PIC X(24)   VALUE                        
020900                                 'W00N-AREA-START  '.                     
021000     SKIP2                                                                
021100 01  W00B-RUBRIK1.                                                        
021200*                                                                         
021300     03  FILLER                  PIC X(3) VALUE SPACES.                   
021400     03  FILLER                  PIC X(21)                                
021500                                VALUE 'VOLVO CAR PARTS      '.            
021600     03  W00B-LISTNR             PIC X(12)                                
021700                                 VALUE 'W61252-00X'.                      
021800     03  FILLER                  PIC X(54)                                
021900                      VALUE '       AK IN HOUSE, REFILL.   '.             
022000     03  W00B-DATUM              PIC XXBXXBXX.                            
022100     03  FILLER                  PIC X(4) VALUE SPACES.                   
022200     03  FILLER                  PIC X(4)                                 
022300                                 VALUE 'SIDA'.                            
022400     03  W00B-SID                PIC Z(4)9.                               
022500     SKIP2                                                                
022600 01  W00B-RUBRIK2.                                                        
022700     03  FILLER                  PIC X(3)  VALUE SPACES.                  
022800     03  FILLER                  PIC X(2)  VALUE 'DC'.                    
022900     03  FILLER                  PIC X(16) VALUE SPACES.                  
023000     03  FILLER                  PIC X(5)  VALUE ' QTY '.                 
023100     03  FILLER                  PIC X(16) VALUE SPACES.                  
023200     03  FILLER                  PIC X(5)  VALUE ' QTY '.                 
023300     03  FILLER                  PIC X(16) VALUE SPACES.                  
023400     03  FILLER                  PIC X(5)  VALUE '     '.                 
023500     03  FILLER                  PIC X(16) VALUE SPACES.                  
023600     03  FILLER                  PIC X(5)  VALUE '     '.                 
023700     03  FILLER                  PIC X(16) VALUE SPACES.                  
023800     03  FILLER                  PIC X(5)  VALUE '%PRIO'.                 
023900                                                                          
024000 01  W00B-RUBRIK3.                                                        
024100     03  FILLER                  PIC X(21) VALUE SPACES.                  
024200     03  FILLER                  PIC X(5)  VALUE 'CASES'.                 
024300     03  FILLER                  PIC X(16) VALUE SPACES.                  
024400     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
024500     03  FILLER                  PIC X(16) VALUE SPACES.                  
024600     03  W00B-TECKEN             PIC X(5)  VALUE '     '.                 
024700     03  FILLER                  PIC X(16) VALUE SPACES.                  
024800     03  FILLER                  PIC X(5)  VALUE '     '.                 
024900     03  FILLER                  PIC X(16) VALUE SPACES.                  
025000     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
025100     EJECT                                                                
025200 01  W001-AREA-START             PIC X(24)   VALUE                        
025300                                 'W001-AREA-START  '.                     
025400     SKIP2                                                                
025500 01  W001-HJALPAREOR.                                                     
025600*                                                                         
025700     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 3.                
025800     03  W001-ANTAL-RADER                                                 
025900                                 PIC 9(3)    VALUE 999.                   
026000     03  W001-MAX-RADER-PER-SIDA                                          
026100                                 PIC 9(3)    VALUE 42.                    
026200     03  W001-MAX-POSITIONER-PER-RAD                                      
026300                                 PIC 9(3)    VALUE 120.                   
026400     03  W001-LISTNR             PIC X(11)   VALUE 'W61252-001'.          
026500     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
026600     EJECT                                                                
026700 01  W041-RAD.                                                            
026800     03  FILLER                  PIC X(121)  VALUE SPACE.                 
026900 01  W092-RAD.                                                            
027000     03  FILLER                  PIC X(121)  VALUE SPACE.                 
027100 01  W043-RAD.                                                            
027200     03  FILLER                  PIC X(121)  VALUE SPACE.                 
027300 01  W044-RAD.                                                            
027400     03  FILLER                  PIC X(121)  VALUE SPACE.                 
027410 01  W045-RAD.                                                            
027420     03  FILLER                  PIC X(121)  VALUE SPACE.                 
027430 01  W046-RAD.                                                            
027440     03  FILLER                  PIC X(121)  VALUE SPACE.                 
027500 01  W051-RAD.                                                            
027600     03  FILLER                  PIC X(121)  VALUE SPACE.                 
027500 01  W047-RAD.                                                            
027600     03  FILLER                  PIC X(121)  VALUE SPACE.                 
027700     EJECT                                                                
032810 01  WERR-RAD.                                                            
032820     03  FILLER                  PIC X(121)  VALUE SPACE.                 
032830     EJECT                                                                
032900 01  W00X-DETALJ.                                                         
033000     03  FILLER                  PIC X(3)  VALUE SPACES.                  
033100     03  W00X-DC                 PIC XX.                                  
033200     03  FILLER                  PIC X(16) VALUE SPACES.                  
033300     03  W00X-QTY-CASES          PIC ZZZZ9.                               
033400     03  FILLER                  PIC X(16) VALUE SPACES.                  
033500     03  W00X-QTY-LINES          PIC ZZZZ9.                               
033600     03  FILLER                  PIC X(10) VALUE SPACES.                  
033700     03  W00X-TOTAL-VALUE        PIC ZBZZZBZZ9V,99.                       
033800     03  FILLER                  PIC X(15) VALUE SPACES.                  
033900     03  W00X-PERCENT-PRIOVALUE  PIC ZZ9V,9.                              
034000     03  FILLER                  PIC X(16) VALUE SPACES.                  
034100     03  W00X-PERCENT-PRIOLINES  PIC ZZ9V,9.                              
034200     EJECT                                                                
034300 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
034400                                  'SORTWS-AREA-START  '.                  
034500     SKIP2                                                                
034600                                                                          
034700*01  AREA -COPY W61251      -PRE SORTWS-                                  
034800 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
034900     EJECT                                                                
035000 PROCEDURE DIVISION.                                                      
035100 MAIN SECTION.                                                            
035200     SKIP2                                                                
035300                                                                          
035400     PERFORM A-INIT                                                       
035500                                                                          
035600     SORT SORTFIL ASCENDING KEY SORT-SHIST-IDDC                           
035700                                SORT-SHIST-IDFAKT                         
035800                                SORT-SHIST-IDORDNR5                       
035900                                SORT-SHIST-IDKUNDNR                       
036000                                SORT-SHIST-IDKOLLI                        
036100                  USING W61251                                            
036200                  OUTPUT PROCEDURE B-SORT-OUTPUT                          
036300                                                                          
036400     IF SORT-RETURN NOT = 0                                               
036500       MOVE SORT-RETURN TO SORT-RETURN-X                                  
036600       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
036700       DELIMITED BY SIZE INTO FELTEXT-STR                                 
036800       DISPLAY FELTEXT                                                    
036900       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
037000       PERFORM S99-ABEND                                                  
037100     ELSE                                                                 
037200       PERFORM D-UTSKRIFT                                                 
037300       PERFORM Z-FINIT                                                    
037400                                                                          
037500       MOVE ZERO TO RETURN-CODE                                           
037600       GOBACK                                                             
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 A-INIT SECTION.                                                          
038100     OPEN OUTPUT W61252-001                                               
038500                                                                          
038600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
038700     STRING D-AAR D-MAANAD D-DAG DELIMITED BY SIZE                        
038800                                 INTO DAGENS-DATUM                        
038900     MOVE DAGENS-DATUM TO W001-DATUM                                      
039000     MOVE DAGENS-DATUM TO W00B-DATUM                                      
039100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039200                                                                          
039300     INITIALIZE TABELL                                                    
039400     MOVE WC-NDC-US-RU  TO IDDC(1)                                        
039500     MOVE WC-NDC-US-BAT TO IDDC(2)                                        
039600     MOVE WC-NDC-US-LA  TO IDDC(3)                                        
039700     MOVE WC-NDC-US-SE  TO IDDC(4)                                        
039800     MOVE WC-NDC-US-CH  TO IDDC(5)                                        
039900     MOVE WC-NDC-US-JA  TO IDDC(6)                                        
040000     MOVE WC-NDC-CA     TO IDDC(7)                                        
040000     MOVE WC-NDC-US-DA  TO IDDC(8)                                        
040100     .                                                                    
040200     EJECT                                                                
040300 B-SORT-OUTPUT SECTION.                                                   
040400     SKIP2                                                                
040500     PERFORM S91-SORT-RETURN                                              
040600     PERFORM UNTIL END-OF-SORTFIL                                         
040700       MOVE  SORTWS-SHIST-IDDC  TO WS-IDDC                                
040800       IF NDC                                                             
040900          PERFORM C-SUMMERA                                               
041000       ELSE                                                               
041100          CONTINUE                                                        
041200       END-IF                                                             
041300       PERFORM S91-SORT-RETURN                                            
041400     END-PERFORM                                                          
041500     .                                                                    
041600     EJECT                                                                
041700 C-SUMMERA SECTION.                                                       
041800     MOVE  SORTWS-SHIST-IDDC  TO WS-IDDC                                  
041900     EVALUATE TRUE                                                        
042000       WHEN NDC-US-RU                                                     
042100          MOVE 1 TO IND2                                                  
042200       WHEN NDC-US-BAT                                                    
042300          MOVE 2 TO IND2                                                  
042400       WHEN NDC-US-LA                                                     
042500          MOVE 3 TO IND2                                                  
042600       WHEN NDC-US-SE                                                     
042700          MOVE 4 TO IND2                                                  
042800       WHEN NDC-US-CH                                                     
042900          MOVE 5 TO IND2                                                  
043000       WHEN NDC-US-JA                                                     
043100          MOVE 6 TO IND2                                                  
043200       WHEN NDC-CA                                                        
043300          MOVE 7 TO IND2                                                  
043200       WHEN NDC-US-DA                                                     
043300          MOVE 8 TO IND2                                                  
043400       WHEN OTHER                                                         
043410          MOVE 9 TO IND2                                                  
043500*         MOVE 'FEL IDDC (C-SEKTIONEN) !!!' TO FELTEXT-STR                
043600*         DISPLAY FELTEXT                                                 
043700*         PERFORM S99-ABEND                                               
043800     END-EVALUATE                                                         
043900                                                                          
044000     IF IND2 = 9                                                          
044001       CONTINUE                                                           
044002     ELSE                                                                 
044400       COMPUTE TEMP-VALUE ROUNDED = SORTWS-SHIST-KVAVIS *                 
044500              (SORTWS-SHIST-PRARTNTO / SORTWS-SHIST-PRKURS)               
044700                                                                          
044800       IF SORTWS-SHIST-FLPRIO = 'N'                                       
044900          ADD TEMP-VALUE TO EJPRIO-TOTVALUE(IND2)                         
045000          ADD 1 TO EJPRIO-ANTAL-RADER(IND2)                               
045100       ELSE                                                               
045200          ADD TEMP-VALUE TO PRIO-TOTVALUE(IND2)                           
045300          ADD 1 TO PRIO-ANTAL-RADER(IND2)                                 
045400       END-IF                                                             
045500                                                                          
045600       IF SPAR-FAKTURA(IND2) = SORTWS-SHIST-IDFAKT                        
045700          IF SPAR-ORDNR5(IND2) = SORTWS-SHIST-IDORDNR5                    
045800             IF SPAR-KUNDNR(IND2) = SORTWS-SHIST-IDKUNDNR                 
045900                CONTINUE                                                  
046000             ELSE                                                         
046100                MOVE ZERO TO SPAR-KOLLI(IND2)                             
046200                MOVE SORTWS-SHIST-IDKUNDNR TO SPAR-KUNDNR(IND2)           
046300             END-IF                                                       
046400          ELSE                                                            
046500             MOVE ZERO TO SPAR-KOLLI(IND2)                                
046600             MOVE SORTWS-SHIST-IDKUNDNR TO SPAR-KUNDNR(IND2)              
046700             MOVE SORTWS-SHIST-IDORDNR5 TO SPAR-ORDNR5(IND2)              
046800          END-IF                                                          
046900       ELSE                                                               
047000          MOVE SORTWS-SHIST-IDFAKT TO SPAR-FAKTURA(IND2)                  
047100          MOVE SORTWS-SHIST-IDORDNR5 TO SPAR-ORDNR5(IND2)                 
047200          MOVE SORTWS-SHIST-IDKUNDNR TO SPAR-KUNDNR(IND2)                 
047300          MOVE ZERO TO SPAR-KOLLI(IND2)                                   
047400       END-IF                                                             
047500                                                                          
047600       IF NOT SORTWS-SHIST-IDKOLLI = SPAR-KOLLI(IND2)                     
047700          ADD 1 TO ANTAL-KOLLI(IND2)                                      
047800          MOVE SORTWS-SHIST-IDKOLLI TO SPAR-KOLLI(IND2)                   
047900       ELSE                                                               
048000           CONTINUE                                                       
048100*          SAMMA KOLLI SOM FÖRRA INPOSTEN                                 
048200       END-IF                                                             
048210     END-IF                                                               
048300     EJECT                                                                
048400     .                                                                    
048500 D-UTSKRIFT SECTION.                                                      
048600     PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 8                        
048700        PERFORM DA-COMPUTE                                                
048800        MOVE IDDC(IND)     TO WS-IDDC                                     
048900        EVALUATE TRUE                                                     
049000           WHEN NDC-US-RU                                                 
049100              MOVE W00X-DETALJ TO W041-RAD                                
049200           WHEN NDC-US-BAT                                                
049300              MOVE W00X-DETALJ TO W092-RAD                                
049400           WHEN NDC-US-LA                                                 
049500              MOVE W00X-DETALJ TO W043-RAD                                
049600           WHEN NDC-US-SE                                                 
049700              MOVE W00X-DETALJ TO W044-RAD                                
049800           WHEN NDC-US-CH                                                 
049900              MOVE W00X-DETALJ TO W045-RAD                                
050000           WHEN NDC-US-JA                                                 
050100              MOVE W00X-DETALJ TO W046-RAD                                
050200           WHEN NDC-CA                                                    
050300              MOVE W00X-DETALJ TO W051-RAD                                
050200           WHEN NDC-US-DA                                                 
050300              MOVE W00X-DETALJ TO W047-RAD                                
050400           WHEN OTHER                                                     
050410              MOVE W00X-DETALJ TO WERR-RAD                                
050500*             MOVE 'FEL IDDC (D-SEKTIONEN) !!!' TO FELTEXT-STR            
050600*             DISPLAY FELTEXT                                             
050700*             PERFORM S99-ABEND                                           
050800        END-EVALUATE                                                      
050900     END-PERFORM                                                          
051000                                                                          
051100* UTSKRIFT AV RUBRIKER                                                    
051200     PERFORM S21A-SKRIV-RUBRIKER                                          
051800                                                                          
051900* UTSKRIFT AV DATA                                                        
052000     PERFORM S21-SKRIV-W61252-001                                         
052600     .                                                                    
052700     EJECT                                                                
052800 DA-COMPUTE SECTION.                                                      
052900     COMPUTE TOTAL-VALUE =                                                
053000                EJPRIO-TOTVALUE(IND) + PRIO-TOTVALUE(IND)                 
053100     IF TOTAL-VALUE = 0                                                   
053200        MOVE 0 TO PERCENT-PRIOVALUE                                       
053300     ELSE                                                                 
053400        COMPUTE PERCENT-PRIOVALUE =                                       
053500                    PRIO-TOTVALUE(IND) / TOTAL-VALUE                      
053600     END-IF                                                               
053700     IF (PRIO-ANTAL-RADER(IND)       = 0 ) AND                            
053800        (EJPRIO-ANTAL-RADER(IND) = 0 )                                    
053900        MOVE 0 TO PERCENT-PRIOLINES                                       
054000     ELSE                                                                 
054100        COMPUTE PERCENT-PRIOLINES = PRIO-ANTAL-RADER(IND) /               
054200              (PRIO-ANTAL-RADER(IND) + EJPRIO-ANTAL-RADER(IND))           
054300     END-IF                                                               
054400     ADD PRIO-ANTAL-RADER(IND) TO EJPRIO-ANTAL-RADER(IND)                 
054500                               GIVING TOT-ANTAL-RADER                     
054600     MOVE IDDC(IND)             TO W00X-DC                                
054700     MOVE ANTAL-KOLLI(IND)      TO W00X-QTY-CASES                         
054800     MOVE TOT-ANTAL-RADER       TO W00X-QTY-LINES                         
054900     MOVE TOTAL-VALUE           TO W00X-TOTAL-VALUE                       
055000                                                                          
055100*   MOVE PERCENT-PRIOVALUE TO W00X-PERCENT-PRIOVALUE                      
055200     COMPUTE W00X-PERCENT-PRIOVALUE ROUNDED =                             
055300             PERCENT-PRIOVALUE * 100                                      
055400*   MOVE PERCENT-PRIOLINES TO W00X-PERCENT-PRIOLINES                      
055500     COMPUTE W00X-PERCENT-PRIOLINES ROUNDED =                             
055600             PERCENT-PRIOLINES * 100                                      
055700     EJECT                                                                
055800     .                                                                    
055900 Z-FINIT SECTION.                                                         
056000     CLOSE W61252-001                                                     
056400                                                                          
056500     MOVE 'S' TO POSTSUM-OPKOD                                            
056600     CALL POSTSUM USING POSTSUM-PARM                                      
056700     .                                                                    
056800     EJECT                                                                
056900 S21-SKRIV-W61252-001  SECTION.                                           
057000                                                                          
057100     MOVE 1 TO W001-SKIP                                                  
057200     IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                        
057300       PERFORM S21A-SKRIV-RUBRIKER                                        
057400     END-IF                                                               
057500     SKIP2                                                                
057600     WRITE W61252-001-RAD FROM W041-RAD AFTER W001-SKIP                   
057700     WRITE W61252-001-RAD FROM W092-RAD AFTER W001-SKIP                   
057800     WRITE W61252-001-RAD FROM W043-RAD AFTER W001-SKIP                   
057900     WRITE W61252-001-RAD FROM W044-RAD AFTER W001-SKIP                   
057910     WRITE W61252-001-RAD FROM W045-RAD AFTER W001-SKIP                   
057920     WRITE W61252-001-RAD FROM W046-RAD AFTER W001-SKIP                   
058000     WRITE W61252-001-RAD FROM W051-RAD AFTER W001-SKIP                   
058000     WRITE W61252-001-RAD FROM W047-RAD AFTER W001-SKIP                   
058100     SKIP2                                                                
058200     MOVE SPACE TO W041-RAD                                               
058300     MOVE SPACE TO W092-RAD                                               
058400     MOVE SPACE TO W043-RAD                                               
058500     MOVE SPACE TO W044-RAD                                               
058510     MOVE SPACE TO W045-RAD                                               
058520     MOVE SPACE TO W046-RAD                                               
058600     MOVE SPACE TO W051-RAD                                               
058600     MOVE SPACE TO W047-RAD                                               
058700     ADD  +8 TO W001-ANTAL-RADER                                          
058800     .                                                                    
058900     EJECT                                                                
059000 S21A-SKRIV-RUBRIKER SECTION.                                             
059100                                                                          
059200     MOVE '  $  '          TO  W00N-TECKEN                                
059300     MOVE 'W61252-001  '   TO  W00N-LISTNR                                
059400                                                                          
059500     ADD +1 TO W001-SIDRAKNARE                                            
059600     MOVE W001-SIDRAKNARE TO W001-SID                                     
059700     WRITE W61252-001-RAD FROM W00N-RUBRIK1 AFTER PAGE                    
059800     WRITE W61252-001-RAD FROM W00N-RUBRIK2 AFTER 2                       
059900     WRITE W61252-001-RAD FROM W00N-RUBRIK3 AFTER 1                       
060000     MOVE +8 TO W001-ANTAL-RADER                                          
060100     SKIP2                                                                
060200     MOVE 3 TO W001-SKIP                                                  
060300     .                                                                    
060400     EJECT                                                                
069400 S91-SORT-RETURN  SECTION.                                                
069500                                                                          
069600     RETURN SORTFIL INTO SORTWS-AREA                                      
069700     AT END                                                               
069800         SET END-OF-SORTFIL TO TRUE                                       
069900     .                                                                    
070000     EJECT                                                                
070100 S99-ABEND SECTION.                                                       
070200                                                                          
070300     SKIP2                                                                
070400     MOVE 'S' TO POSTSUM-OPKOD                                            
070500     CALL POSTSUM USING POSTSUM-PARM                                      
070600     CALL ABEND USING RKOD-ABEND                                          
070700     .                                                                    
