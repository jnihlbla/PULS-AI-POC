000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5550700.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   98/01/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER IN PARAMETERFILEN W55504, SORTERAR DEN STIGANDE            
001000*        OCH LÄSER IN W55501 INNEVARANDE ÅRS INFO SAMT OM                 
001100*        BESTÄLLT, FÖREGÅENDE ÅRS INFO, FIL W55508                        
001200*        OCH UPPDATERAR SEDAN BASEN WDL9/WLLOGA MED DETTA URVAL.          
001210***  FÖLJANDE FIX LADES UPP 02-04-28 FÖR EVEREST                          
001220***  BÖR TAS BORT EFTER 04-02-01, DÅ LEVNR I GAMMAL FORM RENSATS.         
001230***  SÖK PÅ FIX OCH LEVNR                                                 
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  VID SORTFEL                                             
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- PARAMETERFILEN                                             
002500     SELECT W55504                     ASSIGN TO W55507D1.                
002600     SKIP2                                                                
002700*          --- INNEVARANDE-ARS-INFO                                       
002800     SELECT W55501                     ASSIGN TO W55507D2.                
002900     SKIP2                                                                
003000*          --- FOREGAENDE-ARS-INFO                                        
003100     SELECT W55508                     ASSIGN TO W55507D3.                
003200     SKIP2                                                                
003300*          --- SORTERAD-UTFIL                                             
003400     SELECT W55507                     ASSIGN TO W55507D4.                
003500     SKIP2                                                                
003600*          --- SORTERINGSFIL                                              
003700     SELECT SORTFIL                    ASSIGN TO W55507DS.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W55504                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W55504      -L.                                                
004800     SKIP3                                                                
004900 FD  W55501                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  -COPY W55501      -L.                                                
005400     SKIP3                                                                
005500 FD  W55508                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  -COPY W55501      -L.                                                
006000     SKIP3                                                                
006100 FD  W55507                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  POST -COPY W55507 -PRE  UT-  -L.                                     
006600     SKIP2                                                                
006700 SD  SORTFIL.                                                             
006800                                                                          
006900*01  POST -COPY W55504      -PRE SORT-                                    
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200                                                                          
007300*    -- CHECKED BY WY2000                                                 
007400 77  IDPGM                       PIC X(8)    VALUE 'W5550700'.            
007500 77  JA                          PIC X       VALUE 'J'.                   
007600 77  NEJ                         PIC X       VALUE 'N'.                   
007700 77  IX                          PIC S9(3)   VALUE ZERO.                  
007800                                                                          
007900 77  W55504-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-W55504                       VALUE 'J'.                   
008100                                                                          
008200 77  W55501-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W55501                       VALUE 'J'.                   
008400                                                                          
008500 77  W5550X-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W55508                       VALUE 'J'.                   
008700                                                                          
008800 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
008900     88  END-OF-SORTFIL                      VALUE 'J'.                   
009000     EJECT                                                                
009200 01  FOREGAENDE-SW               PIC X       VALUE 'N'.                   
009300 01  INNEVARANDE-SW              PIC X       VALUE 'N'.                   
009400 01  FOREG-ANTAL                 PIC S9(9)   VALUE ZERO COMP-3.           
009500 01  INNEV-ANTAL                 PIC S9(9)   VALUE ZERO COMP-3.           
009600 01  URVAL-ANTAL                 PIC S9(9)   VALUE ZERO COMP-3.           
009700 01  LADD-ANTAL                  PIC S9(9)   VALUE ZERO COMP-3.           
009710***  FÖLJANDE FIX LADES UPP 02-04-28 FÖR EVEREST                          
009720***  BÖR TAS BORT EFTER 04-02-01, DÅ LEVNR I PACKAD FORM RENSATS.         
009730                                                                          
009740*    --- FÄLT FÖR IDLEVNR-KONVERTERING                                    
009760 01  IDLEVNR-DISPLAY             PIC 9(5).                                
009780                                                                          
009801 01  WS-IDLEVNR.                                                          
009810     03  WS-IDLEVNR-X            PIC X(3).                                
009900     03  WS-IDLEVNR-N REDEFINES WS-IDLEVNR-X PIC S9(5) COMP-3.            
010000**** END-FIX                                                              
010400     EJECT                                                                
010500 01 FILLER                      PIC X(16)  VALUE 'TABELL'.                
010600 01 TAB-AREA.                                                             
010700    03 TABELL-RAD OCCURS 200 INDEXED BY TAB-INDEX.                        
010800      05 AREA -PRE TABELL- -COPY W55504.                                  
011000                                                                          
011100 01  DYNAMISKA-SUBPROGRAM.                                                
011300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011400     SKIP2                                                                
011500*    --- PARAMETRAR TILL ABEND                                            
011600                                                                          
011700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012000     SKIP2                                                                
012100 01  FELTEXT.                                                             
012200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012400     EJECT                                                                
012500 01  PRM-AREA-START              PIC X(24)   VALUE                        
012600                                 'PRM-AREA-START  '.                      
012700                                                                          
012900*01  AREA -COPY W55504     -PRE PRM-                                      
013000     EJECT                                                                
013100 01  INNE-AREA-START             PIC X(24)   VALUE                        
013200                                 'INNE-AREA-START  '.                     
013400                                                                          
013500*01  AREA -COPY W55501     -PRE INNE-                                     
013600     EJECT                                                                
013700 01  FORE-AREA-START             PIC X(24)   VALUE                        
013800                                 'FORE-AREA-START  '.                     
014000                                                                          
014100*01  AREA -COPY W55501     -PRE FORE-                                     
014200     EJECT                                                                
014300 01  UT-AREA-START               PIC X(24)   VALUE                        
014400                                 'UT-AREA-START  '.                       
014600                                                                          
014700*01  AREA -COPY W55507     -PRE UT1-                                      
014800     EJECT                                                                
014900 01  WS-SPAR-AREA.                                                        
015000     03 SPAR-FLSORT              PIC X       VALUE SPACE.                 
015100     03 SPAR-IDARTNR             PIC S9(9)   VALUE ZERO COMP-3.           
015200     03 SPAR-INNE-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
015300     SKIP2                                                                
015400 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
015500                                  'SORTWS-AREA-START  '.                  
015600                                                                          
015700*01  AREA -COPY W55504      -PRE SORTWS-                                  
015800 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
015900     EJECT                                                                
016000 PROCEDURE DIVISION.                                                      
016100 MAIN SECTION.                                                            
016200                                                                          
016300     PERFORM A-INIT                                                       
016400                                                                          
016500     SORT SORTFIL ASCENDING KEY SORT-BEST-FLSORT                          
016600                                SORT-BEST-IDARTNR                         
016700                  INPUT PROCEDURE B-SORT-INPUT                            
016800                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
016900                                                                          
017000     IF SORT-RETURN NOT = 0                                               
017100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
017200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
017300       DELIMITED BY SIZE INTO FELTEXT-STR                                 
017400       DISPLAY FELTEXT                                                    
017500       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
017600       CALL ABEND USING RKOD-ABEND                                        
017700     ELSE                                                                 
017800       IF INNEVARANDE-SW = 'J'                                            
017900         PERFORM D-LAS-INNEVARANDE                                        
018000       END-IF                                                             
018100       IF FOREGAENDE-SW = 'J'                                             
018200         PERFORM E-LAS-FOREGAENDE                                         
018300       END-IF                                                             
018400     END-IF                                                               
018500                                                                          
018600     PERFORM Z-FINIT                                                      
018700                                                                          
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 A-INIT SECTION.                                                          
019300                                                                          
019400     OPEN INPUT  W55504                                                   
019500                                                                          
019600     OPEN OUTPUT W55507                                                   
019700                                                                          
019900     MOVE +0 TO FORE-SPAR-IDARTNR                                         
020000                INNE-SPAR-IDARTNR                                         
020100                                                                          
020200     MOVE +1 TO IX                                                        
020300     PERFORM UNTIL IX > 200                                               
020400       MOVE ZERO TO TABELL-BEST-IDARTNR(IX)                               
020500       ADD +1 TO IX                                                       
020600     END-PERFORM                                                          
020700     MOVE +1 TO IX                                                        
020800     .                                                                    
020900     EJECT                                                                
021000 B-SORT-INPUT  SECTION.                                                   
021100                                                                          
021200     PERFORM S01-LAES-W55504                                              
021300                                                                          
021400     PERFORM UNTIL END-OF-W55504                                          
021500       MOVE PRM-AREA TO SORTWS-AREA                                       
021600       PERFORM S31-SORT-RELEASE                                           
021700       PERFORM S01-LAES-W55504                                            
021800     END-PERFORM                                                          
021900     .                                                                    
022000     EJECT                                                                
022100 C-SORT-OUTPUT SECTION.                                                   
022200     SKIP2                                                                
022300     PERFORM S32-SORT-RETURN                                              
022400                                                                          
022500     PERFORM UNTIL END-OF-SORTFIL                                         
022700       IF SORTWS-BEST-FLSORT  NOT = SPAR-FLSORT OR                        
022800          SORTWS-BEST-IDARTNR NOT = SPAR-IDARTNR                          
022900         MOVE SORTWS-AREA TO TABELL-AREA(IX)                              
023000         MOVE SORTWS-BEST-FLSORT  TO SPAR-FLSORT                          
023100         MOVE SORTWS-BEST-IDARTNR TO SPAR-IDARTNR                         
023200         IF SORTWS-BEST-FLSORT = '1'                                      
023300           MOVE JA TO INNEVARANDE-SW                                      
023400         END-IF                                                           
023500         IF SORTWS-BEST-FLSORT = '2'                                      
023600           MOVE JA TO FOREGAENDE-SW                                       
023700         END-IF                                                           
023800                                                                          
023900         ADD +1 TO IX                                                     
024000       END-IF                                                             
024100                                                                          
024200       PERFORM S32-SORT-RETURN                                            
024300     END-PERFORM                                                          
024400     .                                                                    
024500     EJECT                                                                
024600 D-LAS-INNEVARANDE SECTION.                                               
024700     MOVE SPACE TO SPAR-FLSORT                                            
024800     MOVE ZERO  TO SPAR-IDARTNR                                           
024900     MOVE +1 TO IX                                                        
025000                                                                          
025100     IF TABELL-BEST-IDARTNR(1) > 0                                        
025200       OPEN INPUT W55501                                                  
025300                                                                          
025400       PERFORM S02-LAES-W55501                                            
025500       PERFORM UNTIL END-OF-W55501                                        
025600         PERFORM UNTIL TABELL-BEST-IDARTNR(IX) < 1 OR                     
025700                (INNE-SPAR-IDARTNR = TABELL-BEST-IDARTNR(IX)              
025800                 AND TABELL-BEST-FLSORT (IX) = '1')                       
025900           ADD +1 TO IX                                                   
026000         END-PERFORM                                                      
026100         IF TABELL-BEST-FLSORT(IX)  = '1' AND                             
026200            TABELL-BEST-IDARTNR(IX) = INNE-SPAR-IDARTNR                   
026300           MOVE INNE-AREA TO UT1-AREA                                     
026310***  LEVNR FIX                                                            
026400           PERFORM S09-KONV-LEVNR                                         
026401***  LEVNR END FIX                                                        
026410           PERFORM S11-SKRIV-W55507                                       
026500         END-IF                                                           
026600       PERFORM S02-LAES-W55501                                            
026700       MOVE +1 TO IX                                                      
026800       END-PERFORM                                                        
026900                                                                          
027000       CLOSE W55501                                                       
027100                                                                          
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 E-LAS-FOREGAENDE  SECTION.                                               
027600     MOVE SPACE TO SPAR-FLSORT                                            
027700     MOVE ZERO  TO SPAR-IDARTNR                                           
027800     MOVE +1 TO IX                                                        
027900                                                                          
028000     IF TABELL-BEST-IDARTNR(1) > 0                                        
028100       OPEN INPUT W55508                                                  
028200                                                                          
028300       PERFORM S03-LAES-W55508                                            
028400       PERFORM UNTIL END-OF-W55508                                        
028500         PERFORM UNTIL TABELL-BEST-IDARTNR(IX) < 1 OR                     
028600                (FORE-SPAR-IDARTNR = TABELL-BEST-IDARTNR(IX)              
028700                 AND TABELL-BEST-FLSORT (IX) = '2')                       
028800           ADD +1 TO IX                                                   
028900         END-PERFORM                                                      
029000         IF TABELL-BEST-FLSORT(IX)  = '2' AND                             
029100            TABELL-BEST-IDARTNR(IX) = FORE-SPAR-IDARTNR                   
029200           MOVE FORE-AREA TO UT1-AREA                                     
029201***  LEVNR FIX                                                            
029210           PERFORM S09-KONV-LEVNR                                         
029211***  LEVNR END FIX                                                        
029300           PERFORM S11-SKRIV-W55507                                       
029400         END-IF                                                           
029500         PERFORM S03-LAES-W55508                                          
029600         MOVE +1 TO IX                                                    
029700       END-PERFORM                                                        
029800                                                                          
029900       CLOSE W55508                                                       
030000                                                                          
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 Z-FINIT SECTION.                                                         
030500                                                                          
030600     CLOSE W55504                                                         
030700           W55507                                                         
030800                                                                          
030900     DISPLAY 'ANTAL SOL-TRANSAR FÖREGÅENDE ÅR ' FOREG-ANTAL               
031000     DISPLAY 'ANTAL SOL-TRANSAR HITTILLS I ÅR ' INNEV-ANTAL               
031100     DISPLAY 'ANTAL ARTIKAR FÖR URVAL         ' URVAL-ANTAL               
031200     DISPLAY 'ANTAL TRANSAR FÖR LADDNING      ' LADD-ANTAL                
031300     .                                                                    
031400     EJECT                                                                
031500 S01-LAES-W55504  SECTION.                                                
031600                                                                          
031700     READ W55504 INTO PRM-AREA                                            
031800     AT END                                                               
031900        MOVE HIGH-VALUE TO PRM-AREA                                       
032000        SET END-OF-W55504 TO TRUE                                         
032100     NOT AT END                                                           
032200        COMPUTE URVAL-ANTAL = URVAL-ANTAL + 1                             
032300     END-READ                                                             
032400     .                                                                    
032500     EJECT                                                                
032600 S02-LAES-W55501  SECTION.                                                
032700                                                                          
032800     READ W55501 INTO INNE-AREA                                           
032900     AT END                                                               
033000        MOVE 999999999  TO INNE-SPAR-IDARTNR                              
033100        SET END-OF-W55501 TO TRUE                                         
033200     NOT AT END                                                           
033300        COMPUTE INNEV-ANTAL = INNEV-ANTAL + 1                             
033400     END-READ                                                             
033500     .                                                                    
033600     EJECT                                                                
033700 S03-LAES-W55508  SECTION.                                                
033800                                                                          
033900     READ W55508 INTO FORE-AREA                                           
034000     AT END                                                               
034100        MOVE 999999999  TO FORE-SPAR-IDARTNR                              
034200        SET END-OF-W55508 TO TRUE                                         
034300     NOT AT END                                                           
034400        COMPUTE FOREG-ANTAL = FOREG-ANTAL + 1                             
034500     END-READ                                                             
034600     .                                                                    
034700     EJECT                                                                
034701***  LEVNR FIX                                                            
034710 S09-KONV-LEVNR SECTION.                                                  
034711                                                                          
034712     IF UT1-URV-IDPGM = 'W6011C00' OR 'W6011D00' OR 'W6011800' OR         
034713                        'W6019200' OR 'W6019300' OR 'W6110600'            
034715       MOVE UT1-URV-IDLEVNR(1:3)      TO WS-IDLEVNR-X                     
034716       IF WS-IDLEVNR-N NUMERIC                                            
034718         MOVE WS-IDLEVNR-N            TO IDLEVNR-DISPLAY                  
034720                                                                          
034721         MOVE ZERO TO TALLY                                               
034722         INSPECT IDLEVNR-DISPLAY TALLYING TALLY FOR LEADING               
034723                                                       ZEROES             
034724         IF TALLY = 5                                                     
034725           MOVE SPACE                 TO UT1-URV-IDLEVNR                  
034726         ELSE                                                             
034728           MOVE IDLEVNR-DISPLAY(TALLY + 1:)                               
034729                                      TO UT1-URV-IDLEVNR                  
034730         END-IF                                                           
034760       END-IF                                                             
034761     END-IF                                                               
034762     .                                                                    
034763     EJECT                                                                
034770***  LEVNR END FIX                                                        
034800 S11-SKRIV-W55507 SECTION.                                                
034900                                                                          
035000     WRITE UT-POST FROM UT1-AREA                                          
035100                                                                          
035200     COMPUTE LADD-ANTAL = LADD-ANTAL + 1                                  
035300     .                                                                    
035400     SKIP3                                                                
035500 S31-SORT-RELEASE  SECTION.                                               
035600                                                                          
035700     RELEASE SORT-POST FROM SORTWS-AREA                                   
035800     .                                                                    
035900     SKIP3                                                                
036000 S32-SORT-RETURN  SECTION.                                                
036100                                                                          
036200     RETURN SORTFIL INTO SORTWS-AREA                                      
036300     AT END                                                               
036400         SET END-OF-SORTFIL TO TRUE                                       
036500     .                                                                    
