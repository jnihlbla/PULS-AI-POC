000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2212300.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   95/10/17.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LISTA MED ARTIKLAR MED LEVERANSPLANFÖRSLAG                       
001000*        SOM BLIVIT AUTOMATISKT GODKÄNDA                                  
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- ARTIKLAR                                                   
002600     SELECT W22154                     ASSIGN TO W22123D1.                
002700     SKIP2                                                                
002800*          --- BENÄMNINGSREG                                              
002900     SELECT W01174                     ASSIGN TO W22123D2.                
003000     SKIP2                                                                
003100*          --- LISTA                                                      
003200     SELECT W22123-001                 ASSIGN TO W22123D3.                
003300     SKIP2                                                                
003400*          --- SORTERINGSFIL                                              
003500     SELECT SORTFIL                    ASSIGN TO W22123DS.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W22154                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W22154      -L.                                                
004600     SKIP3                                                                
004700 FD  W01174                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY W01174      -L.                                                
005200     SKIP3                                                                
005300 FD  W22123-001                                                           
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600     SKIP2                                                                
005700 01  W22123-001-RAD              PIC X(121).                              
005800     EJECT                                                                
005900 SD  SORTFIL.                                                             
006000                                                                          
006100*01  POST -COPY W22154      -PRE SORT-                                    
006200     03  SORT-BEART              PIC X(25).                               
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006501                                                                          
006510*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(8)    VALUE 'W2212300'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  W22154-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W22154                       VALUE 'J'.                   
007200                                                                          
007300 77  W01174-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W01174                       VALUE 'J'.                   
007500                                                                          
007600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007700     88  END-OF-SORTFIL                      VALUE 'J'.                   
007800     SKIP2                                                                
007900 01  OLD-IDANSK                  PIC 9(3)    VALUE ZERO.                  
008000     SKIP3                                                                
008100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008200 01  FILLER REDEFINES DAGENS-DATUM.                                       
008300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008600     EJECT                                                                
008700 01  DYNAMISKA-SUBPROGRAM.                                                
008800*                                                                         
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     SKIP2                                                                
009200*    --- PARAMETRAR TILL ABEND                                            
009300                                                                          
009400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009600     SKIP2                                                                
009700 01  FELTEXT.                                                             
009800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL POSTSUM                                          
010200*                                                                         
010300*01  -COPY W0005   -PRE  POSTSUM-                                         
010400     EJECT                                                                
010500 01  IN-AREA-START               PIC X(24)   VALUE                        
010600                                 'IN-AREA-START  '.                       
010700     SKIP2                                                                
010800                                                                          
010900*01  AREA -COPY W22154     -PRE IN-                                       
011000     EJECT                                                                
011100 01  REG-AREA-START              PIC X(24)   VALUE                        
011200                                 'REG-AREA-START  '.                      
011300     SKIP2                                                                
011400                                                                          
011500*01  AREA -COPY W01174     -PRE REG-                                      
011600     EJECT                                                                
011700 01  W001-AREA-START             PIC X(24)   VALUE                        
011800                                 'W001-AREA-START  '.                     
011900     SKIP2                                                                
012000 01  W001-HJALPAREOR.                                                     
012100*                                                                         
012200     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
012300     03  W001-ANTAL-RADER                                                 
012400                                 PIC 9(3)    VALUE 999.                   
012500     03  W001-MAX-RADER-PER-SIDA                                          
012600                                 PIC 9(3)    VALUE 42.                    
012700     03  W001-MAX-POSITIONER-PER-RAD                                      
012800                                 PIC 9(3)    VALUE 120.                   
012900     03  W001-LISTNR             PIC X(11)   VALUE 'W22123-001'.          
013000     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
013100     EJECT                                                                
013200 01  W001-RAD.                                                            
013300*                                                                         
013400     03  FILLER                  PIC X(121)  VALUE SPACE.                 
013500     SKIP2                                                                
013600 01  W001-RUBRIK1.                                                        
013700*                                                                         
013800     03  FILLER                  PIC X(3).                                
013900     03  FILLER                  PIC X(21)                                
014000                                VALUE 'VOLVO CAR PARTS      '.            
014100     03  FILLER                  PIC X(12)                                
014200                                 VALUE 'W22123-001'.                      
014300     03  FILLER                  PIC X(47)                                
014400         VALUE 'AUT APPROVED DELIVERY-SCHEDULES    PROCURER '.            
014500     03  W001-IDANSK             PIC 9(3)   VALUE ZERO.                   
014600     03  FILLER                  PIC X(5).                                
014700     03  W001-DATUM              PIC XXBXXBXX.                            
014800     03  FILLER                  PIC X(7)                                 
014900                                 VALUE '    SID'.                         
015000     03  W001-SID                PIC Z(4)9.                               
015100     SKIP2                                                                
015200 01  W001-RUBRIK2.                                                        
015300     03  FILLER                  PIC X(118)                               
015400         VALUE '   SUPPLIER     VOLVO NO     DESCRIPTION'.                
015500     SKIP2                                                                
015600 01  W001-DETALJ1.                                                        
015700     03  FILLER                  PIC X(6)  VALUE SPACE.                   
015800     03  W001-IDLEVNR            PIC X(5)  VALUE SPACE.                   
015900     03  FILLER                  PIC X(4)  VALUE SPACE.                   
016000     03  W001-IDARTNR            PIC Z(9)  VALUE ZERO.                    
016100     03  FILLER                  PIC X(5)  VALUE SPACE.                   
016200     03  W001-BEART              PIC X(25) VALUE SPACE.                   
016300     03  FILLER                  PIC X(30) VALUE SPACE.                   
016400     EJECT                                                                
016500 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
016600                                  'SORTWS-AREA-START  '.                  
016700     SKIP2                                                                
016800                                                                          
016900*01  AREA -COPY W22154      -PRE SORTWS-                                  
017000     03  SORTWS-BEART            PIC X(25).                               
017100                                                                          
017200 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
017300     EJECT                                                                
017400 PROCEDURE DIVISION.                                                      
017500     SKIP2                                                                
017600                                                                          
017700     PERFORM A-INIT                                                       
017800                                                                          
017900     SORT SORTFIL ASCENDING KEY SORT-IDANSK                               
018000                                SORT-IDLEVNR                              
018100                                SORT-IDARTNR                              
018200                  INPUT PROCEDURE B-SORT-INPUT                            
018300                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
018400                                                                          
018500     IF SORT-RETURN NOT = 0                                               
018600       MOVE SORT-RETURN TO SORT-RETURN-X                                  
018700       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
018800       DELIMITED BY SIZE INTO FELTEXT-STR                                 
018900       DISPLAY FELTEXT                                                    
019000       PERFORM S99-ABEND                                                  
019100     ELSE                                                                 
019200       PERFORM Z-FINIT                                                    
019300                                                                          
019400       MOVE ZERO TO RETURN-CODE                                           
019500       GOBACK                                                             
019600     END-IF                                                               
019700                                                                          
019800     .                                                                    
019900     EJECT                                                                
020000 A-INIT SECTION.                                                          
020100                                                                          
020200     OPEN INPUT  W22154                                                   
020300                 W01174                                                   
020400          OUTPUT W22123-001                                               
020500     SKIP2                                                                
020600     ACCEPT DAGENS-DATUM  FROM DATE                                       
020610     MOVE DAGENS-DATUM    TO   W001-DATUM                                 
020700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020800     .                                                                    
020900     EJECT                                                                
021000 B-SORT-INPUT  SECTION.                                                   
021100                                                                          
021200     PERFORM S01-LAES-W22154                                              
021300     PERFORM S02-LAES-W01174                                              
021400                                                                          
021500     PERFORM UNTIL END-OF-W22154                                          
021600       IF IN-IDARTNR > REG-IDARTNR                                        
021700          PERFORM UNTIL IN-IDARTNR NOT > REG-IDARTNR                      
021800             PERFORM S02-LAES-W01174                                      
021900          END-PERFORM                                                     
022000       END-IF                                                             
022100       MOVE IN-AREA TO SORTWS-AREA                                        
022200       IF IN-IDARTNR = REG-IDARTNR                                        
022300          MOVE REG-BEART     (8) TO SORTWS-BEART                          
022400       ELSE                                                               
022500          MOVE SPACE             TO SORTWS-BEART                          
022600       END-IF                                                             
022700       PERFORM S31-SORT-RELEASE                                           
022800                                                                          
022900       PERFORM S01-LAES-W22154                                            
023000     END-PERFORM                                                          
023100     .                                                                    
023200     EJECT                                                                
023300 C-SORT-OUTPUT SECTION.                                                   
023400     SKIP2                                                                
023500     PERFORM S32-SORT-RETURN                                              
023600     PERFORM UNTIL END-OF-SORTFIL                                         
023700       PERFORM CA-SKRIV-LISTA                                             
023800       PERFORM S32-SORT-RETURN                                            
023900     END-PERFORM                                                          
024000     .                                                                    
024100     SKIP3                                                                
024200 CA-SKRIV-LISTA  SECTION.                                                 
024300                                                                          
024400     MOVE SORTWS-IDLEVNR   TO W001-IDLEVNR                                
024500     MOVE SORTWS-IDARTNR   TO W001-IDARTNR                                
024600     MOVE SORTWS-BEART     TO W001-BEART                                  
024700                                                                          
024800     PERFORM S21-SKRIV-W22123-001                                         
024900     .                                                                    
025000     EJECT                                                                
025100 Z-FINIT SECTION.                                                         
025200                                                                          
025300     CLOSE W22154                                                         
025400           W01174                                                         
025500           W22123-001                                                     
025600     SKIP2                                                                
025700     MOVE 'S' TO POSTSUM-OPKOD                                            
025800     CALL POSTSUM USING POSTSUM-PARM                                      
025900     .                                                                    
026000     EJECT                                                                
026100 S01-LAES-W22154  SECTION.                                                
026200     READ W22154 INTO IN-AREA                                             
026300     AT END                                                               
026400        SET END-OF-W22154 TO TRUE                                         
026500                                                                          
026600     NOT AT END                                                           
026700        MOVE 'W22154'   TO POSTSUM-FDNAMN                                 
026800        MOVE 'W22123D1' TO POSTSUM-DDNAMN2                                
026900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
027000        CALL POSTSUM USING POSTSUM-PARM                                   
027100     END-READ                                                             
027200     .                                                                    
027300     SKIP3                                                                
027400 S02-LAES-W01174  SECTION.                                                
027500     READ W01174 INTO REG-AREA                                            
027600     AT END                                                               
027700        MOVE 999999999  TO REG-IDARTNR                                    
027800        SET END-OF-W01174 TO TRUE                                         
027900                                                                          
028000     NOT AT END                                                           
028100        MOVE 'W01174'   TO POSTSUM-FDNAMN                                 
028200        MOVE 'W22123D2' TO POSTSUM-DDNAMN2                                
028300        MOVE 'REG'      TO POSTSUM-TRANSTYP                               
028400        CALL POSTSUM USING POSTSUM-PARM                                   
028500     END-READ                                                             
028600     .                                                                    
028700     EJECT                                                                
028800 S21-SKRIV-W22123-001  SECTION.                                           
028900                                                                          
029000     MOVE 1 TO W001-SKIP                                                  
029100     IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA OR                     
029200       SORTWS-IDANSK NOT = OLD-IDANSK                                     
029300       MOVE SORTWS-IDANSK TO W001-IDANSK OLD-IDANSK                       
029400       PERFORM S21A-SKRIV-RUBRIKER                                        
029500     END-IF                                                               
029600     SKIP2                                                                
029610     MOVE W001-DETALJ1    TO   W001-RAD                                   
029700     WRITE W22123-001-RAD FROM W001-RAD AFTER W001-SKIP                   
029800     SKIP2                                                                
029900     MOVE SPACE TO W001-RAD                                               
030000     ADD  +1 TO W001-ANTAL-RADER                                          
030100     .                                                                    
030200     SKIP3                                                                
030300 S21A-SKRIV-RUBRIKER SECTION.                                             
030400                                                                          
030500     ADD +1 TO W001-SIDRAKNARE                                            
030600     MOVE W001-SIDRAKNARE TO W001-SID                                     
030700     WRITE W22123-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
030800     WRITE W22123-001-RAD FROM W001-RUBRIK2 AFTER 2                       
030900     MOVE +7 TO W001-ANTAL-RADER                                          
031000     SKIP2                                                                
031100     MOVE 3 TO W001-SKIP                                                  
031200     .                                                                    
031300     EJECT                                                                
031400 S31-SORT-RELEASE  SECTION.                                               
031500                                                                          
031600     RELEASE SORT-POST FROM SORTWS-AREA                                   
031700     .                                                                    
031800     SKIP3                                                                
031900 S32-SORT-RETURN  SECTION.                                                
032000                                                                          
032100     RETURN SORTFIL INTO SORTWS-AREA                                      
032200     AT END                                                               
032300         SET END-OF-SORTFIL TO TRUE                                       
032400     .                                                                    
032500     SKIP3                                                                
032600 S99-ABEND SECTION.                                                       
032700                                                                          
032800     SKIP2                                                                
032900     MOVE 'S' TO POSTSUM-OPKOD                                            
033000     CALL POSTSUM USING POSTSUM-PARM                                      
033100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
033200     .                                                                    
