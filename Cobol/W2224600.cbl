000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2224600.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   FEB 2000.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SKRIVER UT EN LISTA VIA MEMO                          
001100*        PROGNOS HAR BLIVIT NOLL                                          
001200*                                                                         
001300*        KOPIERAT FRÅN W2366200                                           
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDP3                                       
001600*                                                                         
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- LARM HÖG ORDERINGÅNG                                       
003100     SELECT W22221B                    ASSIGN TO W22246D1.                
003200     SKIP2                                                                
003300*          --- LISTA UT PÅ MEMO                                           
003400     SELECT W22246                     ASSIGN TO W22246D2.                
003500     SKIP2                                                                
003600*          --- LISTA UT PÅ DISTRIBUTION AND PRINT                         
003700     SELECT W22246A                    ASSIGN TO W22246D3.                
003800     SKIP2                                                                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W22221B                                                              
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700     SKIP2                                                                
004800*01  -COPY W22221L     -L.                                                
004900     SKIP3                                                                
005000 FD  W22246                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400*01  POST -COPY W22246   -L  -PRE UT-                                     
005500     SKIP3                                                                
005600 FD  W22246A                                                              
005700     RECORDING       V                                                    
005800     BLOCK CONTAINS  0.                                                   
005900     SKIP2                                                                
006000 01  UT2-POST PIC X(120).                                                 
006100     SKIP3                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300     SKIP2                                                                
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(8)    VALUE 'W2224600'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900 77  OLD-IDANSK                  PIC 9(3)    VALUE 999.                   
007000 77  SPAR-IDANSK                 PIC 9(3)    VALUE ZERO.                  
007100 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
007200 77  OLD-IDLEVNR                 PIC X(5)    VALUE SPACE.                 
007300 77  SPAR-IDARTNR                PIC 9(9)    VALUE ZERO.                  
007400 77  NY-ARTIKEL                  PIC X       VALUE 'N'.                   
007500 77  NY-ANSK-LEV                 PIC X       VALUE 'N'.                   
007600 77  NAESTA-INLEV                PIC X       VALUE 'N'.                   
007700 77  PUNKT                       PIC X       VALUE '.'.                   
007800 01  IX                          PIC S9(3)   COMP-3 VALUE ZERO.           
007900                                                                          
008000 77  W22221B-EOF-SW              PIC X       VALUE 'N'.                   
008100     88  END-OF-W22221B                      VALUE 'J'.                   
008200     EJECT                                                                
008300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 01  FILLER REDEFINES DAGENS-DATUM.                                       
008500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008800     EJECT                                                                
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000*                                                                         
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     SKIP2                                                                
009600*    --- PARAMETRAR TILL ABEND                                            
009700                                                                          
009800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010000     SKIP2                                                                
010100 01  FELTEXT.                                                             
010200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900 01  IN-AREA-START               PIC X(24)   VALUE                        
011000                                 'IN-AREA-START  '.                       
011100*01  AREA -COPY W22221L    -PRE IN-                                       
011200                                                                          
011300     EJECT                                                                
011400                                                                          
011500 01  UT-AREA-START               PIC X(24)   VALUE                        
011600                                 'UT-AREA-START  '.                       
011700     SKIP2                                                                
011800 01  UT-AREA.                                                             
011900     03  FILLER                  PIC X(119).                              
012000*01  FILLER -COPY W22246     -PRE UT-     -RED  UT-AREA                   
012100     EJECT                                                                
012200*                                                                         
012300 01  UT2-AREA-START               PIC X(24)   VALUE                       
012400                                 'UT2-AREA-START  '.                      
012500     SKIP2                                                                
012600 01  UT2-AREA.                                                            
012700     03  FILLER                  PIC X(120).                              
012800     EJECT                                                                
012900*                                                                         
013000 01  W001-AREA-START             PIC X(24)   VALUE                        
013100                                 'W001-AREA-START  '.                     
013200 01  W001-HJALPAREOR.                                                     
013300*                                                                         
013400     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
013500     03  W001-ANTAL-RADER                                                 
013600                                 PIC 9(3)    VALUE 999.                   
013700     03  W001-MAX-RADER-PER-SIDA                                          
013800                                 PIC 9(3)    VALUE 63.                    
013900     03  W001-MAX-POSITIONER-PER-RAD                                      
014000                                 PIC 9(3)    VALUE 165.                   
014100     03  W001-LISTNR             PIC X(11)   VALUE 'W22246-001'.          
014200     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
014300     03  WDAP-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
014400     EJECT                                                                
014500 01  W001-RAD.                                                            
014600*                                                                         
014700     03  FILLER                  PIC X(165)  VALUE SPACE.                 
014800     EJECT                                                                
014900 01  W001-RUBRIK1.                                                        
015000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
015100     03  W001-RUB1               PIC X(1)    VALUE '1'.                   
015200     03  FILLER                  PIC X(2)    VALUE SPACE.                 
015300     03  FILLER                  PIC X(23)                                
015400                                 VALUE 'VOLVO CUSTOMER SERVICE'.          
015500     03  FILLER                  PIC X(14)                                
015600                                 VALUE 'W22246-001'.                      
015700     03  FILLER                  PIC X(16)                                
015800                               VALUE 'LARM PROGNOS'.                      
015900     03  FILLER                  PIC X(03)   VALUE SPACE.                 
016000     03  FILLER                  PIC X(6)                                 
016100                                 VALUE 'DATUM '.                          
016200     03  W001-DATUM              PIC XXBXXBXX.                            
016300     03  FILLER                  PIC X(2)    VALUE SPACE.                 
016400     03  FILLER                  PIC X(5)                                 
016500                                 VALUE 'SIDA '.                           
016600     03  W001-SID                PIC Z9.                                  
016700     03  FILLER                  PIC X(60)   VALUE SPACE.                 
016800                                                                          
016900 01  W001-RUBRIK2.                                                        
017000     03  W001-RUB2               PIC X(1)    VALUE ' '.                   
017100     03  FILLER                  PIC X(2)  VALUE SPACE.                   
017200     03  FILLER                  PIC X(12) VALUE 'PROGNOSEN HA'.          
017300     03  FILLER                  PIC X(12) VALUE 'R BLIVIT NOL'.          
017400     03  FILLER                  PIC X(12) VALUE 'L           '.          
017500                                                                          
017600 01  W001-RUBRIK3.                                                        
017700     03  W001-RUB3               PIC X(1)    VALUE ' '.                   
017800     03  FILLER                  PIC X(2)  VALUE SPACE.                   
017900     03  FILLER                  PIC X(11) VALUE 'ANSKAFFARE '.           
018000     03  W001-IDANSK             PIC Z(2)9 VALUE ZERO.                    
018100                                                                          
018200 01  W001-RUBRIK4.                                                        
018300     03  W001-RUB4               PIC X(1)    VALUE ' '.                   
018400     03  FILLER                  PIC X(3)  VALUE SPACE.                   
018500     03  FILLER                  PIC X(09) VALUE ' ARTNR   '.             
018600     03  FILLER                  PIC X(3)  VALUE SPACE.                   
018700     03  FILLER                  PIC X(09) VALUE 'PB-SEP NY'.             
018800     03  FILLER                  PIC X(4)  VALUE SPACE.                   
018900     03  FILLER                  PIC X(13) VALUE 'PB-SEP GAMMAL'.         
019000                                                                          
019100     EJECT                                                                
019200 01  W001-DETALJ1.                                                        
019300     03  W001-RAD1               PIC X(1)    VALUE ' '.                   
019400     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019500     03  W001-IDARTNR            PIC Z(9)  VALUE ZERO.                    
019600     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019700     03  W001-KVPB-SEP-NEW       PIC Z(5)9V9 VALUE ZERO.                  
019800     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019900     03  W001-KVPB-SEP-OLD       PIC Z(5)9V9 VALUE ZERO.                  
020000                                                                          
020100     EJECT                                                                
020200 01  MEM-001.                                                             
020300     03  FILLER                  PIC X(80) VALUE ')SEND '.                
020400 01  MEM-002.                                                             
020500     03  FILLER                  PIC X(80)                                
020600                           VALUE 'TITLE HÖG ORD.INGÅNG'.                  
020700 01  MEM-003.                                                             
020800     03  FILLER                  PIC X(80) VALUE 'OPTION FORCE'.          
020900 01  MEM-004.                                                             
021000     03  FILLER                  PIC X(05) VALUE 'DEST '.                 
021100     03  MEM-IDMAIL              PIC X(60) VALUE SPACE.                   
021200 01  MEM-005.                                                             
021300     03  FILLER                  PIC X(05) VALUE 'MEMO '.                 
021400 01  MEM-006.                                                             
021500     03  FILLER                  PIC X(05) VALUE ')END '.                 
021600 01  MEM-007.                                                             
021700     03  FILLER                  PIC X(20) VALUE 'LINESIZE 120'.          
021800     EJECT                                                                
021900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022000*                                                                         
022100                                                                          
022200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022300     SKIP3                                                                
022400 01  NYCKLAR-TILL-DLI.                                                    
022500     03  W-KDARBTYP-X.                                                    
022600         05  W-KDARBTYP          PIC X(08)    VALUE 'ANSK'.               
022700     03  W-IDPERSON-X.                                                    
022800         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
022900     SKIP2                                                                
023000*    --- STATUS-KOD FRÅN IMS                                              
023100 01  STATUS-WS                   PIC XX.                                  
023200     88  SEGMENT-FINNS                       VALUE '  '.                  
023300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023500     SKIP2                                                                
023600 01  GODK-STATUSKODER.                                                    
023700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023800     SKIP3                                                                
023900 01  SSA1                        PIC X(64).                               
024000 01  SSA2                        PIC X(64).                               
024100     EJECT                                                                
024200*    --- IMS FUNKTIONSKODER                                               
024300*01  -COPY W0003                                                          
024400     EJECT                                                                
024500*    ---  DLI INPUT-OUTPUT AREA                                           
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
024700 01  DLI-IO-WDP311.                                                       
024800*    03  -COPY WDP311                                                     
024900     EJECT                                                                
025000 LINKAGE SECTION.                                                         
025100                                                                          
025200                                                                          
025300*01  -COPY W0008  -PRE WDP3-                                              
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600 PROCEDURE DIVISION  USING WDP3-PCB.                                      
025700 MAIN SECTION.                                                            
025800     ENTRY 'DLITCBL' USING WDP3-PCB.                                      
025900                                                                          
026000                                                                          
026100     PERFORM A-INIT                                                       
026200                                                                          
026300     PERFORM S01-LAES-W22221B                                             
026400     PERFORM UNTIL END-OF-W22221B                                         
026500                                                                          
026600       MOVE IN-IDANSK        TO SPAR-IDANSK                               
026700                                W001-IDANSK                               
026800       PERFORM S26-HAMTA-MAIL-ID                                          
026900       PERFORM S20-SKRIV-RUBRIKER                                         
027000                                                                          
027100* ---- TILLS NY ANSKAFFARE                                                
027200       PERFORM UNTIL  END-OF-W22221B                                      
027300       OR             IN-IDANSK  NOT = SPAR-IDANSK                        
027400                                                                          
027500         IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                    
027600           PERFORM S20-SKRIV-RUBRIKER                                     
027700         END-IF                                                           
027800                                                                          
027900         MOVE IN-IDARTNR     TO W001-IDARTNR                              
028000         MOVE IN-KVPB-SEP    TO W001-KVPB-SEP-OLD                         
028100         MOVE IN-NY-KVPB-SEP TO W001-KVPB-SEP-NEW                         
028200         MOVE W001-DETALJ1   TO W001-RAD                                  
028300         MOVE W001-RAD (2:99)                                             
028400                             TO UT-AREA                                   
028500                                UT2-AREA                                  
028600         PERFORM S12-SKRIV-UTPOST-W22246                                  
028700                                                                          
028800         MOVE SPACE          TO W001-RAD                                  
028900         ADD +1              TO W001-ANTAL-RADER                          
029000         MOVE +1             TO W001-SKIP                                 
029100                                                                          
029200         PERFORM S01-LAES-W22221B                                         
029300                                                                          
029400       END-PERFORM                                                        
029500       IF MEM-IDMAIL NOT = 'DAP'                                          
029600         MOVE MEM-006      TO UT-AREA                                     
029700       END-IF                                                             
029800       PERFORM S12-SKRIV-UTPOST-W22246                                    
029900                                                                          
030000     END-PERFORM                                                          
030100                                                                          
030200     PERFORM Z-FINIT                                                      
030300                                                                          
030400     MOVE ZERO TO RETURN-CODE                                             
030500     GOBACK                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 A-INIT SECTION.                                                          
030900                                                                          
031000     OPEN INPUT  W22221B                                                  
031100                                                                          
031200     OPEN OUTPUT W22246                                                   
031300                 W22246A                                                  
031400                                                                          
031500     ACCEPT DAGENS-DATUM FROM DATE                                        
031600     MOVE DAGENS-DATUM TO W001-DATUM                                      
031700     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
031800     .                                                                    
031900     EJECT                                                                
032000     EJECT                                                                
032100 Z-FINIT SECTION.                                                         
032200                                                                          
032300     CLOSE W22221B                                                        
032400           W22246                                                         
032500           W22246A                                                        
032600                                                                          
032700     MOVE 'S' TO POSTSUM-OPKOD                                            
032800     CALL POSTSUM USING POSTSUM-PARM                                      
032900     .                                                                    
033000     EJECT                                                                
033100 S01-LAES-W22221B SECTION.                                                
033200                                                                          
033300     READ W22221B INTO IN-AREA                                            
033400     AT END                                                               
033500        SET END-OF-W22221B TO TRUE                                        
033600                                                                          
033700     NOT AT END                                                           
033800        MOVE 'W22221B'   TO POSTSUM-FDNAMN                                
033900        MOVE 'W22221BD1' TO POSTSUM-DDNAMN2                               
034000        MOVE 'IN'        TO POSTSUM-TRANSTYP                              
034100        CALL POSTSUM USING POSTSUM-PARM                                   
034200     END-READ                                                             
034300     .                                                                    
034400     EJECT                                                                
034500 S12-SKRIV-UTPOST-W22246  SECTION.                                        
034600     IF MEM-IDMAIL = 'DAP'                                                
034700       WRITE UT2-POST FROM UT2-AREA                                       
034800       MOVE 'W22246A'  TO POSTSUM-FDNAMN                                  
034900       MOVE 'W22246D3' TO POSTSUM-DDNAMN2                                 
035000       MOVE 'UT2'      TO POSTSUM-TRANSTYP                                
035100     ELSE                                                                 
035200       WRITE UT-POST FROM UT-AREA                                         
035300       MOVE 'W22246'   TO POSTSUM-FDNAMN                                  
035400       MOVE 'W22246D2' TO POSTSUM-DDNAMN2                                 
035500       MOVE 'UT'       TO POSTSUM-TRANSTYP                                
035600     END-IF                                                               
035700     CALL POSTSUM USING POSTSUM-PARM                                      
035800     .                                                                    
035900     EJECT                                                                
036000 S20-SKRIV-RUBRIKER SECTION.                                              
036100                                                                          
036200     PERFORM S22-SKRIV-RUBRIK1                                            
036300     PERFORM S23-SKRIV-RUBRIK2                                            
036400     PERFORM S24-SKRIV-RUBRIK3                                            
036500     PERFORM S25-SKRIV-RUBRIK4                                            
036600     .                                                                    
036700     EJECT                                                                
036800 S22-SKRIV-RUBRIK1 SECTION.                                               
036900     IF MEM-IDMAIL = 'DAP'                                                
037000       ADD +1 TO WDAP-SIDRAKNARE                                          
037100       MOVE WDAP-SIDRAKNARE   TO W001-SID                                 
037200     ELSE                                                                 
037300       ADD +1 TO W001-SIDRAKNARE                                          
037400       MOVE W001-SIDRAKNARE   TO W001-SID                                 
037500     END-IF                                                               
037600                                                                          
037700     PERFORM S22A-SKAPA-MAIL-DEST                                         
037800                                                                          
037900     MOVE W001-RUBRIK1 (2:99) TO UT-AREA                                  
038000                                 UT2-AREA                                 
038100     PERFORM S12-SKRIV-UTPOST-W22246                                      
038200     MOVE SPACE               TO UT-AREA                                  
038300                                 UT2-AREA                                 
038400     PERFORM S12-SKRIV-UTPOST-W22246                                      
038500                                                                          
038600     MOVE +2 TO W001-ANTAL-RADER                                          
038700                                                                          
038800     MOVE +2 TO W001-SKIP                                                 
038900     .                                                                    
039000     EJECT                                                                
039100 S22A-SKAPA-MAIL-DEST   SECTION.                                          
039200     IF MEM-IDMAIL NOT = 'DAP'                                            
039300       MOVE MEM-001     TO UT-AREA                                        
039400       PERFORM S12-SKRIV-UTPOST-W22246                                    
039500       MOVE MEM-002     TO UT-AREA                                        
039600       PERFORM S12-SKRIV-UTPOST-W22246                                    
039700       MOVE MEM-003     TO UT-AREA                                        
039800       PERFORM S12-SKRIV-UTPOST-W22246                                    
039900       MOVE MEM-007     TO UT-AREA                                        
040000       PERFORM S12-SKRIV-UTPOST-W22246                                    
040100       MOVE MEM-004     TO UT-AREA                                        
040200       PERFORM S12-SKRIV-UTPOST-W22246                                    
040300       MOVE MEM-005     TO UT-AREA                                        
040400       PERFORM S12-SKRIV-UTPOST-W22246                                    
040500     END-IF                                                               
040600     .                                                                    
040700     EJECT                                                                
040800 S23-SKRIV-RUBRIK2 SECTION.                                               
040900                                                                          
041000     MOVE W001-RUBRIK2         TO UT-AREA                                 
041100                                  UT2-AREA                                
041200     PERFORM S12-SKRIV-UTPOST-W22246                                      
041300                                                                          
041400     ADD +1 TO W001-ANTAL-RADER                                           
041500                                                                          
041600     MOVE +1 TO W001-SKIP                                                 
041700     .                                                                    
041800     EJECT                                                                
041900 S24-SKRIV-RUBRIK3 SECTION.                                               
042000                                                                          
042100     MOVE W001-RUBRIK3         TO UT-AREA                                 
042200                                  UT2-AREA                                
042300     PERFORM S12-SKRIV-UTPOST-W22246                                      
042400     MOVE SPACE               TO UT-AREA                                  
042500                                 UT2-AREA                                 
042600     PERFORM S12-SKRIV-UTPOST-W22246                                      
042700                                                                          
042800     ADD +2 TO W001-ANTAL-RADER                                           
042900                                                                          
043000     MOVE +2 TO W001-SKIP                                                 
043100     .                                                                    
043200     EJECT                                                                
043300 S25-SKRIV-RUBRIK4 SECTION.                                               
043400                                                                          
043500     MOVE W001-RUBRIK4         TO UT-AREA                                 
043600                                  UT2-AREA                                
043700     PERFORM S12-SKRIV-UTPOST-W22246                                      
043800     MOVE SPACE               TO UT-AREA                                  
043900                                 UT2-AREA                                 
044000     PERFORM S12-SKRIV-UTPOST-W22246                                      
044100                                                                          
044200     ADD +2 TO W001-ANTAL-RADER                                           
044300                                                                          
044400     MOVE +2 TO W001-SKIP                                                 
044500     .                                                                    
044600     EJECT                                                                
044700 S26-HAMTA-MAIL-ID SECTION.                                               
044800                                                                          
044900     MOVE SPAR-IDANSK           TO W-IDPERSON                             
045000     PERFORM IMS-GET-WDP3-ANSKNAMN                                        
045100     IF SEGMENT-FINNS                                                     
045200     AND PERS-IDMAIL NOT = SPACE                                          
045300        MOVE PERS-IDMAIL               TO MEM-IDMAIL                      
045400     ELSE                                                                 
045500        MOVE 'DAP'                      TO MEM-IDMAIL                     
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900* --- IMS SEKTIONER ---                                                   
046000                                                                          
046100 IMS-GET-WDP3-ANSKNAMN SECTION.                                           
046200                                                                          
046300     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
046400          DELIMITED BY SIZE INTO SSA1                                     
046500     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
046600          DELIMITED BY SIZE INTO SSA2                                     
046700     MOVE '  GE' TO GODK-STATUSKODER                                      
046800     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
046900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
047000     PERFORM IMS-STATUSKONTROLL                                           
047100     .                                                                    
047200     EJECT                                                                
047300 IMS-STATUSKONTROLL SECTION.                                              
047400                                                                          
047500     SET STATUS-IX TO 1                                                   
047600     SEARCH GODK-STATUS                                                   
047700       AT END                                                             
047800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047900           DELIMITED BY SIZE INTO FELTEXT                                 
048000         DISPLAY FELTEXT                                                  
048100         CALL FELLOG                                                      
048200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
048300         CONTINUE                                                         
048400     END-SEARCH                                                           
048500     .                                                                    
