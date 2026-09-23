000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2224100.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   FEB 2000.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SKRIVER UT EN LISTA VIA MEMOAPI                       
001100*        ONORMAL ORDERINGÅNG TILL CDC DE SENASTE 4 VECKORNA               
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
003000*          --- LARM ONORMAL ORDERINGÅNG                                   
003100     SELECT W22235                     ASSIGN TO W22241D1.                
003200     SKIP2                                                                
003300*          --- LISTA TILL MEMOAPI                                         
003400     SELECT W22241                     ASSIGN TO W22241D2.                
003500     SKIP2                                                                
003600*          --- LISTA TILL DAP                                             
003700     SELECT W22241A                    ASSIGN TO W22241D3.                
003800     SKIP2                                                                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W22235                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700     SKIP2                                                                
004800*01  -COPY W22235      -L.                                                
004900     SKIP3                                                                
005000 FD  W22241                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400*01  POST -COPY W22241   -L  -PRE UT-                                     
005500     SKIP3                                                                
005600                                                                          
005700 FD  W22241A                                                              
005800     LABEL RECORD STANDARD                                                
005900     RECORDING  V                                                         
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200 01  UT2-POST  PIC X(124).                                                
006300     SKIP3                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500     SKIP2                                                                
006600                                                                          
006700*    -- CHECKED BY WY2000                                                 
006800 77  IDPGM                       PIC X(8)    VALUE 'W2224100'.            
006900 77  JA                          PIC X       VALUE 'J'.                   
007000 77  NEJ                         PIC X       VALUE 'N'.                   
007100 77  OLD-IDANSK                  PIC 9(3)    VALUE 999.                   
007200 77  SPAR-IDANSK                 PIC 9(3)    VALUE ZERO.                  
007300 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
007400 77  OLD-IDLEVNR                 PIC X(5)    VALUE SPACE.                 
007500 77  SPAR-IDARTNR                PIC 9(9)    VALUE ZERO.                  
007600 77  NY-ARTIKEL                  PIC X       VALUE 'N'.                   
007700 77  NY-ANSK-LEV                 PIC X       VALUE 'N'.                   
007800 77  NAESTA-INLEV                PIC X       VALUE 'N'.                   
007900 77  PUNKT                       PIC X       VALUE '.'.                   
008000 01  IX                          PIC S9(3)   COMP-3 VALUE ZERO.           
008100                                                                          
008200 77  W22235-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W22235                       VALUE 'J'.                   
008400     EJECT                                                                
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000     EJECT                                                                
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     SKIP2                                                                
009800*    --- PARAMETRAR TILL ABEND                                            
009900                                                                          
010000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010200     SKIP2                                                                
010300 01  FELTEXT.                                                             
010400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL POSTSUM                                          
010800*                                                                         
010900*01  -COPY W0005   -PRE  POSTSUM-                                         
011000     EJECT                                                                
011100 01  IN-AREA-START               PIC X(24)   VALUE                        
011200                                 'IN-AREA-START  '.                       
011300*01  AREA -COPY W22235     -PRE IN-                                       
011400                                                                          
011500     EJECT                                                                
011600                                                                          
011700 01  UT-AREA-START               PIC X(24)   VALUE                        
011800                                 'UT-AREA-START  '.                       
011900     SKIP2                                                                
012000 01  UT-AREA.                                                             
012100     03  FILLER                  PIC X(119).                              
012200*01  FILLER -COPY W22241     -PRE UT-     -RED  UT-AREA                   
012300     EJECT                                                                
012400 01  UT2-AREA-START               PIC X(24)   VALUE                       
012500                                 'UT2-AREA-START  '.                      
012600     SKIP2                                                                
012700 01  UT2-AREA.                                                            
012800     03  FILLER                  PIC X(120).                              
012900     EJECT                                                                
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
014100     03  W001-LISTNR             PIC X(11)   VALUE 'W22241-001'.          
014200     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
014300     03  WDAP-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
014400     EJECT                                                                
014500 01  W001-RAD.                                                            
014600*                                                                         
014700     03  FILLER                  PIC X(165)  VALUE SPACE.                 
014800     EJECT                                                                
014900 01  W001-RUBRIK1.                                                        
015000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
015100     03  W001-NYRUB1             PIC X(1)    VALUE SPACE.                 
015200     03  FILLER                  PIC X(23)                                
015300                                 VALUE 'VOLVO CUSTOMER SERVICE'.          
015400     03  FILLER                  PIC X(14)                                
015500                                 VALUE 'W22241-001'.                      
015600     03  FILLER                  PIC X(16)                                
015700                               VALUE 'LARM ORDERINGÅNG'.                  
015800     03  FILLER                  PIC X(03)   VALUE SPACE.                 
015900     03  FILLER                  PIC X(6)                                 
016000                                 VALUE 'DATUM '.                          
016100     03  W001-DATUM              PIC XXBXXBXX.                            
016200     03  FILLER                  PIC X(2)    VALUE SPACE.                 
016300     03  FILLER                  PIC X(5)                                 
016400                                 VALUE 'SIDA '.                           
016500     03  W001-SID                PIC Z9.                                  
016600     03  FILLER                  PIC X(60)   VALUE SPACE.                 
016700                                                                          
016800 01  W001-RUBRIK2.                                                        
016900     03  W001-NYRUB2             PIC X(1)    VALUE SPACE.                 
017000     03  FILLER                  PIC X(12) VALUE 'ARTIKLAR MED'.          
017100     03  FILLER                  PIC X(12) VALUE ' ONORMAL ORD'.          
017200     03  FILLER                  PIC X(12) VALUE 'DERINGÅNG SI'.          
017300     03  FILLER                  PIC X(12) VALUE 'STA 4 VECKOR'.          
017400     03  FILLER                  PIC X(12) VALUE 'NA          '.          
017500                                                                          
017600 01  W001-RUBRIK3.                                                        
017700     03  W001-NYRUB3             PIC X(1)    VALUE SPACE.                 
017800     03  FILLER                  PIC X(11) VALUE 'ANSKAFFARE '.           
017900     03  W001-IDANSK             PIC Z(2)9 VALUE ZERO.                    
018000     03  FILLER                  PIC X(2)  VALUE SPACE.                   
018100     03  FILLER                  PIC X(06) VALUE 'VECKA '.                
018200     03  W001-VECKA              PIC 9(2)  VALUE ZERO.                    
018300     03  FILLER                  PIC X(1)  VALUE SPACE.                   
018400     03  W001-AR                 PIC 9(4)  VALUE ZERO.                    
018500                                                                          
018600 01  W001-RUBRIK4.                                                        
018700     03  W001-NYRUB4             PIC X(1)    VALUE SPACE.                 
018800     03  FILLER                  PIC X(3)  VALUE SPACE.                   
018900     03  FILLER                  PIC X(05) VALUE 'ARTNR'.                 
019000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
019100     03  FILLER                  PIC X(05) VALUE 'OI-2V'.                 
019200     03  FILLER                  PIC X(2)  VALUE SPACE.                   
019300     03  FILLER                  PIC X(05) VALUE 'OI-4V'.                 
019400     03  FILLER                  PIC X(3)  VALUE SPACE.                   
019500     03  FILLER                  PIC X(06) VALUE 'PB-SEP'.                
019600     03  FILLER                  PIC X(4)  VALUE SPACE.                   
019700     03  FILLER                  PIC X(05) VALUE 'PB-2V'.                 
019800     03  FILLER                  PIC X(4)  VALUE SPACE.                   
019900     03  FILLER                  PIC X(05) VALUE 'PB-4V'.                 
020000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
020100     03  FILLER                  PIC X(04) VALUE 'VVKL'.                  
020200     03  FILLER                  PIC X(1)  VALUE SPACE.                   
020300                                                                          
020400     EJECT                                                                
020500 01  W001-DETALJ1.                                                        
020600     03  W001-NYRAD1             PIC X(1)    VALUE SPACE.                 
020700     03  W001-IDARTNR            PIC Z(9)  VALUE ZERO.                    
020800     03  W001-OI-TOT-2V          PIC Z(5)9 VALUE ZERO.                    
020900     03  FILLER                  PIC X(1)  VALUE SPACE.                   
021000     03  W001-OI-TOT-4V          PIC Z(5)9 VALUE ZERO.                    
021100     03  FILLER                  PIC X(1)  VALUE SPACE.                   
021200     03  W001-KVPB-SEP           PIC Z(5)9.9 VALUE ZERO.                  
021300     03  FILLER                  PIC X(1)  VALUE SPACE.                   
021400     03  W001-KVPB-2V            PIC Z(5)9.9 VALUE ZERO.                  
021500     03  FILLER                  PIC X(1)  VALUE SPACE.                   
021600     03  W001-KVPB-4V            PIC Z(5)9.9 VALUE ZERO.                  
021700     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021800     03  W001-KDVVKL             PIC 9     VALUE ZERO.                    
021900     03  FILLER                  PIC X(2)  VALUE SPACE.                   
022000     03  W001-LARM-A             PIC X(9)  VALUE SPACE.                   
022100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
022200     03  W001-LARM-B             PIC X(6)  VALUE SPACE.                   
022300     03  FILLER                  PIC X(3)  VALUE SPACE.                   
022400     03  W001-LARM-C-D           PIC X(6)  VALUE SPACE.                   
022500                                                                          
022600     EJECT                                                                
022700 01  MEM-001.                                                             
022800     03  FILLER                  PIC X(80) VALUE ')SEND '.                
022900 01  MEM-002.                                                             
023000     03  FILLER                  PIC X(80)                                
023100                           VALUE 'TITLE ONOR ORDIN CDC'.                  
023200 01  MEM-003.                                                             
023300     03  FILLER                  PIC X(80) VALUE 'OPTION FORCE'.          
023400 01  MEM-004.                                                             
023500     03  FILLER                  PIC X(05) VALUE 'DEST '.                 
023600     03  MEM-IDMAIL              PIC X(60) VALUE SPACE.                   
023700 01  MEM-005.                                                             
023800     03  FILLER                  PIC X(05) VALUE 'MEMO '.                 
023900 01  MEM-006.                                                             
024000     03  FILLER                  PIC X(05) VALUE ')END '.                 
024100 01  MEM-007.                                                             
024200     03  FILLER                  PIC X(20) VALUE 'LINESIZE 120'.          
024300     EJECT                                                                
024400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024500*                                                                         
024600                                                                          
024700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024800     SKIP3                                                                
024900 01  NYCKLAR-TILL-DLI.                                                    
025000     03  W-KDARBTYP-X.                                                    
025100         05  W-KDARBTYP          PIC X(08)    VALUE 'ANSK'.               
025200     03  W-IDPERSON-X.                                                    
025300         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
025400     SKIP2                                                                
025500*    --- STATUS-KOD FRÅN IMS                                              
025600 01  STATUS-WS                   PIC XX.                                  
025700     88  SEGMENT-FINNS                       VALUE '  '.                  
025800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026000     SKIP2                                                                
026100 01  GODK-STATUSKODER.                                                    
026200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026300     SKIP3                                                                
026400 01  SSA1                        PIC X(64).                               
026500 01  SSA2                        PIC X(64).                               
026600     EJECT                                                                
026700*    --- IMS FUNKTIONSKODER                                               
026800*01  -COPY W0003                                                          
026900     EJECT                                                                
027000*    ---  DLI INPUT-OUTPUT AREA                                           
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
027200 01  DLI-IO-WDP311.                                                       
027300*    03  -COPY WDP311                                                     
027400     EJECT                                                                
027500 LINKAGE SECTION.                                                         
027600                                                                          
027700                                                                          
027800*01  -COPY W0008  -PRE WDP3-                                              
027900     05  FILLER                  PIC X.                                   
028000     EJECT                                                                
028100 PROCEDURE DIVISION  USING WDP3-PCB.                                      
028200 MAIN SECTION.                                                            
028300     ENTRY 'DLITCBL' USING WDP3-PCB.                                      
028400                                                                          
028500                                                                          
028600     PERFORM A-INIT                                                       
028700                                                                          
028800     PERFORM S01-LAES-W22235                                              
028900     PERFORM UNTIL END-OF-W22235                                          
029000                                                                          
029100       MOVE IN-IDANSK        TO SPAR-IDANSK                               
029200                                W001-IDANSK                               
029300       MOVE IN-TIVV          TO W001-VECKA                                
029400       MOVE IN-TIAAAA        TO W001-AR                                   
029500       PERFORM S26-HAMTA-MAIL-ID                                          
029600       PERFORM S20-SKRIV-RUBRIKER                                         
029700                                                                          
029800* ---- TILLS NY ANSKAFFARE                                                
029900       PERFORM UNTIL  END-OF-W22235                                       
030000       OR             IN-IDANSK  NOT = SPAR-IDANSK                        
030100                                                                          
030200         IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                    
030300           PERFORM S20-SKRIV-RUBRIKER                                     
030400         END-IF                                                           
030500                                                                          
030600         MOVE IN-IDARTNR     TO W001-IDARTNR                              
030700         MOVE IN-OI-TOT-2V   TO W001-OI-TOT-2V                            
030800         MOVE IN-OI-TOT-4V   TO W001-OI-TOT-4V                            
030900         MOVE IN-KVPB-SEP    TO W001-KVPB-SEP                             
031000         MOVE IN-KVPB-2V     TO W001-KVPB-2V                              
031100         MOVE IN-KVPB-4V     TO W001-KVPB-4V                              
031200         MOVE IN-KDVVKL      TO W001-KDVVKL                               
031300         MOVE SPACE          TO W001-LARM-A                               
031400                                W001-LARM-B                               
031500                                W001-LARM-C-D                             
031600         IF IN-LARM-A = JA                                                
031700            MOVE 'HÖG 2V+4V' TO W001-LARM-A                               
031800         END-IF                                                           
031900         IF IN-LARM-B = JA                                                
032000            MOVE 'HÖG 2V'    TO W001-LARM-B                               
032100         END-IF                                                           
032200         IF IN-LARM-C = JA                                                
032300            MOVE 'HÖG 4V'    TO W001-LARM-C-D                             
032400         END-IF                                                           
032500         IF IN-LARM-D = JA                                                
032600            MOVE 'LÅG 4V'    TO W001-LARM-C-D                             
032700         END-IF                                                           
032800         MOVE W001-DETALJ1   TO W001-RAD                                  
032900         MOVE W001-RAD (2:99)                                             
033000                             TO UT-AREA                                   
033100                                UT2-AREA                                  
033200         PERFORM S12-SKRIV-UTPOST-W22241                                  
033300                                                                          
033400         MOVE SPACE          TO W001-RAD                                  
033500         ADD +1              TO W001-ANTAL-RADER                          
033600         MOVE +1             TO W001-SKIP                                 
033700                                                                          
033800         PERFORM S01-LAES-W22235                                          
033900                                                                          
034000       END-PERFORM                                                        
034100       IF MEM-IDMAIL NOT = 'DAP'                                          
034200         MOVE MEM-006      TO UT-AREA                                     
034300       END-IF                                                             
034400       PERFORM S12-SKRIV-UTPOST-W22241                                    
034500                                                                          
034600     END-PERFORM                                                          
034700                                                                          
034800     PERFORM Z-FINIT                                                      
034900                                                                          
035000     MOVE ZERO TO RETURN-CODE                                             
035100     GOBACK                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 A-INIT SECTION.                                                          
035500                                                                          
035600     OPEN INPUT  W22235                                                   
035700                                                                          
035800     OPEN OUTPUT W22241                                                   
035900     OPEN OUTPUT W22241A                                                  
036000                                                                          
036100     ACCEPT DAGENS-DATUM FROM DATE                                        
036200     MOVE DAGENS-DATUM TO W001-DATUM                                      
036300     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
036400     .                                                                    
036500     EJECT                                                                
036600     EJECT                                                                
036700 Z-FINIT SECTION.                                                         
036800                                                                          
036900     CLOSE W22235                                                         
037000           W22241                                                         
037100           W22241A                                                        
037200                                                                          
037300     MOVE 'S' TO POSTSUM-OPKOD                                            
037400     CALL POSTSUM USING POSTSUM-PARM                                      
037500     .                                                                    
037600     EJECT                                                                
037700 S01-LAES-W22235 SECTION.                                                 
037800                                                                          
037900     READ W22235 INTO IN-AREA                                             
038000     AT END                                                               
038100        SET END-OF-W22235 TO TRUE                                         
038200                                                                          
038300     NOT AT END                                                           
038400        MOVE 'W22235'    TO POSTSUM-FDNAMN                                
038500        MOVE 'W22235D1'  TO POSTSUM-DDNAMN2                               
038600        MOVE 'IN'        TO POSTSUM-TRANSTYP                              
038700        CALL POSTSUM USING POSTSUM-PARM                                   
038800     END-READ                                                             
038900     .                                                                    
039000     EJECT                                                                
039100 S12-SKRIV-UTPOST-W22241  SECTION.                                        
039200     IF MEM-IDMAIL  = 'DAP'                                               
039300      WRITE UT2-POST FROM UT2-AREA                                        
039400      MOVE 'W22241A'  TO POSTSUM-FDNAMN                                   
039500      MOVE 'W22241D3' TO POSTSUM-DDNAMN2                                  
039600      MOVE 'UT2'      TO POSTSUM-TRANSTYP                                 
039700     ELSE                                                                 
039800      WRITE UT-POST FROM UT-AREA                                          
039900      MOVE 'W22241'   TO POSTSUM-FDNAMN                                   
040000      MOVE 'W22241D2' TO POSTSUM-DDNAMN2                                  
040100      MOVE 'UT'       TO POSTSUM-TRANSTYP                                 
040200     END-IF                                                               
040300     CALL POSTSUM USING POSTSUM-PARM                                      
040400     .                                                                    
040500     EJECT                                                                
040600 S20-SKRIV-RUBRIKER SECTION.                                              
040700                                                                          
040800     PERFORM S22-SKRIV-RUBRIK1                                            
040900     PERFORM S23-SKRIV-RUBRIK2                                            
041000     PERFORM S24-SKRIV-RUBRIK3                                            
041100     PERFORM S25-SKRIV-RUBRIK4                                            
041200     .                                                                    
041300     EJECT                                                                
041400 S22-SKRIV-RUBRIK1 SECTION.                                               
041500     IF MEM-IDMAIL NOT = 'DAP'                                            
041600       MOVE ' ' TO W001-NYRUB1                                            
041700       ADD +1   TO W001-SIDRAKNARE                                        
041800       MOVE W001-SIDRAKNARE   TO W001-SID                                 
041900     ELSE                                                                 
042000       MOVE '1'  TO W001-NYRUB1                                           
042100       ADD +1    TO WDAP-SIDRAKNARE                                       
042200       MOVE WDAP-SIDRAKNARE   TO W001-SID                                 
042300     END-IF                                                               
042400                                                                          
042500     PERFORM S22A-SKAPA-MAIL-DEST                                         
042600                                                                          
042700     MOVE W001-RUBRIK1 (2:99) TO UT-AREA                                  
042800                                 UT2-AREA                                 
042900     PERFORM S12-SKRIV-UTPOST-W22241                                      
043000     MOVE SPACE               TO UT-AREA                                  
043100                                 UT2-AREA                                 
043200     PERFORM S12-SKRIV-UTPOST-W22241                                      
043300                                                                          
043400     MOVE +2 TO W001-ANTAL-RADER                                          
043500                                                                          
043600     MOVE +2 TO W001-SKIP                                                 
043700     .                                                                    
043800     EJECT                                                                
043900 S22A-SKAPA-MAIL-DEST   SECTION.                                          
044000     IF MEM-IDMAIL NOT = 'DAP'                                            
044100       MOVE MEM-001     TO UT-AREA                                        
044200       PERFORM S12-SKRIV-UTPOST-W22241                                    
044300       MOVE MEM-002     TO UT-AREA                                        
044400       PERFORM S12-SKRIV-UTPOST-W22241                                    
044500       MOVE MEM-003     TO UT-AREA                                        
044600       PERFORM S12-SKRIV-UTPOST-W22241                                    
044700       MOVE MEM-007     TO UT-AREA                                        
044800       PERFORM S12-SKRIV-UTPOST-W22241                                    
044900       MOVE MEM-004     TO UT-AREA                                        
045000       PERFORM S12-SKRIV-UTPOST-W22241                                    
045100       MOVE MEM-005     TO UT-AREA                                        
045200       PERFORM S12-SKRIV-UTPOST-W22241                                    
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 S23-SKRIV-RUBRIK2 SECTION.                                               
045700                                                                          
045800     MOVE W001-RUBRIK2         TO UT-AREA                                 
045900                                  UT2-AREA                                
046000     PERFORM S12-SKRIV-UTPOST-W22241                                      
046100                                                                          
046200     ADD +1 TO W001-ANTAL-RADER                                           
046300                                                                          
046400     MOVE +1 TO W001-SKIP                                                 
046500     .                                                                    
046600     EJECT                                                                
046700 S24-SKRIV-RUBRIK3 SECTION.                                               
046800                                                                          
046900     MOVE W001-RUBRIK3         TO UT-AREA                                 
047000                                  UT2-AREA                                
047100     PERFORM S12-SKRIV-UTPOST-W22241                                      
047200     MOVE SPACE               TO UT-AREA                                  
047300                                 UT2-AREA                                 
047400     PERFORM S12-SKRIV-UTPOST-W22241                                      
047500                                                                          
047600     ADD +2 TO W001-ANTAL-RADER                                           
047700                                                                          
047800     MOVE +2 TO W001-SKIP                                                 
047900     .                                                                    
048000     EJECT                                                                
048100 S25-SKRIV-RUBRIK4 SECTION.                                               
048200                                                                          
048300     MOVE W001-RUBRIK4         TO UT-AREA                                 
048400                                  UT2-AREA                                
048500     PERFORM S12-SKRIV-UTPOST-W22241                                      
048600     MOVE SPACE               TO UT-AREA                                  
048700                                 UT2-AREA                                 
048800     PERFORM S12-SKRIV-UTPOST-W22241                                      
048900                                                                          
049000     ADD +2 TO W001-ANTAL-RADER                                           
049100                                                                          
049200     MOVE +2 TO W001-SKIP                                                 
049300     .                                                                    
049400     EJECT                                                                
049500 S26-HAMTA-MAIL-ID SECTION.                                               
049600                                                                          
049700     MOVE SPAR-IDANSK           TO W-IDPERSON                             
049800     PERFORM IMS-GET-WDP3-ANSKNAMN                                        
049900     IF SEGMENT-FINNS                                                     
050000     AND PERS-IDMAIL NOT = SPACE                                          
050100        MOVE PERS-IDMAIL               TO MEM-IDMAIL                      
050200     ELSE                                                                 
050300        MOVE 'DAP' TO MEM-IDMAIL                                          
050400     END-IF                                                               
050500                                                                          
050600     .                                                                    
050700     EJECT                                                                
050800* --- IMS SEKTIONER ---                                                   
050900                                                                          
051000 IMS-GET-WDP3-ANSKNAMN SECTION.                                           
051100                                                                          
051200     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
051300          DELIMITED BY SIZE INTO SSA1                                     
051400     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
051500          DELIMITED BY SIZE INTO SSA2                                     
051600     MOVE '  GE' TO GODK-STATUSKODER                                      
051700     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
051800     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
051900     PERFORM IMS-STATUSKONTROLL                                           
052000     .                                                                    
052100     EJECT                                                                
052200 IMS-STATUSKONTROLL SECTION.                                              
052300                                                                          
052400     SET STATUS-IX TO 1                                                   
052500     SEARCH GODK-STATUS                                                   
052600       AT END                                                             
052700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
052800           DELIMITED BY SIZE INTO FELTEXT                                 
052900         DISPLAY FELTEXT                                                  
053000         CALL FELLOG                                                      
053100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053200         CONTINUE                                                         
053300     END-SEARCH                                                           
053400     .                                                                    
