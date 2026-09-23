000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6126800.                                                
000400 AUTHOR.         JOHAN LINDKVIST.                                         
000500 DATE-WRITTEN.   97/06/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        GOODS IN TRANSIT TO NDC                                          
001100*                                                                         
001200*    ÄNDRING:                                                             
001300*            990311   JOHAN L     JPN / AUS VALUES I SEK                  
001400*            061122   MARKUS A    LISTOR SOM FIL TILL D&P                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- SORTERADE INFIL                                            
002500     SELECT W61266                     ASSIGN TO W61268D1.                
002600     SKIP2                                                                
002700*          --- UTLISTA                                                    
002800     SELECT W61268-001                 ASSIGN TO W61268D2.                
002900*          --- UTLISTA61                                                  
003000     SELECT W61268-061                 ASSIGN TO W61268D3.                
003100*          --- UTLISTA11                                                  
003200     SELECT W61268-011                 ASSIGN TO W61268D4.                
003300     SELECT W61268-002                 ASSIGN TO W61268D5.                
003400     SELECT W61268-003                 ASSIGN TO W61268D6.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W61266                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W61262      -L.                                                
004500     SKIP3                                                                
004600 FD  W61268-001                                                           
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900     SKIP2                                                                
005000 01  W61268-001-RAD              PIC X(121).                              
005100     SKIP3                                                                
005200 FD  W61268-061                                                           
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600 01  W61268-061-RAD              PIC X(125).                              
005700     SKIP3                                                                
005800 FD  W61268-011                                                           
005900     RECORDING       V                                                    
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200 01  W61268-011-RAD              PIC X(125).                              
006300     SKIP3                                                                
006400 FD  W61268-002                                                           
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700     SKIP2                                                                
006800 01  W61268-002-REC              PIC X(121).                              
006900     SKIP3                                                                
007000 FD  W61268-003                                                           
007100     RECORDING       V                                                    
007200     BLOCK CONTAINS  0.                                                   
007300     SKIP2                                                                
007400 01  W61268-003-REC              PIC X(121).                              
007500     SKIP3                                                                
007600 WORKING-STORAGE SECTION.                                                 
007700                                                                          
007800*    -COPY WY2000W3                                                       
007900     SKIP3                                                                
008000 01  I                           PIC 9  COMP-3.                           
008100 01  N                           PIC 9  COMP-3.                           
008200                                                                          
008300 01 TALLYS.                                                               
008400   03 SAVES OCCURS 6 TIMES.                                               
008500       05 RUBRIK-RAD             PIC X(10).                               
008600       05 T-VALUES               PIC S9(7)V99    COMP-3.                  
008700       05 INVOICES               PIC S9(5)       COMP-3.                  
008800       05 NO-CASES               PIC S9(5)       COMP-3.                  
008900       05 NO-LINES               PIC S9(5)       COMP-3.                  
009000       05 NEWPARTS               PIC S9(5)       COMP-3.                  
009100                                                                          
009200 01  SPAR-IDDC                   PIC XX      VALUE SPACE.                 
009300 01  SPAR-CITY                   PIC X(25)   VALUE SPACE.                 
009400 01  WS-PREV-IDDC                PIC XX      VALUE SPACE.                 
009500 01  SPAR-FAKTURA                PIC 9(7)    VALUE 0.                     
009600 01  SPAR-KOLLI                  PIC 9(5)    VALUE 0.                     
009700 01  SPAR-ORDNR5                 PIC 9(5)    VALUE 0.                     
009800 01  SPAR-KUNDNR                 PIC 9(7)    VALUE 0.                     
009900 01  SPAR-KDMFUP                 PIC X(2)    VALUE SPACE.                 
010000                                                                          
010100 01  NY-FAKTURA                  PIC X       VALUE 'N'.                   
010200 01  NYTT-KOLLI                  PIC X       VALUE 'N'.                   
010300                                                                          
010400 01  PASSED.                                                              
010500     03  FILLER                  PIC X(4) VALUE SPACE.                    
010600     03  FILLER                  PIC X(6) VALUE 'PASSED'.                 
010700                                                                          
010800 01  WTOTAL.                                                              
010900     03  FILLER                  PIC X(4) VALUE SPACE.                    
011000     03  FILLER                  PIC X(6) VALUE ' TOTAL'.                 
011100                                                                          
011200 01  HEADER.                                                              
011300     03  HEADER-TITLE OCCURS 6 TIMES.                                     
011400         05  FILLER              PIC X(6)    VALUE SPACES.                
011500         05  HEAD-W              PIC X       VALUE 'W'.                   
011600         05  HEAD-SPACE          PIC X       VALUE SPACES.                
011700         05  HEAD-TIVECKA        PIC XX.                                  
011800     03  HEADER-VECKA OCCURS 5 TIMES.                                     
011900         05  HEAD-TIAA           PIC 99.                                  
012000         05  HEAD-TIVV           PIC 99.                                  
012100 01  HEADER-VECKA-NUM            PIC 9(4).                                
012200                                                                          
012300 01  TEMP-DECTAL                 PIC Z(6)9.99.                            
012400 01  TEMP-HELTAL                 PIC Z(9)9.                               
012500                                                                          
012600 01  TEST-TIAAVV.                                                         
012700     03  TEST-TIAA               PIC 9(2).                                
012800     03  TEST-TIVV               PIC 9(2).                                
012900 01  TEST-TIAAVV-NUM REDEFINES TEST-TIAAVV                                
013000                                 PIC 9(4).                                
013100                                                                          
013200* VARIABLER TILL SUBPROGRAM W009VADD                                      
013300 01  ADD-AAVV                    PIC S9(5)  COMP-3.                       
013400 01  FILLER.                                                              
013500   03 AAVV-NUM                   PIC 9(4).                                
013600   03 FILLER REDEFINES AAVV-NUM.                                          
013700     05 TEMP-ADD-AAVV-AA         PIC 99.                                  
013800     05 TEMP-ADD-AAVV-VV         PIC 99.                                  
013900                                                                          
014000 01  ADD-ANTAL                   PIC S9(3)  COMP-3.                       
014100     EJECT                                                                
014200                                                                          
014300 77  IDPGM                       PIC X(8)    VALUE 'W6126800'.            
014400 77  JA                          PIC X       VALUE 'J'.                   
014500 77  NEJ                         PIC X       VALUE 'N'.                   
014600 77  INDX                        PIC 9       VALUE ZERO.                  
014700                                                                          
014800 77  W61266-EOF-SW               PIC X       VALUE 'N'.                   
014900     88  END-OF-W61266                       VALUE 'J'.                   
015000     EJECT                                                                
015100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015200 01  FILLER REDEFINES DAGENS-DATUM.                                       
015300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015600                                                                          
015700 01  DAGENS-VECKA                PIC 9(4).                                
015800 01  FILLER REDEFINES DAGENS-VECKA.                                       
015900     03  DAGENS-AAR              PIC 9(2).                                
016000     03  DAGENS-VV               PIC 9(2).                                
016100     SKIP2                                                                
016200*      --- VALID IDDC CODES                                               
016300*                                                                         
016400*01    -COPY WWDC99                                                       
016500*01    -COPY WWDCKONS                                                     
016600       EJECT                                                              
016700 01  DYNAMISKA-SUBPROGRAM.                                                
016800*                                                                         
016900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
017100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017200     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
017300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017600     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
017700     SKIP2                                                                
017800*    --- PARAMETRAR TILL ABEND                                            
017900                                                                          
018000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018300     SKIP2                                                                
018400 01  FELTEXT.                                                             
018500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL DATKORT                                          
018900*                                                                         
019000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61268'.              
019100     SKIP2                                                                
019200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
019300     SKIP2                                                                
019400*01  -COPY WDATKORT                                                       
019500     EJECT                                                                
019600*    --- PARAMETRAR TILL POSTSUM                                          
019700*                                                                         
019800*01  -COPY W0005   -PRE  POSTSUM-                                         
019900     EJECT                                                                
020000*01  -COPY WDATAREA                                                       
020100     EJECT                                                                
020200*    --- PARAMETERS FOR WL10WBDC                                          
020300*01  -COPY WL10WBDC                                                       
020400     EJECT                                                                
020500 01  IN-SHIST-AREA-START         PIC X(24)   VALUE                        
020600                                 'IN-SHIST-AREA-START'.                   
020700     SKIP2                                                                
020800                                                                          
020900*01  AREA -COPY W61262     -PRE IN-SHIST-                                 
021000     EJECT                                                                
021100 01  W001-AREA-START             PIC X(24)   VALUE                        
021200                                 'W001-AREA-START  '.                     
021300     SKIP2                                                                
021400 01  W001-HJALPAREOR.                                                     
021500*                                                                         
021600     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 3.                
021700     03  W001-ANTAL-RADER                                                 
021800                                 PIC 9(3)    VALUE 999.                   
021900     03  W001-MAX-RADER-PER-SIDA                                          
022000                                 PIC 9(3)    VALUE 42.                    
022100     03  W001-MAX-POSITIONER-PER-RAD                                      
022200                                 PIC 9(3)    VALUE 120.                   
022300     03  W001-LISTNR             PIC X(11)   VALUE 'W61268-001'.          
022400     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
022500     EJECT                                                                
022600 01  W001-RAD.                                                            
022700*                                                                         
022800     03  FILLER                  PIC X(121)  VALUE SPACE.                 
022900     EJECT                                                                
023000 01  W001-RUBRIK1.                                                        
023100*                                                                         
023200     03  FILLER                  PIC X(3) VALUE SPACE.                    
023300     03  FILLER                  PIC X(21)                                
023400                                VALUE 'VOLVO CAR PARTS      '.            
023500     03  FILLER                  PIC X(12)                                
023600                                 VALUE 'W61268-001'.                      
023700     03  FILLER                  PIC X(44)                                
023800         VALUE 'GOODS IN TRANSIT, AIR FREIGHTS EXCLUDED.'.                
023900     03  FILLER                  PIC X(4) VALUE 'DC '.                    
024000     03  RUBRIK-DC               PIC XX.                                  
024100     03  FILLER                  PIC X(10) VALUE SPACE.                   
024200     03  W001-DATUM              PIC XXBXXBXX.                            
024300     03  FILLER                  PIC X(4) VALUE SPACE.                    
024400     03  FILLER                  PIC X(4)                                 
024500                                 VALUE 'PAGE'.                            
024600     03  W001-SID                PIC Z(4)9.                               
024700     EJECT                                                                
024800 01  W001-DETALJ.                                                         
024900     03  FILLER                  PIC X(10) VALUE SPACE.                   
025000     03  RAD-RUBRIK              PIC X(10).                               
025100     03  FILLER  OCCURS 6.                                                
025200       05  FILLER                PIC XX VALUE SPACE.                      
025300       05  RAD-FAELT             PIC X(10).                               
025400     EJECT                                                                
025500 01  W002-AREA-START             PIC X(24)   VALUE                        
025600                                 'W002-AREA-START  '.                     
025700     SKIP2                                                                
025800 01  W002-CONTROL-REC1.                                                   
025900*                                                                         
026000     03  FILLER                  PIC X(72)   VALUE                        
026100                                 ' ¤DAPW61268-002'.                       
026200     EJECT                                                                
026300 01  W002-CONTROL-REC2.                                                   
026400*                                                                         
026500     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
026600     03  W002-CTL-IDDC           PIC X(2)    VALUE SPACE.                 
026700     03  FILLER                  PIC X(65)   VALUE SPACE.                 
026800     EJECT                                                                
026900 01  W002-REC.                                                            
027000*                                                                         
027100     03  FILLER                  PIC X(76)   VALUE SPACE.                 
027200     EJECT                                                                
027300 01  W002-DETAIL.                                                         
027400     03  W002-IDDC               PIC X(2).                                
027500     03  W002-TITLE              PIC X(10).                               
027600     03  FILLER  OCCURS 6.                                                
027700       05  W002-REC-ARRAY        PIC X(10).                               
027800     03  W002-TIAAVV             PIC X(04).                               
027900     EJECT                                                                
028000 01  W003-AREA-START             PIC X(24)   VALUE                        
028100                                 'W003-AREA-START  '.                     
028200     SKIP2                                                                
028300 01  W003-CONTROL-REC1.                                                   
028400*                                                                         
028500     03  FILLER                  PIC X(72)   VALUE                        
028600                                 ' ¤DAPW61268-003'.                       
028700     EJECT                                                                
028800 01  W003-CONTROL-REC2.                                                   
028900*                                                                         
029000     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
029100     03  W003-CTL-KDMFUP         PIC X(2)    VALUE SPACE.                 
029200     03  FILLER                  PIC X(65)   VALUE SPACE.                 
029300     EJECT                                                                
029400 01  W003-REC.                                                            
029500*                                                                         
029600     03  FILLER                  PIC X(101)  VALUE SPACE.                 
029700     EJECT                                                                
029800 01  W003-DETAIL.                                                         
029900     03  W003-IDDC               PIC X(2).                                
030000     03  W003-CITY               PIC X(25).                               
030100     03  W003-TITLE              PIC X(10).                               
030200     03  FILLER  OCCURS 6.                                                
030300       05  W003-REC-ARRAY        PIC X(10).                               
030400     03  W003-TIAAVV             PIC X(04).                               
030500     EJECT                                                                
030600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
030700     SKIP3                                                                
030800 01  KEYS-FOR-DLI.                                                        
030900     03  W-IDARTNR-X.                                                     
031000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
031100     03  W-IDDC-X.                                                        
031200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
031300                                                                          
031400*    --- STATUS-KOD FRÅN IMS                                              
031500 01  STATUS-WS                   PIC XX.                                  
031600     88  SEGMENT-FOUND                       VALUE '  '.                  
031700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
031800     SKIP2                                                                
031900 01  GOOD-STATUSCODES.                                                    
032000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032100     SKIP3                                                                
032200 01  SSA1                        PIC X(64).                               
032300 01  SSA2                        PIC X(64).                               
032400*    --- IMS FUNCTION CODES                                               
032500*01  -COPY W0003                                                          
032600*    ---  DLI INPUT-OUTPUT AREA                                           
032700 01  DLI-IO-AREA.                                                         
032800     03  IO-AREA                 PIC X(800) VALUE SPACE.                  
032900     SKIP3                                                                
033000                                                                          
033100     03  WDK711   REDEFINES IO-AREA.                                      
033200*        05  -COPY WDK711  -PRE WDK7-                                     
033300                                                                          
033400     03  WDB601   REDEFINES IO-AREA.                                      
033500*        05  -COPY WDB601  -PRE WDB6-                                     
033600                                                                          
033700 LINKAGE SECTION.                                                         
033800                                                                          
033900*01  -COPY W0008  -PRE WDK7-                                              
034000     05  FILLER                  PIC X.                                   
034100 EJECT                                                                    
034200                                                                          
034300*01  -COPY W0008  -PRE WDB6-                                              
034400     05  FILLER                  PIC X.                                   
034500 EJECT                                                                    
034600                                                                          
034700 PROCEDURE DIVISION  USING WDK7-PCB WDB6-PCB.                             
034800 MAIN SECTION.                                                            
034900     ENTRY 'DLITCBL' USING WDK7-PCB WDB6-PCB.                             
035000                                                                          
035100     PERFORM A-INIT                                                       
035200     PERFORM S01-LAES-W61266                                              
035300     PERFORM UNTIL END-OF-W61266                                          
035400       PERFORM B-KONTROLL-NYTT-DC                                         
035500       PERFORM C-KONTROLL-NY-FAKTURA                                      
035600       PERFORM D-KONTROLL-NYTT-KOLLI                                      
035700       PERFORM E-CHECK-INPOST                                             
035800       PERFORM S01-LAES-W61266                                            
035900     END-PERFORM                                                          
036000                                                                          
036100     MOVE SPAR-IDDC              TO WBDC-IDDC                             
036200     PERFORM S27-CHECK-WEBDC                                              
036300     IF NDC-CN OR LDC-CN                                                  
036400       PERFORM S17-WRITE-CHINA-DATA                                       
036500       PERFORM S22-WRITE-CHINA-MGMT-DATA                                  
036600     ELSE                                                                 
036700       PERFORM S12-LIST-UTSKRIFT                                          
036800     END-IF                                                               
036900     PERFORM Z-FINIT                                                      
037000                                                                          
037100     MOVE ZERO TO RETURN-CODE                                             
037200     GOBACK                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 A-INIT SECTION.                                                          
037600                                                                          
037700     OPEN INPUT  W61266                                                   
037800     OPEN OUTPUT W61268-001                                               
037900                 W61268-061                                               
038000                 W61268-011                                               
038100                 W61268-002                                               
038200                 W61268-003                                               
038300                                                                          
038400     INITIALIZE TALLYS                                                    
038500     INITIALIZE W001-DETALJ                                               
038600                W002-DETAIL                                               
038700                W003-DETAIL                                               
038800     SKIP2                                                                
038900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
039000     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
039100     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
039200     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
039300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039400                                                                          
039500*  ÖVERSÄTTER DAGENS DATUM TILL ÅR OCH VECKA                              
039600     MOVE D-AAR    TO   DAGENS-AAR                                        
039700     MOVE D-VECKA  TO   DAGENS-VV                                         
039800                                                                          
039900*  SKAPAR RUBRIKER TILL RADERNA                                           
040000     MOVE WTOTAL TO HEADER-TITLE(6)                                       
040100     MOVE PASSED TO HEADER-TITLE(1)                                       
040200     MOVE DAGENS-VECKA TO HEADER-VECKA(1)                                 
040300                                                                          
040400                                                                          
040500     MOVE 2 TO I                                                          
040600     PERFORM UNTIL I > 5                                                  
040700       MOVE DAGENS-VECKA TO ADD-AAVV                                      
040800       COMPUTE ADD-ANTAL = I - 1                                          
040900       CALL W009VADD USING ADD-AAVV ADD-ANTAL                             
041000       MOVE ADD-AAVV TO AAVV-NUM                                          
041100       MOVE TEMP-ADD-AAVV-AA TO HEAD-TIAA(I)                              
041200       MOVE TEMP-ADD-AAVV-VV TO HEAD-TIVV(I)                              
041300       MOVE TEMP-ADD-AAVV-VV TO HEAD-TIVECKA(I)                           
041400       ADD 1 TO I                                                         
041500     END-PERFORM                                                          
041600     .                                                                    
041700     EJECT                                                                
041800 Z-FINIT SECTION.                                                         
041900     CLOSE W61266                                                         
042000           W61268-001                                                     
042100           W61268-061                                                     
042200           W61268-011                                                     
042300           W61268-002                                                     
042400           W61268-003                                                     
042500                                                                          
042600     MOVE 'S' TO POSTSUM-OPKOD                                            
042700     CALL POSTSUM USING POSTSUM-PARM                                      
042800     .                                                                    
042900     EJECT                                                                
043000 B-KONTROLL-NYTT-DC SECTION.                                              
043100     IF SPAR-IDDC = SPACE                                                 
043200       MOVE IN-SHIST-IDDC TO SPAR-IDDC                                    
043300       MOVE IN-SHIST-IDDC TO WS-IDDC W-IDDC                               
043400       PERFORM IMS-GU-WDB601                                              
043500       IF SEGMENT-FOUND                                                   
043600         MOVE WDB6-DCS-ADCITY IN WDB6-DCS-ADPOST-PNRORT                   
043700                                 TO SPAR-CITY                             
043800       ELSE                                                               
043900         MOVE SPACES             TO SPAR-CITY                             
044000       END-IF                                                             
044100     ELSE                                                                 
044200       IF IN-SHIST-IDDC = SPAR-IDDC                                       
044300         CONTINUE                                                         
044400       ELSE                                                               
044500         MOVE SPAR-IDDC          TO WBDC-IDDC                             
044600         PERFORM S27-CHECK-WEBDC                                          
044700         IF NDC-CN OR LDC-CN                                              
044800           PERFORM S17-WRITE-CHINA-DATA                                   
044900           PERFORM S22-WRITE-CHINA-MGMT-DATA                              
045000           INITIALIZE TALLYS                                              
045100         ELSE                                                             
045200           PERFORM S12-LIST-UTSKRIFT                                      
045300         END-IF                                                           
045400         MOVE IN-SHIST-IDDC TO SPAR-IDDC                                  
045500         MOVE IN-SHIST-IDDC TO WS-IDDC W-IDDC                             
045600         PERFORM IMS-GU-WDB601                                            
045700         IF SEGMENT-FOUND                                                 
045800           MOVE WDB6-DCS-ADCITY IN WDB6-DCS-ADPOST-PNRORT                 
045900                                 TO SPAR-CITY                             
046000         ELSE                                                             
046100           MOVE SPACES           TO SPAR-CITY                             
046200         END-IF                                                           
046300       END-IF                                                             
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 C-KONTROLL-NY-FAKTURA SECTION.                                           
046800                                                                          
046900     IF SPAR-FAKTURA = IN-SHIST-IDFAKT                                    
047000       MOVE NEJ TO NY-FAKTURA                                             
047100         IF SPAR-ORDNR5 = IN-SHIST-IDORDNR5                               
047200           IF SPAR-KUNDNR = IN-SHIST-IDKUNDNR                             
047300             CONTINUE                                                     
047400           ELSE                                                           
047500             MOVE ZERO TO SPAR-KOLLI                                      
047600             MOVE IN-SHIST-IDKUNDNR TO SPAR-KUNDNR                        
047700           END-IF                                                         
047800         ELSE                                                             
047900           MOVE ZERO TO SPAR-KOLLI                                        
048000           MOVE IN-SHIST-IDKUNDNR TO SPAR-KUNDNR                          
048100           MOVE IN-SHIST-IDORDNR5 TO SPAR-ORDNR5                          
048200         END-IF                                                           
048300     ELSE                                                                 
048400       MOVE JA TO NY-FAKTURA                                              
048500       MOVE IN-SHIST-IDFAKT   TO SPAR-FAKTURA                             
048600       MOVE IN-SHIST-IDORDNR5 TO SPAR-ORDNR5                              
048700       MOVE IN-SHIST-IDKUNDNR TO SPAR-KUNDNR                              
048800       MOVE ZERO TO SPAR-KOLLI                                            
048900     END-IF                                                               
049000                                                                          
049100     .                                                                    
049200     EJECT                                                                
049300 D-KONTROLL-NYTT-KOLLI SECTION.                                           
049400                                                                          
049500     IF SPAR-KOLLI = IN-SHIST-IDKOLLI                                     
049600       MOVE NEJ TO NYTT-KOLLI                                             
049700     ELSE                                                                 
049800       MOVE JA TO NYTT-KOLLI                                              
049900       MOVE IN-SHIST-IDKOLLI TO SPAR-KOLLI                                
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 E-CHECK-INPOST SECTION.                                                  
050400                                                                          
050500     MOVE IN-SHIST-TIBERANK TO DAT-I-TIDATUM                              
050600     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
050700     CALL WDATKONV       USING DAT-KDDATFORM                              
050800                               DAT-I-TIDATUM                              
050900                               DAT-O-TIDATUM                              
051000                               DAT-KDSVAR                                 
051100     IF DAT-KDSVAR-OK                                                     
051200         MOVE DAT-TIAA     TO TEST-TIAA                                   
051300         MOVE DAT-TIVV     TO TEST-TIVV                                   
051400     ELSE                                                                 
051500         DISPLAY 'FEL FRÅN DATKONV'                                       
051600         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
051700     END-IF                                                               
051800                                                                          
051900     MOVE TEST-TIAAVV-NUM   TO TMP1-YYWW                                  
052000     MOVE HEADER-VECKA(1)   TO HEADER-VECKA-NUM                           
052100     MOVE HEADER-VECKA-NUM  TO TMP2-YYWW                                  
052200     PERFORM WY2000P3                                                     
052300     MOVE IN-SHIST-IDARTNR         TO W-IDARTNR                           
052400     IF NDC-CN OR LDC-CN                                                  
052500       PERFORM IMS-GU-WDK7-SLAG                                           
052600     END-IF                                                               
052700     EVALUATE TRUE                                                        
052800     WHEN TMP1-YYWW <= TMP2-YYWW                                          
052900       MOVE 1 TO N                                                        
053000       PERFORM S42-ASSIGN-TALLYS                                          
053100     WHEN TEST-TIAAVV = HEADER-VECKA(2)                                   
053200       MOVE 2 TO N                                                        
053300       PERFORM S42-ASSIGN-TALLYS                                          
053400     WHEN TEST-TIAAVV = HEADER-VECKA(3)                                   
053500       MOVE 3 TO N                                                        
053600       PERFORM S42-ASSIGN-TALLYS                                          
053700     WHEN TEST-TIAAVV = HEADER-VECKA(4)                                   
053800       MOVE 4 TO N                                                        
053900       PERFORM S42-ASSIGN-TALLYS                                          
054000     WHEN TEST-TIAAVV = HEADER-VECKA(5)                                   
054100       MOVE 5 TO N                                                        
054200       PERFORM S42-ASSIGN-TALLYS                                          
054300     WHEN OTHER                                                           
054400       CONTINUE                                                           
054500     END-EVALUATE                                                         
054600                                                                          
054700* SUMMERAR IHOP TOTALKOLUMNEN                                             
054800     MOVE 6 TO N                                                          
054900     PERFORM S42-ASSIGN-TALLYS                                            
055000     .                                                                    
055100     EJECT                                                                
055200 S01-LAES-W61266  SECTION.                                                
055300     READ W61266 INTO IN-SHIST-AREA                                       
055400     AT END                                                               
055500        MOVE HIGH-VALUE TO IN-SHIST-AREA                                  
055600        SET END-OF-W61266 TO TRUE                                         
055700                                                                          
055800     NOT AT END                                                           
055900        MOVE 'W61266' TO POSTSUM-FDNAMN                                   
056000        MOVE 'W61268D1' TO POSTSUM-DDNAMN2                                
056100        MOVE IN-SHIST-IDDC TO POSTSUM-TRANSTYP                            
056200        CALL POSTSUM USING POSTSUM-PARM                                   
056300     END-READ                                                             
056400     .                                                                    
056500     EJECT                                                                
056600 S12-LIST-UTSKRIFT SECTION.                                               
056700     PERFORM S13-SKRIV-RUBRIKER                                           
056800     MOVE 1 TO I                                                          
056900     PERFORM UNTIL I > 5                                                  
057000       PERFORM S16-SKAPA-RAD                                              
057100       IF WBDC-FLWEBDC = 'J'                                              
057200         PERFORM S26-WRITE-W61268-002-003                                 
057300       ELSE                                                               
057400         PERFORM S15-SKRIV-W61268-001                                     
057500       END-IF                                                             
057600       ADD 1 TO I                                                         
057700     END-PERFORM                                                          
057800     INITIALIZE TALLYS                                                    
057900     .                                                                    
058000     EJECT                                                                
058100 S13-SKRIV-RUBRIKER SECTION.                                              
058200                                                                          
058300     MOVE 1 TO W001-SIDRAKNARE                                            
058400     MOVE 3 TO W001-SKIP                                                  
058500     MOVE +7 TO W001-ANTAL-RADER                                          
058600     MOVE DAGENS-DATUM TO W001-DATUM                                      
058700     MOVE W001-SIDRAKNARE TO W001-SID                                     
058800     MOVE SPAR-IDDC TO RUBRIK-DC                                          
059200     IF SPAR-IDDC = WC-CDC-SE                                             
059300       WRITE W61268-011-RAD FROM W001-RUBRIK1                             
059400     ELSE                                                                 
059500       IF WBDC-FLWEBDC = 'J'                                              
059600         IF SPAR-IDDC NOT = WS-PREV-IDDC                                  
059700           MOVE SPAR-IDDC      TO W002-CTL-IDDC                           
059800                                    WS-PREV-IDDC                          
059900           PERFORM S21-WRITE-W61268-002-CTL                               
060000         END-IF                                                           
060100       ELSE                                                               
060200         WRITE W61268-001-RAD FROM W001-RUBRIK1 AFTER PAGE                
060400       END-IF                                                             
060500     END-IF                                                               
060600     PERFORM S14-SKAPA-HEADER                                             
061000     IF SPAR-IDDC = WC-CDC-SE                                             
061100       WRITE W61268-011-RAD FROM W001-RAD                                 
061200     ELSE                                                                 
061300       IF WBDC-FLWEBDC = 'J'                                              
061400         PERFORM S26-WRITE-W61268-002-003                                 
061500       ELSE                                                               
061600         WRITE W61268-001-RAD FROM W001-RAD AFTER W001-SKIP               
061700       END-IF                                                             
061900     END-IF                                                               
062000                                                                          
062100     MOVE SPACE TO W001-RAD                                               
062200     .                                                                    
062300     EJECT                                                                
062400 S14-SKAPA-HEADER SECTION.                                                
062500     MOVE 'ETA DAY' TO RAD-RUBRIK                                         
062600     MOVE 1 TO N                                                          
062700     PERFORM UNTIL N > 6                                                  
062800       MOVE HEADER-TITLE(N) TO RAD-FAELT(N)                               
062900       ADD 1 TO N                                                         
063000     END-PERFORM                                                          
063100                                                                          
063200     MOVE W001-DETALJ  TO  W001-RAD                                       
063300                                                                          
063400     MOVE SPAR-IDDC    TO WS-IDDC                                         
063500     IF NDC-PACIFIC OR CDC-SE                                             
063600        MOVE 'VALUE SEK' TO RUBRIK-RAD(1)                                 
063700     ELSE                                                                 
063800        MOVE 'VALUE  $ ' TO RUBRIK-RAD(1)                                 
063900     END-IF                                                               
064000     MOVE 'INVOICES '  TO RUBRIK-RAD(2)                                   
064100     MOVE 'CASES    '  TO RUBRIK-RAD(3)                                   
064200     MOVE 'LINES    '  TO RUBRIK-RAD(4)                                   
064300     MOVE 'NEW PARTS'  TO RUBRIK-RAD(5)                                   
064400     .                                                                    
064500     EJECT                                                                
064600 S15-SKRIV-W61268-001  SECTION.                                           
064700                                                                          
064800     MOVE W001-DETALJ  TO  W001-RAD                                       
065200     IF SPAR-IDDC = WC-CDC-SE                                             
065300       WRITE W61268-011-RAD FROM W001-RAD                                 
065400     ELSE                                                                 
065500       WRITE W61268-001-RAD FROM W001-RAD AFTER W001-SKIP                 
065600     END-IF                                                               
065800                                                                          
065900     MOVE SPACE TO W001-RAD                                               
066000     ADD  +1 TO W001-ANTAL-RADER                                          
066100     .                                                                    
066200     EJECT                                                                
066300 S16-SKAPA-RAD SECTION.                                                   
066400                                                                          
066500     EVALUATE I                                                           
066600     WHEN 1                                                               
066700       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
066800       MOVE 1 TO N                                                        
066900       PERFORM UNTIL N > 6                                                
067000         MOVE T-VALUES(N) TO TEMP-DECTAL                                  
067100         MOVE TEMP-DECTAL TO RAD-FAELT(N)                                 
067200         ADD 1 TO N                                                       
067300       END-PERFORM                                                        
067400     WHEN 2                                                               
067500       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
067600       MOVE 1 TO N                                                        
067700       PERFORM UNTIL N > 6                                                
067800         MOVE INVOICES(N) TO TEMP-HELTAL                                  
067900         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
068000         ADD 1 TO N                                                       
068100       END-PERFORM                                                        
068200     WHEN 3                                                               
068300       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
068400       MOVE 1 TO N                                                        
068500       PERFORM UNTIL N > 6                                                
068600         MOVE NO-CASES(N) TO TEMP-HELTAL                                  
068700         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
068800         ADD 1 TO N                                                       
068900       END-PERFORM                                                        
069000     WHEN 4                                                               
069100       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
069200       MOVE 1 TO N                                                        
069300       PERFORM UNTIL N > 6                                                
069400         MOVE NO-LINES(N) TO TEMP-HELTAL                                  
069500         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
069600         ADD 1 TO N                                                       
069700       END-PERFORM                                                        
069800     WHEN 5                                                               
069900       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
070000       MOVE 1 TO N                                                        
070100       PERFORM UNTIL N > 6                                                
070200         MOVE NEWPARTS(N) TO TEMP-HELTAL                                  
070300         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
070400         ADD 1 TO N                                                       
070500       END-PERFORM                                                        
070600     END-EVALUATE                                                         
070700     .                                                                    
070800     EJECT                                                                
070900 S17-WRITE-CHINA-DATA SECTION.                                            
071000     MOVE DAGENS-VECKA   TO W002-TIAAVV                                   
071100     IF SPAR-IDDC NOT = WS-PREV-IDDC                                      
071200       MOVE SPAR-IDDC TO W002-CTL-IDDC                                    
071300                         WS-PREV-IDDC                                     
071400       PERFORM S21-WRITE-W61268-002-CTL                                   
071500     END-IF                                                               
071600     PERFORM S18-WRITE-HEADER                                             
071700     MOVE 1 TO I                                                          
071800     PERFORM UNTIL I > 4                                                  
071900       PERFORM S19-MOVE-RECORD                                            
072000       PERFORM S20-WRITE-W61268-002                                       
072100       ADD 1 TO I                                                         
072200     END-PERFORM                                                          
072300     .                                                                    
072400     EJECT                                                                
072500 S18-WRITE-HEADER SECTION.                                                
072600     MOVE 'DC'      TO W002-IDDC                                          
072700     MOVE 'ETA DAY' TO W002-TITLE                                         
072800     MOVE 1 TO N                                                          
072900     PERFORM UNTIL N > 6                                                  
073000       MOVE HEADER-TITLE(N) TO W002-REC-ARRAY (N)                         
073100       ADD 1 TO N                                                         
073200     END-PERFORM                                                          
073300                                                                          
073400     MOVE W002-DETAIL  TO  W002-REC                                       
073500                                                                          
073600     MOVE 'INVOICES '  TO RUBRIK-RAD(1)                                   
073700     MOVE 'CASES    '  TO RUBRIK-RAD(2)                                   
073800     MOVE 'LINES    '  TO RUBRIK-RAD(3)                                   
073900     MOVE 'NEW PARTS'  TO RUBRIK-RAD(4)                                   
074000     WRITE W61268-002-REC FROM W002-REC                                   
074100     MOVE SPACE TO W002-REC                                               
074200     .                                                                    
074300     EJECT                                                                
074400 S19-MOVE-RECORD SECTION.                                                 
074500                                                                          
074600     MOVE SPAR-IDDC       TO W002-IDDC                                    
074700     EVALUATE I                                                           
074800     WHEN 1                                                               
074900       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
075000       MOVE 1 TO N                                                        
075100       PERFORM UNTIL N > 6                                                
075200         MOVE INVOICES(N) TO TEMP-HELTAL                                  
075300         MOVE TEMP-HELTAL TO W002-REC-ARRAY (N)                           
075400         ADD 1 TO N                                                       
075500       END-PERFORM                                                        
075600     WHEN 2                                                               
075700       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
075800       MOVE 1 TO N                                                        
075900       PERFORM UNTIL N > 6                                                
076000         MOVE NO-CASES(N) TO TEMP-HELTAL                                  
076100         MOVE TEMP-HELTAL TO W002-REC-ARRAY (N)                           
076200         ADD 1 TO N                                                       
076300       END-PERFORM                                                        
076400     WHEN 3                                                               
076500       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
076600       MOVE 1 TO N                                                        
076700       PERFORM UNTIL N > 6                                                
076800         MOVE NO-LINES(N) TO TEMP-HELTAL                                  
076900         MOVE TEMP-HELTAL TO W002-REC-ARRAY (N)                           
077000         ADD 1 TO N                                                       
077100       END-PERFORM                                                        
077200     WHEN 4                                                               
077300       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
077400       MOVE 1 TO N                                                        
077500       PERFORM UNTIL N > 6                                                
077600         MOVE NEWPARTS(N) TO TEMP-HELTAL                                  
077700         MOVE TEMP-HELTAL TO W002-REC-ARRAY (N)                           
077800         ADD 1 TO N                                                       
077900       END-PERFORM                                                        
078000     END-EVALUATE                                                         
078100     .                                                                    
078200     EJECT                                                                
078300 S20-WRITE-W61268-002  SECTION.                                           
078400                                                                          
078500     MOVE W002-DETAIL  TO  W002-REC                                       
078600     WRITE W61268-002-REC FROM W002-REC                                   
078700     MOVE SPACE TO W002-REC                                               
078800     .                                                                    
078900     EJECT                                                                
079000 S21-WRITE-W61268-002-CTL  SECTION.                                       
079100                                                                          
079200     MOVE W002-CONTROL-REC1 TO W002-REC                                   
079300     WRITE W61268-002-REC FROM W002-REC                                   
079400     MOVE W002-CONTROL-REC2 TO W002-REC                                   
079500     WRITE W61268-002-REC FROM W002-REC                                   
079600     MOVE SPACE TO W002-REC                                               
079700     .                                                                    
079800     EJECT                                                                
079900 S22-WRITE-CHINA-MGMT-DATA SECTION.                                       
080000     MOVE DAGENS-VECKA        TO W003-TIAAVV                              
080100     PERFORM S23-WRITE-MGMT-HEADER                                        
080200     MOVE 1 TO I                                                          
080300     PERFORM UNTIL I > 5                                                  
080400       PERFORM S24-MOVE-MGMT-RECORD                                       
080500       PERFORM S25-WRITE-W61268-003                                       
080600       ADD 1 TO I                                                         
080700     END-PERFORM                                                          
080800     .                                                                    
080900     EJECT                                                                
081000 S23-WRITE-MGMT-HEADER SECTION.                                           
081100     MOVE 'DC'      TO W003-IDDC                                          
081200     MOVE 'CITY'    TO W003-CITY                                          
081300     MOVE 'ETA DAY' TO W003-TITLE                                         
081400     MOVE 1 TO N                                                          
081500     PERFORM UNTIL N > 6                                                  
081600       MOVE HEADER-TITLE(N) TO W003-REC-ARRAY (N)                         
081700       ADD 1 TO N                                                         
081800     END-PERFORM                                                          
081900                                                                          
082000     MOVE W003-DETAIL  TO  W003-REC                                       
082100                                                                          
082200     MOVE 'VALUE    '  TO RUBRIK-RAD(1)                                   
082300     MOVE 'INVOICES '  TO RUBRIK-RAD(2)                                   
082400     MOVE 'CASES    '  TO RUBRIK-RAD(3)                                   
082500     MOVE 'LINES    '  TO RUBRIK-RAD(4)                                   
082600     MOVE 'NEW PARTS'  TO RUBRIK-RAD(5)                                   
082700                                                                          
082800     WRITE W61268-003-REC FROM W003-REC                                   
082900     MOVE SPACE TO W003-REC                                               
083000     .                                                                    
083100     EJECT                                                                
083200 S24-MOVE-MGMT-RECORD SECTION.                                            
083300                                                                          
083400     MOVE SPAR-IDDC       TO W003-IDDC                                    
083500     MOVE SPAR-CITY       TO W003-CITY                                    
083600     EVALUATE I                                                           
083700     WHEN 1                                                               
083800       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
083900       MOVE 1 TO N                                                        
084000       PERFORM UNTIL N > 6                                                
084100         MOVE T-VALUES(N) TO TEMP-DECTAL                                  
084200         MOVE TEMP-DECTAL TO W003-REC-ARRAY (N)                           
084300         ADD 1 TO N                                                       
084400       END-PERFORM                                                        
084500     WHEN 2                                                               
084600       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
084700       MOVE 1 TO N                                                        
084800       PERFORM UNTIL N > 6                                                
084900         MOVE INVOICES(N) TO TEMP-HELTAL                                  
085000         MOVE TEMP-HELTAL TO W003-REC-ARRAY (N)                           
085100         ADD 1 TO N                                                       
085200       END-PERFORM                                                        
085300     WHEN 3                                                               
085400       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
085500       MOVE 1 TO N                                                        
085600       PERFORM UNTIL N > 6                                                
085700         MOVE NO-CASES(N) TO TEMP-HELTAL                                  
085800         MOVE TEMP-HELTAL TO W003-REC-ARRAY (N)                           
085900         ADD 1 TO N                                                       
086000       END-PERFORM                                                        
086100     WHEN 4                                                               
086200       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
086300       MOVE 1 TO N                                                        
086400       PERFORM UNTIL N > 6                                                
086500         MOVE NO-LINES(N) TO TEMP-HELTAL                                  
086600         MOVE TEMP-HELTAL TO W003-REC-ARRAY (N)                           
086700         ADD 1 TO N                                                       
086800       END-PERFORM                                                        
086900     WHEN 5                                                               
087000       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
087100       MOVE 1 TO N                                                        
087200       PERFORM UNTIL N > 6                                                
087300         MOVE NEWPARTS(N) TO TEMP-HELTAL                                  
087400         MOVE TEMP-HELTAL TO W003-REC-ARRAY (N)                           
087500         ADD 1 TO N                                                       
087600       END-PERFORM                                                        
087700     END-EVALUATE                                                         
087800     .                                                                    
087900     EJECT                                                                
088000 S25-WRITE-W61268-003  SECTION.                                           
088100                                                                          
088200     MOVE W003-DETAIL  TO  W003-REC                                       
088300     WRITE W61268-003-REC FROM W003-REC                                   
088400     MOVE SPACE TO W003-REC                                               
088500     .                                                                    
088600     EJECT                                                                
088700 S26-WRITE-W61268-002-003  SECTION.                                       
088800                                                                          
088900     MOVE DAGENS-VECKA           TO W002-TIAAVV                           
089000                                    W003-TIAAVV                           
089100     MOVE RAD-RUBRIK             TO W002-TITLE                            
089200                                    W003-TITLE                            
089300     MOVE SPAR-IDDC              TO W002-IDDC                             
089400                                    W003-IDDC                             
089500     PERFORM                                                              
089600     VARYING INDX FROM +1 BY +1                                           
089700       UNTIL INDX > 6                                                     
089800       MOVE RAD-FAELT (INDX)     TO W002-REC-ARRAY (INDX)                 
089900                                    W003-REC-ARRAY (INDX)                 
090000     END-PERFORM                                                          
090100                                                                          
090200     MOVE W002-DETAIL            TO W002-REC                              
090300     WRITE W61268-002-REC      FROM W002-REC                              
090400     MOVE SPACE                  TO W002-REC                              
090500                                                                          
090600     MOVE SPAR-CITY              TO W003-CITY                             
090700     MOVE W003-DETAIL            TO W003-REC                              
090800     WRITE W61268-003-REC      FROM W003-REC                              
090900     MOVE SPACE                  TO W003-REC                              
091000     .                                                                    
091100     EJECT                                                                
091200 S27-CHECK-WEBDC SECTION.                                                 
091300     CALL WL10WBDC            USING WBDC-AREA                             
091400     IF WBDC-FLWEBDC = 'J'                                                
091500       IF WBDC-KDMFUP = SPAR-KDMFUP                                       
091600         CONTINUE                                                         
091700       ELSE                                                               
091800         MOVE WBDC-KDMFUP        TO SPAR-KDMFUP                           
091900                                    W003-CTL-KDMFUP                       
092000         WRITE W61268-003-REC  FROM W003-CONTROL-REC1                     
092100         WRITE W61268-003-REC  FROM W003-CONTROL-REC2                     
092200       END-IF                                                             
092300     END-IF                                                               
092400     .                                                                    
092500     EJECT                                                                
092600 S42-ASSIGN-TALLYS SECTION.                                               
092700     MOVE IN-SHIST-IDDC   TO WS-IDDC                                      
092800     IF NDC-PACIFIC                                                       
092900       COMPUTE T-VALUES (N) ROUNDED = T-VALUES(N) +                       
093000       (IN-SHIST-PRARTNTO * IN-SHIST-KVAVIS)                              
093100     ELSE                                                                 
093200       IF NDC-CN OR LDC-CN                                                
093300         COMPUTE T-VALUES (N) ROUNDED = T-VALUES(N) +                     
093400         (WDK7-SLAG-PRAVCOST * IN-SHIST-KVAVIS)                           
093500       ELSE                                                               
093600         COMPUTE T-VALUES (N) ROUNDED = T-VALUES(N) +                     
093700         ((IN-SHIST-PRARTNTO / IN-SHIST-PRKURS) * IN-SHIST-KVAVIS)        
093800       END-IF                                                             
093900     END-IF                                                               
094000                                                                          
094100     IF NY-FAKTURA = JA                                                   
094200       ADD 1 TO INVOICES(N)                                               
094300     END-IF                                                               
094400                                                                          
094500     IF NYTT-KOLLI = JA                                                   
094600       ADD 1 TO NO-CASES(N)                                               
094700     END-IF                                                               
094800                                                                          
094900     ADD 1 TO NO-LINES(N)                                                 
095000                                                                          
095100     IF IN-SHIST-FLNYART = JA                                             
095200       ADD 1 TO NEWPARTS(N)                                               
095300     END-IF                                                               
095400     .                                                                    
095500     EJECT                                                                
095600 S99-ABEND SECTION.                                                       
095700                                                                          
095800     SKIP2                                                                
095900     MOVE 'S' TO POSTSUM-OPKOD                                            
096000     CALL POSTSUM USING POSTSUM-PARM                                      
096100     CALL ABEND USING RKOD-ABEND                                          
096200     .                                                                    
096300     EJECT                                                                
096400*    -COPY WY2000P3                                                       
096500 IMS-GU-WDK7-SLAG SECTION.                                                
096600                                                                          
096700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
096800          DELIMITED BY SIZE INTO SSA1                                     
096900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
097000          DELIMITED BY SIZE INTO SSA2                                     
097100     MOVE '  GE' TO GOOD-STATUSCODES                                      
097200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA SSA1 SSA2                 
097300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
097400     PERFORM IMS-STATUSKONTROLL                                           
097500     .                                                                    
097600     EJECT                                                                
097700                                                                          
097800 IMS-GU-WDB601    SECTION.                                                
097900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
098000          DELIMITED BY SIZE INTO SSA1                                     
098100     MOVE '  GE' TO GOOD-STATUSCODES                                      
098200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA SSA1                      
098300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
098400     PERFORM IMS-STATUSKONTROLL                                           
098500     .                                                                    
098600     EJECT                                                                
098700                                                                          
098800 IMS-STATUSKONTROLL SECTION.                                              
098900     SET STATUS-IX TO 1                                                   
099000     SEARCH GOOD-STATUS                                                   
099100       AT END                                                             
099200         CALL FELLOG                                                      
099300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
099400         CONTINUE                                                         
099500     END-SEARCH                                                           
099600     .                                                                    
099700     EJECT                                                                
