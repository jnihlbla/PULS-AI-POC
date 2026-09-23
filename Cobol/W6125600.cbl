000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6125600.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   97/05/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKAPAR UTLISTA PÅ FÖRVÄNTADE FLYGLEVERNSER TILL NDC              
001000*                                                                         
001100*    ÄNDRING:     98 02 24   JOHAN L                                      
001200*                 TILLÄGG AV TOTALRAD                                     
001300*                                                                         
001400*                 99 03 11   JOHAN L                                      
001500*                 JPN / AUS SKALL HA VALUE I SEK                          
001600*                                                                         
001700*                 11 10 20   RAHUL REDDY                                  
001800*                 ADDED REPORTS FOR CHINA                                 
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- SORTERADE INFIL                                            
002800     SELECT W61254                     ASSIGN TO W61256D1.                
002900     SKIP2                                                                
003000*          --- UTISTA                                                     
003100     SELECT W61256-001                 ASSIGN TO W61256D2.                
003200     EJECT                                                                
003300*          --- FOLLOW-UP                                                  
003400     SELECT W61256-002                 ASSIGN TO W61256D3.                
003500     EJECT                                                                
003600*          --- MANAGEMENT FOLLOW-UP                                       
003700     SELECT W61256-003                 ASSIGN TO W61256D4.                
003800     EJECT                                                                
003900*          --- REPORT TO D&P                                              
004000     SELECT W61256-001-DAP             ASSIGN TO W61256D5.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W61254                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W61254      -L.                                                
005100     SKIP3                                                                
005200 FD  W61256-001                                                           
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600 01  W61256-001-RAD              PIC X(121).                              
005700     EJECT                                                                
005800 FD  W61256-002                                                           
005900     RECORDING       V                                                    
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200 01  W61256-002-REC              PIC X(92).                               
006300     EJECT                                                                
006400 FD  W61256-003                                                           
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700     SKIP2                                                                
006800 01  W61256-003-REC              PIC X(117).                              
006900     EJECT                                                                
007000 FD  W61256-001-DAP                                                       
007100     RECORDING       V                                                    
007200     BLOCK CONTAINS  0.                                                   
007300     SKIP2                                                                
007400 01  W61256-001-DAP-RAD          PIC X(125).                              
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007700                                                                          
007800*    -- CHECKED BY WY2000                                                 
007900 01  I                           PIC 9  COMP-3.                           
008000 01  N                           PIC 9  COMP-3.                           
008100 01  INDX                        PIC 9  COMP-3.                           
008200                                                                          
008300 01 TALLYS.                                                               
008400   03 SAVES OCCURS 8 TIMES.                                               
008500       05 RUBRIK-RAD             PIC X(10).                               
008600       05 T-VALUES               PIC S9(7)V99    COMP-3.                  
008700       05 INVOICES               PIC S9(5)       COMP-3.                  
008800       05 NO-CASES               PIC S9(5)       COMP-3.                  
008900       05 NO-LINES               PIC S9(5)       COMP-3.                  
009000       05 NEWPARTS               PIC S9(5)       COMP-3.                  
009100                                                                          
009200 01  SPAR-IDDC                   PIC XX      VALUE SPACE.                 
009300 01  SPAR-ADCITY                 PIC X(25)   VALUE SPACE.                 
009400 01  SPAR-FAKTURA                PIC 9(7)    VALUE 0.                     
009500 01  SPAR-KOLLI                  PIC 9(5)    VALUE 0.                     
009600 01  SPAR-ORDNR5                 PIC 9(5)    VALUE 0.                     
009700 01  SPAR-KUNDNR                 PIC 9(7)    VALUE 0.                     
009800 01  SPAR-KDMFUP                 PIC X(2)    VALUE SPACE.                 
009900 01  NY-FAKTURA                  PIC X       VALUE 'N'.                   
010000 01  NYTT-KOLLI                  PIC X       VALUE 'N'.                   
010100 01  TEMP-DECTAL                 PIC Z(6)9.99.                            
010200 01  TEMP-HELTAL                 PIC Z(9)9.                               
010300 01  WS-PREV-IDDC                PIC X(2)    VALUE SPACE.                 
010400 01  PASSED.                                                              
010500     03 FILLER                   PIC X(4)    VALUE SPACE.                 
010600     03 FILLER                   PIC X(6)    VALUE 'PASSED'.              
010700                                                                          
010800 01  WTOTAL.                                                              
010900     03 FILLER                   PIC X(4)    VALUE SPACE.                 
011000     03 FILLER                   PIC X(6)    VALUE ' TOTAL'.              
011100                                                                          
011200 01  HEADER.                                                              
011300     03  HEADER-DAG OCCURS 8 TIMES.                                       
011400         05  FILLER              PIC X(6)    VALUE SPACES.                
011500         05  HEAD-TIVV           PIC XX.                                  
011600         05  HEAD-SPACE          PIC X       VALUE SPACES.                
011700         05  HEAD-TID            PIC X.                                   
011800     03  HEADER-DATUM OCCURS 7 TIMES.                                     
011900         05  HEAD-TIAA           PIC 99.                                  
012000         05  HEAD-TIMM           PIC 99.                                  
012100         05  HEAD-TIDD           PIC 99.                                  
012200                                                                          
012300 77  IDPGM                       PIC X(8)    VALUE 'W6125600'.            
012400 77  JA                          PIC X       VALUE 'J'.                   
012500 77  NEJ                         PIC X       VALUE 'N'.                   
012600                                                                          
012700 77  W61254-EOF-SW               PIC X       VALUE 'N'.                   
012800     88  END-OF-W61254                       VALUE 'J'.                   
012900     EJECT                                                                
013000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013100 01  FILLER REDEFINES DAGENS-DATUM.                                       
013200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013500     EJECT                                                                
013600*      --- VALID IDDC CODES                                               
013700*                                                                         
013800*01    -COPY WWDC99                                                       
013900       EJECT                                                              
014000 01  DYNAMISKA-SUBPROGRAM.                                                
014100*                                                                         
014200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
014400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
014600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014900     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
015000     SKIP2                                                                
015100*    --- PARAMETRAR TILL ABEND                                            
015200                                                                          
015300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
015500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
015600     SKIP2                                                                
015700 01  FELTEXT.                                                             
015800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
016000     EJECT                                                                
016100*    --- PARAMETRAR TILL DATKORT                                          
016200*                                                                         
016300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61256'.              
016400     SKIP2                                                                
016500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
016600     SKIP2                                                                
016700*01  -COPY WDATKORT                                                       
016800     EJECT                                                                
016900*    --- PARAMETRAR TILL POSTSUM                                          
017000*                                                                         
017100*01  -COPY W0005   -PRE  POSTSUM-                                         
017200     EJECT                                                                
017300*01  -COPY WORKAREA                                                       
017400     EJECT                                                                
017500*01  -COPY WDATAREA                                                       
017600     EJECT                                                                
017700*01  -COPY WL10WBDC                                                       
017800     EJECT                                                                
017900 01  IN-AREA-START               PIC X(24)   VALUE                        
018000                                 'IN-AREA-START  '.                       
018100     SKIP2                                                                
018200                                                                          
018300*01  AREA -COPY W61254     -PRE IN-                                       
018400     EJECT                                                                
018500 01  W001-AREA-START             PIC X(24)   VALUE                        
018600                                 'W001-AREA-START  '.                     
018700     SKIP2                                                                
018800 01  W001-HJALPAREOR.                                                     
018900*                                                                         
019000     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 3.                
019100     03  W001-ANTAL-RADER                                                 
019200                                 PIC 9(3)    VALUE 999.                   
019300     03  W001-MAX-RADER-PER-SIDA                                          
019400                                 PIC 9(3)    VALUE 42.                    
019500     03  W001-MAX-POSITIONER-PER-RAD                                      
019600                                 PIC 9(3)    VALUE 120.                   
019700     03  W001-LISTNR             PIC X(11)   VALUE 'W61256-001'.          
019800     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
019900     EJECT                                                                
020000 01  W001-RAD.                                                            
020100*                                                                         
020200     03  FILLER                  PIC X(121)  VALUE SPACE.                 
020300     EJECT                                                                
020400 01  W001-RUBRIK1.                                                        
020500*                                                                         
020600     03  FILLER                  PIC X(3) VALUE SPACE.                    
020700     03  FILLER                  PIC X(21)                                
020800                                VALUE 'VOLVO CAR PARTS      '.            
020900     03  FILLER                  PIC X(12)                                
021000                                 VALUE 'W61256-001'.                      
021100     03  FILLER                  PIC X(44)                                
021200              VALUE '       EXPECTED AIR FREIGHTS.'.                      
021300     03  FILLER                  PIC X(4) VALUE 'DC '.                    
021400     03  RUBRIK-DC               PIC XX.                                  
021500     03  FILLER                  PIC X(10) VALUE SPACE.                   
021600     03  W001-DATUM              PIC XXBXXBXX.                            
021700     03  FILLER                  PIC X(4) VALUE SPACE.                    
021800     03  FILLER                  PIC X(4)                                 
021900                                 VALUE 'PAGE'.                            
022000     03  W001-SID                PIC Z(4)9.                               
022100     EJECT                                                                
022200 01  W001-DETALJ.                                                         
022300     03  FILLER                  PIC X(10) VALUE SPACE.                   
022400     03  RAD-RUBRIK              PIC X(10).                               
022500     03  FILLER  OCCURS 8.                                                
022600       05  FILLER                PIC XX VALUE SPACE.                      
022700       05  RAD-FAELT             PIC X(10).                               
022800     EJECT                                                                
022900 01  W002-AREA-START             PIC X(24)   VALUE                        
023000                                 'W002-AREA-START  '.                     
023100     SKIP2                                                                
023200 01  W002-CONTROL-REC1.                                                   
023300*                                                                         
023400     03  FILLER                  PIC X(92)   VALUE                        
023500                                 ' ¤DAPW61256-002'.                       
023600     EJECT                                                                
023700 01  W002-CONTROL-REC2.                                                   
023800*                                                                         
023900     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
024000                                                                          
024100     03  W002-CTL-IDDC           PIC X(2)    VALUE SPACE.                 
024200                                                                          
024300     03  FILLER                  PIC X(85)   VALUE SPACE.                 
024400     EJECT                                                                
024500 01  W002-REC.                                                            
024600*                                                                         
024700     03  FILLER                  PIC X(92)   VALUE SPACE.                 
024800     EJECT                                                                
024900 01  W002-DETAIL.                                                         
025000     03  W002-DC                 PIC X(02) VALUE SPACE.                   
025100     03  W002-TITLE              PIC X(10).                               
025200     03  FILLER  OCCURS 8.                                                
025300       05  W002-WEEK-DATA        PIC X(10).                               
025400     EJECT                                                                
025500 01  W003-AREA-START             PIC X(24)   VALUE                        
025600                                 'W003-AREA-START  '.                     
025700     SKIP2                                                                
025800 01  W003-REC.                                                            
025900*                                                                         
026000     03  FILLER                  PIC X(117) VALUE SPACE.                  
026100     EJECT                                                                
026200 01  W003-CONTROL-REC1.                                                   
026300*                                                                         
026400     03  FILLER                  PIC X(92)   VALUE                        
026500                                 ' ¤DAPW61256-003'.                       
026600     EJECT                                                                
026700 01  W003-CONTROL-REC2.                                                   
026800*                                                                         
026900     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
027000                                                                          
027100     03  W003-CTL-KDMFUP         PIC X(2)    VALUE SPACE.                 
027200                                                                          
027300     03  FILLER                  PIC X(85)   VALUE SPACE.                 
027400     EJECT                                                                
027500 01  W003-DETAIL.                                                         
027600     03  W003-DC                 PIC X(02) VALUE SPACE.                   
027700     03  W003-CITY               PIC X(25) VALUE SPACE.                   
027800     03  W003-TITLE              PIC X(10).                               
027900     03  FILLER  OCCURS 8.                                                
028000       05  W003-WEEK-DATA        PIC X(10).                               
028100     EJECT                                                                
028200 01  W004-CONTROL-REC1.                                                   
028300*                                                                         
028400     03  FILLER                  PIC X(121)  VALUE                        
028500                                 ' ¤DAPW61256-001'.                       
028600     EJECT                                                                
028700 01  W004-CONTROL-REC2.                                                   
028800*                                                                         
028900     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
029000                                                                          
029100     03  W004-CTL-IDDC           PIC X(2)    VALUE SPACE.                 
029200                                                                          
029300     03  FILLER                  PIC X(114)  VALUE SPACE.                 
029400     EJECT                                                                
029500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029600     SKIP3                                                                
029700 01  KEYS-FOR-DLI.                                                        
029800     03  W-IDARTNR-X.                                                     
029900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
030000     03  W-IDDC-X.                                                        
030100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
030200                                                                          
030300*    --- STATUS-KOD FRÅN IMS                                              
030400 01  STATUS-WS                   PIC XX.                                  
030500     88  SEGMENT-FOUND                       VALUE '  '.                  
030600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
030700     SKIP2                                                                
030800 01  GOOD-STATUSCODES.                                                    
030900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031000     SKIP3                                                                
031100 01  SSA1                        PIC X(64).                               
031200 01  SSA2                        PIC X(64).                               
031300*    --- IMS FUNCTION CODES                                               
031400*01  -COPY W0003                                                          
031500*    ---  DLI INPUT-OUTPUT AREA                                           
031600 01  DLI-IO-AREA.                                                         
031700     03  IO-AREA                 PIC X(800) VALUE SPACE.                  
031800     SKIP3                                                                
031900                                                                          
032000     03  WDK711   REDEFINES IO-AREA.                                      
032100*        05  -COPY WDK711  -PRE WDK7-                                     
032200                                                                          
032300 LINKAGE SECTION.                                                         
032400                                                                          
032500*01  -COPY W0008  -PRE WDK7-                                              
032600     05  FILLER                  PIC X.                                   
032700 EJECT                                                                    
032800                                                                          
032900 PROCEDURE DIVISION  USING WDK7-PCB.                                      
033000 MAIN SECTION.                                                            
033100     ENTRY 'DLITCBL' USING WDK7-PCB.                                      
033200                                                                          
033300     PERFORM A-INIT                                                       
033400     PERFORM S01-LAES-W61254                                              
033500     PERFORM UNTIL END-OF-W61254                                          
033600       PERFORM B-KONTROLL-NYTT-DC                                         
033700       PERFORM C-KONTROLL-NY-FAKTURA                                      
033800       PERFORM D-KONTROLL-NYTT-KOLLI                                      
033900       PERFORM E-CHECK-INPOST                                             
034000       PERFORM S01-LAES-W61254                                            
034100     END-PERFORM                                                          
034200                                                                          
034300     MOVE SPAR-IDDC              TO WS-IDDC                               
034400                                    WBDC-IDDC                             
034500     PERFORM S27-CHECK-WEBDC                                              
034600     IF NDC-CN OR LDC-CN                                                  
034700       PERFORM S17-WRITE-CHINA-DATA                                       
034800       PERFORM S22-WRITE-CHINA-MGMT-DATA                                  
034900     ELSE                                                                 
035000       PERFORM S12-LIST-UTSKRIFT                                          
035100     END-IF                                                               
035200     PERFORM Z-FINIT                                                      
035300                                                                          
035400     MOVE ZERO TO RETURN-CODE                                             
035500     GOBACK                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 A-INIT SECTION.                                                          
035900                                                                          
036000     OPEN INPUT  W61254                                                   
036100     OPEN OUTPUT W61256-001                                               
036200                 W61256-002                                               
036300                 W61256-003                                               
036400                 W61256-001-DAP                                           
036500                                                                          
036600     INITIALIZE TALLYS                                                    
036700     INITIALIZE W001-DETALJ                                               
036800                W002-DETAIL                                               
036900                W003-DETAIL                                               
037000     SKIP2                                                                
037100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
037200     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
037300     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
037400     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
037500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037600                                                                          
037700     MOVE DAGENS-DATUM TO HEADER-DATUM(1)                                 
037800     MOVE PASSED       TO HEADER-DAG(1)                                   
037900     MOVE WTOTAL       TO HEADER-DAG(8)                                   
038000                                                                          
038100     .                                                                    
038200     EJECT                                                                
038300 B-KONTROLL-NYTT-DC SECTION.                                              
038400     IF SPAR-IDDC = SPACE                                                 
038500       MOVE IN-SHIST-IDDC TO SPAR-IDDC W-IDDC WS-IDDC                     
038600       MOVE IN-SHIST-ADCITY TO SPAR-ADCITY                                
038700       PERFORM S98-WORKDAY                                                
038800     ELSE                                                                 
038900       IF IN-SHIST-IDDC = SPAR-IDDC                                       
039000         CONTINUE                                                         
039100       ELSE                                                               
039200         MOVE SPAR-IDDC      TO WS-IDDC                                   
039300                                WBDC-IDDC                                 
039400         PERFORM S27-CHECK-WEBDC                                          
039500         IF NDC-CN OR LDC-CN                                              
039600           PERFORM S17-WRITE-CHINA-DATA                                   
039700           PERFORM S22-WRITE-CHINA-MGMT-DATA                              
039800           INITIALIZE TALLYS                                              
039900         ELSE                                                             
040000           PERFORM S12-LIST-UTSKRIFT                                      
040100         END-IF                                                           
040200         PERFORM S98-WORKDAY                                              
040300         MOVE IN-SHIST-IDDC   TO SPAR-IDDC W-IDDC WS-IDDC                 
040400         MOVE IN-SHIST-ADCITY TO SPAR-ADCITY                              
040500       END-IF                                                             
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900 C-KONTROLL-NY-FAKTURA SECTION.                                           
041000                                                                          
041100     IF SPAR-FAKTURA = IN-SHIST-IDFAKT                                    
041200       MOVE NEJ TO NY-FAKTURA                                             
041300         IF SPAR-ORDNR5 = IN-SHIST-IDORDNR5                               
041400           IF SPAR-KUNDNR = IN-SHIST-IDKUNDNR                             
041500             CONTINUE                                                     
041600           ELSE                                                           
041700             MOVE ZERO TO SPAR-KOLLI                                      
041800             MOVE IN-SHIST-IDKUNDNR TO SPAR-KUNDNR                        
041900           END-IF                                                         
042000         ELSE                                                             
042100           MOVE ZERO TO SPAR-KOLLI                                        
042200           MOVE IN-SHIST-IDKUNDNR TO SPAR-KUNDNR                          
042300           MOVE IN-SHIST-IDORDNR5 TO SPAR-ORDNR5                          
042400         END-IF                                                           
042500     ELSE                                                                 
042600       MOVE JA TO NY-FAKTURA                                              
042700       MOVE IN-SHIST-IDFAKT   TO SPAR-FAKTURA                             
042800       MOVE IN-SHIST-IDORDNR5 TO SPAR-ORDNR5                              
042900       MOVE IN-SHIST-IDKUNDNR TO SPAR-KUNDNR                              
043000       MOVE ZERO TO SPAR-KOLLI                                            
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 D-KONTROLL-NYTT-KOLLI SECTION.                                           
043500                                                                          
043600     IF SPAR-KOLLI = IN-SHIST-IDKOLLI                                     
043700       MOVE NEJ TO NYTT-KOLLI                                             
043800     ELSE                                                                 
043900       MOVE JA TO NYTT-KOLLI                                              
044000       MOVE IN-SHIST-IDKOLLI TO SPAR-KOLLI                                
044100     END-IF                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 E-CHECK-INPOST SECTION.                                                  
044500                                                                          
044600     IF NDC-CN OR LDC-CN                                                  
044700       MOVE IN-SHIST-IDARTNR         TO W-IDARTNR                         
044800       PERFORM IMS-GU-WDK7-SLAG                                           
044900     END-IF                                                               
045000     EVALUATE TRUE                                                        
045100     WHEN IN-SHIST-TIBERANK <= HEADER-DATUM(1)                            
045200       MOVE 1 TO N                                                        
045300       PERFORM S42-ASSIGN-TALLYS                                          
045400     WHEN IN-SHIST-TIBERANK = HEADER-DATUM(2)                             
045500       MOVE 2 TO N                                                        
045600       PERFORM S42-ASSIGN-TALLYS                                          
045700     WHEN IN-SHIST-TIBERANK = HEADER-DATUM(3)                             
045800       MOVE 3 TO N                                                        
045900       PERFORM S42-ASSIGN-TALLYS                                          
046000     WHEN IN-SHIST-TIBERANK = HEADER-DATUM(4)                             
046100       MOVE 4 TO N                                                        
046200       PERFORM S42-ASSIGN-TALLYS                                          
046300     WHEN IN-SHIST-TIBERANK = HEADER-DATUM(5)                             
046400       MOVE 5 TO N                                                        
046500       PERFORM S42-ASSIGN-TALLYS                                          
046600     WHEN IN-SHIST-TIBERANK = HEADER-DATUM(6)                             
046700       MOVE 6 TO N                                                        
046800       PERFORM S42-ASSIGN-TALLYS                                          
046900     WHEN IN-SHIST-TIBERANK = HEADER-DATUM(7)                             
047000       MOVE 7 TO N                                                        
047100       PERFORM S42-ASSIGN-TALLYS                                          
047200     WHEN OTHER                                                           
047300       CONTINUE                                                           
047400     END-EVALUATE                                                         
047500* HÖÄR NEDAN SUMMERAS ALLA POSTER I KOLUMN 8                              
047600     MOVE 8 TO N                                                          
047700     PERFORM S42-ASSIGN-TALLYS                                            
047800     .                                                                    
047900     EJECT                                                                
048000 Z-FINIT SECTION.                                                         
048100     CLOSE W61254                                                         
048200           W61256-001                                                     
048300           W61256-002                                                     
048400           W61256-003                                                     
048500           W61256-001-DAP                                                 
048600                                                                          
048700     MOVE 'S' TO POSTSUM-OPKOD                                            
048800     CALL POSTSUM USING POSTSUM-PARM                                      
048900     .                                                                    
049000     EJECT                                                                
049100 S01-LAES-W61254  SECTION.                                                
049200     READ W61254 INTO IN-AREA                                             
049300     AT END                                                               
049400        MOVE HIGH-VALUE TO IN-AREA                                        
049500        SET END-OF-W61254 TO TRUE                                         
049600                                                                          
049700     NOT AT END                                                           
049800        MOVE 'W61254' TO POSTSUM-FDNAMN                                   
049900        MOVE 'W61256D1' TO POSTSUM-DDNAMN2                                
050000        MOVE IN-SHIST-IDDC TO POSTSUM-TRANSTYP                            
050100        CALL POSTSUM USING POSTSUM-PARM                                   
050200     END-READ                                                             
050300     .                                                                    
050400     EJECT                                                                
050500 S12-LIST-UTSKRIFT SECTION.                                               
050600     PERFORM S13-SKRIV-RUBRIKER                                           
050700     MOVE 1 TO I                                                          
050800     PERFORM UNTIL I > 5                                                  
050900       PERFORM S16-SKAPA-RAD                                              
051000       IF WBDC-FLWEBDC = 'J'                                              
051100         PERFORM S26-WRITE-W61256-002-003                                 
051200       ELSE                                                               
051300         PERFORM S15-SKRIV-W61256-001                                     
051400       END-IF                                                             
051500       ADD 1 TO I                                                         
051600     END-PERFORM                                                          
051700     INITIALIZE TALLYS                                                    
051800     .                                                                    
051900     EJECT                                                                
052000 S13-SKRIV-RUBRIKER SECTION.                                              
052100                                                                          
052200     MOVE 1 TO W001-SIDRAKNARE                                            
052300     MOVE 3 TO W001-SKIP                                                  
052400     MOVE +7 TO W001-ANTAL-RADER                                          
052500     MOVE DAGENS-DATUM TO W001-DATUM                                      
052600     MOVE W001-SIDRAKNARE TO W001-SID                                     
052700     MOVE SPAR-IDDC TO RUBRIK-DC                                          
052800                       WS-IDDC                                            
052900                                                                          
053000     IF WBDC-FLWEBDC = 'J'                                                
053100       IF SPAR-IDDC NOT = WS-PREV-IDDC                                    
053200         MOVE SPAR-IDDC          TO W002-CTL-IDDC                         
053300                                    WS-PREV-IDDC                          
053400         PERFORM S21-WRITE-W61256-002-CTL                                 
053500       END-IF                                                             
053600     ELSE                                                                 
053700       IF CDC-SE                                                          
053800         IF SPAR-IDDC NOT = WS-PREV-IDDC                                  
053900           MOVE SPAR-IDDC        TO W004-CTL-IDDC                         
054000                                    WS-PREV-IDDC                          
054100           PERFORM S28-WRITE-W61256-001-DAP-CTL                           
054200         END-IF                                                           
054300         WRITE W61256-001-DAP-RAD FROM W001-RUBRIK1                       
054400       ELSE                                                               
054500         WRITE W61256-001-RAD     FROM W001-RUBRIK1 AFTER PAGE            
054600       END-IF                                                             
054700     END-IF                                                               
054800                                                                          
054900     PERFORM S14-SKAPA-HEADER                                             
055000                                                                          
055100     IF WBDC-FLWEBDC = 'J'                                                
055200       PERFORM S26-WRITE-W61256-002-003                                   
055300     ELSE                                                                 
055400       IF CDC-SE                                                          
055500         WRITE W61256-001-DAP-RAD FROM W001-RAD                           
055600       ELSE                                                               
055700         WRITE W61256-001-RAD     FROM W001-RAD AFTER W001-SKIP           
055800       END-IF                                                             
055900       MOVE SPACE                   TO W001-RAD                           
056000     END-IF                                                               
056100                                                                          
056200     .                                                                    
056300     EJECT                                                                
056400 S14-SKAPA-HEADER SECTION.                                                
056500     MOVE 'ETA DAY' TO RAD-RUBRIK                                         
056600     MOVE 1 TO N                                                          
056700     PERFORM UNTIL N > 8                                                  
056800       MOVE HEADER-DAG(N) TO RAD-FAELT(N)                                 
056900       ADD 1 TO N                                                         
057000     END-PERFORM                                                          
057100                                                                          
057200     MOVE W001-DETALJ  TO  W001-RAD                                       
057300                                                                          
057400     MOVE SPAR-IDDC      TO WS-IDDC                                       
057500     IF NDC-PACIFIC OR CDC-SE                                             
057600        MOVE 'VALUE SEK' TO RUBRIK-RAD(1)                                 
057700     ELSE                                                                 
057800        MOVE 'VALUE  $ ' TO RUBRIK-RAD(1)                                 
057900     END-IF                                                               
058000     MOVE 'INVOICES '  TO RUBRIK-RAD(2)                                   
058100     MOVE 'CASES    '  TO RUBRIK-RAD(3)                                   
058200     MOVE 'LINES    '  TO RUBRIK-RAD(4)                                   
058300     MOVE 'NEW PARTS'  TO RUBRIK-RAD(5)                                   
058400     .                                                                    
058500     EJECT                                                                
058600 S15-SKRIV-W61256-001  SECTION.                                           
058700                                                                          
058800     MOVE W001-DETALJ  TO  W001-RAD                                       
058900     IF CDC-SE                                                            
059000       WRITE W61256-001-DAP-RAD FROM W001-RAD                             
059100     ELSE                                                                 
059200       WRITE W61256-001-RAD     FROM W001-RAD AFTER W001-SKIP             
059300     END-IF                                                               
059400     MOVE SPACE TO W001-RAD                                               
059500     ADD  +1 TO W001-ANTAL-RADER                                          
059600     .                                                                    
059700     EJECT                                                                
059800 S16-SKAPA-RAD SECTION.                                                   
059900                                                                          
060000     EVALUATE I                                                           
060100     WHEN 1                                                               
060200       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
060300       MOVE 1 TO N                                                        
060400       PERFORM UNTIL N > 8                                                
060500         MOVE T-VALUES(N) TO TEMP-DECTAL                                  
060600         MOVE TEMP-DECTAL TO RAD-FAELT(N)                                 
060700         ADD 1 TO N                                                       
060800       END-PERFORM                                                        
060900     WHEN 2                                                               
061000       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
061100       MOVE 1 TO N                                                        
061200       PERFORM UNTIL N > 8                                                
061300         MOVE INVOICES(N) TO TEMP-HELTAL                                  
061400         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
061500         ADD 1 TO N                                                       
061600       END-PERFORM                                                        
061700     WHEN 3                                                               
061800       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
061900       MOVE 1 TO N                                                        
062000       PERFORM UNTIL N > 8                                                
062100         MOVE NO-CASES(N) TO TEMP-HELTAL                                  
062200         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
062300         ADD 1 TO N                                                       
062400       END-PERFORM                                                        
062500     WHEN 4                                                               
062600       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
062700       MOVE 1 TO N                                                        
062800       PERFORM UNTIL N > 8                                                
062900         MOVE NO-LINES(N) TO TEMP-HELTAL                                  
063000         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
063100         ADD 1 TO N                                                       
063200       END-PERFORM                                                        
063300     WHEN 5                                                               
063400       MOVE RUBRIK-RAD(I) TO RAD-RUBRIK                                   
063500       MOVE 1 TO N                                                        
063600       PERFORM UNTIL N > 8                                                
063700         MOVE NEWPARTS(N) TO TEMP-HELTAL                                  
063800         MOVE TEMP-HELTAL TO RAD-FAELT(N)                                 
063900         ADD 1 TO N                                                       
064000       END-PERFORM                                                        
064100     END-EVALUATE                                                         
064200     .                                                                    
064300     EJECT                                                                
064400 S17-WRITE-CHINA-DATA SECTION.                                            
064500     IF SPAR-IDDC NOT = WS-PREV-IDDC                                      
064600       MOVE SPAR-IDDC TO W002-CTL-IDDC                                    
064700                          WS-PREV-IDDC                                    
064800       PERFORM S21-WRITE-W61256-002-CTL                                   
064900     END-IF                                                               
065000     PERFORM S18-WRITE-HEADER                                             
065100     MOVE 1 TO I                                                          
065200     PERFORM UNTIL I > 4                                                  
065300       PERFORM S19-MOVE-RECORD                                            
065400       PERFORM S20-WRITE-W61256-002                                       
065500       ADD 1 TO I                                                         
065600     END-PERFORM                                                          
065700     .                                                                    
065800     EJECT                                                                
065900 S18-WRITE-HEADER SECTION.                                                
066000     MOVE SPAR-IDDC TO W002-DC                                            
066100     MOVE 'ETA DAY' TO W002-TITLE                                         
066200     MOVE 1 TO N                                                          
066300     PERFORM UNTIL N > 8                                                  
066400       MOVE HEADER-DAG(N) TO W002-WEEK-DATA(N)                            
066500       ADD 1 TO N                                                         
066600     END-PERFORM                                                          
066700                                                                          
066800     MOVE W002-DETAIL  TO  W002-REC                                       
066900                                                                          
067000     MOVE 'INVOICES '  TO RUBRIK-RAD(1)                                   
067100     MOVE 'CASES    '  TO RUBRIK-RAD(2)                                   
067200     MOVE 'LINES    '  TO RUBRIK-RAD(3)                                   
067300     MOVE 'NEW PARTS'  TO RUBRIK-RAD(4)                                   
067400                                                                          
067500     WRITE W61256-002-REC FROM W002-REC                                   
067600     MOVE SPACE TO W002-REC                                               
067700     .                                                                    
067800     EJECT                                                                
067900 S19-MOVE-RECORD SECTION.                                                 
068000                                                                          
068100     MOVE SPAR-IDDC       TO W002-DC                                      
068200     EVALUATE I                                                           
068300     WHEN 1                                                               
068400       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
068500       MOVE 1 TO N                                                        
068600       PERFORM UNTIL N > 8                                                
068700         MOVE INVOICES(N) TO TEMP-HELTAL                                  
068800         MOVE TEMP-HELTAL TO W002-WEEK-DATA(N)                            
068900         ADD 1 TO N                                                       
069000       END-PERFORM                                                        
069100     WHEN 2                                                               
069200       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
069300       MOVE 1 TO N                                                        
069400       PERFORM UNTIL N > 8                                                
069500         MOVE NO-CASES(N) TO TEMP-HELTAL                                  
069600         MOVE TEMP-HELTAL TO W002-WEEK-DATA(N)                            
069700         ADD 1 TO N                                                       
069800       END-PERFORM                                                        
069900     WHEN 3                                                               
070000       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
070100       MOVE 1 TO N                                                        
070200       PERFORM UNTIL N > 8                                                
070300         MOVE NO-LINES(N) TO TEMP-HELTAL                                  
070400         MOVE TEMP-HELTAL TO W002-WEEK-DATA(N)                            
070500         ADD 1 TO N                                                       
070600       END-PERFORM                                                        
070700     WHEN 4                                                               
070800       MOVE RUBRIK-RAD(I) TO W002-TITLE                                   
070900       MOVE 1 TO N                                                        
071000       PERFORM UNTIL N > 8                                                
071100         MOVE NEWPARTS(N) TO TEMP-HELTAL                                  
071200         MOVE TEMP-HELTAL TO W002-WEEK-DATA(N)                            
071300         ADD 1 TO N                                                       
071400       END-PERFORM                                                        
071500     END-EVALUATE                                                         
071600     .                                                                    
071700     EJECT                                                                
071800 S20-WRITE-W61256-002  SECTION.                                           
071900                                                                          
072000     MOVE W002-DETAIL  TO  W002-REC                                       
072100     WRITE W61256-002-REC FROM W002-REC                                   
072200     MOVE SPACE TO W002-REC                                               
072300     .                                                                    
072400     EJECT                                                                
072500 S21-WRITE-W61256-002-CTL  SECTION.                                       
072600                                                                          
072700     MOVE W002-CONTROL-REC1 TO W002-REC                                   
072800     WRITE W61256-002-REC FROM W002-REC                                   
072900     MOVE W002-CONTROL-REC2 TO W002-REC                                   
073000     WRITE W61256-002-REC FROM W002-REC                                   
073100     MOVE SPACE TO W002-REC                                               
073200     .                                                                    
073300     EJECT                                                                
073400 S22-WRITE-CHINA-MGMT-DATA SECTION.                                       
073500     PERFORM S23-WRITE-MGMT-HEADER                                        
073600     MOVE 1 TO I                                                          
073700     PERFORM UNTIL I > 5                                                  
073800       PERFORM S24-MOVE-MGMT-RECORD                                       
073900       PERFORM S25-WRITE-W61256-003                                       
074000       ADD 1 TO I                                                         
074100     END-PERFORM                                                          
074200     .                                                                    
074300     EJECT                                                                
074400 S23-WRITE-MGMT-HEADER SECTION.                                           
074500     MOVE SPAR-IDDC   TO W003-DC                                          
074600     MOVE SPAR-ADCITY TO W003-CITY                                        
074700     MOVE 'ETA DAY'   TO W003-TITLE                                       
074800     MOVE 1 TO N                                                          
074900     PERFORM UNTIL N > 8                                                  
075000       MOVE HEADER-DAG(N) TO W003-WEEK-DATA(N)                            
075100       ADD 1 TO N                                                         
075200     END-PERFORM                                                          
075300                                                                          
075400     MOVE W003-DETAIL  TO  W003-REC                                       
075500                                                                          
075600     MOVE 'VALUE    '  TO RUBRIK-RAD(1)                                   
075700     MOVE 'INVOICES '  TO RUBRIK-RAD(2)                                   
075800     MOVE 'CASES    '  TO RUBRIK-RAD(3)                                   
075900     MOVE 'LINES    '  TO RUBRIK-RAD(4)                                   
076000     MOVE 'NEW PARTS'  TO RUBRIK-RAD(5)                                   
076100                                                                          
076200     WRITE W61256-003-REC FROM W003-REC                                   
076300     MOVE SPACE TO W003-REC                                               
076400     .                                                                    
076500     EJECT                                                                
076600 S24-MOVE-MGMT-RECORD SECTION.                                            
076700                                                                          
076800     MOVE SPAR-IDDC       TO W003-DC                                      
076900     MOVE SPAR-ADCITY     TO W003-CITY                                    
077000     EVALUATE I                                                           
077100     WHEN 1                                                               
077200       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
077300       MOVE 1 TO N                                                        
077400       PERFORM UNTIL N > 8                                                
077500         MOVE T-VALUES(N) TO TEMP-DECTAL                                  
077600         MOVE TEMP-DECTAL TO W003-WEEK-DATA(N)                            
077700         ADD 1 TO N                                                       
077800       END-PERFORM                                                        
077900     WHEN 2                                                               
078000       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
078100       MOVE 1 TO N                                                        
078200       PERFORM UNTIL N > 8                                                
078300         MOVE INVOICES(N) TO TEMP-HELTAL                                  
078400         MOVE TEMP-HELTAL TO W003-WEEK-DATA(N)                            
078500         ADD 1 TO N                                                       
078600       END-PERFORM                                                        
078700     WHEN 3                                                               
078800       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
078900       MOVE 1 TO N                                                        
079000       PERFORM UNTIL N > 8                                                
079100         MOVE NO-CASES(N) TO TEMP-HELTAL                                  
079200         MOVE TEMP-HELTAL TO W003-WEEK-DATA(N)                            
079300         ADD 1 TO N                                                       
079400       END-PERFORM                                                        
079500     WHEN 4                                                               
079600       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
079700       MOVE 1 TO N                                                        
079800       PERFORM UNTIL N > 8                                                
079900         MOVE NO-LINES(N) TO TEMP-HELTAL                                  
080000         MOVE TEMP-HELTAL TO W003-WEEK-DATA(N)                            
080100         ADD 1 TO N                                                       
080200       END-PERFORM                                                        
080300     WHEN 5                                                               
080400       MOVE RUBRIK-RAD(I) TO W003-TITLE                                   
080500       MOVE 1 TO N                                                        
080600       PERFORM UNTIL N > 8                                                
080700         MOVE NEWPARTS(N) TO TEMP-HELTAL                                  
080800         MOVE TEMP-HELTAL TO W003-WEEK-DATA(N)                            
080900         ADD 1 TO N                                                       
081000       END-PERFORM                                                        
081100     END-EVALUATE                                                         
081200     .                                                                    
081300     EJECT                                                                
081400 S25-WRITE-W61256-003  SECTION.                                           
081500                                                                          
081600     MOVE W003-DETAIL  TO  W003-REC                                       
081700     WRITE W61256-003-REC FROM W003-REC                                   
081800     MOVE SPACE TO W003-REC                                               
081900     .                                                                    
082000     EJECT                                                                
082100 S26-WRITE-W61256-002-003  SECTION.                                       
082200                                                                          
082300     MOVE RAD-RUBRIK             TO W002-TITLE                            
082400                                    W003-TITLE                            
082500     PERFORM                                                              
082600     VARYING INDX FROM +1 BY +1                                           
082700       UNTIL INDX > 8                                                     
082800       MOVE RAD-FAELT (INDX)     TO W002-WEEK-DATA (INDX)                 
082900                                    W003-WEEK-DATA (INDX)                 
083000     END-PERFORM                                                          
083100     MOVE SPAR-IDDC              TO W002-DC                               
083200                                    W003-DC                               
083300     MOVE W002-DETAIL            TO W002-REC                              
083400     WRITE W61256-002-REC      FROM W002-REC                              
083500     MOVE SPACE                  TO W002-REC                              
083600                                                                          
083700     MOVE SPAR-ADCITY            TO W003-CITY                             
083800     MOVE W003-DETAIL            TO W003-REC                              
083900     WRITE W61256-003-REC      FROM W003-REC                              
084000     MOVE SPACE                  TO W003-REC                              
084100     .                                                                    
084200     EJECT                                                                
084300 S27-CHECK-WEBDC SECTION.                                                 
084400                                                                          
084500     CALL WL10WBDC            USING WBDC-AREA                             
084600                                                                          
084700     IF WBDC-FLWEBDC = 'J'                                                
084800       IF WBDC-KDMFUP = SPAR-KDMFUP                                       
084900         CONTINUE                                                         
085000       ELSE                                                               
085100         MOVE WBDC-KDMFUP        TO SPAR-KDMFUP                           
085200                                    W003-CTL-KDMFUP                       
085300         WRITE W61256-003-REC  FROM W003-CONTROL-REC1                     
085400         WRITE W61256-003-REC  FROM W003-CONTROL-REC2                     
085500       END-IF                                                             
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
085900 S28-WRITE-W61256-001-DAP-CTL SECTION.                                    
086000                                                                          
086100     MOVE W004-CONTROL-REC1      TO W001-RAD                              
086200     WRITE W61256-001-DAP-RAD  FROM W001-RAD                              
086300     MOVE W004-CONTROL-REC2      TO W001-RAD                              
086400     WRITE W61256-001-DAP-RAD  FROM W001-RAD                              
086500     MOVE SPACE                  TO W001-RAD                              
086600     .                                                                    
086700     EJECT                                                                
086800 S42-ASSIGN-TALLYS SECTION.                                               
086900     MOVE IN-SHIST-IDDC   TO WS-IDDC                                      
087000     IF NDC-PACIFIC                                                       
087100       COMPUTE T-VALUES (N) ROUNDED = T-VALUES(N) +                       
087200       (IN-SHIST-PRARTNTO * IN-SHIST-KVAVIS)                              
087300     ELSE                                                                 
087400       IF NDC-CN OR LDC-CN                                                
087500         COMPUTE T-VALUES (N) ROUNDED = T-VALUES(N) +                     
087600         (WDK7-SLAG-PRAVCOST * IN-SHIST-KVAVIS)                           
087700       ELSE                                                               
087800         COMPUTE T-VALUES (N) ROUNDED = T-VALUES(N) +                     
087900         ((IN-SHIST-PRARTNTO / IN-SHIST-PRKURS) * IN-SHIST-KVAVIS)        
088000       END-IF                                                             
088100     END-IF                                                               
088200     IF NY-FAKTURA = JA                                                   
088300       ADD 1 TO INVOICES(N)                                               
088400       ADD 1 TO NO-CASES(N)                                               
088500     ELSE                                                                 
088600       IF NYTT-KOLLI = JA                                                 
088700         ADD 1 TO NO-CASES(N)                                             
088800       END-IF                                                             
088900     END-IF                                                               
089000     ADD 1 TO NO-LINES(N)                                                 
089100     IF IN-SHIST-FLNYART = JA                                             
089200       ADD 1 TO NEWPARTS(N)                                               
089300     END-IF                                                               
089400     .                                                                    
089500     EJECT                                                                
089600 S98-WORKDAY SECTION.                                                     
089700                                                                          
089800     MOVE 002           TO WORK-KDCALL                                    
089900     MOVE IN-SHIST-IDDC TO WORK-IDDC                                      
090000     MOVE DAGENS-DATUM TO WORK-TIAAMMDD-FOM                               
090100                                                                          
090200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
090300                                                                          
090400     MOVE 2 TO I                                                          
090500     PERFORM UNTIL I > 7                                                  
090600       COMPUTE WORK-KVWORKD = I - 1                                       
090700       CALL WORKDAY USING WORK-KDCALL                                     
090800                 WORK-DATE-AREA WORK-KDSVAR                               
090900       IF WORK-KDSVAR-OK                                                  
091000         MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO DAT-I-TIDATUM                 
091100*        MOVE WORK-TIAAMMDD-NEXT-WEEK    TO DAT-I-TIDATUM                 
091200         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
091300                         DAT-O-TIDATUM DAT-KDSVAR                         
091400         IF DAT-KDSVAR-OK                                                 
091500           MOVE DAT-TIVV  TO HEAD-TIVV(I)                                 
091600           MOVE DAT-TID   TO HEAD-TID(I)                                  
091700           MOVE DAT-TIAA  TO HEAD-TIAA(I)                                 
091800           MOVE DAT-TIMM  TO HEAD-TIMM(I)                                 
091900           MOVE DAT-TIDD  TO HEAD-TIDD(I)                                 
092000         ELSE                                                             
092100           MOVE 'FEL I WDATKONV' TO FELTEXT-STR                           
092200           DISPLAY FELTEXT                                                
092300           MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                        
092400           PERFORM S99-ABEND                                              
092500         END-IF                                                           
092600       ELSE                                                               
092700         MOVE 'FEL I WORKDAY' TO FELTEXT-STR                              
092800         DISPLAY FELTEXT                                                  
092900         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                          
093000         PERFORM S99-ABEND                                                
093100       END-IF                                                             
093200       ADD 1 TO I                                                         
093300     END-PERFORM                                                          
093400     .                                                                    
093500     EJECT                                                                
093600 S99-ABEND SECTION.                                                       
093700                                                                          
093800     SKIP2                                                                
093900     MOVE 'S' TO POSTSUM-OPKOD                                            
094000     CALL POSTSUM USING POSTSUM-PARM                                      
094100     CALL ABEND USING RKOD-ABEND                                          
094200     .                                                                    
094300 IMS-GU-WDK7-SLAG SECTION.                                                
094400                                                                          
094500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
094600          DELIMITED BY SIZE INTO SSA1                                     
094700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
094800          DELIMITED BY SIZE INTO SSA2                                     
094900     MOVE '  GE' TO GOOD-STATUSCODES                                      
095000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA SSA1 SSA2                 
095100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
095200     PERFORM IMS-STATUSKONTROLL                                           
095300     .                                                                    
095400     EJECT                                                                
095500                                                                          
095600 IMS-STATUSKONTROLL SECTION.                                              
095700     SET STATUS-IX TO 1                                                   
095800     SEARCH GOOD-STATUS                                                   
095900       AT END                                                             
096000         CALL FELLOG                                                      
096100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
096200         CONTINUE                                                         
096300     END-SEARCH                                                           
096400     .                                                                    
096500     EJECT                                                                
