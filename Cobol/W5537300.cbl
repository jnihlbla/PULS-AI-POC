000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5537300.                                                
000301 AUTHOR.         ANDERS HENRIKSSON                                        
000401*DATE-WRITTEN.   2012-07-24                                               
000501                                                                          
000601*    REMARKS                                                              
000701*                                                                         
000801*    FUNKTION:                                                            
000901*        PGM CONTROLS                                                     
001001*        - FILE FROM VCC PURCHASING(PT=985)                               
001101*        - FILE WITH NEW SASPRISER (PT=985 AND IDLEVNR=1002)              
001201*        READS                                                            
001301*        -WDK6                                                            
001401*        -WDF1                                                            
001501*        -WDR2                                                            
001601*        -WDD3                                                            
001701*        -WDK7                                                            
001801*        -WDP3                                                            
001901*        OUTPUT                                                           
002001*        -ERRORLIST                                                       
002101*        -MESSAGELIST                                                     
002201*        -CORRECT TRANSACTIONS == > SCREEN 5111                           
002300*      KONTROLLER:                                                        
002400*        - DC EQUAL DCS-SDC OR DCS-NDC                                    
002501*        - FIL-DATUM ÄR GILTIGT                                           
002600*        - FIL-ENHET FÖR ST,100ST,1000ST O 10ST GODK.,DVS                 
002700*          FIL-KDANTENH = 1,2,3 OCH 7 (BEHANDLAS HÄR,                     
002800*                                  OM FLER UTÖKA)                         
002900*        - FIL-PRARTBEL EJ NOLL                                           
003000*        - FIL-ARTIKEL FINNS PÅ ARTIKELREG                                
003100*        - FIL-ARTIKEL EJ UTGÅNGEN                                        
003200*        - ART-RAD(PRIS) (PÅ 24-SEGMENTET)                                
003300*          - DET NYASTE ÄR EJ PRELIMINÄRT                                 
003400*          - DET AKTUELLA FÖR UPPDAT. EJ LIKA MED FIL-PRARTBEL            
003500*          - DET ÄLDSTA ENDA INLEV.MÄRKTA FÅR EJ TAS BORT                 
003600*          - INLEV.MÄRKTA DATUM EJ SENARE ÄN FIL-DATUM                    
003701*        - FIL-PRARTBEL EJ NOLL                                           
003801*        - FIL-LEVERANTÖR FINNS PÅ LEVERANTÖRSREGISTRET                   
003901*        - HEMTAGNINGSFAKTOR FINNS PÅ LEVERANTÖRSREGISTRET                
004001*        - VALUTAKOD FINNS UPPLAGD PÅ REG. FÖR AKTUELLT ÅR                
004101*        - BESTÄLLNINGSPRIS EJ STÖRRE ÄN 400.000 KR                       
004201*        - PRISFÖRÄNDRING INOM GODKÄND RAM                                
004301*                                                                         
004401*      FEL PÅ FELLISTAN (TRANSARNA STOPPAS FÖR PRISUPPDATERING):          
004501*        - EJ GILTIGT DATUM                                               
004601*        - KDANTENH (KDENH-BEST) ÄR EJ 1 2 3 7                            
004701*        - ISOKOD FEL GAMMAL EMU-VALUTA-KOD                               
004801*        - ISOKOD FEL VALUTAKOD NUMERISK                                  
004901*        - PRELIMINÄRT PRIS FINNS FÖR DENNA LEV.                          
005001*        - FILDATUM EJ STÖRRE ÄN INLEV.MÄRKT RAD                          
005101*        - ARTIKELNS PRISER ÄR NOLL                                       
005201*        - LEVERANTÖR SAKNAS PÅ LEV.REG                                   
005301*        - VAL.KOD FINNS EJ FÖR AKT. ÅR                                   
005401*        - BEST.PRIS STÖRRE ÄN 400000 KR                                  
005501*      EJ FELRAD PÅ FELLISTAN MEN UPPDATERING STOPPAS:                    
005601*        - SKAPA EJ FELRAD   (WUT74-BETEXT)                               
005701*          - SAMMA RADPRIS FINNS REDAN PÅ RAD 1                           
005801*          - EJ FÖRHANDLAT PRIS                                           
005901*          - PRARTBEL PÅ FIL ÄR 0                                         
006001*          - ARTIKELN SAKNAS PÅ ARTREG                                    
006101*          - ARTIKELN ÄR UTGÅNGEN                                         
006201*          - RENAULT-LEV HAR ARTIKEL DÄR PS EJ ÄR 31 - 39                 
006301*                                                                         
006401*      WARNING LIST:                                                      
006501*        - PRISSÄNKN. MER ÄN 50 %                                         
006601*        - PRISHÖJN. MER 100 %                                            
006701*                                                                         
006801     SKIP3                                                                
006901 ENVIRONMENT DIVISION.                                                    
007001 INPUT-OUTPUT SECTION.                                                    
007101 FILE-CONTROL.                                                            
007201     SKIP2                                                                
007301*          --- FILE FROM VCC PURCHASE                                     
007401     SELECT W55372                     ASSIGN TO W55373D1.                
007501*    SELECT W09286                     ASSIGN TO W55373D1.                
007601     SKIP2                                                                
007701*          --- ERRORLIST VCC                                              
007801     SELECT W55374                     ASSIGN TO W55373D2.                
007901     SKIP2                                                                
008001*          --- WARNINGLIST VCC                                            
008101     SELECT W55375                     ASSIGN TO W55373D3.                
008201     SKIP2                                                                
008301*          --- CHECKED RECORDS VCC PURCHASE                               
008401     SELECT W55373                     ASSIGN TO W55373D6.                
008501     EJECT                                                                
008601 DATA DIVISION.                                                           
008701                                                                          
008801 FILE SECTION.                                                            
008901     SKIP2                                                                
009001 FD  W55372                                                               
009101     RECORDING       F                                                    
009201     BLOCK CONTAINS  0.                                                   
009301                                                                          
009401*01  -COPY W55372     -PRE INFIL-  -L.                                    
009501     SKIP2                                                                
009601                                                                          
009701 FD  W55374                                                               
009801     RECORDING       V                                                    
009901     BLOCK CONTAINS  0.                                                   
010001*01  POST -COPY W55374   -PRE  W55374-  -L.                               
010101                                                                          
010201 FD  W55375                                                               
010301     RECORDING       V                                                    
010401     BLOCK CONTAINS  0.                                                   
010501*01  POST -COPY W55375   -PRE  W55375-  -L.                               
010601                                                                          
010701 FD  W55373                                                               
010801     RECORDING       F                                                    
010901     BLOCK CONTAINS  0.                                                   
011001                                                                          
011101*01  POST -COPY W55373   -PRE  W553-  -L.                                 
011201     EJECT                                                                
011301 WORKING-STORAGE SECTION.                                                 
011401                                                                          
011501*    -COPY WY2000W1                                                       
011601     SKIP3                                                                
011701 77  IDPGM                       PIC X(8)    VALUE 'W5537300'.            
011801 77  YES                         PIC X       VALUE 'Y'.                   
011901 77  NOO                         PIC X       VALUE 'N'.                   
012001 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
012101 77  SLUTA-LAES                  PIC X       VALUE 'N'.                   
012201 77  INLEV-FINNS                 PIC X       VALUE 'N'.                   
012301 77  FIRST-PRICE                 PIC X       VALUE 'N'.                   
012302 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
012401 77  W55372-EOF-SW               PIC X       VALUE 'N'.                   
012501     88  END-OF-W55372                       VALUE 'Y'.                   
012601                                                                          
012701 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0   COMP-3.        
012801 01  SPRL-TIPRLIST               PIC 9(6).                                
012901 01  OLD-PRARTBES                PIC S9(7)V9(2) VALUE +0   COMP-3.        
013001 01  W-PRKURS                    PIC S9(5)V9(5) VALUE +0   COMP-3.        
013101 01  W-REVALUTA                  PIC S9(5)      VALUE +0   COMP-3.        
013201 01  INFIL-TIPRLIST              PIC 9(6).                                
013301 01  WS-PRARTBEL                 PIC S9(8)V9(5) VALUE +0   COMP-3.        
013401 01  NEW-PRARTBES                PIC S9(8)V9(3) VALUE +0   COMP-3.        
013501 01  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
013601 01  W-KVPB                      PIC S9(5)V9    VALUE +0   COMP-3.        
013701 01  W-KVLS                      PIC S9(7)      VALUE +0   COMP-3.        
013801 01  W-IDLAND-PREV-ERR           PIC X(2)       VALUE SPACE.              
013901 01  W-IDLAND-PREV-WARN          PIC X(2)       VALUE SPACE.              
014001                                                                          
014101 01  MAX-PRARTBES            PIC S9(7)V9(2) VALUE +400000 COMP-3.         
014201 01  PRISANDRING                 PIC 9(8)V9(10) VALUE  0.                 
014301 01  W-TIAAVV                    PIC 9(4)       VALUE ZERO.               
014401                                                                          
014501 01  W-TIPRLIST                  PIC 9(6)       VALUE ZERO.               
014601 01  DAGENS-DATUM                PIC 9(6)       VALUE ZERO.               
014701 01  FILLER REDEFINES DAGENS-DATUM.                                       
014801     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014901     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015001     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015101                                                                          
015700     EJECT                                                                
015800 01  DYNAMISKA-SUBPROGRAM.                                                
015900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
016400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016410     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
016500     SKIP3                                                                
016600 01  NYCKLAR-TILL-DLI.                                                    
016700     03  W-IDARTNR-X.                                                     
016800         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
016901     03  W-KDSEGKEY-X.                                                    
017001         05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                       
017100     03  W-IDLEVNR-X.                                                     
017200         05    W-IDLEVNR     PIC X(5)    VALUE SPACE.                     
017300     03  W-IDLAND-X.                                                      
017400         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
018200     03  W-IDSKYLT-X.                                                     
018300         05    W-IDSKYLT     PIC X(3)    VALUE 'S  '.                     
018400     03  W-IDDC-X.                                                        
018500         05 W-IDDC               PIC X(2).                                
018601     03  W-IDDC-B6-X.                                                     
018701         05 W-IDDC-B6            PIC X(2).                                
019301     03  W-KDARBTYP-X.                                                    
019401         05  W-KDARBTYP          PIC X(8)    VALUE 'ANSK    '.            
019501     03  W-IDPERSON-X.                                                    
019601         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
020101     SKIP3                                                                
020201                                                                          
020301*    --- PARAMETRAR TILL ABEND                                            
020401                                                                          
020501 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
020601 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
020701     EJECT                                                                
020801*    --- PARAMETRAR TILL DATKORT                                          
020901 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W55312'.              
021001                                                                          
021101 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
021201     SKIP3                                                                
021301*01  -COPY WDATKORT                                                       
021401     EJECT                                                                
021501 01  WDATAREA                    PIC X(8)    VALUE 'WDATAREA'.            
021601                                                                          
021701*01       -COPY WDATAREA                                                  
021801     EJECT                                                                
021802*01       -COPY W510CURR                                                  
021803     EJECT                                                                
021901*    --- PARAMETRAR TILL POSTSUM                                          
022001*                                                                         
022101*01  -COPY W0005   -PRE  POSTSUM-                                         
022201     EJECT                                                                
022301 01  TEST-KDVALISO       PIC X(3).                                        
022401                                                                          
022501*01  FILLER  -COPY WWISOEMU  -RED TEST-KDVALISO                           
022601     EJECT                                                                
022701                                                                          
022801* WARNING LIST                                                            
022901 01  UT-CONTROL-WARN.                                                     
023001     03  FILLER                  PIC X(15)   VALUE                        
023101                                 ' ¤DAPW55375-002'.                       
023201     EJECT                                                                
023301 01  UT-CONTROL-LAND.                                                     
023401     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
023501     03  UT-CTL-IDLANDX2         PIC X(2)    VALUE SPACE.                 
023601     EJECT                                                                
023701                                                                          
023801 01  WARNING-AREA.                                                        
023901     03  WW-HEADER.                                                       
024001         05 FILLER              PIC X(6)  VALUE 'PARTNO'.                 
024101         05 FILLER              PIC X     VALUE ';'.                      
024201         05 FILLER              PIC X(6)  VALUE 'REASON'.                 
024301         05 FILLER              PIC X     VALUE ';'.                      
024401         05 FILLER              PIC X(4)  VALUE 'NAME'.                   
024501         05 FILLER              PIC X     VALUE ';'.                      
024601         05 FILLER              PIC X(5)  VALUE 'SUPNO'.                  
024701         05 FILLER              PIC X     VALUE ';'.                      
024801         05 FILLER              PIC X(10) VALUE 'ORDPRICE/1'.             
024901         05 FILLER              PIC X     VALUE ';'.                      
025001         05 FILLER              PIC X(3)  VALUE 'CUR'.                    
025101         05 FILLER              PIC X     VALUE ';'.                      
025201         05 FILLER              PIC X(4)  VALUE 'DATE'.                   
025301         05 FILLER              PIC X     VALUE ';'.                      
025401         05 FILLER              PIC X(9)  VALUE 'OLD ORDPR'.              
025501         05 FILLER              PIC X     VALUE ';'.                      
025601         05 FILLER              PIC X(4)  VALUE 'NEED'.                   
025701         05 FILLER              PIC X     VALUE ';'.                      
025801         05 FILLER              PIC X(5)  VALUE 'STOCK'.                  
025901         05 FILLER              PIC X     VALUE ';'.                      
026001         05 FILLER              PIC X(2)  VALUE 'DC'.                     
026101         05 FILLER              PIC X     VALUE ';'.                      
026201         05 FILLER              PIC X(10) VALUE 'PURCH NAME'.             
026301         05 FILLER              PIC X     VALUE ';'.                      
026401         05 FILLER              PIC X(8)  VALUE 'PURCH NO'.               
026501         05 FILLER              PIC X     VALUE ';'.                      
026601         05 FILLER              PIC X(5)  VALUE 'EMAIL'.                  
026701         05 FILLER              PIC X     VALUE ';'.                      
026801* ERRORLIST                                                               
026901 01  UT-CONTROL-ERR.                                                      
027001     03  FILLER                  PIC X(15)   VALUE                        
027101                                 ' ¤DAPW55375-001'.                       
027201     EJECT                                                                
027301                                                                          
027401 01  ERROR-AREA.                                                          
027501     03  WE-HEADER.                                                       
027601         05 FILLER              PIC X(6)  VALUE 'PARTNO'.                 
027701         05 FILLER              PIC X     VALUE ';'.                      
027801         05 FILLER              PIC X(6)  VALUE 'REASON'.                 
027901         05 FILLER              PIC X     VALUE ';'.                      
028001         05 FILLER              PIC X(5)  VALUE 'SUPNO'.                  
028101         05 FILLER              PIC X     VALUE ';'.                      
028201         05 FILLER              PIC X(10) VALUE 'ORDPRICE/1'.             
028301         05 FILLER              PIC X     VALUE ';'.                      
028401         05 FILLER              PIC X(3)  VALUE 'CUR'.                    
028501         05 FILLER              PIC X     VALUE ';'.                      
028601         05 FILLER              PIC X(4)  VALUE 'DATE'.                   
028701         05 FILLER              PIC X     VALUE ';'.                      
028801         05 FILLER              PIC X(3)  VALUE 'EMB'.                    
028901         05 FILLER              PIC X     VALUE ';'.                      
029001         05 FILLER              PIC X(2)  VALUE 'DC'.                     
029101         05 FILLER              PIC X     VALUE ';'.                      
029201         05 FILLER              PIC X(10) VALUE 'PURCH NAME'.             
029301         05 FILLER              PIC X     VALUE ';'.                      
029401         05 FILLER              PIC X(8)  VALUE 'PURCH NO'.               
029501         05 FILLER              PIC X     VALUE ';'.                      
029601         05 FILLER              PIC X(5)  VALUE 'EMAIL'.                  
029701         05 FILLER              PIC X     VALUE ';'.                      
029801                                                                          
029901 01  FILLER                      PIC X(24) VALUE 'WIN-AREA'.              
030001*01  AREA -COPY W55372        -PRE WIN-                                   
030101                                                                          
031001 01  FILLER                      PIC X(24) VALUE 'WUT-AREA'.              
031101*01  AREA -COPY W55373        -PRE WUT-                                   
031201                                                                          
031301 01  FILLER                      PIC X(24) VALUE 'WUT74-AREA'.            
031401*01  AREA -COPY W55374        -PRE WUT74-                                 
031501                                                                          
031601 01  FILLER                      PIC X(24) VALUE 'WUT75-AREA'.            
031701*01  AREA -COPY W55375        -PRE WUT75-                                 
031800     EJECT                                                                
031900******************************************************************        
032000*                                                                         
032100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032200*                                                                         
032300******************************************************************        
032400                                                                          
032500 01  IMS-WS.                                                              
032600     03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.           
032700     SKIP3                                                                
032800*                        **** STATUS-KOD FRÅN IMS ****                    
032900     03    STATUS-WS           PIC XX.                                    
033000         88    SEGMENT-FOUND               VALUE '  '.                    
033100         88    SEGMENT-MISSING             VALUE 'GE'.                    
033200         88    SEGMENT-FOUND-EXISTS        VALUE 'II'.                    
033300     SKIP3                                                                
033400     03    GOOD-STATUSCODES.                                              
033500         05    GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
033600     SKIP3                                                                
033700 01  SSA1                      PIC X(128).                                
033800 01  SSA2                      PIC X(128).                                
033901 01  SSA3                      PIC X(128).                                
034000     EJECT                                                                
034100*                        **** IMS FUNKTIONSKODER ****                     
034200*01    -COPY W0003                                                        
034300     EJECT                                                                
034700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDF101'.           
034800 01  DLI-IO-WDF101.                                                       
034900*    03  -COPY WDF101                                                     
035000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDF102'.           
035100 01  DLI-IO-WDF102.                                                       
035200*    03  -COPY WDF102 -PRE LEV-                                           
035300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601'.           
035400 01  DLI-IO-WDK601.                                                       
035500*    03  -COPY WDK601                                                     
035600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611'.           
035700 01  DLI-IO-WDK611.                                                       
035800*    03  -COPY WDK611                                                     
035900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK621'.           
036000 01  DLI-IO-WDK621.                                                       
036100*    03  -COPY WDK621                                                     
036200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDD311'.           
036300 01  DLI-IO-WDD311.                                                       
036400*    03  -COPY WDD311                                                     
036500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
036600 01  DLI-IO-WDK701.                                                       
036700*    03  -COPY WDK701                                                     
036800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
036900 01  DLI-IO-WDK711.                                                       
037000*    03  -COPY WDK711                                                     
037101 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK712'.           
037201 01  DLI-IO-WDK712.                                                       
037301*    03  -COPY WDK712                                                     
037401 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK722'.           
037501 01  DLI-IO-WDK722.                                                       
037601*    03  -COPY WDK722                                                     
037701 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK724'.           
037801 01  DLI-IO-WDK724.                                                       
037901*    03  -COPY WDK724                                                     
038000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB601'.           
038100 01  DLI-IO-WDB601.                                                       
038200*    03  -COPY WDB601                                                     
038301 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDP311'.           
038401 01  DLI-IO-WDP311.                                                       
038501*    03  -COPY WDP311                                                     
038600     EJECT                                                                
039001                                                                          
039100 LINKAGE SECTION.                                                         
039200*01    -COPY W0008     -PRE WDK6-                                         
039300     05  FILLER                  PIC X(13).                               
039400                                                                          
039500*01    -COPY W0008     -PRE LEV-                                          
039600     05  FILLER                  PIC X(5).                                
039700                                                                          
040100*01    -COPY W0008     -PRE WDD3-                                         
040200     05  FILLER                  PIC X(8).                                
040300                                                                          
040400*01    -COPY W0008     -PRE WDK7-                                         
040500      05 FILLER                  PIC X(18).                               
040600                                                                          
040700*01    -COPY W0008     -PRE WDB6-                                         
040800      05 FILLER                  PIC X(18).                               
040901*01    -COPY W0008     -PRE WDP3-                                         
041001      05 FILLER                  PIC X.                                   
041101                                                                          
041201*01    -COPY W0008     -PRE 9305-                                         
041301      05 FILLER                  PIC X.                                   
041400     EJECT                                                                
041500 PROCEDURE DIVISION  USING WDK6-PCB LEV-PCB WDD3-PCB                      
041600                           WDK7-PCB WDB6-PCB WDP3-PCB 9305-PCB.           
041700     ENTRY 'DLITCBL' USING WDK6-PCB LEV-PCB WDD3-PCB                      
041800                           WDK7-PCB WDB6-PCB WDP3-PCB 9305-PCB.           
041900     PERFORM A-INIT                                                       
042000                                                                          
042100     PERFORM S01-LAS-W55372                                               
042200     PERFORM UNTIL END-OF-W55372                                          
042300      IF WIN-IDPTYP = '985'                                               
042400       MOVE SPACE                  TO WUT74-BETEXT                        
042500                                      WUT75-BETEXT                        
042600       PERFORM B-FORMELLA-KONTROLLER-INPOST                               
042700                                                                          
042800       IF WUT74-BETEXT   = SPACE                                          
042900         PERFORM C-BERAKNA-STYCKEPRISER                                   
043000         MOVE WIN-IDARTNR TO W-IDARTNR                                    
043100                                                                          
043200         PERFORM IMS-GU-WLARTC01                                          
043300          IF SEGMENT-FOUND                                                
043400            IF ART-KDERS-UTG = 0                                          
043500               PERFORM E-READ-REST-OF-ARTREG-SEGMENT                      
043600               IF WUT74-BETEXT = SPACE                                    
043700                  PERFORM F-BEHANDLA-LEVERANTOR                           
043800                  IF WUT74-BETEXT = SPACE                                 
043900                    PERFORM G-BEHANDLA-VALUTAKURS                         
044000                    IF WUT74-BETEXT = SPACE                               
044101                      PERFORM I-CHECK-WDK712                              
044201                      IF WUT74-BETEXT = SPACE                             
044301                        PERFORM H-CREATE-OK-RECORD                        
044400                        IF WUT75-BETEXT NOT = SPACE                       
044500                          PERFORM S05-CREATE-W55375                       
044600                        END-IF                                            
044700                      END-IF                                              
044801                    END-IF                                                
044900                  END-IF                                                  
045000               END-IF                                                     
045100            ELSE                                                          
045200              MOVE 'PART SUPERSEDED'        TO WUT74-BETEXT               
045300              PERFORM S03-CREATE-W55374                                   
045400            END-IF                                                        
045500          ELSE                                                            
045600            MOVE 'PART MISSING IN PULS'     TO WUT74-BETEXT               
045701            PERFORM S03-CREATE-W55374                                     
045801          END-IF                                                          
045900       END-IF                                                             
046000      END-IF                                                              
046100      PERFORM S01-LAS-W55372                                              
046200     END-PERFORM                                                          
046300     PERFORM Z-FINIT                                                      
046400                                                                          
046500     MOVE ZERO TO RETURN-CODE                                             
046600     GOBACK                                                               
046700     .                                                                    
046800     EJECT                                                                
046901                                                                          
047000 A-INIT SECTION.                                                          
047100     OPEN INPUT  W55372                                                   
047200                                                                          
047300     OPEN OUTPUT W55373                                                   
047400                 W55374                                                   
047500                 W55375                                                   
047600                                                                          
047700     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
047800     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
047900                              W-TIAAVV(1:2)                               
048000     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
048100     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
048200     MOVE D-VECKA          TO W-TIAAVV(3:2)                               
048300     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
048400     MOVE SPACE            TO WUT74-BETEXT                                
048501                                                                          
048601     MOVE FUNCTION CURRENT-DATE (3:4) TO W-DATE-AAMM                      
048800     .                                                                    
048900     EJECT                                                                
049001                                                                          
049100 B-FORMELLA-KONTROLLER-INPOST SECTION.                                    
049200*    VALIDATE DC                                                          
049301     MOVE 'N'            TO INLEV-FINNS                                   
049401     MOVE WIN-IDDC       TO W-IDDC-B6                                     
049501     MOVE WIN-IDARTNR    TO W-IDARTNR                                     
049601     PERFORM IMS-GU-WDB601                                                
049701     IF SEGMENT-FOUND                                                     
049801       IF DCS-SDC OR DCS-NDC                                              
049901         MOVE DCS-IDLANDX2 TO W-IDLAND                                    
050001       ELSE                                                               
050101         MOVE 'DC NOT VALID    '  TO WUT74-BETEXT                         
050201         PERFORM S03-CREATE-W55374                                        
050301       END-IF                                                             
050401     ELSE                                                                 
050501       MOVE 'DC DO NOT EXIST    '  TO WUT74-BETEXT                        
050601       PERFORM S03-CREATE-W55374                                          
050701     END-IF                                                               
050801                                                                          
050900*    VALIDATE DATE                                                        
051000                                                                          
051100     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
051200     MOVE WIN-TIPRLIST            TO INFIL-TIPRLIST                       
051300     MOVE INFIL-TIPRLIST          TO DAT-I-TIDATUM                        
051400     CALL WDATKONV USING             DAT-KDDATFORM                        
051500                                     DAT-I-TIDATUM                        
051600                                     DAT-O-TIDATUM                        
051700                                     DAT-KDSVAR                           
051800     IF DAT-KDSVAR-FEL                                                    
051900       MOVE 'WRONG DATE      '    TO WUT74-BETEXT                         
052000       PERFORM S03-CREATE-W55374                                          
052100     END-IF                                                               
052200                                                                          
052300     IF WIN-KDANTENH NOT = '1' AND '2' AND '3' AND '7'                    
052400       MOVE 'KDENH BEST IS NO 1 2 3 7'  TO WUT74-BETEXT                   
052500       PERFORM S03-CREATE-W55374                                          
052600     END-IF                                                               
052700                                                                          
052800     IF WIN-PRARTBEL = +0                                                 
052900       MOVE 'ZERO PRICE'                TO WUT74-BETEXT                   
053001       PERFORM S03-CREATE-W55374                                          
053100     END-IF                                                               
053200                                                                          
053301     MOVE DCS-KDVALISO      TO CURR-KDVALISO-HUV                          
053401     MOVE WIN-KDVALISO      TO WS-KDVALISO                                
053501     IF CURR-KDVALISO-HUV = WIN-KDVALISO                                  
053601       MOVE 1 TO W-PRKURS                                                 
053701       MOVE 1 TO W-REVALUTA                                               
053801     ELSE                                                                 
053901       MOVE WIN-KDVALISO       TO CURR-KDVALISO-ROW                       
054902       MOVE 'M'                TO CURR-KDVALTYP                           
054903       MOVE W-DATE-AAMM        TO CURR-TIAAMM                             
054904       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
054905       IF CURR-KDSVAR = ' '                                               
054906         MOVE CURR-PRKURS-NEW  TO W-PRKURS                                
054907         MOVE CURR-REVALUTA-TO TO W-REVALUTA                              
054908       ELSE                                                               
054910         MOVE 'CURRENCY CODE NOT VALID   '  TO WUT74-BETEXT               
054911         PERFORM S03-CREATE-W55374                                        
054920       END-IF                                                             
055001     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055301                                                                          
055400 C-BERAKNA-STYCKEPRISER SECTION.                                          
055500     IF WS-KDVALISO = 'GBP'                                               
055600***  PUND HAR TRE DECIMALER                                               
055700       COMPUTE WS-PRARTBEL = WIN-PRARTBEL / 1000                          
055800     ELSE                                                                 
055900       COMPUTE WS-PRARTBEL = WIN-PRARTBEL / 100                           
056000***  ALLA ÖVRIGA WIN-PRARTBEL HAR TVÅ DECIMALER                           
056100     END-IF                                                               
056200                                                                          
056300     IF WIN-KDANTENH = '2'                                                
056400****   ENHETEN ÄR 100                                                     
056500       COMPUTE WS-PRARTBEL = WS-PRARTBEL / 100                            
056600     END-IF                                                               
056700                                                                          
056800     IF WIN-KDANTENH = '3'                                                
056900****   ENHETEN ÄR 1000                                                    
057000       COMPUTE WS-PRARTBEL = WS-PRARTBEL / 1000                           
057100     END-IF                                                               
057200                                                                          
057300     IF WIN-KDANTENH = '7'                                                
057400****   ENHETEN ÄR 10  (NEDCAR)                                            
057500       COMPUTE WS-PRARTBEL = WS-PRARTBEL / 10                             
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057901                                                                          
058000 E-READ-REST-OF-ARTREG-SEGMENT SECTION.                                   
058100     PERFORM IMS-GU-WDK701                                                
058200     IF SEGMENT-FOUND                                                     
058301       MOVE WIN-IDDC                  TO W-IDDC                           
058401       PERFORM IMS-GU-WDK711                                              
058501       IF SEGMENT-FOUND                                                   
058600         MOVE ZERO                    TO W-KVPB                           
058700                                         W-KVLS                           
058801                                         W-RETULF                         
058900         COMPUTE W-KVLS =                                                 
059000          SLAG-KVLS + SLAG-KVEFRS                                         
059100         MOVE ZERO                TO OLD-PRARTBES                         
059200         MOVE SLAG-KVPB-REF       TO W-KVPB                               
059300         PERFORM IMS-GNP-WDK722                                           
059400          IF SEGMENT-FOUND                                                
059500            IF XLAG-KDAVT = +0                                            
059600               MOVE 'NO AGREEMENT'  TO WUT74-BETEXT                       
059700               PERFORM S03-CREATE-W55374                                  
059800            END-IF                                                        
059900          END-IF                                                          
060000***   DÄREFTER, KONTROLL OM PRISRAD REDAN ÄR UPPDATERAD                   
060100***   ELLER ATT PREL-RAD EJ FINNS (GAMLA APU, NÄSTAN UTGÅNGEN)            
060200         IF WUT74-BETEXT = SPACE                                          
060300           PERFORM EA-CHECK-EXIST                                         
060400         END-IF                                                           
060500                                                                          
060600*      OM FELFRITT, KONTROLLERA OM NYUPPLÄGG ELLER                        
060700*      UPPDATERING ÄR TILLÅTEN FÖR ANGIVET DATUM                          
060800         IF WUT74-BETEXT = SPACE                                          
060900           PERFORM EB-CHECK-IF-UPDATE-OK                                  
061000         END-IF                                                           
061101       END-IF                                                             
061200     END-IF                                                               
061300     .                                                                    
061400     EJECT                                                                
061501                                                                          
061600 EA-CHECK-EXIST SECTION.                                                  
061701                                                                          
061800     PERFORM IMS-GNP-WDK724                                               
061900     PERFORM UNTIL SEGMENT-MISSING OR WUT74-BETEXT NOT = SPACE            
062000       MOVE SPRL-DAPRLIST-9KOMPL TO SPRL-TIPRLIST                         
062100       COMPUTE SPRL-TIPRLIST = 99999999 - SPRL-DAPRLIST-9KOMPL            
062200       MOVE INFIL-TIPRLIST    TO TMP1-YYMMDD                              
062300       MOVE SPRL-TIPRLIST     TO TMP2-YYMMDD                              
062400       PERFORM WY2000P1                                                   
062500       IF (WS-PRARTBEL  = SPRL-PRARTBEL-PR) AND                           
062600          (WIN-IDLEVNR  = SPRL-IDLEVNR-PR)  AND                           
062700          (WS-KDVALISO  = SPRL-KDVALISO)    AND                           
062800          (WIN-KDFPKPRI = SPRL-KDFPKPRI)    AND                           
062900          (TMP1-YYMMDD  = TMP2-YYMMDD)                                    
063000         MOVE 'PRICE ALREADY EXISTS' TO WUT74-BETEXT                      
063101         PERFORM S03-CREATE-W55374                                        
063200       END-IF                                                             
063300       PERFORM IMS-GNP-WDK724                                             
063400     END-PERFORM                                                          
063500     .                                                                    
063600     EJECT                                                                
063701                                                                          
063800 EB-CHECK-IF-UPDATE-OK  SECTION.                                          
063900     PERFORM IMS-GNP-WDK724-FIRST                                         
064000     MOVE 1 TO INDX                                                       
064100     MOVE NOO TO SLUTA-LAES                                               
064200     PERFORM UNTIL SEGMENT-MISSING OR SLUTA-LAES = YES OR                 
064300                   WUT74-BETEXT NOT = SPACE                               
064400       MOVE SPRL-DAPRLIST-9KOMPL TO SPRL-TIPRLIST                         
064500       COMPUTE SPRL-TIPRLIST = 99999999 - SPRL-DAPRLIST-9KOMPL            
064600       MOVE INFIL-TIPRLIST  TO TMP1-YYMMDD                                
064700       MOVE SPRL-TIPRLIST   TO TMP2-YYMMDD                                
064800       PERFORM WY2000P1                                                   
064900                                                                          
065000       IF  WIN-IDLEVNR = SPRL-IDLEVNR-PR                                  
065100         IF (WS-PRARTBEL  = SPRL-PRARTBEL-PR) AND                         
065200            (WIN-IDLEVNR  = SPRL-IDLEVNR-PR)  AND                         
065300            (WS-KDVALISO  = SPRL-KDVALISO)    AND                         
065400            (WIN-KDFPKPRI = SPRL-KDFPKPRI)    AND                         
065500            (TMP1-YYMMDD >= TMP2-YYMMDD)                                  
065600            IF INDX = 1                                                   
065700              MOVE 'PRICE ALREADY EXISTS' TO WUT74-BETEXT                 
065801              PERFORM S03-CREATE-W55374                                   
065900            END-IF                                                        
066000         ELSE                                                             
066100           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
066201             IF SPRL-SUINLEV-PR > 0                                       
066301                MOVE 'Y'  TO INLEV-FINNS                                  
066401             END-IF                                                       
066500             ADD 1 TO INDX                                                
066600           ELSE                                                           
066700             IF SPRL-SUINLEV-PR > 0                                       
066801                MOVE 'Y'  TO INLEV-FINNS                                  
066901             END-IF                                                       
067000             IF SPRL-SUINLEV-PR > 0 AND                                   
067100               (SPRL-IDLEVNR-PR = WIN-IDLEVNR)                            
067200             MOVE 'DATE IS NOT HIGHER THEN INBOUND ROW  ' TO              
067300                    WUT74-BETEXT                                          
067400               PERFORM S03-CREATE-W55374                                  
067500             ELSE                                                         
067600               IF INFIL-TIPRLIST = SPRL-TIPRLIST AND                      
067700                                SPRL-SUINLEV-PR = 0                       
067800                 MOVE YES TO SLUTA-LAES                                   
067900               ELSE                                                       
068000                 ADD 1 TO INDX                                            
068100               END-IF                                                     
068200             END-IF                                                       
068300           END-IF                                                         
068400         END-IF                                                           
068500       END-IF                                                             
068600       PERFORM IMS-GNP-WDK724                                             
068700     END-PERFORM                                                          
068800     .                                                                    
068900     EJECT                                                                
069001                                                                          
069100 F-BEHANDLA-LEVERANTOR SECTION.                                           
069200     IF W-RETULF = 0.0000                                                 
069300       MOVE 1.0000                TO W-RETULF                             
069400       MOVE WIN-IDLEVNR           TO W-IDLEVNR                            
069500       PERFORM IMS-GU-WDF101                                              
069600       IF SEGMENT-FOUND                                                   
069700         MOVE DCS-IDLANDX2 TO W-IDLAND                                    
069800         PERFORM IMS-GNP-WDF102                                           
069900         IF SEGMENT-FOUND                                                 
070000           IF LEV-TULL-TITULF > DAGENS-DATUM                              
070100             MOVE LEV-TULL-RETULF-2  TO W-RETULF                          
070200           ELSE                                                           
070300             MOVE LEV-TULL-RETULF-1  TO W-RETULF                          
070400           END-IF                                                         
070500         END-IF                                                           
070600       ELSE                                                               
070700         MOVE 'SUPPLIER MISSING ON REGISTER' TO WUT74-BETEXT              
070800         PERFORM S03-CREATE-W55374                                        
070900       END-IF                                                             
071000     END-IF                                                               
071100     .                                                                    
071200     EJECT                                                                
071301                                                                          
071400 G-BEHANDLA-VALUTAKURS SECTION.                                           
071500*    CHECK EXCHANGE RATE FOR SPECIFIED CURRENCY       **                  
071600                                                                          
072500     IF WUT74-BETEXT = SPACE                                              
072600       COMPUTE NEW-PRARTBES ROUNDED = WS-PRARTBEL * W-RETULF *            
072700                                     W-PRKURS / W-REVALUTA                
072800       IF NEW-PRARTBES > MAX-PRARTBES                                     
072900         MOVE 'PRICE BIGGER THAN 400000  ' TO                             
073000                                   WUT74-BETEXT                           
073100         PERFORM S03-CREATE-W55374                                        
073200       END-IF                                                             
073300     END-IF                                                               
073400                                                                          
073500     IF WUT74-BETEXT = SPACE                                              
073600       COMPUTE PRISANDRING ROUNDED =                                      
073700                             WS-PRARTBEL * W-RETULF * W-PRKURS /          
073800                             W-REVALUTA / OLD-PRARTBES                    
073900       ON SIZE ERROR                                                      
074000         MOVE 999.99                   TO PRISANDRING                     
074100       END-COMPUTE                                                        
074200                                                                          
074300*  VARNING    0.20 <------OK-----> 10.00  VARNING                         
074400           IF PRISANDRING < 0.5                                           
074501             IF INLEV-FINNS  = 'Y'                                        
074601                MOVE 'D. > 50% '      TO WUT75-BETEXT                     
074701             END-IF                                                       
074800           END-IF                                                         
074900                                                                          
075000           IF PRISANDRING > 2.00                                          
075101             IF INLEV-FINNS  = 'Y'                                        
075200                MOVE 'I. > 100%'      TO WUT75-BETEXT                     
075300             END-IF                                                       
075400           END-IF                                                         
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075801                                                                          
075900 I-CHECK-WDK712 SECTION.                                                  
076001     PERFORM IMS-GU-WDK701                                                
076101     IF SEGMENT-FOUND                                                     
076201       MOVE WIN-IDDC       TO W-IDDC-B6                                   
076301       PERFORM IMS-GU-WDB601                                              
076401       IF SEGMENT-FOUND                                                   
076501         MOVE DCS-IDLANDX2 TO W-IDLAND                                    
076601         PERFORM IMS-GNP-WDK712                                           
076701         IF SEGMENT-FOUND                                                 
076801           CONTINUE                                                       
076901         ELSE                                                             
077001           MOVE 'PART NOT CORRECT CREATED    ' TO WUT74-BETEXT            
077101           PERFORM S03-CREATE-W55374                                      
077201         END-IF                                                           
077301       END-IF                                                             
077401     END-IF                                                               
077500     .                                                                    
077601     EJECT                                                                
077701                                                                          
077801 H-CREATE-OK-RECORD SECTION.                                              
077900     MOVE WIN-IDARTNR              TO WUT-IDARTNR                         
078001     MOVE WIN-IDDC                 TO WUT-IDDC                            
078101     MOVE WIN-IDLEVNR              TO WUT-IDLEVNR                         
078201     MOVE W-RETULF                 TO WUT-RETULF                          
078301     MOVE WS-PRARTBEL              TO WUT-PRARTBEL-PR                     
078401     MOVE WS-KDVALISO              TO WUT-KDVALISO                        
078501     MOVE INFIL-TIPRLIST           TO WUT-TIREGDAT                        
078601     MOVE YES                      TO WUT-FLHUVLEV                        
078700     MOVE WIN-IDUSER               TO WUT-IDUSER                          
078800     IF WIN-IDPTYP = '985' AND WIN-IDUSER = 'INKOP   '                    
078900       MOVE WIN-KDFPKPRI           TO WUT-KDFPKPRI                        
079000     ELSE                                                                 
079100       MOVE ' '                    TO WUT-KDFPKPRI                        
079200     END-IF                                                               
079300     PERFORM S02-WRITE-W55373                                             
079400     .                                                                    
079500     EJECT                                                                
079601                                                                          
079701 J-READ-WDP311 SECTION.                                                   
079801*READ PURCHAGE NAME,NUMBER AND EMAIL                                      
079901     PERFORM IMS-GU-WDK611                                                
080001      IF SEGMENT-FOUND                                                    
080101        IF CLAG-IDINK NOT = SPACE                                         
080201          IF CLAG-IDINK (1:3) NUMERIC                                     
080301             MOVE CLAG-IDINK (1:3)    TO W-IDPERSON                       
080401          ELSE                                                            
080500             IF CLAG-IDINK (2:3) NUMERIC                                  
080600                MOVE CLAG-IDINK (2:3) TO W-IDPERSON                       
080700             ELSE                                                         
080800                MOVE ZERO             TO W-IDPERSON                       
080900             END-IF                                                       
081000          END-IF                                                          
081100        END-IF                                                            
081201                                                                          
081301        MOVE W-IDPERSON    TO WUT74-IDINK                                 
081401                              WUT75-IDINK                                 
081501      END-IF                                                              
081600     PERFORM IMS-GU-WDP311                                                
081700      IF SEGMENT-FOUND                                                    
081800        MOVE PERS-IDNAMN   TO WUT74-IDNAMN                                
081901                              WUT75-IDNAMN                                
082000        MOVE PERS-IDMAIL   TO WUT74-IDMAIL                                
082101                              WUT75-IDMAIL                                
082201        MOVE PERS-IDPERSON TO WUT74-IDINK                                 
082301                              WUT75-IDINK                                 
082401      ELSE                                                                
082501        MOVE SPACE         TO WUT74-IDNAMN                                
082601                              WUT75-IDNAMN                                
082701                              WUT74-IDMAIL                                
082801                              WUT75-IDMAIL                                
082901        MOVE ZERO          TO WUT74-IDINK                                 
083001                              WUT75-IDINK                                 
083100      END-IF                                                              
083200      .                                                                   
083300      EJECT                                                               
083400                                                                          
083500 Z-FINIT SECTION.                                                         
083600     CLOSE W55372                                                         
083700           W55373                                                         
083800           W55374                                                         
083900           W55375                                                         
084000                                                                          
084100     MOVE 'S' TO POSTSUM-OPKOD                                            
084200     CALL POSTSUM USING POSTSUM-PARM                                      
084300     .                                                                    
084401     EJECT                                                                
084501                                                                          
084600 S01-LAS-W55372   SECTION.                                                
084700     READ W55372 INTO WIN-AREA                                            
084800     AT END                                                               
084900         SET END-OF-W55372 TO TRUE                                        
085000     NOT AT END                                                           
085100         MOVE WIN-IDPTYP  TO POSTSUM-TRANSTYP                             
085200         MOVE 'W55372'    TO POSTSUM-FDNAMN                               
085300         MOVE 'W55373D1'  TO POSTSUM-DDNAMN2                              
085400         CALL POSTSUM USING POSTSUM-PARM                                  
085500     END-READ                                                             
085600     .                                                                    
085700     SKIP3                                                                
085801                                                                          
085900 S02-WRITE-W55373 SECTION.                                                
086000     WRITE W553-POST FROM WUT-AREA                                        
086100                                                                          
086200     MOVE ' UT '      TO POSTSUM-TRANSTYP                                 
086300     MOVE 'W55373'    TO POSTSUM-FDNAMN                                   
086400     MOVE 'W55373D6'  TO POSTSUM-DDNAMN2                                  
086500     CALL POSTSUM USING POSTSUM-PARM                                      
086600     .                                                                    
086700     EJECT                                                                
086801                                                                          
086900 S03-CREATE-W55374 SECTION.                                               
087000*CREATE ERROR RECORD                                                      
087100     MOVE WIN-IDARTNR              TO WUT74-IDARTNR                       
087200     MOVE WIN-IDLEVNR              TO WUT74-IDLEVNR                       
087300     MOVE WS-PRARTBEL              TO WUT74-PRARTBEL                      
087400     MOVE WIN-KDVALISO             TO WUT74-KDVALISO                      
087500     MOVE WIN-TIPRLIST             TO W-TIPRLIST                          
087601     MOVE W-TIPRLIST               TO WUT74-TIPRLIST                      
087700     MOVE WIN-KDFPKPRI             TO WUT74-KDFPKPRI                      
087801     MOVE WIN-IDDC                 TO WUT74-IDDC                          
087901                                                                          
088000     PERFORM J-READ-WDP311                                                
088100     PERFORM S04-WRITE-W55374                                             
088200     .                                                                    
088300     EJECT                                                                
088400                                                                          
088501 S04-WRITE-W55374 SECTION.                                                
088601*WRITE ERROR RECORD                                                       
088701     IF W-IDLAND = W-IDLAND-PREV-ERR                                      
088801       CONTINUE                                                           
088901     ELSE                                                                 
089001       MOVE  W-IDLAND    TO   W-IDLAND-PREV-ERR                           
089101                              UT-CTL-IDLANDX2                             
089201                                                                          
089301       WRITE W55374-POST FROM UT-CONTROL-ERR                              
089401       WRITE W55374-POST FROM UT-CONTROL-LAND                             
089501       WRITE W55374-POST FROM WE-HEADER                                   
089601     END-IF                                                               
089701     WRITE W55374-POST   FROM WUT74-AREA                                  
089801                                                                          
089901     MOVE ' UT '      TO POSTSUM-TRANSTYP                                 
090001     MOVE 'W55374'    TO POSTSUM-FDNAMN                                   
090101     MOVE 'W55373D2'  TO POSTSUM-DDNAMN2                                  
090201     CALL POSTSUM USING POSTSUM-PARM                                      
090301     .                                                                    
090401     EJECT                                                                
090501                                                                          
090601 S05-CREATE-W55375 SECTION.                                               
090701*CREATE WARNING RECORD                                                    
090801     PERFORM IMS-GU-WDD3-BEN                                              
090901     IF SEGMENT-FOUND                                                     
091001       MOVE TEXT-BEART TO WUT75-BEART                                     
091101     ELSE                                                                 
091200       MOVE ' '        TO WUT75-BEART                                     
091301     END-IF                                                               
091401     MOVE WIN-IDARTNR              TO WUT75-IDARTNR                       
091501     MOVE WIN-IDLEVNR              TO WUT75-IDLEVNR                       
091601     MOVE WS-PRARTBEL              TO WUT75-PRARTBEL                      
091701     MOVE WIN-KDVALISO             TO WUT75-KDVALISO                      
091801     MOVE WIN-TIPRLIST             TO W-TIPRLIST                          
091901     MOVE W-TIPRLIST               TO WUT75-TIPRLIST                      
092001     MOVE OLD-PRARTBES             TO WUT75-PRARTBES                      
092100     MOVE W-KVPB                   TO WUT75-KVPB                          
092200     MOVE W-KVLS                   TO WUT75-KVLS                          
092300     MOVE WIN-IDDC                 TO WUT75-IDDC                          
092400     PERFORM J-READ-WDP311                                                
092500     PERFORM S06-WRITE-W55375                                             
092600     .                                                                    
092700     EJECT                                                                
092800                                                                          
092900 S06-WRITE-W55375 SECTION.                                                
093001*WRITE WARNING RECORD                                                     
093101                                                                          
093201     IF W-IDLAND = W-IDLAND-PREV-WARN                                     
093301       CONTINUE                                                           
093401     ELSE                                                                 
093501       MOVE  W-IDLAND    TO   W-IDLAND-PREV-WARN                          
093601                              UT-CTL-IDLANDX2                             
093701                                                                          
093801       WRITE W55375-POST FROM UT-CONTROL-WARN                             
093901       WRITE W55375-POST FROM UT-CONTROL-LAND                             
094001       WRITE W55375-POST FROM WW-HEADER                                   
094101     END-IF                                                               
094201     WRITE W55375-POST   FROM WUT75-AREA                                  
094301                                                                          
094401     MOVE ' UT '        TO POSTSUM-TRANSTYP                               
094501     MOVE 'W55375'      TO POSTSUM-FDNAMN                                 
094601     MOVE 'W55375D3'    TO POSTSUM-DDNAMN2                                
094701     CALL POSTSUM USING POSTSUM-PARM                                      
094801     .                                                                    
094900     EJECT                                                                
095001                                                                          
095100 IMS-GU-WLARTC01 SECTION.                                                 
095200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
095300     DELIMITED  BY SIZE INTO SSA1                                         
095400     MOVE '  GE' TO GOOD-STATUSCODES                                      
095500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
095600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
095700     PERFORM IMS-STATUSCHECK                                              
095800     .                                                                    
095900     SKIP2                                                                
096000 IMS-GNP-WDK724   SECTION.                                                
096100     MOVE 'WDK724   ' TO SSA1                                             
096200     MOVE '  GE' TO GOOD-STATUSCODES                                      
096300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
096400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
096500     PERFORM IMS-STATUSCHECK                                              
096600     .                                                                    
096700     SKIP2                                                                
096801                                                                          
096900 IMS-GNP-WDK724-FIRST SECTION.                                            
097000     MOVE 'WDK724  *F' TO SSA1                                            
097100     MOVE '  GE' TO GOOD-STATUSCODES                                      
097200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
097300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
097400     PERFORM IMS-STATUSCHECK                                              
097500     .                                                                    
097600     EJECT                                                                
097701                                                                          
097800 IMS-GU-WDF101 SECTION.                                                   
097900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
098000     DELIMITED BY SIZE INTO SSA1                                          
098100     MOVE '  GE' TO GOOD-STATUSCODES                                      
098200     CALL CBLTDLI USING GU LEV-PCB DLI-IO-WDF101 SSA1                     
098300     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
098400     PERFORM IMS-STATUSCHECK                                              
098500     .                                                                    
098600     SKIP3                                                                
098700 IMS-GNP-WDF102 SECTION.                                                  
098800     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
098900     DELIMITED BY SIZE INTO SSA1                                          
099000     MOVE '  GE' TO GOOD-STATUSCODES                                      
099100     CALL CBLTDLI USING GNP LEV-PCB DLI-IO-WDF102 SSA1                    
099200     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
099300     PERFORM IMS-STATUSCHECK                                              
099400     .                                                                    
099500     SKIP3                                                                
100700 IMS-GU-WDD3-BEN SECTION.                                                 
100800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
100900            DELIMITED BY SIZE INTO SSA1                                   
101000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
101100            DELIMITED BY SIZE INTO SSA2                                   
101200     MOVE '  GE' TO GOOD-STATUSCODES                                      
101300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
101400     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSCHECK                                              
101600     .                                                                    
101700     EJECT                                                                
101800 IMS-GU-WDK701 SECTION.                                                   
101900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
102000     DELIMITED  BY SIZE INTO SSA1                                         
102100     MOVE '  GE' TO GOOD-STATUSCODES                                      
102200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
102300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
102400     PERFORM IMS-STATUSCHECK                                              
102500     .                                                                    
102600     SKIP3                                                                
102700 IMS-GU-WDK711 SECTION.                                                   
102801     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
102901     DELIMITED  BY SIZE INTO SSA1                                         
103001     STRING 'WDK711  (IDDC     =' W-IDDC-X    ')'                         
103101     DELIMITED  BY SIZE INTO SSA2                                         
103200     MOVE '  GE' TO GOOD-STATUSCODES                                      
103300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
103400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
103500     PERFORM IMS-STATUSCHECK                                              
103600     .                                                                    
103700     EJECT                                                                
103801 IMS-GNP-WDK712 SECTION.                                                  
103901     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
104001     DELIMITED  BY SIZE INTO SSA1                                         
104101     MOVE '  GE' TO GOOD-STATUSCODES                                      
104201     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK712 SSA1                   
104301     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
104401     PERFORM IMS-STATUSCHECK                                              
104501     .                                                                    
104601     SKIP2                                                                
104701                                                                          
104801 IMS-GNP-WDK722 SECTION.                                                  
104901     MOVE 'WDK722' TO SSA1                                                
105001     MOVE '  GE'                 TO GOOD-STATUSCODES                      
105101     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
105201     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
105301     PERFORM IMS-STATUSCHECK                                              
105401     .                                                                    
105501     SKIP2                                                                
105600                                                                          
105701 IMS-GU-WDB601    SECTION.                                                
105800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
105900          DELIMITED BY SIZE INTO SSA1                                     
106000     MOVE '  GE' TO GOOD-STATUSCODES                                      
106100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
106200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
106300     PERFORM IMS-STATUSCHECK                                              
106400     IF SEGMENT-MISSING                                                   
106500         MOVE SPACE TO DCS-KDDC                                           
106600     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106901                                                                          
108401 IMS-GU-WDK611 SECTION.                                                   
108501     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
108601          DELIMITED BY SIZE INTO SSA1                                     
108701     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
108801          DELIMITED BY SIZE INTO SSA2                                     
108901     MOVE '  GE' TO GOOD-STATUSCODES                                      
109001     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
109101     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
109201     PERFORM IMS-STATUSCHECK                                              
109301     .                                                                    
109401     EJECT                                                                
109501                                                                          
109601 IMS-GU-WDP311 SECTION.                                                   
109701     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
109801          DELIMITED BY SIZE INTO SSA1                                     
109901     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
110001          DELIMITED BY SIZE INTO SSA2                                     
110101     MOVE '  GE' TO GOOD-STATUSCODES                                      
110201     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
110301     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
110401     PERFORM IMS-STATUSCHECK                                              
110501     .                                                                    
110601     EJECT                                                                
110701                                                                          
110801 IMS-STATUSCHECK SECTION.                                                 
110900     SET STATUS-IX TO 1                                                   
111000     SEARCH GOOD-STATUS AT END CALL FELLOG                                
111100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
111200     END-SEARCH                                                           
111300     .                                                                    
111400     EJECT                                                                
111500*    -COPY WY2000P1                                                       
