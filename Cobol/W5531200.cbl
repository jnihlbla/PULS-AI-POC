000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5531200.                                                
000400 AUTHOR.         ROYNA LUND / KARL JOHAN HANSSON                          
000500*DATE-WRITTEN.   93/10/07.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET KONTROLLERAR                                          
001100*             - FIL FRÅN PV-INKÖP(PT=985),                                
001200*             - FIL MED NYA SATSPRISER (PT=985 OCH IDLEVNR=1002).         
001300*        LÄSER ART.REG(WDK6), LEVERANTÖRSREG.(WDF1) OCH                   
001400*        VALUTAREGISTRET(WDR2) SAMT BENÄMNINGSREG (WDD3).                 
001500*        OCH WDK7, SALDO PÅ ALLA DC UTOM NDC-NA.                          
001600*        TRANSAR SOM EJ UPPFYLLER BEGÄRDA VILLKOR                         
001700*        SKRIVS PÅ FELLISTA,                                              
001800*        KORREKTA TRANSAR SKRIVS PÅ FIL SOM SEDAN                         
001900*        SKALL BLI BAKGRUNDSTRANSAR TILL BILD 5111.                       
002000*        VARNINGSLISTA SKAPAS.                                            
002100*        READS WDP3 FOR PURCHASE INFO                                     
002200*                                                                         
002300*      KONTROLLER:                                                        
002400*        - FIL-DATUM ÄR GILTIGT                                           
002500*        - FIL-ENHET FÖR ST,100ST,1000ST O 10ST GODK.,DVS                 
002600*          FIL-KDANTENH = 1,2,3 OCH 7 (BEHANDLAS HÄR,                     
002700*                                  OM FLER UTÖKA)                         
002800*        - FIL-PRARTBEL EJ NOLL                                           
002900*        - FIL-ARTIKEL FINNS PÅ ARTIKELREG                                
003000*        - FIL-ARTIKEL EJ UTGÅNGEN                                        
003100*        - ART-RAD(PRIS) (PÅ 21-SEGMENTET)                                
003200*          - DET NYASTE ÄR EJ PRELIMINÄRT                                 
003300*          - DET AKTUELLA FÖR UPPDAT. EJ LIKA MED FIL-PRARTBEL            
003400*          - DET ÄLDSTA ENDA INLEV.MÄRKTA FÅR EJ TAS BORT                 
003500*          - INLEV.MÄRKTA DATUM EJ SENARE ÄN FIL-DATUM                    
003600*        - ART-PRINK EJ NOLL                                              
003700*        - ART-PRARTSTD EJ NOLL                                           
003800*        - ART-PRARTBES EJ NOLL                                           
003900*        - ART-PRARTSJK EJ NOLL                                           
004000*        - FIL-PRARTBEL EJ NOLL                                           
004100*        - FIL-LEVERANTÖR FINNS PÅ LEVERANTÖRSREGISTRET                   
004200*        - HEMTAGNINGSFAKTOR FINNS PÅ LEVERANTÖRSREGISTRET                
004300*        - VALUTAKOD FINNS UPPLAGD PÅ REG. FÖR AKTUELLT ÅR                
004400*        - BESTÄLLNINGSPRIS EJ STÖRRE ÄN 400.000 KR                       
004500*        - PRISFÖRÄNDRING INOM GODKÄND RAM                                
004600*                                                                         
004700*      FEL PÅ FELLISTAN (TRANSARNA STOPPAS FÖR PRISUPPDATERING):          
004800*        - EJ GILTIGT DATUM                                               
004900*        - KDANTENH (KDENH-BEST) ÄR EJ 1 2 3 7                            
005000*        - ISOKOD FEL GAMMAL EMU-VALUTA-KOD                               
005100*        - ISOKOD FEL VALUTAKOD NUMERISK                                  
005200*        - PRELIMINÄRT PRIS FINNS FÖR DENNA LEV.                          
005300*        - FILDATUM EJ STÖRRE ÄN INLEV.MÄRKT RAD                          
005400*        - ARTIKELNS PRISER ÄR NOLL                                       
005500*        - LEVERANTÖR SAKNAS PÅ LEV.REG                                   
005600*        - VAL.KOD FINNS EJ FÖR AKT. ÅR                                   
005700*        - BEST.PRIS STÖRRE ÄN 400000 KR                                  
005800*      EJ FELRAD PÅ FELLISTAN MEN UPPDATERING STOPPAS:                    
005900*        - SKAPA EJ FELRAD   (FELLISTA-FELORSAK)                          
006000*          - SAMMA RADPRIS FINNS REDAN PÅ RAD 1                           
006100*          - EJ FÖRHANDLAT PRIS                                           
006200*          - PRARTBEL PÅ FIL ÄR 0                                         
006300*          - ARTIKELN SAKNAS PÅ ARTREG                                    
006400*          - ARTIKELN ÄR UTGÅNGEN                                         
006500*          - RENAULT-LEV HAR ARTIKEL DÄR PS EJ ÄR 31 - 39                 
006600*                                                                         
006700*      VARNINGAR PÅ VARNINGSLISTAN:                                       
006800*        - PRISSÄNKN. MER ÄN 50 %                                         
006900*        - PRISHÖJN. MER 100 %                                            
007000*                                                                         
007100*                                                                         
007200     SKIP3                                                                
007300 ENVIRONMENT DIVISION.                                                    
007400     SKIP2                                                                
007500 INPUT-OUTPUT SECTION.                                                    
007600                                                                          
007700 FILE-CONTROL.                                                            
007800     SKIP2                                                                
007900*          --- FIL FRÅN PV-INKÖP                                          
008000     SELECT W09286                     ASSIGN TO W55312D1.                
008100     SKIP2                                                                
008200*          --- FELLISTA VCC REPORT1 TO BE SENT OUT BY EMAIL (D&P)         
008300     SELECT W55312A                    ASSIGN TO W55312D2.                
008400     SKIP2                                                                
008500*          --- VARNINGSLISTA VCC REPORT2 TO BE SENT BY (D&P)              
008600     SELECT W55312B                    ASSIGN TO W55312D3.                
008700     SKIP2                                                                
008800*          --- KONTROLLERADE POSTER (PV-INKÖP)                            
008900     SELECT W55312                     ASSIGN TO W55312D4.                
009000     EJECT                                                                
009100 DATA DIVISION.                                                           
009200                                                                          
009300 FILE SECTION.                                                            
009400     SKIP2                                                                
009500 FD  W09286                                                               
009600     RECORDING       F                                                    
009700     BLOCK CONTAINS  0.                                                   
009800                                                                          
009900*01  -COPY W55310     -PRE INFIL-  -L.                                    
010000     SKIP2                                                                
010100 FD  W55312A                                                              
010200     RECORDING       V                                                    
010300     BLOCK CONTAINS  0.                                                   
010400                                                                          
010500 01  W55312A-RAD                 PIC X(191).                              
010600     SKIP2                                                                
010700 FD  W55312B                                                              
010800     RECORDING       V                                                    
010900     BLOCK CONTAINS  0.                                                   
011000                                                                          
011100 01  W55312B-RAD                 PIC X(224).                              
011200     SKIP2                                                                
011300 FD  W55312                                                               
011400     RECORDING       F                                                    
011500     BLOCK CONTAINS  0.                                                   
011600                                                                          
011700*01  POST -COPY W55312   -PRE  W553-  -L.                                 
011800     EJECT                                                                
011900 WORKING-STORAGE SECTION.                                                 
012000                                                                          
012100*    -COPY WY2000W1                                                       
012200     SKIP3                                                                
012300 77  IDPGM                       PIC X(8)    VALUE 'W5531200'.            
012400 77  JA                          PIC X       VALUE 'J'.                   
012500 77  NEJ                         PIC X       VALUE 'N'.                   
012600 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
012700 77  SLUTA-LAES                  PIC X       VALUE 'N'.                   
012800                                                                          
012900 77  W09286-EOF-SW               PIC X       VALUE 'N'.                   
013000     88  END-OF-W09286                       VALUE 'J'.                   
013100 77  WDK601-SW                   PIC X       VALUE 'N'.                   
013200     88  WDK601-FOUND                        VALUE 'J'.                   
013300 77  WDK611-SW                   PIC X       VALUE 'N'.                   
013400     88  WDK611-FOUND                        VALUE 'J'.                   
013500 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
013600 77  W-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                 
013700                                                                          
013800 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0   COMP-3.        
013900 01  PRL-TIPRLIST                PIC 9(6).                                
014000 01  W-PRKURS                    PIC S9(5)V9(2) VALUE +0   COMP-3.        
014100 01  W-REVALUTA                  PIC S9(3)      VALUE +0   COMP-3.        
014200 01  INFIL-TIPRLIST              PIC 9(6).                                
014300 01  WS-PRARTBEL                 PIC S9(8)V9(5) VALUE +0   COMP-3.        
014400 01  WS-PRARTBES-FIRST           PIC S9(8)V9(5) VALUE +0   COMP-3.        
014500 01  WS-PRARTBES-NEW             PIC S9(8)V9(5) VALUE +0   COMP-3.        
014600 01  NEW-PRARTBES                PIC S9(8)V9(3) VALUE +0   COMP-3.        
014700 01  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
014800 01  W-PRARTSTD                  PIC S9(7)V9(2) VALUE +0   COMP-3.        
014900 01  W-TIFINLV                   PIC S9(5)      VALUE +0   COMP-3.        
015000 01  W-KVPB                      PIC S9(5)V9    VALUE +0   COMP-3.        
015100 01  W-KVLS                      PIC S9(7)      VALUE +0   COMP-3.        
015200                                                                          
015300 01  MAX-PRARTBES            PIC S9(7)V9(2) VALUE +400000 COMP-3.         
015400 01  PRISANDRING                 PIC 9(8)V9(10) VALUE  0.                 
015500 01  W-TIAAVV                    PIC 9(4)       VALUE ZERO.               
015600                                                                          
015700 01  DAGENS-DATUM                PIC 9(6)       VALUE ZERO.               
015800 01  FILLER REDEFINES DAGENS-DATUM.                                       
015900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
016000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016200     EJECT                                                                
016300 01  DYNAMISKA-SUBPROGRAM.                                                
016400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
016900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017000     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
017100     SKIP3                                                                
017200 01  NYCKLAR-TILL-DLI.                                                    
017300     03  W-IDARTNR-X.                                                     
017400         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
017500     03  W-IDLEVNR-X.                                                     
017600         05    W-IDLEVNR     PIC X(5)    VALUE SPACE.                     
017700     03  W-IDLAND-X.                                                      
017800         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
017900     03  W-IDSKYLT-X.                                                     
018000         05    W-IDSKYLT     PIC X(3)    VALUE 'S  '.                     
018100     03  W-IDDC-B6-X.                                                     
018200         05 W-IDDC-B6            PIC X(2).                                
018300     03  W-KDARBTYP-X.                                                    
018400         05  W-KDARBTYP          PIC X(8)    VALUE 'INK     '.            
018500     03  W-IDPERSON-X.                                                    
018600         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
018700     SKIP3                                                                
018800                                                                          
018900*    --- PARAMETRAR TILL ABEND                                            
019000                                                                          
019100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
019300     EJECT                                                                
019400*    --- PARAMETRAR TILL DATKORT                                          
019500*                                                                         
019600 01  WS-COUNTER1                 PIC S9(3)   VALUE ZERO  COMP-3.          
019700 01  WS-COUNTER2                 PIC S9(3)   VALUE ZERO  COMP-3.          
019800 01  WS-COUNTER3                 PIC S9(3)   VALUE ZERO  COMP-3.          
019900*                                                                         
020000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W55312'.              
020100                                                                          
020200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
020300     SKIP3                                                                
020400*01  -COPY WDATKORT                                                       
020500     EJECT                                                                
020600 01  WDATAREA                  PIC X(8)    VALUE 'WDATAREA'.              
020700                                                                          
020800*01       -COPY WDATAREA                                                  
020900     EJECT                                                                
021000*01       -COPY W510CURR                                                  
021100*    --- PARAMETRAR TILL POSTSUM                                          
021200*                                                                         
021300*01  -COPY W0005   -PRE  POSTSUM-                                         
021400     EJECT                                                                
021500 01  TEST-KDVALISO       PIC X(3).                                        
021600                                                                          
021700*01  FILLER  -COPY WWISOEMU  -RED TEST-KDVALISO                           
021800     EJECT                                                                
021900 01  FILLER                      PIC X(24) VALUE 'V003AREA-START'.        
022000* FELLISTOR                                                               
022100 01  W001-RUBRIK2.                                                        
022200     03  FILLER                  PIC X(5)  VALUE 'ARTNR'.                 
022300     03  FILLER                  PIC X     VALUE ';'.                     
022400     03  FILLER                  PIC X(8)  VALUE 'FELORSAK'.              
022500     03  FILLER                  PIC X     VALUE ';'.                     
022600     03  FILLER                  PIC X(5)  VALUE 'LEVNR'.                 
022700     03  FILLER                  PIC X     VALUE ';'.                     
022800     03  FILLER                  PIC X(11) VALUE 'BESTPRIS/ST'.           
022900     03  FILLER                  PIC X     VALUE ';'.                     
023000     03  FILLER                  PIC X(6)  VALUE 'VALKOD'.                
023100     03  FILLER                  PIC X     VALUE ';'.                     
023200     03  FILLER                  PIC X(9)  VALUE 'PRISDATUM'.             
023300     03  FILLER                  PIC X     VALUE ';'.                     
023400     03  FILLER                  PIC X(7)  VALUE 'FÖRP FL'.               
023500     03  FILLER                  PIC X     VALUE ';'.                     
023600     03  FILLER                  PIC X(10) VALUE 'PURCH NAME'.            
023700     03  FILLER                  PIC X     VALUE ';'.                     
023800     03  FILLER                  PIC X(8)  VALUE 'PURCH NO'.              
023900     03  FILLER                  PIC X     VALUE ';'.                     
024000     03  FILLER                  PIC X(5)  VALUE 'EMAIL'.                 
024100     03  FILLER                  PIC X     VALUE ';'.                     
024200     EJECT                                                                
024300                                                                          
024400 01  FELLISTA-DETALJ1.                                                    
024500     03  FELLISTA-IDARTNR        PIC Z(9).                                
024600     03  FILLER                  PIC X     VALUE ';'.                     
024700     03  FELLISTA-FELORSAK       PIC X(39) VALUE SPACE.                   
024800     03  FILLER                  PIC X     VALUE ';'.                     
024900     03  FELLISTA-LEVNR          PIC X(5).                                
025000     03  FILLER                  PIC X     VALUE ';'.                     
025100     03  FELLISTA-PRARTBEL       PIC Z(8).99999.                          
025200     03  FILLER                  PIC X     VALUE ';'.                     
025300     03  FELLISTA-KDVALISO       PIC X(3).                                
025400     03  FILLER                  PIC X     VALUE ';'.                     
025500     03  FELLISTA-TIPRLIST       PIC Z9(6).                               
025600     03  FILLER                  PIC X     VALUE ';'.                     
025700     03  FELLISTA-KDFPKPRI       PIC X.                                   
025800     03  FILLER                  PIC X     VALUE ';'.                     
025900     03  FELLISTA-IDNAMN         PIC X(40) VALUE SPACE.                   
026000     03  FILLER                  PIC X     VALUE ';'.                     
026100     03  FELLISTA-IDINK          PIC Z(2)9 VALUE ZEROS.                   
026200     03  FILLER                  PIC X     VALUE ';'.                     
026300     03  FELLISTA-IDMAIL         PIC X(60) VALUE SPACE.                   
026400     03  FILLER                  PIC X     VALUE ';'.                     
026500     EJECT                                                                
026600                                                                          
026700 01  W002-RUBRIK2-4.                                                      
026800     03  FILLER                    PIC X(5)  VALUE 'ARTNR'.               
026900     03  FILLER                    PIC X     VALUE ';'.                   
027000     03  FILLER                    PIC X(5)  VALUE 'ORSAK'.               
027100     03  FILLER                    PIC X     VALUE ';'.                   
027200     03  FILLER                    PIC X(9)  VALUE 'BENÄMNING'.           
027300     03  FILLER                    PIC X     VALUE ';'.                   
027400     03  FILLER                    PIC X(5)  VALUE 'LEVNR'.               
027500     03  FILLER                    PIC X     VALUE ';'.                   
027600     03  FILLER                    PIC X(11) VALUE 'BESTPRIS/ST'.         
027700     03  FILLER                    PIC X     VALUE ';'.                   
027800     03  FILLER                    PIC X(16)                              
027900                                       VALUE 'NEW BESTPRIS/SEK'.          
028000     03  FILLER                    PIC X     VALUE ';'.                   
028100     03  FILLER                    PIC X(3)  VALUE 'ISO'.                 
028200     03  FILLER                    PIC X     VALUE ';'.                   
028300     03  FILLER                    PIC X(5)  VALUE 'DATUM'.               
028400     03  FILLER                    PIC X     VALUE ';'.                   
028500     03  FILLER                    PIC X(10) VALUE 'GAM BESTPR'.          
028600     03  FILLER                    PIC X     VALUE ';'.                   
028700     03  FILLER                    PIC X(5)  VALUE 'VOLYM'.               
028800     03  FILLER                    PIC X     VALUE ';'.                   
028900     03  FILLER                    PIC X(6)  VALUE 'PUBLVA'.              
029000     03  FILLER                    PIC X     VALUE ';'.                   
029100     03  FILLER                    PIC X(7)  VALUE 'STDPRIS'.             
029200     03  FILLER                    PIC X     VALUE ';'.                   
029300     03  FILLER                    PIC X(16)                              
029400                                       VALUE 'OLD BESTPRIS/SEK'.          
029500     03  FILLER                    PIC X     VALUE ';'.                   
029600     03  FILLER                    PIC X(5)  VALUE 'SALDO'.               
029700     03  FILLER                    PIC X     VALUE ';'.                   
029800     03  FILLER                    PIC X(10) VALUE 'PURCH NAME'.          
029900     03  FILLER                    PIC X     VALUE ';'.                   
030000     03  FILLER                    PIC X(8)  VALUE 'PURCH NO'.            
030100     03  FILLER                    PIC X     VALUE ';'.                   
030200     03  FILLER                    PIC X(5)  VALUE 'EMAIL'.               
030300     03  FILLER                    PIC X     VALUE ';'.                   
030400     EJECT                                                                
030500                                                                          
030600 01  VARNLISTA-DETALJ.                                                    
030700     03  VARNLISTA-IDARTNR          PIC Z(9).                             
030800     03  FILLER                     PIC X     VALUE ';'.                  
030900     03  VARNLISTA-ORSAK            PIC X(10) VALUE SPACE.                
031000     03  FILLER                     PIC X     VALUE ';'.                  
031100     03  VARNLISTA-BEART            PIC X(13).                            
031200     03  FILLER                     PIC X     VALUE ';'.                  
031300     03  VARNLISTA-LEVNR            PIC X(5).                             
031400     03  FILLER                     PIC X     VALUE ';'.                  
031500     03  VARNLISTA-PRARTBEL         PIC Z(8).99999.                       
031600     03  FILLER                     PIC X     VALUE ';'.                  
031700     03  VARNLISTA-NEW-PRARTBES     PIC Z(8).99999.                       
031800     03  FILLER                     PIC X     VALUE ';'.                  
031900     03  VARNLISTA-KDVALISO         PIC X(3).                             
032000     03  FILLER                     PIC X     VALUE ';'.                  
032100     03  VARNLISTA-TIPRLIST         PIC Z9(6).                            
032200     03  FILLER                     PIC X     VALUE ';'.                  
032300     03  VARNLISTA-BES-OLD          PIC Z(8).99.                          
032400     03  FILLER                     PIC X     VALUE ';'.                  
032500     03  VARNLISTA-KVPB             PIC Z(5)9.9.                          
032600     03  FILLER                     PIC X     VALUE ';'.                  
032700     03  VARNLISTA-TIFINLV          PIC ZZ9(5).                           
032800     03  FILLER                     PIC X     VALUE ';'.                  
032900     03  VARNLISTA-PRARTSTD         PIC Z(7)9.99.                         
033000     03  FILLER                     PIC X     VALUE ';'.                  
033100     03  VARNLISTA-OLD-PRARTBES     PIC Z(8).99999.                       
033200     03  FILLER                     PIC X     VALUE ';'.                  
033300     03  VARNLISTA-KVLS             PIC Z(7)9.                            
033400     03  FILLER                     PIC X     VALUE ';'.                  
033500     03  VARNLISTA-IDNAMN           PIC X(40) VALUE SPACE.                
033600     03  FILLER                     PIC X     VALUE ';'.                  
033700     03  VARNLISTA-IDINK            PIC Z(2)9 VALUE ZEROS.                
033800     03  FILLER                     PIC X     VALUE ';'.                  
033900     03  VARNLISTA-IDMAIL           PIC X(60) VALUE SPACE.                
034000     03  FILLER                     PIC X(1)  VALUE ';'.                  
034100     EJECT                                                                
034200 01  FILLER                      PIC X(24) VALUE 'WIN-AREA'.              
034300                                                                          
034400*01  AREA -COPY W55310        -PRE WIN-                                   
034500     EJECT                                                                
034600 01  FILLER                      PIC X(24) VALUE 'WUT-AREA'.              
034700                                                                          
034800*01  AREA -COPY W55312        -PRE WUT-                                   
034900     EJECT                                                                
035000 01  W001-DAP.                                                            
035100     03  FILLER                  PIC X(165)  VALUE SPACE.                 
035200******************************************************************        
035300*                                                                         
035400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
035500*                                                                         
035600******************************************************************        
035700                                                                          
035800 01  IMS-WS.                                                              
035900     03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.           
036000     SKIP3                                                                
036100*                        **** STATUS-KOD FRÅN IMS ****                    
036200     03    STATUS-WS           PIC XX.                                    
036300         88    SEGMENT-FINNS               VALUE '  '.                    
036400         88    SEGMENT-SAKNAS              VALUE 'GE'.                    
036500         88    SEGMENT-FINNS-REDA          VALUE 'II'.                    
036600     SKIP3                                                                
036700     03    GODK-STATUSKODER.                                              
036800         05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
036900     SKIP3                                                                
037000 01  SSA1                      PIC X(128).                                
037100 01  SSA2                      PIC X(128).                                
037200     EJECT                                                                
037300*                        **** IMS FUNKTIONSKODER ****                     
037400*01    -COPY W0003                                                        
037500     EJECT                                                                
037600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDF101'.           
037700 01  DLI-IO-WDF101.                                                       
037800*    03  -COPY WDF101                                                     
037900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDF102'.           
038000 01  DLI-IO-WDF102.                                                       
038100*    03  -COPY WDF102 -PRE LEV-                                           
038200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601'.           
038300 01  DLI-IO-WDK601.                                                       
038400*    03  -COPY WDK601                                                     
038500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611'.           
038600 01  DLI-IO-WDK611.                                                       
038700*    03  -COPY WDK611                                                     
038800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK621'.           
038900 01  DLI-IO-WDK621.                                                       
039000*    03  -COPY WDK621                                                     
039100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDD311'.           
039200 01  DLI-IO-WDD311.                                                       
039300*    03  -COPY WDD311                                                     
039400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
039500 01  DLI-IO-WDK701.                                                       
039600*    03  -COPY WDK701                                                     
039700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
039800 01  DLI-IO-WDK711.                                                       
039900*    03  -COPY WDK711                                                     
040000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB601'.           
040100 01  DLI-IO-WDB601.                                                       
040200*    03  -COPY WDB601                                                     
040300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDP311'.           
040400 01  DLI-IO-WDP311.                                                       
040500*    03  -COPY WDP311                                                     
040600     EJECT                                                                
040700 LINKAGE SECTION.                                                         
040800*01    -COPY W0008     -PRE WDK6-                                         
040900     05  FILLER                  PIC X(13).                               
041000                                                                          
041100*01    -COPY W0008     -PRE LEV-                                          
041200     05  FILLER                  PIC X(5).                                
041300                                                                          
041400*01    -COPY W0008     -PRE WDG2-                                         
041500     05  FILLER                  PIC X(30).                               
041600                                                                          
041700*01    -COPY W0008     -PRE WDD3-                                         
041800     05  FILLER                  PIC X(8).                                
041900                                                                          
042000*01    -COPY W0008     -PRE WDK7-                                         
042100      05 FILLER                  PIC X(18).                               
042200                                                                          
042300*01    -COPY W0008     -PRE WDB6-                                         
042400      05 FILLER                  PIC X(18).                               
042500*01    -COPY W0008     -PRE WDP3-                                         
042600      05 FILLER                  PIC X.                                   
042700     EJECT                                                                
042800 PROCEDURE DIVISION  USING WDK6-PCB LEV-PCB WDG2-PCB WDD3-PCB             
042900                           WDK7-PCB WDB6-PCB WDP3-PCB.                    
043000     ENTRY 'DLITCBL' USING WDK6-PCB LEV-PCB WDG2-PCB WDD3-PCB             
043100                           WDK7-PCB WDB6-PCB WDP3-PCB.                    
043200     PERFORM A-INIT                                                       
043300                                                                          
043400     PERFORM S01-LAS-W09286                                               
043500     PERFORM UNTIL END-OF-W09286                                          
043600      IF WIN-IDPTYP = '985'                                               
043700       MOVE SPACE                  TO FELLISTA-FELORSAK                   
043800                                      VARNLISTA-ORSAK                     
043900       PERFORM B-FORMELLA-KONTROLLER-INPOST                               
044000       IF FELLISTA-FELORSAK = SPACE                                       
044100          PERFORM C-BERAKNA-STYCKEPRISER                                  
044200          MOVE WIN-IDARTNR TO W-IDARTNR                                   
044300                                                                          
044400          PERFORM D-READ-WLARTC01                                         
044500          IF FELLISTA-FELORSAK = SPACE                                    
044600             PERFORM E-LAS-OVRIGA-ARTREG-SEGMENT                          
044700                                                                          
044800             IF FELLISTA-FELORSAK = SPACE                                 
044900               PERFORM F-BEHANDLA-LEVERANTOR                              
045000                                                                          
045100               IF FELLISTA-FELORSAK = SPACE                               
045200                 PERFORM G-BEHANDLA-VALUTAKURS                            
045300                                                                          
045400                 IF FELLISTA-FELORSAK = SPACE                             
045500                   PERFORM H-SKAPA-RATTPOST                               
045600                   IF VARNLISTA-ORSAK NOT = SPACE                         
045700                     PERFORM S09-HAMTA-SLAGERSALDO                        
045800                     PERFORM I-SKAPA-VARNINGSLISTA                        
045900                   END-IF                                                 
046000                 END-IF                                                   
046100               END-IF                                                     
046200             END-IF                                                       
046300          END-IF                                                          
046400       END-IF                                                             
046500      END-IF                                                              
046600      PERFORM S01-LAS-W09286                                              
046700     END-PERFORM                                                          
046800     PERFORM Z-FINIT                                                      
046900                                                                          
047000     MOVE ZERO TO RETURN-CODE                                             
047100     GOBACK                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 A-INIT SECTION.                                                          
047500                                                                          
047600     OPEN INPUT  W09286                                                   
047700                                                                          
047800     OPEN OUTPUT W55312                                                   
047900                 W55312A                                                  
048000                 W55312B                                                  
048100                                                                          
048200     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
048300     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
048400                              W-TIAAVV(1:2)                               
048500     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
048600     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
048700     MOVE D-VECKA          TO W-TIAAVV(3:2)                               
048800     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
048900     MOVE SPACE            TO FELLISTA-FELORSAK                           
049000     .                                                                    
049100     EJECT                                                                
049200 B-FORMELLA-KONTROLLER-INPOST SECTION.                                    
049300                                                                          
049400     MOVE WIN-IDARTNR TO W-IDARTNR                                        
049500     PERFORM IMS-GU-WLARTC01                                              
049600     IF SEGMENT-FINNS                                                     
049700       MOVE 'J'       TO WDK601-SW                                        
049800       PERFORM IMS-GNP-WLARTC11                                           
049900       IF SEGMENT-FINNS                                                   
050000         MOVE 'J'     TO WDK611-SW                                        
050100       ELSE                                                               
050200         MOVE 'N'     TO WDK611-SW                                        
050300       END-IF                                                             
050400     ELSE                                                                 
050500       MOVE 'N'       TO WDK601-SW                                        
050600     END-IF                                                               
050700*         HÄR KONTROLLERAS OM ANGIVIT DATUM ÄR ETT       **               
050800*         GILTIGT DATUM                                  **               
050900                                                                          
051000     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
051100     MOVE WIN-TIPRLIST            TO INFIL-TIPRLIST                       
051200     MOVE INFIL-TIPRLIST          TO DAT-I-TIDATUM                        
051300     CALL WDATKONV USING             DAT-KDDATFORM                        
051400                                     DAT-I-TIDATUM                        
051500                                     DAT-O-TIDATUM                        
051600                                     DAT-KDSVAR                           
051700     IF DAT-KDSVAR-FEL                                                    
051800       MOVE 'WRONG DATE      '          TO FELLISTA-FELORSAK              
051900       PERFORM S05-SKAPA-FELRAD                                           
052000     END-IF                                                               
052100                                                                          
052200     IF WIN-KDANTENH NOT = '1' AND '2' AND '3' AND '7'                    
052300       MOVE 'KDENH BEST IS NO 1 2 3 7'  TO FELLISTA-FELORSAK              
052400       PERFORM S05-SKAPA-FELRAD                                           
052500     END-IF                                                               
052600                                                                          
052700     IF WIN-PRARTBEL = +0                                                 
052800       MOVE 'ZERO PRICE'                TO FELLISTA-FELORSAK              
052900       PERFORM S05-SKAPA-FELRAD                                           
053000     END-IF                                                               
053100                                                                          
053200     MOVE WIN-KDVALISO                  TO TEST-KDVALISO                  
053300     IF EMU-STOPPAD-KDVALISO                                              
053400       MOVE 'CURRENCY CODE NOT APPROVED IN EMU'                           
053500                                          TO FELLISTA-FELORSAK            
053600       PERFORM S05-SKAPA-FELRAD                                           
053700     ELSE                                                                 
053800       IF WIN-KDVALISO NUMERIC                                            
053900         MOVE 'CURRENCY CODE IS NUMERIC'  TO FELLISTA-FELORSAK            
054000         PERFORM S05-SKAPA-FELRAD                                         
054100       ELSE                                                               
054200         MOVE WIN-KDVALISO                TO WS-KDVALISO                  
054300       END-IF                                                             
054400     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
054700 C-BERAKNA-STYCKEPRISER SECTION.                                          
054800                                                                          
054900     IF WS-KDVALISO = 'GBP'                                               
055000***  PUND HAR TRE DECIMALER                                               
055100       COMPUTE WS-PRARTBEL = WIN-PRARTBEL / 1000                          
055200     ELSE                                                                 
055300       COMPUTE WS-PRARTBEL = WIN-PRARTBEL / 100                           
055400***  ALLA ÖVRIGA WIN-PRARTBEL HAR TVÅ DECIMALER                           
055500     END-IF                                                               
055600                                                                          
055700     IF WIN-KDANTENH = '2'                                                
055800****   ENHETEN ÄR 100                                                     
055900       COMPUTE WS-PRARTBEL = WS-PRARTBEL / 100                            
056000     END-IF                                                               
056100                                                                          
056200     IF WIN-KDANTENH = '3'                                                
056300****   ENHETEN ÄR 1000                                                    
056400       COMPUTE WS-PRARTBEL = WS-PRARTBEL / 1000                           
056500     END-IF                                                               
056600                                                                          
056700     IF WIN-KDANTENH = '7'                                                
056800****   ENHETEN ÄR 10  (NEDCAR)                                            
056900       COMPUTE WS-PRARTBEL = WS-PRARTBEL / 10                             
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300 D-READ-WLARTC01 SECTION.                                                 
057400     IF WDK601-FOUND                                                      
057500       MOVE ART-TIFINLV                 TO W-TIFINLV                      
057600       IF ART-KDERS-UTG = 0                                               
057700            IF WDK611-FOUND                                               
057800              IF CLAG-KDAVT = +0                                          
057810                IF WIN-IDLEVNR = '1441'                                   
057820                  CONTINUE                                                
057830                ELSE                                                      
057900                 MOVE 'NO AGREEMENT'    TO FELLISTA-FELORSAK              
058000                 PERFORM S05-SKAPA-FELRAD                                 
058100                END-IF                                                    
059000              END-IF                                                      
060000            ELSE                                                          
070000              MOVE 'PART CREATION NOT COMPLETED'                          
080000                                        TO FELLISTA-FELORSAK              
080100              PERFORM S05-SKAPA-FELRAD                                    
080200            END-IF                                                        
080300       ELSE                                                               
080400          MOVE 'PART SUPERSEDED'        TO FELLISTA-FELORSAK              
080500          PERFORM S05-SKAPA-FELRAD                                        
080600       END-IF                                                             
080700     ELSE                                                                 
080800       MOVE 'PART MISSING IN PULS'      TO FELLISTA-FELORSAK              
080900       PERFORM S05-SKAPA-FELRAD                                           
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300 E-LAS-OVRIGA-ARTREG-SEGMENT SECTION.                                     
081400                                                                          
081500     MOVE ZERO                    TO W-KVPB                               
081600                                     W-PRARTSTD                           
081700                                     W-KVLS                               
081800     IF WDK611-FOUND                                                      
081900       COMPUTE W-KVLS =                                                   
082000        CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC + CLAG-KVAKS-PAV         
082100        + CLAG-KVAKS-T                                                    
082200       MOVE CLAG-RETULF           TO W-RETULF                             
082300       MOVE CLAG-PRARTSTD         TO W-PRARTSTD                           
082400       MOVE CLAG-KVPB-SEP         TO W-KVPB                               
082500       ADD  CLAG-KVPB-SATS        TO W-KVPB                               
082600                                                                          
082700       IF CLAG-PRINK    = +0 OR                                           
082800          CLAG-PRARTSTD = +0 OR                                           
082900          CLAG-PRARTSJK = +0                                              
083000         MOVE 'ITEM PRICES ARE ZERO'     TO FELLISTA-FELORSAK             
083100         PERFORM S05-SKAPA-FELRAD                                         
083200       END-IF                                                             
083300                                                                          
083400*** DÄREFTER, KONTROLL OM PRISRAD REDAN ÄR UPPDATERAD                     
083500*** ELLER ATT PREL-RAD EJ FINNS (GAMLA APU, NÄSTAN UTGÅNGEN)              
083600       IF FELLISTA-FELORSAK = SPACE                                       
083700         PERFORM EA-KOLLA-OM-REDAN-FINNS                                  
083800       END-IF                                                             
083900                                                                          
084000*    OM FELFRITT, KONTROLLERA OM NYUPPLÄGG ELLER                          
084100*    UPPDATERING ÄR TILLÅTEN FÖR ANGIVET DATUM                            
084200       IF FELLISTA-FELORSAK = SPACE                                       
084300         PERFORM EB-KOLLA-OM-UPPDATERING-OK                               
084400       END-IF                                                             
084500     ELSE                                                                 
084600       MOVE 'PART CREATION NOT COMPLETED'                                 
084700                                 TO FELLISTA-FELORSAK                     
084800       PERFORM S05-SKAPA-FELRAD                                           
084900     END-IF                                                               
085000     .                                                                    
085100     EJECT                                                                
085200 EA-KOLLA-OM-REDAN-FINNS SECTION.                                         
085300                                                                          
085400* NOLLA INITIALT PRL-PRARTBES-PR IFALL K621 SAKNAS                        
085500* FÖR ATT UNDVIKA 0C7 I G-SECTIONEN SOM RÄKNAR MED FÄLTET!                
085600     MOVE ZERO TO PRL-PRARTBES-PR                                         
085700                                                                          
085800     PERFORM IMS-GNP-WLARTC21                                             
085900     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
086000                   FELLISTA-FELORSAK NOT = SPACE                          
086100       MOVE PRL-DAPRLIST-9KOMPL TO PRL-TIPRLIST                           
086200       COMPUTE PRL-TIPRLIST = 99999999 - PRL-DAPRLIST-9KOMPL              
086300       IF  PRL-KDSTATUS-PR = 0                                            
086400       AND PRL-IDLEVNR     = WIN-IDLEVNR                                  
086500         MOVE 'PRELIMINARY PRICE AVAILABLE FOR THIS LEV'                  
086600                                TO FELLISTA-FELORSAK                      
086700         PERFORM S05-SKAPA-FELRAD                                         
086800       ELSE                                                               
086900         MOVE INFIL-TIPRLIST    TO TMP1-YYMMDD                            
087000         MOVE PRL-TIPRLIST      TO TMP2-YYMMDD                            
087100         PERFORM WY2000P1                                                 
087200         IF  (WS-PRARTBEL  = PRL-PRARTBEL-PR)                             
087300         AND (WIN-IDLEVNR  = PRL-IDLEVNR)                                 
087400         AND (WS-KDVALISO  = PRL-KDVALISO)                                
087500         AND (WIN-KDFPKPRI = PRL-KDFPKPRI)                                
087600         AND (TMP1-YYMMDD  = TMP2-YYMMDD)                                 
087700           MOVE 'PRICE ALREADY EXISTS'  TO FELLISTA-FELORSAK              
087800           PERFORM S05-SKAPA-FELRAD                                       
087900         END-IF                                                           
088000       END-IF                                                             
088100       PERFORM IMS-GNP-WLARTC21                                           
088200       ADD  +1                  TO   WS-COUNTER3                          
088300     END-PERFORM                                                          
088400     .                                                                    
088500     EJECT                                                                
088600 EB-KOLLA-OM-UPPDATERING-OK SECTION.                                      
088700                                                                          
088800* NOLLA INITIALT PRL-PRARTBES-PR IFALL K621 SAKNAS                        
088900* FÖR ATT UNDVIKA 0C7 I G-SECTIONEN SOM RÄKNAR MED FÄLTET!                
089000     MOVE ZERO TO PRL-PRARTBES-PR                                         
089100                                                                          
089200     PERFORM IMS-GNP-WLARTC21-FIRST                                       
089300     MOVE 1 TO INDX                                                       
089400     MOVE NEJ TO SLUTA-LAES                                               
089500     PERFORM UNTIL SEGMENT-SAKNAS OR SLUTA-LAES = JA OR                   
089600                   FELLISTA-FELORSAK NOT = SPACE                          
089700       MOVE PRL-DAPRLIST-9KOMPL TO PRL-TIPRLIST                           
089800       COMPUTE PRL-TIPRLIST = 99999999 - PRL-DAPRLIST-9KOMPL              
089900       MOVE INFIL-TIPRLIST TO TMP1-YYMMDD                                 
090000       MOVE PRL-TIPRLIST   TO TMP2-YYMMDD                                 
090100       PERFORM WY2000P1                                                   
090200                                                                          
090300       IF  WIN-IDLEVNR = PRL-IDLEVNR                                      
090400         IF  (WS-PRARTBEL  = PRL-PRARTBEL-PR)                             
090500         AND (WIN-IDLEVNR  = PRL-IDLEVNR)                                 
090600         AND (WS-KDVALISO  = PRL-KDVALISO)                                
090700         AND (WIN-KDFPKPRI = PRL-KDFPKPRI)                                
090800         AND (TMP1-YYMMDD >= TMP2-YYMMDD)                                 
090900            IF INDX = 1                                                   
091000              MOVE 'PRICE ALREADY EXISTS'    TO FELLISTA-FELORSAK         
091100              PERFORM S05-SKAPA-FELRAD                                    
091200            END-IF                                                        
091300         ELSE                                                             
091400           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
091500           AND PRL-SUINLEV-PR > 0                                         
091600           AND PRL-KDSTATUS-PR = 1                                        
091700           AND (PRL-IDLEVNR = WIN-IDLEVNR)                                
091800             ADD 1 TO INDX                                                
091900             MOVE 'DATE NOT HIGHER THAN INLEV MARKED LINE'                
092000                                         TO FELLISTA-FELORSAK             
092100             PERFORM S05-SKAPA-FELRAD                                     
092200           ELSE                                                           
092300             IF INFIL-TIPRLIST = PRL-TIPRLIST                             
092400             AND WIN-IDLEVNR = PRL-IDLEVNR                                
092500               ADD 1 TO INDX                                              
092600               MOVE 'SAME DATE FOR SUPPLIER EXIST'                        
092700                                         TO FELLISTA-FELORSAK             
092800               PERFORM S05-SKAPA-FELRAD                                   
092900             ELSE                                                         
093000               IF INFIL-TIPRLIST = PRL-TIPRLIST                           
093100               AND PRL-SUINLEV-PR = 0                                     
093200                 MOVE JA TO SLUTA-LAES                                    
093300               ELSE                                                       
093400                 ADD 1 TO INDX                                            
093500               END-IF                                                     
093600             END-IF                                                       
093700           END-IF                                                         
093800         END-IF                                                           
093900       END-IF                                                             
094000       PERFORM IMS-GNP-WLARTC21                                           
094100     END-PERFORM                                                          
094200     .                                                                    
094300     EJECT                                                                
094400 F-BEHANDLA-LEVERANTOR SECTION.                                           
094500                                                                          
094600     IF W-RETULF = 0.0000                                                 
094700       MOVE 1.0812                TO W-RETULF                             
094800       MOVE WIN-IDLEVNR           TO W-IDLEVNR                            
094900       PERFORM IMS-GU-WDF101                                              
095000       IF SEGMENT-FINNS                                                   
095100         MOVE 'SE' TO W-IDLAND                                            
095200         PERFORM IMS-GNP-WDF102                                           
095300         IF SEGMENT-FINNS                                                 
095400           IF LEV-TULL-TITULF > DAGENS-DATUM                              
095500             MOVE LEV-TULL-RETULF-2  TO W-RETULF                          
095600           ELSE                                                           
095700             MOVE LEV-TULL-RETULF-1  TO W-RETULF                          
095800           END-IF                                                         
095900         END-IF                                                           
096000       ELSE                                                               
096100         MOVE 'SUPPLIER MISSING ON REGISTER' TO FELLISTA-FELORSAK         
096200         PERFORM S05-SKAPA-FELRAD                                         
096300       END-IF                                                             
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700 G-BEHANDLA-VALUTAKURS SECTION.                                           
096800                                                                          
096900*    HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
097000*    INMATAD VALUTA                                    **                 
097100                                                                          
097200     MOVE ZERO                     TO WS-PRARTBES-NEW                     
097300     IF DAT-TIAA > DAGENS-DATUM-AAR AND DAT-TISEKEL = 20                  
097400        MOVE DAT-TIAA              TO W-DATE-AAMM(1:2)                    
097500     ELSE                                                                 
097600        MOVE DAGENS-DATUM-AAR      TO W-DATE-AAMM(1:2)                    
097700     END-IF                                                               
097800     MOVE WS-KDVALISO              TO CURR-KDVALISO-ROW                   
097900     MOVE W-KDVALISO-HUV           TO CURR-KDVALISO-HUV                   
098000                                                                          
098100     MOVE 01                       TO W-DATE-AAMM(3:2)                    
098200     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
098300     MOVE 'A'                      TO CURR-KDVALTYP                       
098400     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
098500     IF CURR-KDSVAR = ' '                                                 
098600       MOVE CURR-PRKURS-NEW        TO W-PRKURS                            
098700       MOVE CURR-REVALUTA-TO       TO W-REVALUTA                          
098800     ELSE                                                                 
098900       MOVE 'CHOICE CODE NOT AVAILABLE FOR FILE YEARS'                    
099000                                           TO FELLISTA-FELORSAK           
099100       PERFORM S05-SKAPA-FELRAD                                           
099200     END-IF                                                               
099300                                                                          
099400     IF FELLISTA-FELORSAK = SPACE                                         
099500       COMPUTE NEW-PRARTBES ROUNDED = WS-PRARTBEL * W-RETULF *            
099600                                     W-PRKURS / W-REVALUTA                
099700       IF NEW-PRARTBES > MAX-PRARTBES                                     
099800         MOVE 'BEST. PRICE GREATER THAN 400000 SEK' TO                    
099900                                   FELLISTA-FELORSAK                      
100000         PERFORM S05-SKAPA-FELRAD                                         
100100       END-IF                                                             
100200     END-IF                                                               
100300                                                                          
100400* CHECKS IF THE SUPPLIER IS SAME FROM THE  INPUT FILE  AND    *           
100500*      THE SUPPLIER FROM WDK621                               *           
100600     PERFORM IMS-GNP-WLARTC21-FIRST                                       
100700     IF SEGMENT-FINNS                                                     
100800        MOVE PRL-PRARTBES-PR       TO WS-PRARTBES-NEW                     
100900     END-IF                                                               
101000     PERFORM UNTIL (WIN-IDLEVNR = PRL-IDLEVNR) OR SEGMENT-SAKNAS          
101100       PERFORM IMS-GNP-WLARTC21                                           
101200     END-PERFORM                                                          
101300                                                                          
101400* IF NOT SAME THEN MOVE THE FIRST ROW ORDER PRICE TO          *           
101500* PRARTBES-FIRST                                                          
101600     IF (WIN-IDLEVNR NOT = PRL-IDLEVNR)                                   
101700       MOVE    WS-PRARTBES-NEW      TO WS-PRARTBES-FIRST                  
101800     ELSE                                                                 
101900       MOVE    PRL-PRARTBES-PR      TO WS-PRARTBES-FIRST                  
102000     END-IF                                                               
102100                                                                          
102200     IF FELLISTA-FELORSAK = SPACE AND WS-PRARTBES-FIRST > 0               
102300        COMPUTE PRISANDRING ROUNDED =                                     
102400                             WS-PRARTBEL * W-RETULF * W-PRKURS /          
102500                             W-REVALUTA / WS-PRARTBES-FIRST               
102600        ON SIZE ERROR                                                     
102700          MOVE 999.99              TO PRISANDRING                         
102800        END-COMPUTE                                                       
102900                                                                          
103000*  VARNING    0.20 <------GODK-----> 10.00  VARNING                       
103100         IF PRISANDRING < 0.5                                             
103200           IF CLAG-KDTIPPR = 1                                            
103300              MOVE 'S. < 50% '        TO VARNLISTA-ORSAK                  
103400           END-IF                                                         
103500         END-IF                                                           
103600                                                                          
103700         IF PRISANDRING > 2.0                                             
103800           IF CLAG-KDTIPPR = 1                                            
103900              MOVE 'H. > 100%'     TO VARNLISTA-ORSAK                     
104000           END-IF                                                         
104100         END-IF                                                           
104200     END-IF                                                               
104300     .                                                                    
104400 H-SKAPA-RATTPOST SECTION.                                                
104500                                                                          
104600     MOVE WS-KDVALISO              TO WUT-KDVALISO                        
104700     MOVE WIN-IDARTNR              TO WUT-IDARTNR                         
104800     MOVE WIN-IDLEVNR              TO WUT-IDLEVNR                         
104900     MOVE WIN-IDUSER               TO WUT-IDUSER                          
105000     MOVE W-RETULF                 TO WUT-RETULF                          
105100     MOVE WS-PRARTBEL              TO WUT-PRARTBEL-PR                     
105200     MOVE INFIL-TIPRLIST           TO WUT-TIREGDAT                        
105300     MOVE JA                       TO WUT-FLHUVLEV                        
105400     IF WIN-IDPTYP = '985' AND WIN-IDUSER = 'INKOP   '                    
105500       MOVE WIN-KDFPKPRI           TO WUT-KDFPKPRI                        
105600     ELSE                                                                 
105700       MOVE ' '                    TO WUT-KDFPKPRI                        
105800     END-IF                                                               
105900     PERFORM S02-SKRIV-W55312                                             
106000     .                                                                    
106100     EJECT                                                                
106200 I-SKAPA-VARNINGSLISTA SECTION.                                           
106300                                                                          
106400     PERFORM IMS-GU-WDD3-BEN                                              
106500     IF SEGMENT-FINNS                                                     
106600       MOVE TEXT-BEART             TO VARNLISTA-BEART                     
106700     ELSE                                                                 
106800       MOVE ' '                    TO VARNLISTA-BEART                     
106900     END-IF                                                               
107000     IF  (WS-COUNTER3 <=  1 )                                             
107100       MOVE ZERO                     TO WS-COUNTER3                       
107200       CONTINUE                                                           
107300     ELSE                                                                 
107400       MOVE ZERO                     TO WS-COUNTER3                       
107500       MOVE WIN-IDARTNR              TO VARNLISTA-IDARTNR                 
107600       MOVE WIN-IDLEVNR              TO VARNLISTA-LEVNR                   
107700       MOVE WS-PRARTBEL              TO VARNLISTA-PRARTBEL                
107800       MOVE WIN-KDVALISO             TO VARNLISTA-KDVALISO                
107900       MOVE WIN-TIPRLIST             TO VARNLISTA-TIPRLIST                
108000       MOVE WS-PRARTBES-FIRST        TO VARNLISTA-BES-OLD                 
108100       MOVE WS-PRARTBES-FIRST        TO VARNLISTA-OLD-PRARTBES            
108200       MOVE NEW-PRARTBES             TO VARNLISTA-NEW-PRARTBES            
108300       MOVE W-KVPB                   TO VARNLISTA-KVPB                    
108400       MOVE W-PRARTSTD               TO VARNLISTA-PRARTSTD                
108500       MOVE W-TIFINLV                TO VARNLISTA-TIFINLV                 
108600       MOVE W-KVLS                   TO VARNLISTA-KVLS                    
108700       PERFORM J-READ-WDP311                                              
108800       PERFORM S08-SKRIV-VARNINGSRAD                                      
108900     END-IF                                                               
109000     .                                                                    
109100     EJECT                                                                
109200 J-READ-WDP311 SECTION.                                                   
109300                                                                          
109400     IF WDK611-FOUND                                                      
109500       IF CLAG-IDINK NOT = SPACE                                          
109600         IF CLAG-IDINK (1:3) NUMERIC                                      
109700            MOVE CLAG-IDINK (1:3)    TO W-IDPERSON                        
109800         ELSE                                                             
109900            IF CLAG-IDINK (2:3) NUMERIC                                   
110000               MOVE CLAG-IDINK (2:3) TO W-IDPERSON                        
110100            ELSE                                                          
110200               MOVE ZERO             TO W-IDPERSON                        
110300            END-IF                                                        
110400         END-IF                                                           
110500       END-IF                                                             
110600       MOVE W-IDPERSON               TO FELLISTA-IDINK                    
110700                                        VARNLISTA-IDINK                   
111300     END-IF                                                               
111400     PERFORM IMS-GU-WDP311                                                
111500     IF SEGMENT-FINNS                                                     
111600       MOVE PERS-IDNAMN              TO FELLISTA-IDNAMN                   
111700                                        VARNLISTA-IDNAMN                  
111800       MOVE PERS-IDMAIL              TO FELLISTA-IDMAIL                   
111900                                        VARNLISTA-IDMAIL                  
112000       MOVE PERS-IDPERSON            TO FELLISTA-IDINK                    
112100                                        VARNLISTA-IDINK                   
112200     ELSE                                                                 
112300       MOVE SPACE                    TO FELLISTA-IDNAMN                   
112400                                        VARNLISTA-IDNAMN                  
112500                                        FELLISTA-IDMAIL                   
112600                                        VARNLISTA-IDMAIL                  
112700       MOVE ZERO                     TO FELLISTA-IDINK                    
112800                                        VARNLISTA-IDINK                   
112900     END-IF                                                               
113000     .                                                                    
113100     EJECT                                                                
113200 Z-FINIT SECTION.                                                         
113300                                                                          
113400     CLOSE W09286                                                         
113500           W55312                                                         
113600           W55312A                                                        
113700           W55312B                                                        
113800                                                                          
113900     MOVE 'S' TO POSTSUM-OPKOD                                            
114000     CALL POSTSUM USING POSTSUM-PARM                                      
114100     .                                                                    
114200     SKIP3                                                                
114300 S01-LAS-W09286   SECTION.                                                
114400                                                                          
114500     READ W09286 INTO WIN-AREA                                            
114600     AT END                                                               
114700         SET END-OF-W09286 TO TRUE                                        
114800     NOT AT END                                                           
114900         MOVE WIN-IDPTYP  TO POSTSUM-TRANSTYP                             
115000         MOVE 'W09286'    TO POSTSUM-FDNAMN                               
115100         MOVE 'W55312D1'  TO POSTSUM-DDNAMN2                              
115200         CALL POSTSUM USING POSTSUM-PARM                                  
115300     END-READ                                                             
115400     .                                                                    
115500     SKIP3                                                                
115600 S02-SKRIV-W55312 SECTION.                                                
115700                                                                          
115800     WRITE W553-POST FROM WUT-AREA                                        
115900                                                                          
116000     MOVE ' UT '      TO POSTSUM-TRANSTYP                                 
116100     MOVE 'W55312'    TO POSTSUM-FDNAMN                                   
116200     MOVE 'W55312D4'  TO POSTSUM-DDNAMN2                                  
116300     CALL POSTSUM USING POSTSUM-PARM                                      
116400     .                                                                    
116500     EJECT                                                                
116600 S05-SKAPA-FELRAD SECTION.                                                
116700                                                                          
116800     IF WIN-IDLEVNR NOT = '1002'                                          
116900       MOVE WIN-IDARTNR              TO FELLISTA-IDARTNR                  
117000       MOVE WIN-IDLEVNR              TO FELLISTA-LEVNR                    
117100       MOVE WS-PRARTBEL              TO FELLISTA-PRARTBEL                 
117200       MOVE WIN-KDVALISO             TO FELLISTA-KDVALISO                 
117300       MOVE WIN-TIPRLIST             TO FELLISTA-TIPRLIST                 
117400       MOVE WIN-KDFPKPRI             TO FELLISTA-KDFPKPRI                 
117500                                                                          
117600        IF WS-COUNTER1 = ZERO                                             
117700           MOVE  ' ¤DAPW55312-001' TO W001-DAP                            
117800           WRITE W55312A-RAD FROM W001-DAP                                
117900           MOVE  ' ¤DAPW553' TO W001-DAP                                  
118000           WRITE W55312A-RAD FROM W001-DAP                                
118100           WRITE W55312A-RAD FROM W001-RUBRIK2                            
118200        END-IF                                                            
118300        PERFORM J-READ-WDP311                                             
118400        PERFORM S06-SKRIV-W55312A                                         
118500     END-IF                                                               
118600     .                                                                    
118700     EJECT                                                                
118800 S06-SKRIV-W55312A     SECTION.                                           
118900         ADD +1 TO WS-COUNTER1                                            
119000         WRITE W55312A-RAD FROM FELLISTA-DETALJ1                          
119100     .                                                                    
119200     SKIP3                                                                
119300 S08-SKRIV-VARNINGSRAD SECTION.                                           
119400                                                                          
119500     IF WIN-IDLEVNR NOT = '1002'                                          
119600       IF WS-COUNTER2 = ZERO                                              
119700          MOVE ' ¤DAPW55312-002'TO W001-DAP                               
119800          WRITE W55312B-RAD FROM W001-DAP                                 
119900          MOVE ' ¤DAPW553'  TO W001-DAP                                   
120000          WRITE W55312B-RAD FROM W001-DAP                                 
120100          WRITE W55312B-RAD FROM W002-RUBRIK2-4                           
120200       END-IF                                                             
120300       ADD +1 TO WS-COUNTER2                                              
120400       WRITE W55312B-RAD FROM VARNLISTA-DETALJ                            
120500     END-IF                                                               
120600     .                                                                    
120700     EJECT                                                                
120800 S09-HAMTA-SLAGERSALDO  SECTION.                                          
120900                                                                          
121000     PERFORM IMS-GU-WDK701                                                
121100     IF SEGMENT-FINNS                                                     
121200       PERFORM IMS-GNP-WDK711                                             
121300       PERFORM UNTIL SEGMENT-SAKNAS                                       
121400        IF NOT DCS-IDDC = SLAG-IDDC                                       
121500           MOVE SLAG-IDDC       TO W-IDDC-B6                              
121600           PERFORM IMS-GU-WDB601                                          
121700        END-IF                                                            
121800**** OTHER COMPANIES STOCKVALUE SHOULD NOT BE INCLUEDED                   
121900        IF DCS-NDC-NA                                                     
122000        OR DCS-LAND-NON-VCC-OWNED                                         
122100        OR DCS-CHINA                                                      
122200          CONTINUE                                                        
122300        ELSE                                                              
122400          COMPUTE W-KVLS = W-KVLS + SLAG-KVLS +                           
122500              SLAG-KVEFRS + SLAG-KVAKS-PAV + SLAG-KVAKS-SDC               
122600        END-IF                                                            
122700        PERFORM IMS-GNP-WDK711                                            
122800       END-PERFORM                                                        
122900     END-IF                                                               
123000     .                                                                    
123100     EJECT                                                                
123200 IMS-GU-WLARTC01 SECTION.                                                 
123300                                                                          
123400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
123500     DELIMITED  BY SIZE INTO SSA1                                         
123600     MOVE '  GE' TO GODK-STATUSKODER                                      
123700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
123800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
123900     PERFORM IMS-STATUSKONTROLL                                           
124000     .                                                                    
124100     SKIP2                                                                
124200 IMS-GNP-WLARTC11 SECTION.                                                
124300                                                                          
124400     MOVE 'WDK611   ' TO SSA1                                             
124501     MOVE '  GE' TO GODK-STATUSKODER                                      
124600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
124700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
124800     PERFORM IMS-STATUSKONTROLL                                           
124900     .                                                                    
125000     SKIP2                                                                
125100 IMS-GNP-WLARTC21 SECTION.                                                
125200                                                                          
125300     MOVE 'WDK621   ' TO SSA1                                             
125400     MOVE '  GE' TO GODK-STATUSKODER                                      
125500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
125600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
125700     PERFORM IMS-STATUSKONTROLL                                           
125800     .                                                                    
125900     SKIP2                                                                
126000 IMS-GNP-WLARTC21-FIRST SECTION.                                          
126100                                                                          
126200     MOVE 'WDK621  *F' TO SSA1                                            
126300     MOVE '  GE' TO GODK-STATUSKODER                                      
126400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
126500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126900 IMS-GU-WDF101 SECTION.                                                   
127000                                                                          
127100     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
127200     DELIMITED BY SIZE INTO SSA1                                          
127300     MOVE '  GE' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING GU LEV-PCB DLI-IO-WDF101 SSA1                     
127500     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     SKIP3                                                                
127900 IMS-GNP-WDF102 SECTION.                                                  
128000                                                                          
128100     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
128200     DELIMITED BY SIZE INTO SSA1                                          
128300     MOVE '  GE' TO GODK-STATUSKODER                                      
128400     CALL CBLTDLI USING GNP LEV-PCB DLI-IO-WDF102 SSA1                    
128500     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
128600     PERFORM IMS-STATUSKONTROLL                                           
128700     .                                                                    
128800     SKIP3                                                                
128900 IMS-GU-WDD3-BEN SECTION.                                                 
129000                                                                          
129100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
129200            DELIMITED BY SIZE INTO SSA1                                   
129300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
129400            DELIMITED BY SIZE INTO SSA2                                   
129500     MOVE '  GE' TO GODK-STATUSKODER                                      
129600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
129700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
129800     PERFORM IMS-STATUSKONTROLL                                           
129900     .                                                                    
130000     EJECT                                                                
130100 IMS-GU-WDK701 SECTION.                                                   
130200                                                                          
130300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
130400     DELIMITED  BY SIZE INTO SSA1                                         
130500     MOVE '  GE' TO GODK-STATUSKODER                                      
130600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
130700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
130800     PERFORM IMS-STATUSKONTROLL                                           
130900     .                                                                    
131000     SKIP3                                                                
131100 IMS-GNP-WDK711 SECTION.                                                  
131200                                                                          
131300     MOVE 'WDK711   ' TO SSA1                                             
131400     MOVE '  GE' TO GODK-STATUSKODER                                      
131500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
131600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
131700     PERFORM IMS-STATUSKONTROLL                                           
131800     .                                                                    
131900     EJECT                                                                
132000 IMS-GU-WDB601    SECTION.                                                
132100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
132200          DELIMITED BY SIZE INTO SSA1                                     
132300     MOVE '  GE' TO GODK-STATUSKODER                                      
132400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
132500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
132600     PERFORM IMS-STATUSKONTROLL                                           
132700     IF SEGMENT-SAKNAS                                                    
132800         MOVE SPACE TO DCS-KDDC                                           
132900     END-IF                                                               
133000     .                                                                    
133100     EJECT                                                                
133200                                                                          
133300 IMS-GU-WDP311 SECTION.                                                   
133400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
133500          DELIMITED BY SIZE INTO SSA1                                     
133600     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
133700          DELIMITED BY SIZE INTO SSA2                                     
133800     MOVE '  GE' TO GODK-STATUSKODER                                      
133900     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
134000     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
134100     PERFORM IMS-STATUSKONTROLL                                           
134200     .                                                                    
134300     EJECT                                                                
134400                                                                          
134500 IMS-STATUSKONTROLL SECTION.                                              
134600                                                                          
134700     SET STATUS-IX TO 1                                                   
134800     SEARCH GODK-STATUS AT END CALL FELLOG                                
134900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
135000     END-SEARCH                                                           
135100     .                                                                    
135200     EJECT                                                                
136000*    -COPY WY2000P1                                                       
