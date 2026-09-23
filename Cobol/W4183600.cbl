000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4183600.                                                
000400 AUTHOR.         BO HAMMARIN.                                             
000500 DATE-WRITTEN.   97/09/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PGM ADMINISTRERAR RETURER SOM EJ BLIVIT ÅTGÄRDADE INOM           
001000*        UTSATTA TIDSGRÄNSER TROTS EN FÖRSTA PÅMINNELSE.                  
001100*        1. FÖR RETURER SOM HAR PÅMINNELSEDATUM = 0 KONTROLLERAS          
001200*           OM DAGENS-DATUM > RETURDATUM + ANTAL PÅMINNELSEDAGAR          
001300*           (HÄMTAS FRÅN KUNDREG) OCH I FÖREKOMMANDE FALL                 
001400*           - GENERERAS EN VARNING TILL VIPS ALT. EN LISTPOST             
001500*           - UPPDATERAS WDA211                                           
001600*        2. FÖR RETURER SOM HAR PÅMINNELSEDATUM > 0 KONTROLLERAS          
001700*           OM DAGENS-DATUM > PÅMINNELSEDAT + ANTAL ÅTGÄRDSDAGAR          
001800*           (HÄMTAS FRÅN KUNDREG) OCH I FÖREKOMMANDE FALL                 
001900*           - UPPDATERAS WDA201                                           
002000*           - UPPDATERAS WDA211                                           
002100*                                                                         
002200*        PROGRAMMET UPPDATERAR WDA2 VIA WDA2A1                            
002300*        PROGRAMMET LÄSER      WDA3F                                      
002400*        PROGRAMMET LÄSER      WDB2                                       
002500*        PROGRAMMET LÄSER      WDR5  (WDGX4128)                           
002600*                                                                         
002700*                                                                         
002800*    E'TRACKER ID: 1673797 JANUARI 2005                                   
002900*    E'TRACKER ID: 1658417 MARS    2006                                   
003000*    E'TRACKER ID: 6086729 DECEMBER 2007                                  
003100*    E'TRACKER ID: 6155012 JANUARI 2008                                   
003200*    E'TRACKER ID: 6292160 FEBRUARI 2008                                  
003300*    E'TRACKER ID: 10143271 CHINA WAREHOUSE STEP 1                        
003400*    E'TRACKER ID: 10137113 LEAD TIME CLAIM AND RP (2013)                 
003500*                                                                         
003600*    ABENDKODER:                                                          
003700*        U0016 -  . . . .                                                 
003800*        U1000 -  . . . .                                                 
003900*                                                                         
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     SKIP2                                                                
004400 INPUT-OUTPUT SECTION.                                                    
004500                                                                          
004600 FILE-CONTROL.                                                            
004700     SKIP2                                                                
004800*          --- VIPS-TRANSAKTIONER                                         
004900     SELECT W41870                     ASSIGN TO W41836D1.                
005000     SKIP2                                                                
005100*          --- RAPPORT-FIL                                                
005200     SELECT W41871                     ASSIGN TO W41836D2.                
005300     EJECT                                                                
005400 DATA DIVISION.                                                           
005500     SKIP3                                                                
005600 FILE SECTION.                                                            
005700     SKIP3                                                                
005800 FD  W41870                                                               
005900     RECORDING       V                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200 01  RKJ-POST PIC X(80).                                                  
006300     SKIP3                                                                
006400 FD  W41871                                                               
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800*01  RP-POST -COPY W41836    -L.                                          
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007100     SKIP2                                                                
007200*    -COPY WY2000W4                                                       
007300     SKIP3                                                                
007400 77  IDPGM                       PIC X(8)    VALUE 'W4183600'.            
007500 01  CHKP-VAR.                                                            
007600     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
007700     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
007800     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
007900     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
008000     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
008100     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
008200 77  JA                          PIC X       VALUE 'J'.                   
008300 77  NEJ                         PIC X       VALUE 'N'.                   
008400                                                                          
008500 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
008600 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
008700                                                                          
008800 01  SW-REGEL-VALD               PIC X       VALUE 'N'.                   
008900 01  SW-DEF-PER-DISTR            PIC X       VALUE 'N'.                   
009000 01  SW-DEF-PER-DISTR-I          PIC X       VALUE 'N'.                   
009100 01  CURR-IDDISTR-FOM            PIC 9(5)    VALUE ZERO.                  
009200 01  CURR-IDDISTR-TOM            PIC 9(5)    VALUE 9999.                  
009300 01  CURR-IDKUNDNR-FOM           PIC 9(7)    VALUE ZERO.                  
009400 01  CURR-IDKUNDNR-TOM           PIC 9(7)    VALUE 999999.                
009500 01  CURR-KDANMORS               PIC X(2)    VALUE SPACE.                 
009600 01  CURR-IDDISTR-FOM-D          PIC 9(5)    VALUE ZERO.                  
009700 01  CURR-IDDISTR-TOM-D          PIC 9(5)    VALUE 9999.                  
009800 01  CURR-IDKUNDNR-FOM-D         PIC 9(7)    VALUE ZERO.                  
009900 01  CURR-IDKUNDNR-TOM-D         PIC 9(7)    VALUE 999999.                
010000 01  CURR-KDANMORS-D             PIC X(2)    VALUE SPACE.                 
010100 01  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
010200 01  DEF-IDKUNDNR-FOM            PIC S9(7)   VALUE ZERO   COMP-3.         
010300 01  DEF-IDKUNDNR-TOM            PIC S9(7)   VALUE 999999 COMP-3.         
010400 01  DEF-IDDISTR-99              PIC S9(5)   VALUE +99    COMP-3.         
010500 01  DEF-KDANMORS                PIC X(2)    VALUE SPACE.                 
010600 77  WS-KVDAGAR-RET-DEF          PIC 9(3)    VALUE ZERO.                  
010700 77  WS-KVDAGAR-RTRP-DEF         PIC 9(3)    VALUE ZERO.                  
010800                                                                          
010900 77  WS-KVDAGAR-RET              PIC 9(3)    VALUE ZERO.                  
011000 77  WS-KVDAGAR-RTRP             PIC 9(3)    VALUE ZERO.                  
011100                                                                          
011700 77  W-FYRA                      PIC X       VALUE '4'.                   
011800 77  SW-DARTPMN-SKALL-UPPDAT     PIC X       VALUE 'N'.                   
011900 77  SW-KDLEVANM-SKALL-UPPDAT    PIC X       VALUE 'N'.                   
012000 77  SW-VIPS-KUND                PIC X       VALUE 'N'.                   
012100 77  SW-RENAULT                  PIC X       VALUE 'N'.                   
012200 77  SW-EJ-RENAULT-RETUR         PIC X       VALUE 'N'.                   
012300 77  WS-KVRADER-ANN              PIC S9(7)   VALUE +0    COMP-3.          
012400 77  SW-LANDROVER                PIC X       VALUE 'N'.                   
012500 77  SW-EJ-LANDROVER-RETUR       PIC X       VALUE 'N'.                   
012600 77  WS-KVRADER-ANN-LANDROVER    PIC S9(7)   VALUE +0    COMP-3.          
012700 77  WS-IDARTNR-ALFA             PIC X(9)    VALUE SPACE.                 
012800 77  WS-DAGENS-DATUM-AADDD       PIC 9(5).                                
012900 77  WS-ETT-AR                   PIC 9(3)    VALUE 365.                   
013000 77  WS-TVA-AR                   PIC 9(5)    VALUE 730.                   
013100 77  WS-DDD                      PIC 9(3)    VALUE ZERO.                  
013200 77  WS-DARTPMN                  PIC 9(6)    VALUE ZERO.                  
013300 77  WS-DARETILL                 PIC 9(6)    VALUE ZERO.                  
013400     EJECT                                                                
013500 01  WS-ANM-DATUM-AADDD          PIC 9(5)    VALUE ZERO.                  
013600 01  FILLER REDEFINES WS-ANM-DATUM-AADDD.                                 
013700     03  WS-ANM-DATUM-AA         PIC 9(2).                                
013800     03  WS-ANM-DATUM-DDD        PIC 9(3).                                
013900*                                                                         
014000 01  WS-TAL                      PIC 9(2)V9(3) VALUE ZERO.                
014100 01  FILLER REDEFINES WS-TAL.                                             
014200     03  WS-HELTAL               PIC 9(2).                                
014300     03  WS-RESTEN               PIC 9(3).                                
014400*                                                                         
014500 01  WS-JFRDAT                   PIC 9(5).                                
014600 01  WS-JFRDAT-TEST.                                                      
014700     03  WS-JFRDAT-AA            PIC 9(2).                                
014800     03  WS-JFRDAT-DDD           PIC 9(3).                                
014900     SKIP2                                                                
015000 01  FELTEXT.                                                             
015100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015300     EJECT                                                                
015400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015500 01  FILLER REDEFINES DAGENS-DATUM.                                       
015600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015900     EJECT                                                                
016000 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
016100                                                                          
016200 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
016300*01  FILLER   -COPY WWDIST68   -RED TEST-IDDISTR.                         
016400     EJECT                                                                
016500*01  FILLER   -COPY WWDIST47   -RED TEST-IDDISTR.                         
016600     EJECT                                                                
016700*01  FILLER   -COPY WWDIST26   -RED TEST-IDDISTR.                         
016700     EJECT                                                                
016700                                                                          
016800 01  DYNAMISKA-SUBPROGRAM.                                                
016900*                                                                         
017000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
017400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017600     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
017700     EJECT                                                                
017800*    --- PARAMETRAR TILL ABEND                                            
017900                                                                          
018000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018200     SKIP2                                                                
018300*    --- PARAMETRAR TILL DATKORT                                          
018400*                                                                         
018500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41836'.              
018600     SKIP2                                                                
018700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
018800     SKIP2                                                                
018900*01  -COPY WDATKORT                                                       
019000     EJECT                                                                
019100*    --- PARAMETRAR TILL POSTSUM                                          
019200*                                                                         
019300*01  -COPY W0005   -PRE  POSTSUM-                                         
019400     EJECT                                                                
019500 01  FILLER                      PIC X(8)    VALUE 'WDATAREA'.            
019600*    --- PARAMETERS FOR SUBPROGRAM WDATKONV                               
019700*01 -COPY WDATAREA                                                        
019800     EJECT                                                                
019900 01  VIPS-AREA-START             PIC X(24)   VALUE                        
020000                                             'VIPS-AREA-START'.           
020100     SKIP2                                                                
020200                                                                          
020300*01  RKJ-AREA -COPY W461RKJN                                              
020400     EJECT                                                                
020500 01  RAPP-AREA-START             PIC X(24)   VALUE                        
020600                                             'RAPP-AREA-START'.           
020700*01  RP-AREA -COPY W41836                                                 
020800*                                                                         
020900     EJECT                                                                
021000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021100                                                                          
021200 01  NYCKLAR-TILL-DLI.                                                    
021500                                                                          
021300     03  W-IDLEVANM-X.                                                    
021400         05  W-WDA2-IDDISTR      PIC S9(5) COMP-3 VALUE ZERO.             
021500         05  W-WDA2-IDKUNDNR     PIC S9(7) COMP-3 VALUE ZERO.             
021600         05  W-WDA2-IDRAPPNR     PIC 9(7)         VALUE ZERO.             
021700                                                                          
021800     03  W-WDA211KY-X.                                                    
021900         05  W-WDA2-IDARTNR      PIC S9(9) COMP-3 VALUE ZERO.             
022000         05  W-WDA2-IDRADNR      PIC S9(5) COMP-3 VALUE ZERO.             
022100                                                                          
022200     03 W-WDA2A1KY-MIN-X.                                                 
022300        05  W-KDLEVANM-MIN       PIC X(1)         VALUE '4'.              
022400        05  W-IDFTG-MIN          PIC 9(2)         VALUE ZERO.             
022400        05  W-IDDISTR-MIN        PIC S9(5) COMP-3 VALUE ZERO.             
022400        05  W-IDKUNDNR-MIN       PIC S9(7) COMP-3 VALUE ZERO.             
022400        05  W-IDRAPPNR-MIN       PIC 9(7)         VALUE ZERO.             
022800                                                                          
022900     03 W-WDA2A1KY-MAX-X.                                                 
023000        05  W-KDLEVANM-MAX       PIC X(1)         VALUE '4'.              
023100        05  FILLER               PIC X(16)   VALUE HIGH-VALUE.            
023200                                                                          
023300     03  W-WDA2A1KY-X.                                                    
023400         05  W-KDLEVANM          PIC X(1)         VALUE '4'.              
023500         05  W-IDFTG             PIC 9(2)         VALUE ZERO.             
023600         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
023700         05  W-IDKUNDNR          PIC S9(7) COMP-3 VALUE ZERO.             
023800         05  W-IDRAPPNR          PIC 9(7)         VALUE ZERO.             
023900                                                                          
025200     03  W-WDA3F1KY-MIN-X.                                                
025300         05  W-WDA3-IDDC-MIN     PIC X(2)         VALUE SPACE.            
025400         05  W-WDA3-IDDISTR-MIN  PIC S9(5) COMP-3 VALUE ZERO.             
025500         05  W-WDA3-IDKUNDNR-MIN PIC S9(7) COMP-3 VALUE ZERO.             
025600         05  W-WDA3-IDRAPPNR-MIN PIC 9(7)         VALUE ZERO.             
025700         05  W-WDA3-IDRT-MIN     PIC X(3)         VALUE SPACE.            
025800         05  W-WDA3-IDRTLOP-MIN  PIC 9(3)         VALUE ZERO.             
025900         05  W-WDA3-IDKOLLI-MIN  PIC S9(5) COMP-3 VALUE ZERO.             
026000         05  W-WDA3-DAREGDAT-MIN PIC S9(8)        VALUE ZERO.             
026100         05  W-WDA3-TIKLOCK-MIN  PIC S9(9) COMP-3 VALUE ZERO.             
026200     03  W-WDA3F1KY-MAX-X.                                                
026300         05  W-WDA3-IDDC-MAX     PIC X(2)         VALUE SPACE.            
026400         05  W-WDA3-IDDISTR-MAX  PIC S9(5) COMP-3 VALUE ZERO.             
026500         05  W-WDA3-IDKUNDNR-MAX PIC S9(7) COMP-3 VALUE ZERO.             
026600         05  W-WDA3-IDRAPPNR-MAX PIC 9(7)         VALUE ZERO.             
026700         05  W-WDA3-IDRT-MAX     PIC X(3)  VALUE HIGH-VALUE.              
026800         05  W-WDA3-IDRTLOP-MAX  PIC 9(3)  VALUE 999.                     
026900         05  W-WDA3-IDKOLLI-MAX  PIC S9(5) COMP-3                         
027000                                           VALUE +99999.                  
027100         05  W-WDA3-DAREGDAT-MAX PIC S9(8)                                
027200                                           VALUE 99999999.                
027300         05  W-WDA3-TIKLOCK-MAX  PIC S9(9) COMP-3                         
027400                                           VALUE +999999999.              
027500                                                                          
027600     03  W-IDARTNR-X.                                                     
027700         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
027800                                                                          
027900     03  W-IDGMT-X.                                                       
028000         05  W-WDB2-IDDISTR      PIC S9(5) COMP-3 VALUE ZERO.             
028100         05  W-WDB2-IDKUNDNR     PIC S9(7) COMP-3 VALUE ZERO.             
028200                                                                          
028300     03  W-WDGXKEY-4127-X.                                                
028400         05  W-IDHTYP-4127       PIC  X(4)   VALUE '4127'.                
028500         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
028600                                                                          
028700     03  W-KEY4128-X.                                                     
028800         05  W-4128-IDDISTR-FKY  PIC S9(5)        COMP-3.                 
028900         05  W-4128-IDDISTR-TKY  PIC S9(5)        COMP-3.                 
029000         05  W-4128-IDKUNDNR-FKY PIC S9(7)        COMP-3.                 
029100         05  W-4128-IDKUNDNR-TKY PIC S9(7)        COMP-3.                 
029200         05  W-4128-KDANMORS-KY  PIC  X(2).                               
029300                                                                          
029400     03  W-4128-IDDISTR-X.                                                
029500         05  W-4128-IDDISTR      PIC S9(5)        COMP-3.                 
029600     SKIP2                                                                
029700*    --- STATUS-KOD FRÅN IMS                                              
029800 01  STATUS-WS                   PIC XX.                                  
029900     88  SEGMENT-FINNS                       VALUE '  '.                  
030000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
030200     88  IMS-EJ-OK                           VALUE 'XD'.                  
030300     SKIP2                                                                
030400 01  GODK-STATUSKODER.                                                    
030500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030600     SKIP3                                                                
030700 01  ALL-SSA.                                                             
030800     03 SSA1                     PIC X(128).                              
030900     03 SSA2                     PIC X(64).                               
031000     EJECT                                                                
031100*    --- IMS FUNKTIONSKODER                                               
031200*01  -COPY W0003                                                          
031300     EJECT                                                                
031400*    ---  DLI INPUT-OUTPUT AREA                                           
031500                                                                          
031600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA2A1'.                      
031700 01  DLI-IO-WDA2A1.                                                       
031800*    03  -COPY WDA2A1                                                     
031900     EJECT                                                                
032000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
032100 01  DLI-IO-WDA201.                                                       
032200*    03  -COPY WDA201                                                     
032300     EJECT                                                                
032400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
032500 01  DLI-IO-WDA211.                                                       
032600*    03  -COPY WDA211                                                     
032700     EJECT                                                                
032800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA3F1'.                      
032900 01  DLI-IO-WDA3F1.                                                       
033000*    03  -COPY WDA3F1                                                     
033100     EJECT                                                                
033200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
033300 01  DLI-IO-WDB201.                                                       
033400*    03  -COPY WDB201                                                     
033500     EJECT                                                                
033600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
033700 01  DLI-IO-WDGX01.                                                       
033800*    03  -COPY WDGX01                                                     
033900                                                                          
034000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4128'.                    
034100 01  DLI-IO-WDGX4128.                                                     
034200*    03  -COPY WDGX4128                                                   
034300                                                                          
034400*    ---  LÄNKAREA TILL W418OKOD                                          
034500     SKIP3                                                                
034600*    03 -COPY W418OKOD           -PRE OKOD-.                              
034700     EJECT                                                                
034800                                                                          
034900     EJECT                                                                
035000 LINKAGE SECTION.                                                         
035100                                                                          
035200*01  -COPY W0009   -PRE MSG-                                              
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE WDA2A-                                             
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE WDA2-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE WDA3-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE WDB2-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01  -COPY W0008  -PRE WDR5-                                              
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900 PROCEDURE DIVISION  USING MSG-PCB                                        
037000                          WDA2A-PCB                                       
037100                           WDA2-PCB                                       
037200                           WDA3-PCB                                       
037300                           WDB2-PCB                                       
037400                           WDR5-PCB.                                      
037500 MAIN SECTION.                                                            
037600     ENTRY 'DLITCBL' USING MSG-PCB                                        
037700                          WDA2A-PCB                                       
037800                           WDA2-PCB                                       
037900                           WDA3-PCB                                       
038000                           WDB2-PCB                                       
038100                           WDR5-PCB.                                      
038200                                                                          
038300     PERFORM A-INIT                                                       
038400                                                                          
038500*    --- LÄS SAMTLIGA WDA201 VIA WDA2A1 MED KDLEVANM = 4                  
038800     PERFORM IMS-GU-WDA2A1-4                                              
038900                                                                          
039000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
039100                                                                          
039100      MOVE SEQA-IDDISTR        TO TEST-IDDISTR                            
039100      IF NOT (DIST26-EXCP-REF-RETURN)                                     
039200       MOVE SEQA-IDLEVANM                   TO W-IDLEVANM-X               
039300       PERFORM IMS-GU-WDA201                                              
039400       PERFORM IMS-GHNP-WDA211                                            
039500                                                                          
039600       PERFORM UNTIL SEGMENT-SAKNAS                                       
039700         MOVE LEV-IDDC-RET     TO W-WDA3-IDDC-MIN                         
039800                                  W-WDA3-IDDC-MAX                         
039900         MOVE ANM-IDDISTR      TO W-WDA3-IDDISTR-MIN                      
040000                                  W-WDA3-IDDISTR-MAX                      
040100         MOVE ANM-IDKUNDNR     TO W-WDA3-IDKUNDNR-MIN                     
040200                                  W-WDA3-IDKUNDNR-MAX                     
040300         MOVE ANM-IDRAPPNR     TO W-WDA3-IDRAPPNR-MIN                     
040400                                  W-WDA3-IDRAPPNR-MAX                     
040500         PERFORM IMS-GU-WDA3F1                                            
040600                                                                          
040700*  --- OM SEGMENT SAKNAS PÅ WDA3F SKALL POSTEN BEHANDLAS                  
040800*  --- HÄMTA INFO FRÅN KUNDREG                                            
040900                                                                          
041000         IF SEGMENT-SAKNAS                                                
041100           MOVE ANM-IDDISTR    TO W-WDB2-IDDISTR                          
041200           MOVE ANM-IDKUNDNR   TO W-WDB2-IDKUNDNR                         
041300           PERFORM IMS-GU-WDB201                                          
041400           PERFORM H-HAMTA-LEDTID                                         
041500                                                                          
041600           MOVE LEV-IDARTNR  TO W-IDARTNR                                 
041700*  --- OM PÅMINNELSE EJ TIDIGARE UTSKICKAD, SKICKAS PÅMINNELSE            
041800*  --- FOR KODER SOM GENERERAR RETILL                                     
041900*  --- OM PÅMINNELSE TIDIGARE UTSKICKAD, ANNULLERAS RT                    
042000                                                                          
042100           IF ANM-DARTPMN = 0                                             
042200             PERFORM B-BEARBETNING-VARNING                                
042300           ELSE                                                           
042400             PERFORM C-BEARBETNING-ANNULLATION                            
042500           END-IF                                                         
042600                                                                          
042700         END-IF                                                           
042800                                                                          
042900         MOVE NEJ          TO SW-REGEL-VALD                               
043000                              SW-DEF-PER-DISTR                            
043100                              SW-DEF-PER-DISTR-I                          
043200         PERFORM IMS-GHNP-WDA211                                          
043300       END-PERFORM                                                        
043400                                                                          
043500*  --- KONTROLLERA/EV. UTFÖR UPPDAT AV WDA201                             
043600       PERFORM D-UPPDAT-WDA201                                            
043700                                                                          
043800       IF CHKP-ANT > CHKP-MAX                                             
043900         PERFORM X-TAG-CHECKPOINT                                         
044000       END-IF                                                             
044100                                                                          
044200       MOVE NEJ            TO SW-KDLEVANM-SKALL-UPPDAT                    
044300                              SW-DARTPMN-SKALL-UPPDAT                     
044400                              SW-RENAULT                                  
044500                              SW-EJ-RENAULT-RETUR                         
044600                              SW-LANDROVER                                
044700                              SW-EJ-LANDROVER-RETUR                       
044800       MOVE +0             TO WS-KVRADER-ANN                              
044900       MOVE +0             TO WS-KVRADER-ANN-LANDROVER                    
045000                                                                          
045100      END-IF                                                              
045100      PERFORM IMS-GN-WDA2A1-4                                             
045200     END-PERFORM                                                          
045300                                                                          
045400     PERFORM Z-FINIT                                                      
045500                                                                          
045600     MOVE ZERO TO RETURN-CODE                                             
045700     GOBACK                                                               
045800     .                                                                    
045900     EJECT                                                                
046000 A-INIT SECTION.                                                          
046100     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
046200                                                                          
046300     PERFORM IMS-RESTART                                                  
046400                                                                          
046500     OPEN OUTPUT W41870                                                   
046600                 W41871                                                   
046700                                                                          
046800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
046900     MOVE D-AAR                  TO DAGENS-DATUM-AAR                      
047000     MOVE D-MAANAD               TO DAGENS-DATUM-MAANAD                   
047100     MOVE D-DAG                  TO DAGENS-DATUM-DAG                      
047200     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
047300                                                                          
047400     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
047500     MOVE DAGENS-DATUM           TO DAT-I-TIDATUM                         
047600                                                                          
047700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
047800                     DAT-O-TIDATUM DAT-KDSVAR                             
047900                                                                          
048000     IF DAT-KDSVAR-OK                                                     
048100       MOVE DAT-TIAADDD          TO WS-DAGENS-DATUM-AADDD                 
048200     ELSE                                                                 
048300       MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                               
048400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
048500     END-IF                                                               
048600     PERFORM AA-BERAKNA-DEFAULT-LEDTID                                    
048700     .                                                                    
048800     EJECT                                                                
048900 AA-BERAKNA-DEFAULT-LEDTID SECTION.                                       
049000     MOVE 'AA-DEF-LEDTID   ' TO CURRENT-SECTION                           
049100                                                                          
049200     MOVE 1                      TO W-4128-IDDISTR-FKY                    
049300     MOVE 9999                   TO W-4128-IDDISTR-TKY                    
049400     MOVE ZERO                   TO W-4128-IDKUNDNR-FKY                   
049500     MOVE 999999                 TO W-4128-IDKUNDNR-TKY                   
049600     MOVE SPACE                  TO W-4128-KDANMORS-KY                    
049700     PERFORM IMS-GU-WDGX4128                                              
049800                                                                          
049900     IF SEGMENT-FINNS                                                     
050000        MOVE 4128-KVDAGAR-RET    TO WS-KVDAGAR-RET-DEF                    
050100        MOVE 4128-KVDAGAR-RTRP   TO WS-KVDAGAR-RTRP-DEF                   
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 B-BEARBETNING-VARNING SECTION.                                           
050600     MOVE 'B-BEARB-VARNING ' TO CURRENT-SECTION                           
050700                                                                          
050800     MOVE LEV-KDANMORS         TO OKOD-KDANMORS                           
050900     CALL W418OKOD USING OKOD-W418OKOD                                    
051000                                                                          
051100     IF OKOD-FL-RETILL = JA                                               
051200                                                                          
051300*SO-FIX FÖR RENAULT  OCH LANDROVER                                        
051400       IF LEV-KDKREBEH(1:1) = 'Y' OR 'J' OR 'C' OR 'Ä'                    
051500         MOVE JA TO   SW-EJ-RENAULT-RETUR                                 
051600                      SW-EJ-LANDROVER-RETUR                               
051700       END-IF                                                             
051800*SO FIX-SLUT                                                              
051900                                                                          
052000                                                                          
052100       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
052200       MOVE ANM-DARETILL       TO DAT-I-TIDATUM                           
052300                                                                          
052400       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
052500                       DAT-O-TIDATUM DAT-KDSVAR                           
052600                                                                          
052700       IF DAT-KDSVAR-OK                                                   
052800         MOVE DAT-TIAADDD      TO WS-ANM-DATUM-AADDD                      
052900       ELSE                                                               
053000         MOVE 'FEL FRÅN DATKONV' TO FELTEXT                               
053100         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
053200       END-IF                                                             
053300                                                                          
053400       PERFORM S01-KOLLA-DATUM-VARNING                                    
053500                                                                          
053600       IF WS-JFRDAT-DDD > 365                                             
053700         SUBTRACT 365 FROM WS-JFRDAT-DDD                                  
053800         ADD    1   TO   WS-JFRDAT-AA                                     
053900       END-IF                                                             
054000       MOVE WS-JFRDAT-TEST     TO WS-JFRDAT                               
054100                                                                          
054200*      --- KONTROLLERA OM TIDSLIMIT ÄR UPPNÅDD                            
054300*      --- FÖR UTSKICK AV VARNING                                         
054400                                                                          
054500       MOVE WS-DAGENS-DATUM-AADDD   TO TMP1-YYDDD                         
054600       MOVE WS-JFRDAT               TO TMP2-YYDDD                         
054700       PERFORM WY2000P4                                                   
054800       IF TMP1-YYDDD > TMP2-YYDDD                                         
054900         MOVE JA               TO SW-DARTPMN-SKALL-UPPDAT                 
055000                                                                          
055100*      --- VIPS-KUNDER UT PÅ W461RKJ FÖR VIDARE BEARB. I VIPS/            
055200*      --- ÖVRIGA UT PÅ RAPPORTFIL FÖR UTSKRIFT                           
055300                                                                          
055400         MOVE ANM-IDDISTR      TO TEST-IDDISTR                            
055500                                                                          
055600                                                                          
055700         IF DIST68-VIPS-DISTR                                             
055800           PERFORM BA-SKAPA-VIPSTRANS                                     
055900         ELSE                                                             
056000           PERFORM BB-SKAPA-RAPPTRANS                                     
056100         END-IF                                                           
056200       END-IF                                                             
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 BA-SKAPA-VIPSTRANS SECTION.                                              
056700     MOVE 'BA-SKAPA-VIPS   ' TO CURRENT-SECTION                           
056800                                                                          
056900     MOVE 'RKJ'              TO RKJ-IDPTYP                                
057000     MOVE ANM-IDDISTR        TO RKJ-IDDISTR                               
057100     MOVE ANM-IDKUNDNR       TO RKJ-IDKUNDNR                              
057200     MOVE ANM-IDRAPPNR       TO RKJ-IDRAPPNR                              
057300     MOVE LEV-IDRADNR        TO RKJ-IDRADNR                               
057400     MOVE LEV-KDANMORS       TO RKJ-KDANMORS                              
057500                                                                          
057600*- KOD 27 SKALL BARA FINNAS INOM PULS, BYTS TILL 22 IGEN I VIPS.          
057700     IF RKJ-KDANMORS = 27                                                 
057800       MOVE 22               TO RKJ-KDANMORS                              
057900     END-IF                                                               
058000                                                                          
058100     MOVE LEV-IDARTNR        TO RKJ-IDARTNR                               
058200     MOVE LEV-KVLEVANM       TO RKJ-KVLEVANM                              
058300     MOVE DAGENS-DATUM       TO RKJ-DARTPMN                               
058400     MOVE WS-KVDAGAR-RTRP    TO RKJ-KVDAGAR-RTATG                         
058500                                                                          
058600     PERFORM S11-SKRIV-W461RKJ                                            
058700     .                                                                    
058800     EJECT                                                                
058900 BB-SKAPA-RAPPTRANS SECTION.                                              
059000     MOVE 'BB-SKAPA-RAPP   ' TO CURRENT-SECTION                           
059100                                                                          
059200     MOVE 'RP '              TO RP-IDPTYP                                 
059300     MOVE ANM-IDFTG          TO RP-IDFTG                                  
059400     MOVE ANM-IDDISTR        TO RP-IDDISTR                                
059500     MOVE ANM-IDKUNDNR       TO RP-IDKUNDNR                               
059600     MOVE ANM-IDRAPPNR       TO RP-IDRAPPNR                               
059700     MOVE LEV-IDRADNR        TO RP-IDRADNR                                
059800     MOVE LEV-KDKREBEH       TO RP-KDKREBEH                               
059900     MOVE LEV-IDARTNR        TO RP-IDARTNR                                
060000     MOVE LEV-KVLEVANM       TO RP-KVLEVANM                               
060100     MOVE DAGENS-DATUM       TO RP-DARTPMN                                
060200     MOVE WS-KVDAGAR-RTRP    TO RP-KVDAGAR-RTATG                          
060300                                                                          
060400     PERFORM S12-SKRIV-W41836                                             
060500     .                                                                    
060600     EJECT                                                                
060700 C-BEARBETNING-ANNULLATION SECTION.                                       
060800     MOVE 'C-BEARB-SNNULL  ' TO CURRENT-SECTION                           
060900                                                                          
061000*SO FIX FÖR RENAULT OCH LANDROVER                                         
061100     MOVE LEV-KDANMORS         TO OKOD-KDANMORS                           
061200     CALL W418OKOD USING OKOD-W418OKOD                                    
061300                                                                          
061400     IF OKOD-FL-RETILL = JA                                               
061500       IF LEV-KDKREBEH(1:1) = 'Y' OR 'J' OR 'C' OR 'Ä'                    
061600         MOVE JA       TO SW-EJ-RENAULT-RETUR                             
061700                          SW-EJ-LANDROVER-RETUR                           
061800       END-IF                                                             
061900     END-IF                                                               
062000*SO FIX-SLUT                                                              
062100                                                                          
062200     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
062300     MOVE ANM-DARETILL         TO WS-DARETILL                             
062400     MOVE WS-DARETILL          TO DAT-I-TIDATUM                           
062500                                                                          
062600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
062700                     DAT-O-TIDATUM DAT-KDSVAR                             
062800                                                                          
062900     IF DAT-KDSVAR-OK                                                     
063000       MOVE DAT-TIAADDD        TO WS-ANM-DATUM-AADDD                      
063100     ELSE                                                                 
063200       MOVE 'FEL FRÅN DATKONV' TO FELTEXT                                 
063300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
063400     END-IF                                                               
063500                                                                          
063600     PERFORM S02-KOLLA-DATUM-ANN                                          
063700                                                                          
063800     IF WS-JFRDAT-DDD > 365                                               
063900       SUBTRACT 365 FROM WS-JFRDAT-DDD                                    
064000       ADD      1   TO   WS-JFRDAT-AA                                     
064100     END-IF                                                               
064200     MOVE WS-JFRDAT-TEST       TO WS-JFRDAT                               
064300                                                                          
064400*    --- KONTROLLERA OM TIDSLIMIT ÄR UPPNÅDD                              
064500*    --- FÖR ANNULLATION                                                  
064600     MOVE WS-DAGENS-DATUM-AADDD   TO TMP1-YYDDD                           
064700     MOVE WS-JFRDAT               TO TMP2-YYDDD                           
064800     PERFORM WY2000P4                                                     
064900     IF TMP1-YYDDD > TMP2-YYDDD                                           
065000                                                                          
065100       MOVE LEV-KDANMORS TO OKOD-KDANMORS                                 
065200       CALL W418OKOD USING OKOD-W418OKOD                                  
065300                                                                          
065400       IF OKOD-FL-RETILL = JA                                             
065500                                                                          
065600         IF LEV-KDKREBEH(1:1) = 'Y' OR 'J' OR 'C' OR 'Ä'                  
065700           MOVE JA       TO SW-KDLEVANM-SKALL-UPPDAT                      
065800                                                                          
065900           MOVE 'ANN'    TO LEV-KDKREBEH                                  
066000           MOVE 'J'      TO LEV-FLANNULL                                  
066100                                                                          
066200           PERFORM IMS-REPL-WDA211                                        
066300           ADD +1        TO CHKP-ANT                                      
066400         END-IF                                                           
066500       END-IF                                                             
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 D-UPPDAT-WDA201 SECTION.                                                 
067000     MOVE 'C-UPD-WDA201    ' TO CURRENT-SECTION                           
067100                                                                          
067200     IF SW-KDLEVANM-SKALL-UPPDAT = JA                                     
067300                                                                          
067400       MOVE ANM-IDDISTR      TO W-WDA2-IDDISTR                            
067500       MOVE ANM-IDKUNDNR     TO W-WDA2-IDKUNDNR                           
067600       MOVE ANM-IDRAPPNR     TO W-WDA2-IDRAPPNR                           
067700       MOVE ANM-IDFTG        TO W-IDFTG                                   
067800                                                                          
067900       PERFORM IMS-GHU-WDA201                                             
068000                                                                          
068100       MOVE '7'              TO ANM-KDLEVANM                              
068200       PERFORM IMS-REPL-WDA201                                            
068300       ADD +1                TO CHKP-ANT                                  
068400     ELSE                                                                 
068500       IF SW-DARTPMN-SKALL-UPPDAT = JA                                    
068600         MOVE ANM-IDDISTR    TO W-WDA2-IDDISTR                            
068700         MOVE ANM-IDKUNDNR   TO W-WDA2-IDKUNDNR                           
068800         MOVE ANM-IDRAPPNR   TO W-WDA2-IDRAPPNR                           
068900         MOVE ANM-IDFTG      TO W-IDFTG                                   
069000                                                                          
069100         PERFORM IMS-GHU-WDA201                                           
069200                                                                          
069300         MOVE DAGENS-DATUM   TO ANM-DARTPMN                               
069400                                                                          
069500         IF DAGENS-DATUM-AAR > 50                                         
069600           MOVE 19           TO ANM-DARTPMN (1:2)                         
069700         ELSE                                                             
069800           MOVE 20           TO ANM-DARTPMN (1:2)                         
069900         END-IF                                                           
070000                                                                          
070100*SO-- FIX FÖR RENAULT                                                     
070200         IF SW-RENAULT = JA                                               
070300           COMPUTE ANM-KVRADER-RT   =                                     
070400                   ANM-KVRADER-RT   - WS-KVRADER-ANN                      
070500           COMPUTE ANM-KVRADER-OBEH =                                     
070600                   ANM-KVRADER-OBEH - WS-KVRADER-ANN                      
070700         END-IF                                                           
070800*SO-- FIX-SLUT                                                            
070900                                                                          
071000*SO-- FIX FÖR LANDROVER                                                   
071100         IF SW-LANDROVER = JA                                             
071200           COMPUTE ANM-KVRADER-RT   =                                     
071300                   ANM-KVRADER-RT   - WS-KVRADER-ANN-LANDROVER            
071400           COMPUTE ANM-KVRADER-OBEH =                                     
071500                   ANM-KVRADER-OBEH - WS-KVRADER-ANN-LANDROVER            
071600         END-IF                                                           
071700*SO-- FIX-SLUT                                                            
071800                                                                          
071900         PERFORM IMS-REPL-WDA201                                          
072000         ADD +1              TO CHKP-ANT                                  
072100       END-IF                                                             
072200     END-IF                                                               
072300                                                                          
072400*SO --- FIX FÖR ANNULLATION AV RENAULT ARTIKLAR 20080116                  
072500     IF SW-KDLEVANM-SKALL-UPPDAT = NEJ AND                                
072600        SW-DARTPMN-SKALL-UPPDAT = NEJ  AND                                
072700        SW-EJ-RENAULT-RETUR = NEJ      AND                                
072800        SW-RENAULT = JA                                                   
072900                                                                          
073000        MOVE ANM-IDDISTR      TO W-WDA2-IDDISTR                           
073100        MOVE ANM-IDKUNDNR     TO W-WDA2-IDKUNDNR                          
073200        MOVE ANM-IDRAPPNR     TO W-WDA2-IDRAPPNR                          
073300        MOVE ANM-IDFTG        TO W-IDFTG                                  
073400                                                                          
073500        PERFORM IMS-GHU-WDA201                                            
073600                                                                          
073700        MOVE '7'              TO ANM-KDLEVANM                             
073800                                                                          
073900        PERFORM IMS-REPL-WDA201                                           
074000        ADD +1                TO CHKP-ANT                                 
074100     END-IF                                                               
074200                                                                          
074300     IF SW-KDLEVANM-SKALL-UPPDAT = NEJ AND                                
074400        SW-DARTPMN-SKALL-UPPDAT = NEJ  AND                                
074500        SW-EJ-LANDROVER-RETUR = NEJ      AND                              
074600        SW-LANDROVER = JA                                                 
074700                                                                          
074800        MOVE ANM-IDDISTR      TO W-WDA2-IDDISTR                           
074900        MOVE ANM-IDKUNDNR     TO W-WDA2-IDKUNDNR                          
075000        MOVE ANM-IDRAPPNR     TO W-WDA2-IDRAPPNR                          
075100        MOVE ANM-IDFTG        TO W-IDFTG                                  
075200                                                                          
075300        PERFORM IMS-GHU-WDA201                                            
075400                                                                          
075500        MOVE '7'              TO ANM-KDLEVANM                             
075600                                                                          
075700        PERFORM IMS-REPL-WDA201                                           
075800        ADD +1                TO CHKP-ANT                                 
075900     END-IF                                                               
076000                                                                          
076100     IF SW-KDLEVANM-SKALL-UPPDAT = NEJ AND                                
076200        SW-DARTPMN-SKALL-UPPDAT = NEJ  AND                                
076300        SW-EJ-RENAULT-RETUR = JA       AND                                
076400        SW-RENAULT = JA                                                   
076500                                                                          
076600        MOVE ANM-IDDISTR      TO W-WDA2-IDDISTR                           
076700        MOVE ANM-IDKUNDNR     TO W-WDA2-IDKUNDNR                          
076800        MOVE ANM-IDRAPPNR     TO W-WDA2-IDRAPPNR                          
076900        MOVE ANM-IDFTG        TO W-IDFTG                                  
077000                                                                          
077100        PERFORM IMS-GHU-WDA201                                            
077200                                                                          
077300        COMPUTE ANM-KVRADER-RT   =                                        
077400                ANM-KVRADER-RT   - WS-KVRADER-ANN                         
077500        COMPUTE ANM-KVRADER-OBEH =                                        
077600                ANM-KVRADER-OBEH - WS-KVRADER-ANN                         
077700                                                                          
077800        PERFORM IMS-REPL-WDA201                                           
077900        ADD +1                TO CHKP-ANT                                 
078000     END-IF                                                               
078100                                                                          
078200     IF SW-KDLEVANM-SKALL-UPPDAT = NEJ AND                                
078300        SW-DARTPMN-SKALL-UPPDAT = NEJ  AND                                
078400        SW-EJ-LANDROVER-RETUR = JA       AND                              
078500        SW-LANDROVER = JA                                                 
078600                                                                          
078700        MOVE ANM-IDDISTR      TO W-WDA2-IDDISTR                           
078800        MOVE ANM-IDKUNDNR     TO W-WDA2-IDKUNDNR                          
078900        MOVE ANM-IDRAPPNR     TO W-WDA2-IDRAPPNR                          
079000        MOVE ANM-IDFTG        TO W-IDFTG                                  
079100                                                                          
079200        PERFORM IMS-GHU-WDA201                                            
079300                                                                          
079400        COMPUTE ANM-KVRADER-RT   =                                        
079500                ANM-KVRADER-RT   - WS-KVRADER-ANN-LANDROVER               
079600        COMPUTE ANM-KVRADER-OBEH =                                        
079700                ANM-KVRADER-OBEH - WS-KVRADER-ANN-LANDROVER               
079800                                                                          
079900        PERFORM IMS-REPL-WDA201                                           
080000        ADD +1                TO CHKP-ANT                                 
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400 H-HAMTA-LEDTID SECTION.                                                  
080500     MOVE 'H-HAMTA-LEDTID  ' TO CURRENT-SECTION                           
080600                                                                          
080700     MOVE SPACE                TO CURR-KDANMORS                           
080800     MOVE ANM-IDDISTR          TO W-4128-IDDISTR-FKY                      
080900                                  W-4128-IDDISTR-TKY                      
081000                                  W-4128-IDDISTR                          
081100                                  TEST-IDDISTR                            
081200     MOVE ANM-IDKUNDNR         TO W-4128-IDKUNDNR-FKY                     
081300                                  W-4128-IDKUNDNR-TKY                     
081400     MOVE LEV-KDANMORS         TO W-4128-KDANMORS-KY                      
081500                                  WS-KDANMORS                             
081600                                                                          
081700     PERFORM IMS-GU-WDGX4128                                              
081800     IF SEGMENT-SAKNAS                                                    
081900        PERFORM IMS-GU-WDR501                                             
082000        PERFORM IMS-GNP-WDGX4128-DISTRIKT                                 
082100        PERFORM UNTIL SEGMENT-SAKNAS                                      
082200           PERFORM HA-KOLLA-SPARA-REGEL                                   
082300           PERFORM IMS-GNP-WDGX4128-DISTRIKT                              
082400        END-PERFORM                                                       
082500     ELSE                                                                 
082600        MOVE JA                  TO SW-REGEL-VALD                         
082700        MOVE 4128-IDDISTR-FOM    TO CURR-IDDISTR-FOM                      
082800        MOVE 4128-IDDISTR-TOM    TO CURR-IDDISTR-TOM                      
082900        MOVE 4128-IDKUNDNR-FOM   TO CURR-IDKUNDNR-FOM                     
083000        MOVE 4128-IDKUNDNR-TOM   TO CURR-IDKUNDNR-TOM                     
083100        MOVE 4128-KDANMORS       TO CURR-KDANMORS                         
083200     END-IF                                                               
083300                                                                          
083400     IF SW-REGEL-VALD = JA OR                                             
083500        SW-DEF-PER-DISTR   OR                                             
083600        SW-DEF-PER-DISTR-I                                                
083700       IF SW-DEF-PER-DISTR-I = JA                                         
083800          MOVE CURR-IDDISTR-FOM-D    TO W-4128-IDDISTR-FKY                
083900          MOVE CURR-IDDISTR-TOM-D    TO W-4128-IDDISTR-TKY                
084000          MOVE CURR-IDKUNDNR-FOM-D TO W-4128-IDKUNDNR-FKY                 
084100          MOVE CURR-IDKUNDNR-TOM-D TO W-4128-IDKUNDNR-TKY                 
084200          MOVE CURR-KDANMORS-D       TO W-4128-KDANMORS-KY                
084300       ELSE                                                               
084400          MOVE CURR-IDDISTR-FOM TO W-4128-IDDISTR-FKY                     
084500          MOVE CURR-IDDISTR-TOM TO W-4128-IDDISTR-TKY                     
084600          MOVE CURR-IDKUNDNR-FOM TO W-4128-IDKUNDNR-FKY                   
084700          MOVE CURR-IDKUNDNR-TOM TO W-4128-IDKUNDNR-TKY                   
084800          MOVE CURR-KDANMORS     TO W-4128-KDANMORS-KY                    
084900       END-IF                                                             
085000                                                                          
085100       PERFORM IMS-GU-WDGX4128                                            
085200                                                                          
085300       IF SEGMENT-FINNS                                                   
085400          IF GMT-FLLDCKND = JA                                            
085500             IF 4128-KVDAGAR-RET-LDC > ZERO                               
085600                MOVE 4128-KVDAGAR-RET-LDC  TO WS-KVDAGAR-RET              
085700             ELSE                                                         
085800                MOVE 4128-KVDAGAR-RET      TO WS-KVDAGAR-RET              
085900             END-IF                                                       
086000             IF 4128-KVDAGAR-RTRP-LDC > ZERO                              
086100                MOVE 4128-KVDAGAR-RTRP-LDC TO WS-KVDAGAR-RTRP             
086200             ELSE                                                         
086300                MOVE 4128-KVDAGAR-RTRP     TO WS-KVDAGAR-RTRP             
086400             END-IF                                                       
086500          ELSE                                                            
086600             MOVE 4128-KVDAGAR-RET         TO WS-KVDAGAR-RET              
086700             MOVE 4128-KVDAGAR-RTRP        TO WS-KVDAGAR-RTRP             
086800          END-IF                                                          
086900       END-IF                                                             
087000     ELSE                                                                 
087100       MOVE WS-KVDAGAR-RET-DEF             TO WS-KVDAGAR-RET              
087200       MOVE WS-KVDAGAR-RTRP-DEF            TO WS-KVDAGAR-RTRP             
087300     END-IF                                                               
087400     .                                                                    
087500     EJECT                                                                
087600 HA-KOLLA-SPARA-REGEL SECTION.                                            
087700     MOVE 'HA-KOLLA-SPARA  ' TO CURRENT-SECTION                           
087800                                                                          
087900     IF 4128-IDKUNDNR-FOM = DEF-IDKUNDNR-FOM AND                          
088000        4128-IDKUNDNR-TOM = DEF-IDKUNDNR-TOM AND                          
088100        4128-KDANMORS     = DEF-KDANMORS                                  
088200       IF DIST47-INTERNA AND 4128-IDDISTR-TOM = DEF-IDDISTR-99            
088300         MOVE JA TO SW-DEF-PER-DISTR-I                                    
088400         MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM                       
088500         MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM                       
088600         MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM                      
088700         MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM                      
088800         MOVE 4128-KDANMORS     TO CURR-KDANMORS                          
088900         MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM-D                     
089000         MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM-D                     
089100         MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM-D                    
089200         MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM-D                    
089300         MOVE 4128-KDANMORS     TO CURR-KDANMORS-D                        
089400       ELSE                                                               
089500         IF NOT DIST47-INTERNA                                            
089600           MOVE JA TO SW-DEF-PER-DISTR                                    
089700           MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM                     
089800           MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM                     
089900           MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM                    
090000           MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM                    
090100           MOVE 4128-KDANMORS     TO CURR-KDANMORS                        
090200         END-IF                                                           
090300       END-IF                                                             
090400     END-IF                                                               
090500     IF 4128-KDANMORS(1:1) = WS-KDANMORS(1:1)                             
090600     OR 4128-KDANMORS      = SPACE                                        
090700        IF  4128-IDKUNDNR-FOM = W-4128-IDKUNDNR-FKY                       
090800        AND 4128-IDKUNDNR-TOM = W-4128-IDKUNDNR-TKY                       
090900          IF  4128-KDANMORS = WS-KDANMORS                                 
091000          OR (4128-KDANMORS(2:1) = 'X'                                    
091100          AND 4128-KDANMORS(1:1) = WS-KDANMORS(1:1))                      
091200          OR  4128-KDANMORS = SPACE                                       
091300            MOVE JA                TO SW-REGEL-VALD                       
091400            MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM                    
091500            MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM                    
091600            MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM                   
091700            MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM                   
091800            MOVE 4128-KDANMORS     TO CURR-KDANMORS                       
091900          END-IF                                                          
092000        ELSE                                                              
092100           IF  W-4128-IDKUNDNR-FKY NOT < 4128-IDKUNDNR-FOM                
092200           AND W-4128-IDKUNDNR-TKY NOT > 4128-IDKUNDNR-TOM                
092300               IF  (4128-IDKUNDNR-FOM > CURR-IDKUNDNR-FOM                 
092400                OR  4128-IDKUNDNR-TOM < CURR-IDKUNDNR-TOM)                
092500                   MOVE JA                TO SW-REGEL-VALD                
092600                   MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM             
092700                   MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM             
092800                   MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM            
092900                   MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM            
093000                   MOVE 4128-KDANMORS     TO CURR-KDANMORS                
093100               ELSE                                                       
093200                IF (4128-IDKUNDNR-FOM = CURR-IDKUNDNR-FOM                 
093300                AND 4128-IDKUNDNR-TOM = CURR-IDKUNDNR-TOM)                
093400                  IF  4128-KDANMORS = WS-KDANMORS                         
093500                  OR (4128-KDANMORS(2:1) = 'X'                            
093600                  AND 4128-KDANMORS(1:1) = WS-KDANMORS(1:1))              
093700                  OR  4128-KDANMORS = SPACE                               
093800                     MOVE JA                TO SW-REGEL-VALD              
093900                     MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM           
094000                     MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM           
094100                     MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM          
094200                     MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM          
094300                     MOVE 4128-KDANMORS     TO CURR-KDANMORS              
094400                  END-IF                                                  
094500                END-IF                                                    
094600               END-IF                                                     
094700           END-IF                                                         
094800        END-IF                                                            
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200 Z-FINIT SECTION.                                                         
095300     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
095400                                                                          
095500     CLOSE W41870                                                         
095600           W41871                                                         
095700                                                                          
095800     MOVE 'S' TO POSTSUM-OPKOD                                            
095900     CALL POSTSUM USING POSTSUM-PARM                                      
096000     .                                                                    
096100     EJECT                                                                
096200 S01-KOLLA-DATUM-VARNING SECTION.                                         
096300                                                                          
096400     IF GMT-FLLDCKND = JA                                                 
096500        COMPUTE WS-TAL   =  (WS-KVDAGAR-RET - 1) / 356                    
096600     ELSE                                                                 
096700        COMPUTE WS-TAL   =  (WS-KVDAGAR-RET - 3) / 356                    
096800     END-IF                                                               
096900                                                                          
097000     IF WS-HELTAL = 0                                                     
097100       MOVE WS-ANM-DATUM-AA      TO WS-JFRDAT-AA                          
097200                                                                          
097300       IF GMT-FLLDCKND = JA                                               
097400          COMPUTE WS-JFRDAT-DDD = WS-ANM-DATUM-DDD +                      
097500                                  WS-KVDAGAR-RET - 1                      
097600       ELSE                                                               
097700          COMPUTE WS-JFRDAT-DDD = WS-ANM-DATUM-DDD +                      
097800                                  WS-KVDAGAR-RET - 3                      
097900       END-IF                                                             
098000     END-IF                                                               
098100                                                                          
098200     IF WS-HELTAL = 1                                                     
098300       IF GMT-FLLDCKND = JA                                               
098400          COMPUTE WS-DDD = (WS-KVDAGAR-RET - 1) - WS-ETT-AR               
098500       ELSE                                                               
098600          COMPUTE WS-DDD = (WS-KVDAGAR-RET - 3) - WS-ETT-AR               
098700       END-IF                                                             
098800                                                                          
098900                                                                          
099000       COMPUTE WS-JFRDAT-AA    = WS-ANM-DATUM-AA +                        
099100                                 1                                        
099200       END-COMPUTE                                                        
099300                                                                          
099400       COMPUTE WS-JFRDAT-DDD   = WS-ANM-DATUM-DDD +                       
099500                                 WS-DDD                                   
099600       END-COMPUTE                                                        
099700     END-IF                                                               
099800                                                                          
099900     IF WS-HELTAL = 2                                                     
100000       IF GMT-FLLDCKND = JA                                               
100100          COMPUTE WS-DDD = (WS-KVDAGAR-RET - 1) - WS-TVA-AR               
100200       ELSE                                                               
100300          COMPUTE WS-DDD = (WS-KVDAGAR-RET - 3) - WS-TVA-AR               
100400       END-IF                                                             
100500                                                                          
100600       COMPUTE WS-JFRDAT-AA    = WS-ANM-DATUM-AA +                        
100700                                 2                                        
100800       END-COMPUTE                                                        
100900                                                                          
101000       COMPUTE WS-JFRDAT-DDD   = WS-ANM-DATUM-DDD +                       
101100                                 WS-DDD                                   
101200       END-COMPUTE                                                        
101300     END-IF                                                               
101400     .                                                                    
101500     EJECT                                                                
101600 S02-KOLLA-DATUM-ANN SECTION.                                             
101700                                                                          
101800     COMPUTE WS-TAL = (WS-KVDAGAR-RET + WS-KVDAGAR-RTRP) / 365            
101900                                                                          
102000     IF WS-HELTAL = 0                                                     
102100       MOVE WS-ANM-DATUM-AA TO WS-JFRDAT-AA                               
102200                                                                          
102300       COMPUTE WS-JFRDAT-DDD = WS-ANM-DATUM-DDD +                         
102400                               WS-KVDAGAR-RET + WS-KVDAGAR-RTRP           
102500       END-COMPUTE                                                        
102600     END-IF                                                               
102700                                                                          
102800     IF WS-HELTAL = 1                                                     
102900       COMPUTE WS-DDD        = WS-KVDAGAR-RET + WS-KVDAGAR-RTRP -         
103000                               WS-ETT-AR                                  
103100       END-COMPUTE                                                        
103200                                                                          
103300       COMPUTE WS-JFRDAT-AA  = WS-ANM-DATUM-AA +                          
103400                               1                                          
103500       END-COMPUTE                                                        
103600                                                                          
103700       COMPUTE WS-JFRDAT-DDD = WS-ANM-DATUM-DDD +                         
103800                               WS-DDD                                     
103900       END-COMPUTE                                                        
104000     END-IF                                                               
104100                                                                          
104200     IF WS-HELTAL = 2                                                     
104300       COMPUTE WS-DDD        = WS-KVDAGAR-RET + WS-KVDAGAR-RTRP -         
104400                               WS-TVA-AR                                  
104500       END-COMPUTE                                                        
104600                                                                          
104700       COMPUTE WS-JFRDAT-AA  = WS-ANM-DATUM-AA +                          
104800                               2                                          
104900       END-COMPUTE                                                        
105000                                                                          
105100       COMPUTE WS-JFRDAT-DDD = WS-ANM-DATUM-DDD +                         
105200                               WS-DDD                                     
105300       END-COMPUTE                                                        
105400     END-IF                                                               
105500     .                                                                    
105600     EJECT                                                                
105700 S11-SKRIV-W461RKJ SECTION.                                               
105800                                                                          
105900     WRITE RKJ-POST FROM RKJ-AREA                                         
106000                                                                          
106100     MOVE RKJ-IDPTYP     TO POSTSUM-TRANSTYP                              
106200     MOVE 'W461RKJ'      TO POSTSUM-FDNAMN                                
106300     MOVE 'W41836D1'     TO POSTSUM-DDNAMN2                               
106400     CALL POSTSUM USING POSTSUM-PARM                                      
106500     .                                                                    
106600     EJECT                                                                
106700 S12-SKRIV-W41836 SECTION.                                                
106800                                                                          
106900     WRITE RP-POST FROM RP-AREA                                           
107000                                                                          
107100     MOVE RP-IDPTYP    TO POSTSUM-TRANSTYP                                
107200     MOVE 'W41836 '    TO POSTSUM-FDNAMN                                  
107300     MOVE 'W41836D2'   TO POSTSUM-DDNAMN2                                 
107400     CALL POSTSUM USING POSTSUM-PARM                                      
107500     .                                                                    
107600     EJECT                                                                
107700 X-TAG-CHECKPOINT   SECTION.                                              
107800                                                                          
107900* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
108000* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
108100     PERFORM IMS-CHECKPOINT                                               
108200     MOVE ZERO              TO CHKP-ANT                                   
108300                                                                          
108400* --- LÄS OM DATABAS OM DET BEHÖVS                                        
108500*                                                                         
108600     MOVE ANM-IDDISTR       TO W-IDDISTR                                  
108700     MOVE ANM-IDKUNDNR      TO W-IDKUNDNR                                 
108800     MOVE ANM-IDRAPPNR      TO W-IDRAPPNR                                 
108900     MOVE ANM-IDFTG         TO W-IDFTG                                    
109000     PERFORM IMS-GU-WDA2A1-UNIK                                           
109100                                                                          
109200     IF SEGMENT-SAKNAS                                                    
109300****** DET BETYDER ATT KDLEVANM HAR ÄNDRATS !!!                           
109400****** GN-LÄSNINGEN I HUVUDSLINGAN KLARAR AV ATT POSITIONERA              
109500****** I BASEN MHA INITIERINGEN AV MIN-NYCKLARNA NEDAN !!! LASSI          
109600       MOVE ANM-IDDISTR     TO W-IDDISTR-MIN                              
109700       MOVE ANM-IDKUNDNR    TO W-IDKUNDNR-MIN                             
109800       MOVE ANM-IDRAPPNR    TO W-IDRAPPNR-MIN                             
109900       MOVE ANM-IDFTG       TO W-IDFTG-MIN                                
110000     END-IF                                                               
110100     .                                                                    
110200     EJECT                                                                
110300* --- IMS SEKTIONER ---                                                   
110400 IMS-GU-WDA2A1-UNIK SECTION.                                              
110500     MOVE 'GU-WDA2A1-UNIK  ' TO CURRENT-SECTION                           
110600                                                                          
110700     MOVE SPACE            TO ALL-SSA                                     
110800     STRING 'WDA2A1  (WDA2A1KY =' W-WDA2A1KY-X ')'                        
110900          DELIMITED BY SIZE INTO SSA1                                     
111000     MOVE '  GE'           TO GODK-STATUSKODER                            
111100     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
111200     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500     EJECT                                                                
111600 IMS-GU-WDA2A1-4 SECTION.                                                 
111700     MOVE 'GU-WDA2A1       ' TO CURRENT-SECTION                           
111800                                                                          
111900     MOVE SPACE            TO ALL-SSA                                     
112000     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
112100                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X ')'                    
112600          DELIMITED BY SIZE INTO SSA1                                     
112700     MOVE '  GE'           TO GODK-STATUSKODER                            
112800     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
112900     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
113000     PERFORM IMS-STATUSKONTROLL                                           
113100     .                                                                    
113200     EJECT                                                                
113300 IMS-GN-WDA2A1-4 SECTION.                                                 
113400     MOVE 'GN-WDA2A1       ' TO CURRENT-SECTION                           
113500                                                                          
113600     MOVE SPACE            TO ALL-SSA                                     
113700     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
113800                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X ')'                    
114300          DELIMITED BY SIZE INTO SSA1                                     
114400     MOVE '  GEGB'         TO GODK-STATUSKODER                            
114500     CALL CBLTDLI USING GN WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
114600     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
114700     PERFORM IMS-STATUSKONTROLL                                           
114800     .                                                                    
114900     EJECT                                                                
115000 IMS-GU-WDA201 SECTION.                                                   
115100     MOVE 'GU-WDA201       ' TO CURRENT-SECTION                           
115200                                                                          
115300     MOVE SPACE            TO ALL-SSA                                     
115400     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
115500          DELIMITED BY SIZE INTO SSA1                                     
115600     MOVE '  GE'         TO GODK-STATUSKODER                              
115700     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-WDA201 SSA1                    
115800     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
115900     PERFORM IMS-STATUSKONTROLL                                           
116000     .                                                                    
116100     SKIP3                                                                
116200 IMS-GHU-WDA201 SECTION.                                                  
116300     MOVE 'GHU-WDA201      ' TO CURRENT-SECTION                           
116400                                                                          
116500     MOVE SPACE            TO ALL-SSA                                     
116600     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
116700          DELIMITED BY SIZE INTO SSA1                                     
116800     MOVE '  GE'         TO GODK-STATUSKODER                              
116900     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA201 SSA1                   
117000     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
117100     PERFORM IMS-STATUSKONTROLL                                           
117200     .                                                                    
117300     SKIP3                                                                
117400 IMS-REPL-WDA201 SECTION.                                                 
117500     MOVE 'REPL-WDA201     ' TO CURRENT-SECTION                           
117600                                                                          
117700     MOVE SPACE            TO ALL-SSA                                     
117800     MOVE '  '             TO GODK-STATUSKODER                            
117900     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA201                       
118000     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     .                                                                    
118300     SKIP3                                                                
118400 IMS-GHNP-WDA211 SECTION.                                                 
118500     MOVE 'GHNP-WDA211     ' TO CURRENT-SECTION                           
118600                                                                          
118700     MOVE SPACE            TO ALL-SSA                                     
118800     MOVE 'WDA211   '      TO SSA1                                        
118900     MOVE '  GE'           TO GODK-STATUSKODER                            
119000     CALL CBLTDLI USING GHNP WDA2-PCB DLI-IO-WDA211 SSA1                  
119100     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
119200     PERFORM IMS-STATUSKONTROLL                                           
119300     .                                                                    
119400     SKIP3                                                                
119500 IMS-REPL-WDA211 SECTION.                                                 
119600     MOVE 'REPL-WDA211     ' TO CURRENT-SECTION                           
119700                                                                          
119800     MOVE SPACE            TO ALL-SSA                                     
119900     MOVE '  '             TO GODK-STATUSKODER                            
120000     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA211                       
120100     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
120200     PERFORM IMS-STATUSKONTROLL                                           
120300     .                                                                    
120400     SKIP3                                                                
120500 IMS-GU-WDA3F1 SECTION.                                                   
120600     MOVE 'GU-WDA3F1       ' TO CURRENT-SECTION                           
120700                                                                          
120800     MOVE SPACE            TO ALL-SSA                                     
120900     STRING 'WDA3F1  (WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
121000                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
121100          DELIMITED BY SIZE INTO SSA1                                     
121200     MOVE '  GE'           TO GODK-STATUSKODER                            
121300     CALL CBLTDLI USING GU WDA3-PCB DLI-IO-WDA3F1 SSA1                    
121400     MOVE WDA3-STATUS-CODE TO STATUS-WS                                   
121500     PERFORM IMS-STATUSKONTROLL                                           
121600     .                                                                    
121700     EJECT                                                                
121800 IMS-GU-WDB201 SECTION.                                                   
121900     MOVE 'GU-WDB201       ' TO CURRENT-SECTION                           
122000                                                                          
122100     MOVE SPACE            TO ALL-SSA                                     
122200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
122300          DELIMITED BY SIZE INTO SSA1                                     
122400     MOVE '  '             TO GODK-STATUSKODER                            
122500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
122600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     .                                                                    
122900     EJECT                                                                
123000 IMS-GU-WDR501 SECTION.                                                   
123100     MOVE 'IMS-GU-WDR501   '  TO CURRENT-IMS-SECTION                      
123200                                                                          
123300     MOVE SPACE               TO ALL-SSA                                  
123400     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
123500          DELIMITED BY SIZE INTO SSA1                                     
123600     MOVE '  '                TO GODK-STATUSKODER                         
123700     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX01 SSA1                    
123800     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
123900     PERFORM IMS-STATUSKONTROLL                                           
124000     .                                                                    
124100     EJECT                                                                
124200 IMS-GU-WDGX4128 SECTION.                                                 
124300     MOVE 'IMS-GHU-WDGX4128'  TO CURRENT-IMS-SECTION                      
124400                                                                          
124500     MOVE SPACE               TO ALL-SSA                                  
124600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
124700          DELIMITED BY SIZE INTO SSA1                                     
124800     STRING 'WDGX4128(KY4128   =' W-KEY4128-X ')'                         
124900          DELIMITED BY SIZE INTO SSA2                                     
125000     MOVE '  GE'              TO GODK-STATUSKODER                         
125100     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX4128 SSA1 SSA2             
125200     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
125300     PERFORM IMS-STATUSKONTROLL                                           
125400     .                                                                    
125500     EJECT                                                                
125600 IMS-GNP-WDGX4128-DISTRIKT SECTION.                                       
125700     MOVE 'GNP-4128-DISTRIK'  TO CURRENT-IMS-SECTION                      
125800                                                                          
125900     MOVE SPACE               TO ALL-SSA                                  
126000     STRING 'WDGX4128(IDDISTRF<=' W-4128-IDDISTR-X                        
126100                    '&IDDISTRT>=' W-4128-IDDISTR-X ')'                    
126200          DELIMITED BY SIZE INTO SSA1                                     
126300     MOVE '  GE'              TO GODK-STATUSKODER                         
126400     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4128 SSA1                 
126500     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126900 IMS-RESTART SECTION.                                                     
127000     SKIP2                                                                
127100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
127200     MOVE '  ' TO GODK-STATUSKODER                                        
127300     CALL CBLTDLI USING XRST MSG-PCB                                      
127400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
127500                        CHKP-AREA-LENGTH CHKP-AREA                        
127600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127700     PERFORM IMS-STATUSKONTROLL                                           
127800     .                                                                    
127900     EJECT                                                                
128000 IMS-CHECKPOINT SECTION.                                                  
128100     SKIP2                                                                
128200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
128300     MOVE '  XD' TO GODK-STATUSKODER                                      
128400     CALL CBLTDLI USING CHKP MSG-PCB                                      
128500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
128600                        CHKP-AREA-LENGTH CHKP-AREA                        
128700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
128800     PERFORM IMS-STATUSKONTROLL                                           
128900                                                                          
129000     IF IMS-EJ-OK                                                         
129100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
129200       DISPLAY FELTEXT                                                    
129300       CALL FELLOG                                                        
129400     END-IF                                                               
129500     .                                                                    
129600     EJECT                                                                
129700 IMS-STATUSKONTROLL SECTION.                                              
129800     SKIP2                                                                
129900     SET STATUS-IX TO 1                                                   
130000     SEARCH GODK-STATUS                                                   
130100       AT END                                                             
130200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
130300           DELIMITED BY SIZE INTO FELTEXT                                 
130400         DISPLAY FELTEXT                                                  
130500         CALL FELLOG                                                      
130600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
130700         CONTINUE                                                         
130800     END-SEARCH                                                           
130900     .                                                                    
131000     EJECT                                                                
131100*    -COPY WY2000P4                                                       
