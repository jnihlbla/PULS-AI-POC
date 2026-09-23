000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2711P00.                                                
000400 AUTHOR.         ARUP DATTA.                                              
000500 DATE-WRITTEN.   2022-08-25.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*                                                                         
001000*        PROGRAM WRITES 'PURCHASE ORDER' FOR                              
001100*        LOCAL SUPPLIER FOR ALL DCS                                       
001200*                                                                         
001300*        NOTE :                                                           
001400*        - W2711P IS THE COMMON PPFA WITH ENGLISH LANG SUPPORT            
001500*        - W2711T IS THE PPFA WITH THAI LANG SUPPORT                      
001600*                                                                         
001700*                                                                         
001800*        PROGRAM READS         WDK6                                       
001900*                              WDD3                                       
002000*                              WDF1                                       
002100*                              WDF5                                       
002200*                              WDB6                                       
002300*        PROGRAM UPDATES       WDL6                                       
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     SKIP2                                                                
003700*          ---                                                            
003800     SELECT W2711P                     ASSIGN TO W2711PD1.                
003900     SKIP2                                                                
004000*          ---                                                            
004100     SELECT W271UT                     ASSIGN TO W2711PD2.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W2711P                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000*01  -COPY W27115      -L.                                                
005100                                                                          
005200                                                                          
005300 FD  W271UT                                                               
005400     RECORDING V                                                          
005500     BLOCK CONTAINS  0 RECORDS.                                           
005600 01  ORDER-REPORT-REC        PIC X(160).                                  
005700                                                                          
005800                                                                          
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100                                                                          
006200*    -CHECKED BY WY2000                                                   
006300     SKIP3                                                                
006400 77  IDPGM                       PIC X(8)    VALUE 'W2711P00'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700                                                                          
006800 77  W2711P-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W2711P                       VALUE 'J'.                   
007000                                                                          
007100 77  SW-LOCAL-TEXT               PIC X       VALUE 'N'.                   
007200     88  LOCAL-TEXT-FOUND                    VALUE 'J'.                   
007300                                                                          
007400     EJECT                                                                
007500 01  ARBETSAREOR.                                                         
007600                                                                          
007700     03 WS-IDARTNR               PIC S9(9)  VALUE ZERO COMP-3.            
007800     03 WS-TIME                  PIC  9(8)  VALUE ZERO.                   
007900     03 WS-PAGE                  PIC  9(4)  VALUE ZERO.                   
008000     03 WS-RADANT                PIC  9(2)  VALUE ZERO.                   
008100     03 WS-SPARA-IDLEVNR         PIC X(5)   VALUE SPACE.                  
008200     03 WS-SPARA-IDDC            PIC X(2)   VALUE SPACE.                  
008300     03 WS-SPARA-KVDAGAR         PIC 9(3)   VALUE ZERO.                   
008400     03 WS-SPARA-KDVALISO        PIC X(3)   VALUE SPACES.                 
008500     03 WS-BEART                 PIC X(25)  VALUE SPACES.                 
008600     03 WS-BEART-UTF8            PIC X(25)  VALUE SPACES.                 
008700     03 WS-BEARTEXT              PIC X(100) VALUE SPACES.                 
008800     03 WS-BEARTEXT-UTF8         PIC X(100) VALUE SPACES.                 
008900     03 W-IDSKYLT-GB             PIC X(3)   VALUE 'GB '.                  
009000     03 WS-RED-DATUM-DAGENS      PIC X(8)   VALUE SPACES.                 
009100     03 WS-RED-DATUM.                                                     
009200       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
009300       05  FILLER                PIC X      VALUE '/'.                    
009400       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
009500       05  FILLER                PIC X      VALUE '/'.                    
009600       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
009700     03 WS-TIPRLIST              PIC 9(8)   VALUE ZERO.                   
009800     03 WS-DAGENS-DATUM          PIC 9(8)   VALUE ZERO.                   
009900     03 WS-HELTAL                PIC 9(8)   VALUE ZERO.                   
010000     03 WS-HELTAL-RED            PIC Z(5)9  VALUE ZERO.                   
010100     03 WS-DECIMAL               PIC 9(3)   VALUE ZERO.                   
010200     03 WS-PRARTBEL-PR           PIC 9(8)V9(3)                            
010300                                            VALUE ZERO.                   
010400     03 WS-UNIT-TOT-COST         PIC 9(9)V9(3)                            
010500                                            VALUE ZERO.                   
010600     03 WS-RED-UNIT-COST-INT     PIC Z(8)9  VALUE ZERO.                   
010700     03 WS-RED-UNIT-TOT-COST-INT PIC Z(8)9  VALUE ZERO.                   
010800                                                                          
010900     03 WS-RED-UNIT-COST         PIC Z(6)9.9(2)                           
011000                                            VALUE ZERO.                   
011100     03 WS-RED-UNIT-TOT-COST     PIC Z(8)9.9(2)                           
011200                                            VALUE ZERO.                   
011300                                                                          
011400     03 WS-IDAVTAL               PIC 9(12)  VALUE ZERO.                   
011500     03 WS-RED-IDAVTAL           PIC X(14)  VALUE ZERO.                   
011600     03 WS-IDBENR                PIC S9     VALUE ZERO COMP-3.            
011700     03 WS-DAPRLIST              PIC 9(8)   VALUE ZERO.                   
011800     03 WS-TOT-SUPP-ORDER        PIC 9(11)V9(2)                           
011900                                            VALUE ZERO.                   
012000     03 WS-RED-TOT-SUPP-ORDER    PIC Z(10)9.9(2)                          
012100                                            VALUE ZERO.                   
012200     03 WS-AFTER                 PIC 9(3)   VALUE ZERO.                   
012300                                                                          
012400     03  TAB-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
012500     03  FIL-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
012600     03  IX                      PIC S9(9)  VALUE ZERO COMP-3.            
012700                                                                          
012800                                                                          
012900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013000 01  FILLER REDEFINES DAGENS-DATUM.                                       
013100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013400                                                                          
013500 01  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
013600                                                                          
013700     EJECT                                                                
013800                                                                          
013900******************************************************************        
014000*      TABELLER                                                           
014100******************************************************************        
014200                                                                          
014300                                                                          
014400 01  TABENTRY-PARM.                                                       
014500                                                                          
014600     03  STEGLANGD                 PIC S9(9) COMP.                        
014700     03  POST-ANTAL                PIC S9(9) COMP.                        
014800     03  NYCKELLANGD               PIC S9(9) COMP.                        
014900                                                                          
015000 01  TAB-MAX                     PIC S9(9) COMP  VALUE ZERO.              
015100                                                                          
015200 01  RETURANTALTABELL.                                                    
015300     03  RETURANTAL OCCURS 5.                                             
015400        05  TAB-IDDC                  PIC  X(2).                          
015500        05  TAB-ANTAL                 PIC S9(7)    COMP-3.                
015600                                                                          
015700 01  DYNAMISKA-SUBPROGRAM.                                                
015800*                                                                         
015900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
016300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
016400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016500     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
016600     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
016700     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
016800     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
016900     03  WCNVUNTH                PIC X(8)    VALUE 'WCNVUNTH'.            
017000     SKIP2                                                                
017100*    --- PARAMETRAR TILL ABEND                                            
017200                                                                          
017300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017500     SKIP2                                                                
017600 01  FELTEXT.                                                             
017700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017900     EJECT                                                                
018000*    --- PARAMETRAR TILL DATKORT                                          
018100*                                                                         
018200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2711P'.              
018300     SKIP2                                                                
018400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
018500     SKIP2                                                                
018600*01  -COPY WDATKORT                                                       
018700     EJECT                                                                
018800*******                      PARAMETRAR TILL WORKDAY                      
018900*                                                                         
019000*01  -COPY WORKAREA                                                       
019100     EJECT                                                                
019200*01  -COPY WDATAREA                                                       
019300     EJECT                                                                
019400*******                      PARAMETRAR TILL WDAGKONV                     
019500*                                                                         
019600*01  -COPY WDAGAREA                                                       
019700     EJECT                                                                
019800*                                                                         
019900 01  FILLER              PIC X(24)   VALUE 'WTRAUTF8-START   '.           
020000*01  -COPY WTRAUTF8                                                       
020100     EJECT                                                                
020200                                                                          
020300 01  FILLER              PIC X(24)   VALUE 'WCNVAREA-START   '.           
020400*01   -COPY WCNVAREA       -PRE CNV-                                      
020500     EJECT                                                                
020600*                                                                         
020700 01  W2711P-AREA-START           PIC X(24)   VALUE                        
020800                                 'W2711P-AREA-START  '.                   
020900     SKIP2                                                                
021000                                                                          
021100*01  AREA -COPY W27115     -PRE W2711P-                                   
021200     EJECT                                                                
021300 01  W271UT-AREA-START           PIC X(24)   VALUE                        
021400                                 'W271UT-AREA-START  '.                   
021500     SKIP2                                                                
021600 01  W001-DAP-HEAD.                                                       
021700     03  FILLER                  PIC X(80)   VALUE SPACE.                 
021800     SKIP2                                                                
021900                                                                          
022000 01  UT-AREOR.                                                            
022100     03 UT-RPT-PRINT-LINES.                                               
022200       05 UT-HEADING-A.                                                   
022300         10 FILLER               PIC X(05) VALUE SPACE.                   
022400         10 UT-RECV-COMPANY-1    PIC X(35) VALUE SPACE.                   
022500         10 FILLER               PIC X(03) VALUE SPACE.                   
022600         10 UT-INVC-COMPANY-1    PIC X(35) VALUE SPACE.                   
022700         10 FILLER               PIC X(01) VALUE SPACE.                   
022800       05 UT-HEADING-B.                                                   
022900         10 FILLER               PIC X(05) VALUE SPACE.                   
023000         10 UT-RECV-COMPANY-2    PIC X(35) VALUE SPACE.                   
023100         10 FILLER               PIC X(03) VALUE SPACE.                   
023200         10 UT-INVC-COMPANY-2    PIC X(35) VALUE SPACE.                   
023300         10 FILLER               PIC X(01) VALUE SPACE.                   
023400       05 UT-HEADING-C.                                                   
023500         10 FILLER               PIC X(05) VALUE SPACE.                   
023600         10 UT-RECV-ADDRESS-1    PIC X(35) VALUE SPACE.                   
023700         10 FILLER               PIC X(03) VALUE SPACE.                   
023800         10 UT-INVC-ADDRESS-1    PIC X(35) VALUE SPACE.                   
023900         10 FILLER               PIC X(01) VALUE SPACE.                   
024000       05 UT-HEADING-D.                                                   
024100         10 FILLER               PIC X(05) VALUE SPACE.                   
024200         10 UT-RECV-ADDRESS-2    PIC X(35) VALUE SPACE.                   
024300         10 FILLER               PIC X(03) VALUE SPACE.                   
024400         10 UT-INVC-ADDRESS-2    PIC X(35) VALUE SPACE.                   
024500         10 FILLER               PIC X(01) VALUE SPACE.                   
024600       05 UT-HEADING-E.                                                   
024700         10 FILLER               PIC X(05) VALUE SPACE.                   
024800         10 UT-RECV-ADDRESS-3    PIC X(35) VALUE SPACE.                   
024900         10 FILLER               PIC X(03) VALUE SPACE.                   
025000         10 UT-INVC-ADDRESS-3    PIC X(35) VALUE SPACE.                   
025100         10 FILLER               PIC X(01) VALUE SPACE.                   
025200       05 UT-HEADING-1.                                                   
025300         10 FILLER               PIC X(06) VALUE SPACE.                   
025400         10 UT-VENDOR-CODE       PIC X(05).                               
025500         10 FILLER               PIC X(55) VALUE SPACE.                   
025600         10 FILLER               PIC X(04) VALUE '267-'.                  
025700         10 UT-ORDER-NUMBER      PIC Z(4)9.                               
025800         10 FILLER               PIC X(04) VALUE '-096'.                  
025900       05 UT-HEADING-2.                                                   
026000         10 FILLER               PIC X(06) VALUE SPACE.                   
026100         10 UT-VENDOR-NAME       PIC X(30) VALUE SPACE.                   
026200         10 FILLER               PIC X(24) VALUE SPACE.                   
026300         10 UT-REPORT-PAGE-JP    PIC ZZZ9.                                
026400         10 FILLER               PIC X(16) VALUE SPACE.                   
026500       05 UT-HEADING-3.                                                   
026600         10 FILLER               PIC X(06) VALUE SPACE.                   
026700         10 UT-VENDOR-ADDR1      PIC X(30) VALUE SPACE.                   
026800         10 FILLER               PIC X(39) VALUE SPACE.                   
026900         10 UT-REPORT-PAGE       PIC ZZZ9.                                
027000       05 UT-HEADING-4.                                                   
027100         10 FILLER               PIC X(06) VALUE SPACE.                   
027200         10 UT-VENDOR-ADDR2      PIC X(30) VALUE SPACE.                   
027300         10 FILLER               PIC X(23) VALUE SPACE.                   
027400         10 UT-REPORT-DATE-JP    PIC X(08) VALUE SPACE.                   
027500         10 FILLER               PIC X(12) VALUE SPACE.                   
027600       05 UT-HEADING-5.                                                   
027700         10 FILLER               PIC X(06) VALUE SPACE.                   
027800         10 UT-VENDOR-CITY       PIC X(30) VALUE SPACE.                   
027900         10 FILLER               PIC X(05) VALUE SPACE.                   
028000         10 FILLER               PIC X(30) VALUE SPACE.                   
028100         10 UT-REPORT-DATE       PIC X(08) VALUE SPACE.                   
028200       05 UT-HEADING-6.                                                   
028300         10 FILLER               PIC X(02) VALUE SPACE.                   
028400         10 UT-PART-ID           PIC Z(8)9 VALUE ZERO.                    
028500         10 FILLER               PIC X(01) VALUE SPACE.                   
028600         10 UT-PART-DESC         PIC X(100) VALUE SPACE.                  
028700         10 FILLER               PIC X(01) VALUE SPACE.                   
028800         10 UT-VENDOR-NUMBER     PIC X(10) VALUE SPACE.                   
028900         10 FILLER               PIC X(01) VALUE SPACE.                   
029000         10 UT-WORK-QTY          PIC Z(6)9 VALUE ZERO.                    
029100         10 FILLER               PIC X(01) VALUE SPACE.                   
029200         10 UT-UNIT-COST         PIC X(10) VALUE SPACE.                   
029300         10 FILLER               PIC X(02) VALUE SPACE.                   
029400         10 UT-UNIT-TOT-COST     PIC X(12) VALUE SPACE.                   
029500         10 FILLER               PIC X(01) VALUE SPACE.                   
029600       05 UT-HEADING-7.                                                   
029700         10 FILLER               PIC X(25) VALUE SPACE.                   
029800         10 UT-LEV-DATUM         PIC X(09) VALUE SPACE.                   
029900         10 FILLER               PIC X(31) VALUE SPACE.                   
030000         10 UT-TOT-SUPP-ORDER                                             
030100                                 PIC X(14) VALUE SPACE.                   
030200         10 FILLER               PIC X(10) VALUE SPACE.                   
030300       05 UT-HEADING-8.                                                   
030400         10 FILLER               PIC X(67) VALUE SPACE.                   
030500         10 FILLER               PIC X(09) VALUE 'PRICE IN'.              
030600         10 UT-KDVALISO          PIC X(03) VALUE SPACE.                   
030700         10 FILLER               PIC X(10) VALUE SPACE.                   
030800     EJECT                                                                
030900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031000*                                                                         
031100     EJECT                                                                
031200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031300     SKIP3                                                                
031400 01  NYCKLAR-TILL-DLI.                                                    
031500     03  W-IDARTNR-X.                                                     
031600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
031700     03  W-IDDC-X.                                                        
031800         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
031900     03  W-IDLEVNR-X.                                                     
032000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
032100     03  W-KDSEGKEY-X.                                                    
032200         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
032300     03  W-IDSKYLT-X.                                                     
032400         05 W-IDSKYLT            PIC X(3)    VALUE SPACES.                
032500     03  W-WDGXKEY-X.                                                     
032600         05 W-IDHTYP             PIC X(4)    VALUE '2503'.                
032700         05 W-LOWVALUE           PIC X(26)   VALUE LOW-VALUE.             
032800     03  W-IDLEVNR-21-X.                                                  
032900         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
033000     03  W-DAPRLIST-21-N.                                                 
033100         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
033200     03  W-IDDC-B6-X.                                                     
033300         05 W-IDDC-B6                  PIC X(2).                          
033400     SKIP2                                                                
033500*    --- STATUS-KOD FRÅN IMS                                              
033600 01  STATUS-WS                   PIC XX.                                  
033700     88  SEGMENT-FINNS                       VALUE '  '.                  
033800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033900     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
034000                                                   'GB'.                  
034100     SKIP2                                                                
034200 01  GODK-STATUSKODER.                                                    
034300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034400     SKIP3                                                                
034500 01  SSA1                        PIC X(64).                               
034600 01  SSA2                        PIC X(64).                               
034700     EJECT                                                                
034800*    --- IMS FUNKTIONSKODER                                               
034900*01  -COPY W0003                                                          
035000     EJECT                                                                
035100******************************************************************        
035200*          DLI INPUT - OUTPUT AREA                                        
035300******************************************************************        
035400                                                                          
035500                                                                          
035600                                                                          
035700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
035800     SKIP3                                                                
035900 01  DLI-IO-AREA-WDK601.                                                  
036000*    03  -COPY WDK601                                                     
036100*                                                                         
036200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
036300     SKIP3                                                                
036400 01  DLI-IO-AREA-WDK611.                                                  
036500*    03  -COPY WDK611                                                     
036600*                                                                         
036700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK621'.             
036800     SKIP3                                                                
036900 01  DLI-IO-AREA-WDK621.                                                  
037000*    03  -COPY WDK621                                                     
037100*                                                                         
037200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
037300     SKIP3                                                                
037400 01  DLI-IO-AREA-WDD301.                                                  
037500*    03  -COPY WDD301                                                     
037600*                                                                         
037700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
037800     SKIP3                                                                
037900 01  DLI-IO-AREA-WDD311.                                                  
038000*    03  -COPY WDD311                                                     
038100*                                                                         
038200     EJECT                                                                
038300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF101'.             
038400     SKIP3                                                                
038500 01  DLI-IO-AREA-WDF101.                                                  
038600*    03  -COPY WDF101                                                     
038700*                                                                         
038800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF106'.             
038900     SKIP3                                                                
039000 01  DLI-IO-AREA-WDF106.                                                  
039100*    03  -COPY WDF106                                                     
039200     EJECT                                                                
039300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF117'.             
039400     SKIP3                                                                
039500 01  DLI-IO-AREA-WDF117.                                                  
039600*    03  -COPY WDF117                                                     
039700     EJECT                                                                
039800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF501'.             
039900     SKIP3                                                                
040000 01  DLI-IO-AREA-WDF501.                                                  
040100*    03  -COPY WDF501                                                     
040200*                                                                         
040300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF502'.             
040400     SKIP3                                                                
040500 01  DLI-IO-AREA-WDF502.                                                  
040600*    03  -COPY WDF502                                                     
040700*                                                                         
040800     EJECT                                                                
040900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL601'.             
041000 01  DLI-IO-AREA-WDL601.                                                  
041100*    03  -COPY WDL601 -PRE INLC-                                          
041200     EJECT                                                                
041300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL612'.             
041400 01  DLI-IO-AREA-WDL612.                                                  
041500*    03  -COPY WDL612                                                     
041600     EJECT                                                                
041700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-GX2504'.             
041800     SKIP3                                                                
041900 01  DLI-IO-AREA-GX2504.                                                  
042000*    03      -COPY WDGX2504                                               
042100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTS01'.             
042200     SKIP3                                                                
042300 01  DLI-IO-AREA-ARTS01.                                                  
042400*        05  -COPY WDK701                                                 
042500     EJECT                                                                
042600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTS11'.             
042700     SKIP3                                                                
042800 01  DLI-IO-AREA-ARTS11.                                                  
042900*        05  -COPY WDK711                                                 
043000     EJECT                                                                
043100 01  FILLER                  PIC X(16) VALUE 'WDB601 AREA'.               
043200 01   DLI-IO-AREA-B601.                                                   
043300*     03  -COPY WDB601                                                    
043400     EJECT                                                                
043500 01  FILLER                  PIC X(16) VALUE 'WDB602 AREA'.               
043600 01   DLI-IO-AREA-B602.                                                   
043700*     03  -COPY WDB602                                                    
043800     EJECT                                                                
043900 LINKAGE SECTION.                                                         
044000                                                                          
044100*01  -COPY W0009   -PRE MSG-                                              
044200     EJECT                                                                
044300     EJECT                                                                
044400*01  -COPY W0008  -PRE WDK6-                                              
044500     05  FILLER                  PIC X.                                   
044600     EJECT                                                                
044700*01  -COPY W0008  -PRE WDD3-                                              
044800     05  FILLER                  PIC X.                                   
044900     EJECT                                                                
045000*01  -COPY W0008  -PRE WDF1-                                              
045100     05  FILLER                  PIC X.                                   
045200     EJECT                                                                
045300*01  -COPY W0008  -PRE WDF5-                                              
045400     05  FILLER                  PIC X.                                   
045500     EJECT                                                                
045600*01  -COPY W0008  -PRE WDL6-                                              
045700     05  FILLER                  PIC X.                                   
045800*01  -COPY W0008   -PRE WDR2-                                             
045900     05  FILLER                  PIC X.                                   
046000*01  -COPY W0008  -PRE  ARTS-                                             
046100     05  FILLER                  PIC X.                                   
046200*01  -COPY W0008  -PRE  WDB6-                                             
046300     05  FILLER                  PIC X.                                   
046400     EJECT                                                                
046500 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDD3-PCB                      
046600     WDF1-PCB WDF5-PCB WDL6-PCB WDR2-PCB ARTS-PCB WDB6-PCB.               
046700     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDD3-PCB                      
046800     WDF1-PCB WDF5-PCB WDL6-PCB WDR2-PCB ARTS-PCB WDB6-PCB.               
046900                                                                          
047000                                                                          
047100     PERFORM A-INIT                                                       
047200     PERFORM B-CREATE-LPO                                                 
047300     PERFORM Z-FINIT                                                      
047400                                                                          
047500     MOVE ZERO TO RETURN-CODE                                             
047600     GOBACK                                                               
047700     .                                                                    
047800     EJECT                                                                
047900                                                                          
048000                                                                          
048100 A-INIT SECTION.                                                          
048200                                                                          
048300     OPEN INPUT  W2711P                                                   
048400                                                                          
048500     OPEN OUTPUT W271UT                                                   
048600                                                                          
048700     ACCEPT DAGENS-DATUM FROM DATE                                        
048800                                                                          
048900     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
049000     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
049100                                                                          
049200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
049300                     DAT-O-TIDATUM DAT-KDSVAR                             
049400                                                                          
049500     IF DAT-KDSVAR-OK                                                     
049600****             HÄMTA SEKELSIFFROR                                       
049700                                                                          
049800       MOVE DAT-TISEKEL       TO WS-DAGENS-DATUM(1:2)                     
049900       MOVE DAGENS-DATUM      TO WS-DAGENS-DATUM(3:6)                     
050000                                                                          
050100     ELSE                                                                 
050200         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
050300         DELIMITED BY SIZE INTO FELTEXT                                   
050400         CALL FELLOG                                                      
050500     END-IF                                                               
050600     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
050700     MOVE DAGENS-DATUM-MAANAD                                             
050800                             TO WS-RED-MM                                 
050900     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
051000     MOVE WS-RED-DATUM       TO WS-RED-DATUM-DAGENS                       
051100     ACCEPT WS-TIME FROM TIME                                             
051200     .                                                                    
051300     EJECT                                                                
051400 B-CREATE-LPO  SECTION.                                                   
051500     PERFORM S01-LAES-W2711P                                              
051600                                                                          
051700     PERFORM UNTIL END-OF-W2711P                                          
051800       IF W2711P-IDDC NOT = WS-SPARA-IDDC                                 
051900          INITIALIZE UT-RPT-PRINT-LINES                                   
052000          PERFORM S21-WRITE-DAP-RULE                                      
052100          PERFORM S05-GET-WDB6-INFO                                       
052200       END-IF                                                             
052300       PERFORM BA-SKAPA-POSTER                                            
052400     END-PERFORM                                                          
052500     .                                                                    
052600     EJECT                                                                
052700 BA-SKAPA-POSTER SECTION.                                                 
052800                                                                          
052900     MOVE ZERO                  TO WS-PAGE                                
053000                                   WS-RADANT                              
053100                                   WS-TOT-SUPP-ORDER                      
053200*    GENERERA/HÄMTA NYTT ORDERNR                                          
053300     PERFORM IMS-GHU-2504                                                 
053400*  ---   EFTERSOM ÄVEN TRANSFERS ANVÄNDER SIG                             
053500*  ---   AV DETTA ORDERNR SÅ FINNS EN BEGRÄNSNING                         
053600*  ---   INTERNT I SYSTEMET PÅ ORDERNR MAX 94999                          
053700     IF 2504-IDORDNR5 < 94999                                             
053800       ADD 1                    TO 2504-IDORDNR5                          
053900     ELSE                                                                 
054000       MOVE 1                   TO 2504-IDORDNR5                          
054100     END-IF                                                               
054200     PERFORM IMS-REPL-2504                                                
054300     PERFORM S05-GET-WDB6-INFO                                            
054400     PERFORM BAA-LOKAL-LEVNR                                              
054500     MOVE W2711P-IDDC           TO WS-SPARA-IDDC                          
054600                                   W-IDDC                                 
054700     MOVE W2711P-IDLEVNR        TO WS-SPARA-IDLEVNR                       
054800     MOVE W2711P-KVDAGAR        TO WS-SPARA-KVDAGAR                       
054900                                                                          
055000     PERFORM UNTIL END-OF-W2711P                                          
055100     OR W2711P-IDDC          NOT = WS-SPARA-IDDC                          
055200     OR W2711P-IDLEVNR       NOT = WS-SPARA-IDLEVNR                       
055300     OR W2711P-KVDAGAR       NOT = WS-SPARA-KVDAGAR                       
055400       MOVE W2711P-IDARTNR      TO W-IDARTNR                              
055500       MOVE ZERO                TO WS-RED-UNIT-COST-INT                   
055600                                   WS-RED-UNIT-COST                       
055700                                   WS-RED-UNIT-TOT-COST-INT               
055800                                   WS-RED-UNIT-TOT-COST                   
055900       PERFORM IMS-GU-WDK6-ART                                            
056000       IF SEGMENT-FINNS                                                   
056100         PERFORM BAC-HAMTA-PRIS                                           
056200       END-IF                                                             
056300                                                                          
056400       PERFORM BAG-HAEMTA-BENAEMNING                                      
056500                                                                          
056600       MOVE W2711P-IDARTNR      TO UT-PART-ID                             
056700       MOVE W2711P-KVBEART      TO UT-WORK-QTY                            
056800       PERFORM BAD-HAMTA-VENDOR-NUMBER                                    
056900       PERFORM BAE-SKRIV-LISTA                                            
057000       ADD 100                  TO WS-TIME                                
057100       MOVE W2711P-IDARTNR      TO INLC-ART-IDARTNR                       
057200       PERFORM IMS-ISRT-WDL6-ART                                          
057300       MOVE WS-DAGENS-DATUM     TO ORD-DAREGDAT                           
057400       MOVE WS-TIME (1:6)       TO ORD-TIREGTID                           
057500       MOVE W2711P-IDDC         TO ORD-IDDC                               
057600       MOVE 2504-IDORDNR5       TO ORD-IDKUNDRF                           
057700       MOVE W2711P-IDLEVNR      TO ORD-IDLEVNR                            
057800       MOVE W2711P-KVBEART      TO ORD-KVBEART                            
057900       MOVE WORK-TIAAMMDD-TOM                                             
058000                                TO ORD-TIBERANK                           
058100       MOVE ZERO                TO ORD-IDLOPNRM                           
058200                                   ORD-KVAVIS                             
058300       PERFORM IMS-ISRT-WDL6-ORD                                          
058400       PERFORM S01-LAES-W2711P                                            
058500     END-PERFORM                                                          
058600     COMPUTE WS-AFTER = 36 - WS-RADANT                                    
058700     MOVE WS-TOT-SUPP-ORDER     TO WS-RED-TOT-SUPP-ORDER                  
058800     MOVE WS-RED-TOT-SUPP-ORDER TO UT-TOT-SUPP-ORDER                      
058900     MOVE UT-HEADING-7          TO ORDER-REPORT-REC                       
059000     WRITE ORDER-REPORT-REC  AFTER WS-AFTER                               
059100     MOVE PRL-KDVALISO          TO UT-KDVALISO                            
059200     MOVE UT-HEADING-8          TO ORDER-REPORT-REC                       
059300     WRITE ORDER-REPORT-REC  AFTER 2                                      
059400     .                                                                    
059500     EJECT                                                                
059600 BAA-LOKAL-LEVNR SECTION.                                                 
059700     MOVE SPACE              TO UT-VENDOR-CODE                            
059800     MOVE SPACE              TO UT-VENDOR-NAME                            
059900                                UT-VENDOR-ADDR1                           
060000                                UT-VENDOR-ADDR2                           
060100                                UT-VENDOR-CITY                            
060200                                UT-LEV-DATUM                              
060300                                                                          
060400     MOVE W2711P-IDLEVNR     TO W-IDLEVNR                                 
060500     PERFORM IMS-GU-WDF1-ROT                                              
060600     IF SEGMENT-FINNS                                                     
060700       MOVE LEV-IDLEVNR      TO UT-VENDOR-CODE                            
060800                                                                          
060900       PERFORM IMS-GNP-WDF1-ADRESS                                        
061000       IF SEGMENT-FINNS                                                   
061100         MOVE ADR-BELEV      TO UT-VENDOR-NAME                            
061200         MOVE ADR-ADLEV-RAD1 TO UT-VENDOR-ADDR1                           
061300         MOVE ADR-ADLEV-RAD2 TO UT-VENDOR-ADDR2                           
061400         MOVE ADR-ADLEV-ORT  TO UT-VENDOR-CITY                            
061500       END-IF                                                             
061600                                                                          
061700*JP*   IF DCS-JAPAN                                                       
061800*JP*      PERFORM IMS-GNP-JPN-ADRESS                                      
061900*JP*      IF SEGMENT-FINNS                                                
062000*JP*        MOVE JPN-BELEV      TO UT-VENDOR-NAME                         
062100*JP*        MOVE JPN-ADLEV-RAD1 TO UT-VENDOR-ADDR1                        
062200*JP*        MOVE JPN-ADLEV-RAD2 TO UT-VENDOR-ADDR2                        
062300*JP*        MOVE JPN-ADLEV-ORT  TO UT-VENDOR-CITY                         
062400*JP*      END-IF                                                          
062500*JP*   END-IF                                                             
062600                                                                          
062700       PERFORM BAAA-LEVERANS-DATUM                                        
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 BAAA-LEVERANS-DATUM SECTION.                                             
063200                                                                          
063300     MOVE DAGENS-DATUM       TO WORK-TIAAMMDD-FOM                         
063400     MOVE 002                TO WORK-KDCALL                               
063500     MOVE W2711P-KVDAGAR     TO WORK-KVWORKD                              
063600                                                                          
063700***     WORKDAY FUNGERAR SÅ ATT OM MAN FYLLER I ETT (1) I                 
063800***     WORK-KVWORKD FÅR MAN FRAM SAMMA DATUM I TOM-DATUM                 
063900***     SOM I FOM-DATUM, DÄRFÖR ADDERAS ETT TILL KVARBDAG                 
064000***     DESSUTOM ADDERAS YTTERLIGARE EN DAG TILL FÖR ATT                  
064100***     DAGENS DATUM INTE SKA RÄKNAS MED                                  
064200***     JÄMFÖR MED W2035300                                               
064300     ADD +2                  TO WORK-KVWORKD                              
064400     MOVE W2711P-IDDC        TO WORK-IDDC                                 
064500     CALL WORKDAY            USING                                        
064600                             WORK-KDCALL                                  
064700                             WORK-DATE-AREA                               
064800                             WORK-KDSVAR                                  
064900     IF WORK-KDSVAR NOT = SPACE                                           
065000         DISPLAY 'FEL I ANROP TILL WORKDAY '                              
065100         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
065200     END-IF                                                               
065300     MOVE WORK-TIAAMMDD-TOM (1:2)                                         
065400                             TO WS-RED-AA                                 
065500     MOVE WORK-TIAAMMDD-TOM (3:2)                                         
065600                             TO WS-RED-MM                                 
065700     MOVE WORK-TIAAMMDD-TOM (5:2)                                         
065800                             TO WS-RED-DD                                 
065900     MOVE WS-RED-DATUM       TO UT-LEV-DATUM                              
066000     .                                                                    
066100     EJECT                                                                
066200 BAC-HAMTA-PRIS SECTION.                                                  
066300                                                                          
066400     MOVE W2711P-IDLEVNR         TO W-IDLEVNR-21                          
066500     COMPUTE W-DAPRLIST-21 = 99999999 - WS-DAGENS-DATUM                   
066600     PERFORM IMS-GNP-WDK621                                               
066700     PERFORM UNTIL SEGMENT-SAKNAS OR PRL-KDSTATUS-PR = 1                  
066800       PERFORM IMS-GNP-WDK621                                             
066900     END-PERFORM                                                          
067000                                                                          
067100     IF SEGMENT-FINNS                                                     
067200***    GODKÄNT PRIS ***************                                       
067300       COMPUTE WS-UNIT-TOT-COST ROUNDED =                                 
067400           W2711P-KVBEART * PRL-PRARTBEL-PR                               
067500       MOVE PRL-PRARTBEL-PR         TO WS-RED-UNIT-COST-INT               
067600                                       WS-RED-UNIT-COST                   
067700       IF PRL-PRARTBEL-PR > 9999999                                       
067800          MOVE WS-RED-UNIT-COST-INT TO UT-UNIT-COST                       
067900       ELSE                                                               
068000          MOVE WS-RED-UNIT-COST     TO UT-UNIT-COST                       
068100       END-IF                                                             
068200       MOVE WS-UNIT-TOT-COST        TO WS-RED-UNIT-TOT-COST               
068300       MOVE WS-RED-UNIT-TOT-COST    TO UT-UNIT-TOT-COST                   
068400                                                                          
068500       ADD  WS-UNIT-TOT-COST        TO WS-TOT-SUPP-ORDER                  
068600     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
068900 BAD-HAMTA-VENDOR-NUMBER SECTION.                                         
069000     MOVE SPACE              TO UT-VENDOR-NUMBER                          
069100     PERFORM IMS-GU-F501-ARTIKEL                                          
069200     IF SEGMENT-FINNS                                                     
069300       PERFORM IMS-GNP-F502-LEV-NUMMER                                    
069400       MOVE ZERO             TO WS-IDBENR                                 
069500                                                                          
069600       PERFORM UNTIL SEGMENT-SAKNAS                                       
069700         IF  XLEV-IDLEVNR = W2711P-IDLEVNR                                
069800         AND XLEV-IDBENR  > WS-IDBENR                                     
069900           MOVE XLEV-BELEVART   TO UT-VENDOR-NUMBER                       
070000           MOVE XLEV-IDBENR  TO WS-IDBENR                                 
070100         END-IF                                                           
070200         PERFORM IMS-GNP-F502-LEV-NUMMER                                  
070300       END-PERFORM                                                        
070400     END-IF                                                               
070500     .                                                                    
070600     EJECT                                                                
070700 BAE-SKRIV-LISTA SECTION.                                                 
070800     IF WS-RADANT = ZERO                                                  
070900     OR WS-RADANT > 33                                                    
071000       ADD 1                 TO WS-PAGE                                   
071100       MOVE 2504-IDORDNR5    TO UT-ORDER-NUMBER                           
071200*JP*   IF DCS-JAPAN                                                       
071300*JP*     MOVE WS-PAGE          TO UT-REPORT-PAGE-JP                       
071400*JP*     MOVE SPACE            TO UT-REPORT-PAGE(1:4)                     
071500*JP*                                                                      
071600*JP*     MOVE WS-RED-DATUM-DAGENS TO UT-REPORT-DATE-JP                    
071700*JP*     MOVE SPACE               TO UT-REPORT-DATE(1:8)                  
071800*JP*   ELSE                                                               
071900         MOVE SPACE               TO UT-REPORT-PAGE-JP(1:4)               
072000         MOVE WS-PAGE             TO UT-REPORT-PAGE                       
072100                                                                          
072200         MOVE SPACE               TO UT-REPORT-DATE-JP(1:8)               
072300         MOVE WS-RED-DATUM-DAGENS TO UT-REPORT-DATE                       
072400*JP*   END-IF                                                             
072500       MOVE SPACE            TO ORDER-REPORT-REC                          
072600       WRITE ORDER-REPORT-REC AFTER PAGE                                  
072700       MOVE UT-HEADING-A     TO ORDER-REPORT-REC                          
072800       WRITE ORDER-REPORT-REC                                             
072900       MOVE UT-HEADING-B     TO ORDER-REPORT-REC                          
073000       WRITE ORDER-REPORT-REC                                             
073100       MOVE UT-HEADING-C     TO ORDER-REPORT-REC                          
073200       WRITE ORDER-REPORT-REC                                             
073300       MOVE UT-HEADING-D     TO ORDER-REPORT-REC                          
073400       WRITE ORDER-REPORT-REC                                             
073500       MOVE UT-HEADING-E     TO ORDER-REPORT-REC                          
073600       WRITE ORDER-REPORT-REC                                             
073700       MOVE UT-HEADING-1     TO ORDER-REPORT-REC                          
073800       WRITE ORDER-REPORT-REC AFTER 2                                     
073900       MOVE UT-HEADING-2     TO ORDER-REPORT-REC                          
074000       WRITE ORDER-REPORT-REC                                             
074100       MOVE UT-HEADING-3     TO ORDER-REPORT-REC                          
074200       WRITE ORDER-REPORT-REC                                             
074300       MOVE UT-HEADING-4     TO ORDER-REPORT-REC                          
074400       WRITE ORDER-REPORT-REC                                             
074500       MOVE UT-HEADING-5     TO ORDER-REPORT-REC                          
074600       WRITE ORDER-REPORT-REC                                             
074700       MOVE SPACE            TO ORDER-REPORT-REC                          
074800*JP*   IF DCS-JAPAN                                                       
074900*JP*     WRITE ORDER-REPORT-REC AFTER 5                                   
075000*JP*   ELSE                                                               
075100         WRITE ORDER-REPORT-REC AFTER 7                                   
075200*JP*   END-IF                                                             
075300       MOVE ZERO             TO WS-RADANT                                 
075400     END-IF                                                               
075500                                                                          
075600     MOVE UT-HEADING-6       TO ORDER-REPORT-REC                          
075700     WRITE ORDER-REPORT-REC                                               
075800     ADD 1                   TO WS-RADANT                                 
075900                                                                          
076000     .                                                                    
076100     EJECT                                                                
076200 BAG-HAEMTA-BENAEMNING SECTION.                                           
076300                                                                          
076400     MOVE NEJ                        TO SW-LOCAL-TEXT                     
076500     MOVE SPACES                     TO WS-BEARTEXT                       
076600                                        UT-PART-DESC                      
076700     MOVE W-IDSKYLT-GB               TO W-IDSKYLT                         
076800     PERFORM IMS-GU-WDD311-BSEQ                                           
076900     IF SEGMENT-FINNS                                                     
077000        MOVE TEXT-BEART              TO UT-PART-DESC                      
077100     ELSE                                                                 
077200        MOVE 'NO DESCRIPTION       ' TO UT-PART-DESC                      
077300     END-IF                                                               
077400*                                                                         
077500***  GET PART DESCRIPTION ON LOCAL LANGUAGE FOR THAI MARKET               
077600*                                                                         
077700     IF DCS-FTG-TH                                                        
077800        MOVE DCS-IDSKYLT-DB          TO W-IDSKYLT                         
077900        PERFORM IMS-GU-WDD311-BSEQ                                        
078000        IF SEGMENT-FINNS                                                  
078100           MOVE TEXT-BEARTEXT        TO WS-BEARTEXT                       
078200           IF WS-BEARTEXT > SPACES                                        
078300              SET LOCAL-TEXT-FOUND   TO TRUE                              
078400           END-IF                                                         
078500        END-IF                                                            
078600     END-IF                                                               
078700*                                                                         
078800     IF DCS-FTG-TH AND                                                    
078900        LOCAL-TEXT-FOUND                                                  
079000        MOVE 'UTF8'                  TO TRAUTF8-KDCP                      
079100        MOVE WS-BEARTEXT             TO TRAUTF8-TECONV-FROM               
079200*                                                                         
079300***     VALIDATE UNICODE STRING                                           
079400*                                                                         
079500        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
079600        MOVE TRAUTF8-TECONV-TO       TO WS-BEARTEXT-UTF8                  
079700*                                                                         
079800***     CONVERT UNICODE TO EBCDIC                                         
079900*                                                                         
080000        STRING WS-BEARTEXT-UTF8                                           
080100                 DELIMITED BY SIZE INTO CNV-TECONV-FROM (1:100)           
080200        MOVE 25  TO CNV-KVMAXTL                                           
080300        MOVE 'J' TO CNV-FLUTF8                                            
080400        MOVE 'N' TO CNV-FLTXTENT                                          
080500        CALL WCNVUNTH  USING CNV-WCNVAREA                                 
080600        IF CNV-KDSVAR = 'F'                                               
080700           MOVE SPACES               TO UT-PART-DESC                      
080800        ELSE                                                              
080900           MOVE CNV-TECONV-TO        TO UT-PART-DESC                      
081000        END-IF                                                            
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400 Z-FINIT SECTION.                                                         
081500     CLOSE W2711P                                                         
081600           W271UT                                                         
081700     .                                                                    
081800     EJECT                                                                
081900                                                                          
082000                                                                          
082100 S01-LAES-W2711P  SECTION.                                                
082200     READ W2711P INTO W2711P-AREA                                         
082300     AT END                                                               
082400        SET END-OF-W2711P TO TRUE                                         
082500     NOT AT END                                                           
082600        CONTINUE                                                          
082700     END-READ                                                             
082800     .                                                                    
082900     EJECT                                                                
083000                                                                          
083100 S05-GET-WDB6-INFO SECTION.                                               
083200     MOVE W2711P-IDDC    TO W-IDDC-B6                                     
083300     PERFORM IMS-GU-WDB601                                                
083400     IF SEGMENT-FINNS                                                     
083500        IF DCS-FLLPO = JA                                                 
083600           MOVE DCS-BEGMT-RAD1        TO UT-RECV-COMPANY-1                
083700           MOVE DCS-BEGMT-RAD2        TO UT-RECV-COMPANY-2                
083800           MOVE DCS-ADGMT-GATA        TO UT-RECV-ADDRESS-1                
083900           MOVE DCS-ADGMT-PADR        TO UT-RECV-ADDRESS-2                
084000           MOVE DCS-ADGMT-LAND        TO UT-RECV-ADDRESS-3                
084100           MOVE DCS-KDVALISO          TO WS-SPARA-KDVALISO                
084200*                                                                         
084300           PERFORM IMS-GNP-WDB602                                         
084400           IF SEGMENT-FINNS                                               
084500              MOVE INV-BEGMT-RAD1     TO UT-INVC-COMPANY-1                
084600              MOVE INV-BEGMT-RAD2     TO UT-INVC-COMPANY-2                
084700              MOVE INV-ADGMT-GATA     TO UT-INVC-ADDRESS-1                
084800              MOVE INV-ADGMT-PADR     TO UT-INVC-ADDRESS-2                
084900              MOVE INV-ADGMT-LAND     TO UT-INVC-ADDRESS-3                
085000           END-IF                                                         
085100        END-IF                                                            
085200     END-IF                                                               
085300     .                                                                    
085400     EJECT                                                                
085500                                                                          
085600 S21-WRITE-DAP-RULE SECTION.                                              
085700                                                                          
085800     MOVE ' ¤DAPW2711P-001'    TO W001-DAP-HEAD                           
085900     WRITE ORDER-REPORT-REC  FROM W001-DAP-HEAD                           
086000                                                                          
086100     MOVE SPACE                TO W001-DAP-HEAD                           
086200*                                                                         
086300     STRING ' ¤DAP' W2711P-IDDC                                           
086400           DELIMITED BY SIZE INTO W001-DAP-HEAD                           
086500     WRITE ORDER-REPORT-REC  FROM W001-DAP-HEAD                           
086600                                                                          
086700     MOVE SPACE                TO W001-DAP-HEAD                           
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100                                                                          
087200* --- IMS SEKTIONER --   &&&                                              
087300     SKIP3                                                                
087400                                                                          
087500                                                                          
087600 IMS-GU-WDK6-ART SECTION.                                                 
087700                                                                          
087800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
087900          DELIMITED BY SIZE INTO SSA1                                     
088000     MOVE '  GE' TO GODK-STATUSKODER                                      
088100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
088200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088500     EJECT                                                                
088600                                                                          
088700 IMS-GNP-WDK621 SECTION.                                                  
088800     MOVE   'WDK611  (KDSEGKEY =1)' TO SSA1                               
088900                                                                          
089000     STRING 'WDK621  (DAPRLIST>=' W-DAPRLIST-21-N                         
089100                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
089200          DELIMITED BY SIZE INTO SSA2                                     
089300     MOVE '  GE' TO GODK-STATUSKODER                                      
089400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1 SSA2         
089500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
089600     PERFORM IMS-STATUSKONTROLL                                           
089700     SKIP3                                                                
089800     .                                                                    
089900 IMS-GU-WDD311-BSEQ SECTION.                                              
090000                                                                          
090100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
090400          DELIMITED BY SIZE INTO SSA2                                     
090500     MOVE '  GE' TO GODK-STATUSKODER                                      
090600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD311 SSA1 SSA2          
090700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
090800     PERFORM IMS-STATUSKONTROLL                                           
090900     .                                                                    
091000     SKIP3                                                                
091100 IMS-GU-WDD301-BSEQ SECTION.                                              
091200                                                                          
091300     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
091400          DELIMITED BY SIZE INTO SSA1                                     
091500     MOVE '  GE' TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD301 SSA1               
091700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000     SKIP3                                                                
092100 IMS-GNP-WDD311 SECTION.                                                  
092200                                                                          
092300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
092400          DELIMITED BY SIZE INTO SSA1                                     
092500     MOVE '  GE' TO GODK-STATUSKODER                                      
092600     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-WDD311 SSA1              
092700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
092800     PERFORM IMS-STATUSKONTROLL                                           
092900     .                                                                    
093000     EJECT                                                                
093100                                                                          
093200 IMS-GU-WDF1-ROT SECTION.                                                 
093300                                                                          
093400     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
093500          DELIMITED BY SIZE INTO SSA1                                     
093600     MOVE '  GE' TO GODK-STATUSKODER                                      
093700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-WDF101 SSA1               
093800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
093900     PERFORM IMS-STATUSKONTROLL                                           
094000     .                                                                    
094100                                                                          
094200 IMS-GNP-WDF1-ADRESS SECTION.                                             
094300                                                                          
094400     MOVE 'WDF106   ' TO SSA1                                             
094500     MOVE '  GE' TO GODK-STATUSKODER                                      
094600     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-AREA-WDF106 SSA1              
094700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000                                                                          
095100*IMS-GNP-JPN-ADRESS SECTION.                                              
095200*JP*                                                                      
095300*JP* STRING 'WDF117  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
095400*JP*      DELIMITED BY SIZE INTO SSA1                                     
095500*JP* MOVE '  GE' TO GODK-STATUSKODER                                      
095600*JP* CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-AREA-WDF117 SSA1              
095700*JP* MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
095800*JP* PERFORM IMS-STATUSKONTROLL                                           
095900*JP* .                                                                    
096000                                                                          
096100 IMS-GU-F501-ARTIKEL     SECTION.                                         
096200     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
096300            DELIMITED BY SIZE INTO SSA1                                   
096400     MOVE '  GE' TO GODK-STATUSKODER                                      
096500     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA-WDF501 SSA1               
096600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     CONTINUE.                                                            
096900     SKIP2                                                                
097000                                                                          
097100 IMS-GNP-F502-LEV-NUMMER    SECTION.                                      
097200     MOVE 'WDF502  ' TO SSA1                                              
097300     MOVE '  GE' TO GODK-STATUSKODER                                      
097400     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA-WDF502 SSA1              
097500     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
097600     PERFORM IMS-STATUSKONTROLL                                           
097700     CONTINUE.                                                            
097800     SKIP2                                                                
097900                                                                          
098000 IMS-ISRT-WDL6-ART SECTION.                                               
098100     SKIP2                                                                
098200     MOVE 'WDL601   ' TO SSA1                                             
098300     MOVE '  II' TO GODK-STATUSKODER                                      
098400     CALL CBLTDLI USING ISRT WDL6-PCB                                     
098500                        DLI-IO-AREA-WDL601 SSA1                           
098600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
098700     PERFORM IMS-STATUSKONTROLL                                           
098800     .                                                                    
098900     SKIP3                                                                
099000 IMS-ISRT-WDL6-ORD SECTION.                                               
099100     SKIP2                                                                
099200     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
099300             DELIMITED BY SIZE INTO SSA1                                  
099400     MOVE 'WDL612   ' TO SSA2                                             
099500     MOVE '  ' TO GODK-STATUSKODER                                        
099600     CALL CBLTDLI USING ISRT WDL6-PCB                                     
099700                        DLI-IO-AREA-WDL612 SSA1 SSA2                      
099800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
099900     PERFORM IMS-STATUSKONTROLL                                           
100000     .                                                                    
100100     SKIP3                                                                
100200 IMS-GHU-2504 SECTION.                                                    
100300     STRING 'WL250301(WDGXKEY  =' W-WDGXKEY-X ')'                         
100400          DELIMITED BY SIZE INTO SSA1                                     
100500     MOVE 'WL250311 '         TO SSA2                                     
100600     MOVE '  ' TO GODK-STATUSKODER                                        
100700     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA-GX2504 SSA1 SSA2         
100800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
100900     PERFORM IMS-STATUSKONTROLL                                           
101000     .                                                                    
101100     EJECT                                                                
101200 IMS-REPL-2504 SECTION.                                                   
101300                                                                          
101400     MOVE '  ' TO GODK-STATUSKODER                                        
101500     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-AREA-GX2504                  
101600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
101700     PERFORM IMS-STATUSKONTROLL                                           
101800     .                                                                    
101900     SKIP3                                                                
102000 IMS-GU-WDB601    SECTION.                                                
102100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
102200          DELIMITED BY SIZE INTO SSA1                                     
102300     MOVE '  GE' TO GODK-STATUSKODER                                      
102400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
102500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
102600     PERFORM IMS-STATUSKONTROLL                                           
102700     IF SEGMENT-SAKNAS                                                    
102800         MOVE SPACE TO DCS-KDDC                                           
102900     END-IF                                                               
103000     .                                                                    
103100 IMS-GNP-WDB602   SECTION.                                                
103200     MOVE 'WDB602 '           TO SSA1                                     
103300     MOVE '  GE' TO GODK-STATUSKODER                                      
103400     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B602 SSA1                
103500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
103600     PERFORM IMS-STATUSKONTROLL                                           
103700     .                                                                    
103800 IMS-STATUSKONTROLL SECTION.                                              
103900                                                                          
104000     SET STATUS-IX TO 1                                                   
104100     SEARCH GODK-STATUS                                                   
104200       AT END                                                             
104300         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
104400         DISPLAY FELTEXT                                                  
104500         CALL FELLOG                                                      
104600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
104700         CONTINUE                                                         
104800     END-SEARCH                                                           
104900     .                                                                    
