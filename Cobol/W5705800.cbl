000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5705800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   20210624.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*                                                                         
000900*    FUNKTION:                                                            
001000*       -PGM LÄSER KONTROLLERADE/KOMPLETTERADE EKONOMISKA                 
001100*        HÄNDELSETRANSAKTIONER OCH MATCHAR DESSA MOT                      
001200*        EKONOMISKA STYRPARAMETRAR FÖR ATT I SLUTÄNDEN                    
001300*        PRODUCERA POSTER TILL R3 I FORM AV                               
001400*        1 "LINE RECORD" HUVUDBOK               (PTYP 610)                
001500*        2 "LINE RECORD" KUNDRESKONTRA          (PTYP 310)                
001600*        3 "LINE RECORD" LEVERANTÖRSRESKONTRA   (PTYP 210)                
001700*                                                                         
001800*       -PGM SKAPAR/SKRIVER ÄVEN FÖLJANDE POSTER TILL R3                  
001900*        1 "HEADER RECORD" HUVUDBOK             (PTYP 600)                
002000*        2 "HEADER RECORD" KUNDRESKONTRA        (PTYP 300)                
002100*        3 "HEADER RECORD" LEVERANTÖRSRESKONTRA (PTYP 200)                
002200*                                                                         
002300*       -PGM PLOCKAR UNDAN NY MÅNADS POSTER VID MÅNADSSKIFTE              
002400*        FÖR ATT TA IN DESSA VID NÄSTA KÖRNING.                           
002500*        (NY MÅNADS POSTER = DATUMKORTS MÅNAD + 1, OM DENNA ÄR            
002600*         LIKA MED IN-POSTENS DAVERDAT'S MÅNAD,                           
002700*         SKRIVS POSTEN PÅ UTFIL FÖR AT TAS IN NÄSTA KÖRNING).            
002800*                                                                         
002900*       -PROGRAMMET LÄSER      WDH5                                       
003000*                              WDB1                                       
003100*                              WDB2                                       
003200*                              WDR1                                       
003300*                         MÅNADSKURSER WDG2                               
003400*                           DCREGISTER WDB6                               
003500*                                                                         
003600*    ABENDKODER:                                                          
003700*        U0016 -  . . . .                                                 
003800*        U1000 -  . . . .                                                 
003900*                                                                         
004000                                                                          
004100 ENVIRONMENT DIVISION.                                                    
004200                                                                          
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600*          --- KONTR./KOMPL. HÄNDELSETRANSAKTIONER                        
004700     SELECT W57066                     ASSIGN TO W57058D1.                
004800                                                                          
004900*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
005000     SELECT W57051A                    ASSIGN TO W57058D2.                
005100                                                                          
005200*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005300     SELECT W57052A                    ASSIGN TO W57058D3.                
005400                                                                          
005500*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005600     SELECT W57053A                    ASSIGN TO W57058D4.                
005700                                                                          
005800*          --- LOGG TILL ON-DEMAND                                        
005900     SELECT W57055                     ASSIGN TO W57058D5.                
006000                                                                          
006100*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006200     SELECT W57058                     ASSIGN TO W57058D6.                
006300                                                                          
006400*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006500     SELECT W5705N                     ASSIGN TO W57058D7.                
006600                                                                          
006700*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006800     SELECT W51350                     ASSIGN TO W57058D8.                
006900     EJECT                                                                
007000                                                                          
007100 DATA DIVISION.                                                           
007200                                                                          
007300 FILE SECTION.                                                            
007400 FD  W57066                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700 01  SAP-POST.                                                            
007800*    03  -COPY WDR801        -L.                                          
007900     03 FILLER                   PIC X(6).                                
008000                                                                          
008100 FD  W57051A                                                              
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400*01  51INIT-POST -COPY R3INIT20               -L.                         
008500*01  51HEAD-POST -COPY R3HEAD20               -L.                         
008600*01  51LINE-POST -COPY R3LINE20               -L.                         
008700                                                                          
008800 FD  W57052A                                                              
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100*01  52LINE-POST -COPY R3LINE20               -L.                         
009200                                                                          
009300 FD  W57053A                                                              
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  53HEAD-POST -COPY R3HEAD20               -L.                         
009700*01  53LINE-POST -COPY R3LINE20               -L.                         
009800                                                                          
009900 FD  W57055                                                               
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200*01  LOGG-POST   -COPY W57073                 -L.                         
010300                                                                          
010400 FD  W57058                                                               
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700*01  AVST-POST   -COPY W57070                 -L.                         
010800                                                                          
010900 FD  W5705N                                                               
011000     RECORDING       F                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300 01  SAPUT-POST.                                                          
011400*    03  -COPY WDR801        -L.                                          
011500     03 FILLER                   PIC X(6).                                
011600                                                                          
011700 FD  W51350                                                               
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012100                                                                          
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400*    -- CHECKED BY WY2000                                                 
012500 77  IDPGM                        PIC X(8)    VALUE 'W5705800'.           
012600 77  JA                           PIC X       VALUE 'J'.                  
012700 77  NEJ                          PIC X       VALUE 'N'.                  
012800 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
012900 77  W57066-EOF-SW                PIC X       VALUE 'N'.                  
013000     88  END-OF-W57066                        VALUE 'J'.                  
013100 77  WS-HEADER-SW                 PIC X       VALUE 'N'.                  
013200 77  WS-LINE-SW                   PIC X       VALUE 'N'.                  
013300 77  WS-STATUS                    PIC XX      VALUE '  '.                 
013400 77  WS-LINE-AMOUNT               PIC S9(13)V99 COMP-3.                   
013500 77  WS-LINE-AMOUNT-121-1         PIC S9(13)V99 COMP-3.                   
013600 77  WS-LINE-AMOUNT-121-2         PIC S9(13)V99 COMP-3.                   
013700 77  SPAR-SUMMA-102-125         PIC S9(13)V99  COMP-3 VALUE ZERO.         
013800 77  WS-BELOPP                  PIC S9(13)V99  COMP-3.                    
013900 77  WS-LOP                       PIC 9       VALUE ZERO.                 
014000 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
014100 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
014200 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
014300 77  SPAR-LINE-ORDER              PIC X(12).                              
014400 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
014500 77  WS-RED-IDKST                 PIC X(10).                              
014600 77  WS-IDPTYP                    PIC X(3).                               
014700 77  WS-FAKTURA-DATUM             PIC X(16).                              
014800 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
014900 77  SPAR-SUMMA                 PIC S9(13)V99  COMP-3 VALUE ZERO.         
015000 77  SPAR-PRDMTRL               PIC S9(13)V99  COMP-3 VALUE ZERO.         
015100 77  SPAR-PROVRPAL              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015200 77  SPAR-PRDIRLON              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015300 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
015400 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
015500 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
015600 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
015700 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
015800 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
015900 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
016000 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
016100                                                                          
016200 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
016300       88  WDB6-A-FINNS                       VALUE 'J'.                  
016400       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
016500                                                                          
016600*01  -COPY WWPRODSL                                                       
016700                                                                          
016800*01  -COPY WWDCKONS                                                       
016900     EJECT                                                                
017000                                                                          
017100 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
017200*01  -COPY WWIDFTG                                                        
017300     EJECT                                                                
017400                                                                          
017500 01  FELTEXT                      PIC X(80).                              
017600 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
017700*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
017800     EJECT                                                                
017900                                                                          
018000 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
018100 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
018200     EJECT                                                                
018300 01  WS-IDDISTR-IDKUNDNR.                                                 
018400     03  FILLER                   PIC X(2)    VALUE SPACE.                
018500     03  WS-IDDISTR               PIC 9(4).                               
018600     03  WS-IDKUNDNR              PIC 9(6).                               
018700                                                                          
018800 01  WS-KDBETVIL                  PIC X(4).                               
018900 01  WS-KDVALISO-WDB1             PIC X(3).                               
019000 01  WS-KDVALISO                  PIC X(3).                               
019100 01  WS-KDVALISO-USD              PIC X(3) VALUE 'USD'.                   
019200 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
019300 01  WS-PRKURS-USD                PIC S9(6)V9(5) COMP-3.                  
019400 01  WS-PRKURS-USD2               PIC S9(6)V9(5) COMP-3.                  
019500 01  WS-PRKURS-USD3               PIC S9(6)V9(5) COMP-3.                  
019600 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
019700 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
019800                                                                          
019900 01  WS-ALLOCATE.                                                         
020000     03  WS-ALLOCATE-DC           PIC X(2).                               
020100     03  WS-ALLOCATE-DISTR        PIC X(5).                               
020200     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
020300     03  FILLER                   PIC X(4)    VALUE SPACE.                
020400                                                                          
020500 01  WS-TEXT.                                                             
020600     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
020700     03  WS-TEXT-KDEKHHT          PIC X(3).                               
020800     03  WS-TEXT-KDEKSHT          PIC X(3).                               
020900     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
021000     03  FILLER                   PIC X(7)    VALUE SPACE.                
021100                                                                          
021200 01  WS-LINE-TEXT.                                                        
021300     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
021400     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
021500     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
021600     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
021700     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
021800     03  FILLER                   PIC X(22)   VALUE SPACE.                
021900                                                                          
022000 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
022100 01  WS-PRCTR.                                                            
022200     03  WS-PRCTR-PRODSL          PIC X(2).                               
022300     03  FILLER                   PIC X(1).                               
022400     03  FILLER                   PIC X(7).                               
022500                                                                          
022600 01  WS-R3-ACCOUNT.                                                       
022700     03  WS-R3-ACCOUNT-ALFA.                                              
022800         05 FILLER                PIC X(4).                               
022900         05 WS-R3-ACCOUNT-6       PIC X(6).                               
023000     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
023100         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
023200                                                                          
023300 01  WS-ACCOUNT.                                                          
023400     03  FILLER                   PIC X(7).                               
023500     03  WS-ACCOUNT-4             PIC X(1).                               
023600     03  FILLER                   PIC X(2).                               
023700                                                                          
023800 01  SPAR-AREA.                                                           
023900     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
024000     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
024100     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
024200     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
024300                                                                          
024400 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
024500 01  FILLER REDEFINES DAGENS-DATUM.                                       
024600     03  DAGENS-DATUM-AAR         PIC 9(2).                               
024700     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
024800     03  DAGENS-DATUM-DAG         PIC 9(2).                               
024900                                                                          
025000 01  WS-NEW-MONTH                 PIC 9(2).                               
025100                                                                          
025200 01  WS-DAREGDAT.                                                         
025300     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
025400     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
025500                                                                          
025600 01  WS-TIREGDAT-TOT.                                                     
025700     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
025800     03  WS-TIREGDAT              PIC 9(6).                               
025900                                                                          
026000 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
026100 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
026200     EJECT                                                                
026300                                                                          
026400 01  DYNAMISKA-SUBPROGRAM.                                                
026500     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
026600     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
026700     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
026800     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
026900     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
027000     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
027100                                                                          
027200*    --- PARAMETRAR TILL ABEND                                            
027300 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
027400 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
027500 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
027600     EJECT                                                                
027700                                                                          
027800*    --- PARAMETRAR TILL DATKORT                                          
027900 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57058'.             
028000                                                                          
028100 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
028200*01  -COPY WDATKORT                                                       
028300     EJECT                                                                
028400                                                                          
028500*    --- PARAMETRAR TILL POSTSUM                                          
028600*01  -COPY W0005   -PRE  POSTSUM-                                         
028700     EJECT                                                                
028800                                                                          
028900 01  FILLER                          PIC X(16) VALUE 'W510CURR '.         
029000*01  -COPY W510CURR                                                       
029100     EJECT                                                                
029200                                                                          
029300 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
029400*01  AREA -COPY WDR801           -PRE IN-                                 
029500*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
029600         05   IN-EKH-IDSYSMOT     PIC X(6).                               
029700                                                                          
029800     EJECT                                                                
029900 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
030000                                                                          
030100*01  -COPY R3LINE20              -PRE R3-                                 
030200*01  -COPY R3HEAD20              -PRE R3-                                 
030300*01  -COPY R3INIT20              -PRE R3-                                 
030400*01  -COPY W57073                -PRE LOGG-                               
030500*01  -COPY W57070                -PRE AVST-                               
030600*01  -COPY W517RW1               -PRE RW1-                                
030700*01  -COPY W517RW2               -PRE RW2-                                
030800*01  -COPY W51310                -PRE INV-                                
030900     EJECT                                                                
031000                                                                          
031100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031200 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
031300                                                                          
031400 01  NYCKLAR-TILL-DLI.                                                    
031500     03  W-WDH501KY-X.                                                    
031600         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
031700         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
031800     03  W-KDEKSHT-X.                                                     
031900         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
032000     03  W-KDEKNIVA-X.                                                    
032100         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
032200     03  W-WDH531KY-X.                                                    
032300         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
032400         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
032500     03  W-IDRADNR-X.                                                     
032600         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
032700                                                                          
032800     03  W-IDGMT-KEY.                                                     
032900         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
033000         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
033100                                                                          
033200     03  W-WDB101KY-X.                                                    
033300         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
033400         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
033500                                                                          
033600     03  W-WDGXKEY-5121-X.                                                
033700         05  FILLER               PIC X(4)    VALUE '5121'.               
033800         05  FILLER               PIC X(2)    VALUE '61'.                 
033900         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
034000     03  W-WDGXKEY-5122-X.                                                
034100         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
034200         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
034300     03  W-WDGXKEY-5122-MIN-X.                                            
034400         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
034500         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
034600     03  W-WDGXKEY-5122-MAX-X.                                            
034700         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
034800         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
034900                                                                          
035000     03  W-IDDC-B6-X.                                                     
035100         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
035200                                                                          
035300     03  W-IDARTNR-X.                                                     
035400         05 W-IDARTNR             PIC S9(9) COMP-3.                       
035500                                                                          
035600     03  W-IDFAKT-X.                                                      
035700         05 W-IDFAKT              PIC S9(7) COMP-3.                       
035800                                                                          
035900     EJECT                                                                
036000                                                                          
036100*    --- STATUS-KOD FRÅN IMS                                              
036200 01  STATUS-WS                    PIC XX.                                 
036300     88  SEGMENT-FINNS                        VALUE '  '.                 
036400     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
036500                                                                          
036600 01  GODK-STATUSKODER.                                                    
036700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036800                                                                          
036900 01  SSA1                         PIC X(128).                             
037000 01  SSA2                         PIC X(64).                              
037100 01  SSA3                         PIC X(64).                              
037200     EJECT                                                                
037300                                                                          
037400*    --- IMS FUNKTIONSKODER                                               
037500*01  -COPY W0003                                                          
037600     EJECT                                                                
037700                                                                          
037800*    ---  DLI INPUT-OUTPUT AREA                                           
037900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
038000 01  DLI-IO-WDH501.                                                       
038100*    03  -COPY WDH501                                                     
038200     EJECT                                                                
038300                                                                          
038400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
038500 01  DLI-IO-WDH511.                                                       
038600*    03  -COPY WDH511                                                     
038700     EJECT                                                                
038800                                                                          
038900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
039000 01  DLI-IO-WDH521.                                                       
039100*    03  -COPY WDH521                                                     
039200     EJECT                                                                
039300                                                                          
039400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
039500 01  DLI-IO-WDH531.                                                       
039600*    03  -COPY WDH531                                                     
039700     EJECT                                                                
039800                                                                          
039900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
040000 01  DLI-IO-WDB101.                                                       
040100*    03  -COPY WDB101                                                     
040200     EJECT                                                                
040300                                                                          
040400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
040500 01  DLI-IO-WDB201.                                                       
040600*    03  -COPY WDB201                                                     
040700     EJECT                                                                
040800                                                                          
040900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
041000 01  DLI-IO-WDGX5121.                                                     
041100*    03  -COPY WDGX5121                                                   
041200     EJECT                                                                
041300                                                                          
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
041500 01  DLI-IO-WDGX5122.                                                     
041600*    03  -COPY WDGX5122                                                   
041700     EJECT                                                                
041800                                                                          
041900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042000 01   DLI-IO-AREA-B601.                                                   
042100*     03  -COPY WDB601                                                    
042200     EJECT                                                                
042300 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
042400 01   DLI-IO-AREA-B617.                                                   
042500*     03  -COPY WDB617                                                    
042600     EJECT                                                                
042700 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
042800 01   DLI-IO-AREA-L601.                                                   
042900*     03  -COPY WDL601                                                    
043000     EJECT                                                                
043100 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
043200 01   DLI-IO-AREA-L611.                                                   
043300*     03  -COPY WDL611                                                    
043400     EJECT                                                                
043500 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6C1'.                
043600     SKIP3                                                                
043700                                                                          
043800 LINKAGE SECTION.                                                         
043900*01  -COPY W0008  -PRE WDH5-                                              
044000     05  FILLER                  PIC X.                                   
044100                                                                          
044200*01  -COPY W0008  -PRE WDB2-                                              
044300     05  FILLER                  PIC X.                                   
044400                                                                          
044500*01  -COPY W0008  -PRE WDB1-                                              
044600     05  FILLER                  PIC X.                                   
044700                                                                          
044800*01  -COPY W0008  -PRE 5121-                                              
044900     05  FILLER                  PIC X.                                   
045000                                                                          
045100*01  -COPY W0008  -PRE WDG2-                                              
045200     05  FILLER                  PIC X.                                   
045300                                                                          
045400*01  -COPY W0008  -PRE WDB6-                                              
045500     05  FILLER                  PIC X.                                   
045600                                                                          
045700*01  -COPY W0008  -PRE WDL6-                                              
045800     05  FILLER                  PIC X.                                   
045900                                                                          
046000                                                                          
046100     EJECT                                                                
046200                                                                          
046300 PROCEDURE DIVISION  USING WDH5-PCB WDB2-PCB WDB1-PCB 5121-PCB            
046400                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
046500 MAIN SECTION.                                                            
046600     ENTRY 'DLITCBL' USING WDH5-PCB WDB2-PCB WDB1-PCB 5121-PCB            
046700                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
046800                                                                          
046900     PERFORM A-INIT                                                       
047000                                                                          
047100     PERFORM S01-READ-W57066                                              
047200     PERFORM UNTIL END-OF-W57066                                          
047300*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
047400       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
047500       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
047600       AND WS-NEW-MONTH > 01                                              
047700         PERFORM S60-WRITE-W5705N                                         
047800       ELSE                                                               
047900         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
048000         PERFORM S30-READ-DATABASE-B2-B1                                  
048100         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
048200           PERFORM C-EXECUTE                                              
048300         END-IF                                                           
048400       END-IF                                                             
048500       PERFORM S01-READ-W57066                                            
048600     END-PERFORM                                                          
048700                                                                          
048800     PERFORM Z-FINI                                                       
048900                                                                          
049000     MOVE ZERO TO RETURN-CODE                                             
049100     GOBACK                                                               
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500 A-INIT SECTION.                                                          
049600     OPEN INPUT  W57066                                                   
049700                                                                          
049800     OPEN OUTPUT W57058                                                   
049900                 W57051A                                                  
050000                 W57052A                                                  
050100                 W57053A                                                  
050200                 W57055                                                   
050300                 W5705N                                                   
050400                 W51350                                                   
050500                                                                          
050600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
050700     MOVE 20               TO RW1-DAVVREG(1:2)                            
050800     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
050900                              RW1-DAVVREG(3:2)                            
051000                              W-DATE-AAMM(1:2)                            
051100                              WS-TIAA                                     
051200     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
051300                              W-DATE-AAMM(3:2)                            
051400                              WS-TIMM                                     
051500                              WS-NEW-MONTH                                
051600     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
051700     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
051800     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
051900                                                                          
052000*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
052100*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
052200     IF WS-NEW-MONTH = 12                                                 
052300       MOVE 1              TO WS-NEW-MONTH                                
052400     ELSE                                                                 
052500       ADD 1               TO WS-NEW-MONTH                                
052600*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
052700***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
052800***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
052900***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
053000***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
053100       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
053200       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
053300         ADD 1             TO WS-NEW-MONTH                                
053400         ADD 1             TO WS-TIMM                                     
053500         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
053600       END-IF                                                             
053700     END-IF                                                               
053800                                                                          
053900     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
054000                                                                          
054100     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
054200                                                                          
054300     ACCEPT DAGENS-KLOCKA FROM TIME                                       
054400     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
054500                                                                          
054600     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
054700     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
054800     MOVE 'M'                   TO CURR-KDVALTYP                          
054900                                                                          
055000     MOVE WS-KDVALISO-USD       TO CURR-KDVALISO-ROW                      
055100     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
055200     IF CURR-KDSVAR = ' '                                                 
055300       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-USD                          
055400     ELSE                                                                 
055500       MOVE 1                   TO WS-PRKURS-USD                          
055600     END-IF                                                               
055700     COMPUTE WS-PRKURS-USD2 ROUNDED = 1 / WS-PRKURS-USD                   
055800     MOVE WS-PRKURS-USD         TO WS-PRKURS-USD3                         
055900     .                                                                    
056000     EJECT                                                                
056100                                                                          
056200 C-EXECUTE SECTION.                                                       
056300     MOVE WC-IDFTG-AE           TO W-IDFTG                                
056400     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
056500     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
056600     IF IN-EKH-KDEKNIVA = 'TDET'                                          
056700       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
056800     END-IF                                                               
056900     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
057000     PERFORM IMS-GU-WDH521                                                
057100     PERFORM IMS-GNP-WDH531                                               
057200                                                                          
057300     PERFORM S13-GET-LANDING-COST                                         
057400                                                                          
057500     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
057600     IF IN-EKH-IDDISTR > ZERO                                             
057700       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
057800     ELSE                                                                 
057900       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
058000     END-IF                                                               
058100     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
058200                                                                          
058300* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
058400* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
058500* BOKFÖRINGEN I SAP SKER DOCK ALLTID I USD, DÄRFÖR BYTET HÄR:             
058600*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
058700*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '106')               
058800*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '107')               
058900*      MOVE 'USD'               TO WS-KDVALISO                            
059000*    END-IF                                                               
059100                                                                          
059200     IF  ((IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                               
059300     AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                                
059400     OR (IN-EKH-KDEKHHT = '303'                                           
059500     AND IN-EKH-KDEKSHT = '301')                                          
059600     OR (IN-EKH-KDEKHHT = '303'                                           
059700     AND IN-EKH-KDEKSHT = '307')                                          
059800     OR (IN-EKH-KDEKHHT = '303'                                           
059900     AND IN-EKH-KDEKSHT = '361'))                                         
060000       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
060100     ELSE                                                                 
060200       MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                             
060300       MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                             
060400       IF WS-LOP = 9                                                      
060500         MOVE ZERO  TO WS-LOP                                             
060600       ELSE                                                               
060700         ADD +1     TO WS-LOP                                             
060800       END-IF                                                             
060900       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
061000     END-IF                                                               
061100* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
061200     IF SYST-IDPTYP = '210'                                               
061300       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
061400     ELSE                                                                 
061500       IF SYST-IDPTYP = '310'                                             
061600         PERFORM CC-CREATE-WRITE-HEADER-AR                                
061700       ELSE                                                               
061800* TEST OM BRYTNING PÅ VERIFIKATION                                        
061900         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
062000         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
062100         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
062200         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
062300           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
062400           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
062500           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
062600           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
062700           IF (IN-EKH-KDEKHHT = '102'                                     
062800           AND IN-EKH-KDEKSHT = '121')                                    
062900           OR (IN-EKH-KDEKHHT = '102'                                     
063000           AND IN-EKH-KDEKSHT = '122')                                    
063100           OR (IN-EKH-KDEKHHT = '102'                                     
063200           AND IN-EKH-KDEKSHT = '131')                                    
063300           OR (IN-EKH-KDEKHHT = '102'                                     
063400           AND IN-EKH-KDEKSHT = '132')                                    
063500             PERFORM S80-GET-CURRENCY-RATE                                
063600           END-IF                                                         
063700           IF (IN-EKH-KDEKHHT = '303'                                     
063800           AND IN-EKH-KDEKSHT = '301')                                    
063900           OR (IN-EKH-KDEKHHT = '303'                                     
064000           AND IN-EKH-KDEKSHT = '307')                                    
064100           OR (IN-EKH-KDEKHHT = '303'                                     
064200           AND IN-EKH-KDEKSHT = '371')                                    
064300           OR (IN-EKH-KDEKHHT = '303'                                     
064400           AND IN-EKH-KDEKSHT = '3XX')                                    
064500             PERFORM S81-GET-CURRENCY-RATE                                
064600           END-IF                                                         
064700*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
064800*   HEADER-POST TILL HUVUDBOKEN                                           
064900           IF (IN-EKH-KDEKHHT = '102'                                     
065000           AND IN-EKH-KDEKSHT = '120')                                    
065100           OR (IN-EKH-KDEKHHT = '102'                                     
065200           AND IN-EKH-KDEKSHT = '124')                                    
065300           OR (IN-EKH-KDEKHHT = '102'                                     
065400           AND IN-EKH-KDEKSHT = '125')                                    
065500           OR (IN-EKH-KDEKHHT = '102'                                     
065600           AND IN-EKH-KDEKSHT = '130')                                    
065700           OR (IN-EKH-KDEKHHT = '102'                                     
065800           AND IN-EKH-KDEKSHT = '134')                                    
065900           OR (IN-EKH-KDEKHHT = '103'                                     
066000           AND IN-EKH-KDEKSHT = '102')                                    
066100           OR (IN-EKH-KDEKHHT = '103'                                     
066200           AND IN-EKH-KDEKSHT = '106')                                    
066300           OR (IN-EKH-KDEKHHT = '103'                                     
066400           AND IN-EKH-KDEKSHT = '107')                                    
066500           OR (IN-EKH-KDEKHHT = '204'                                     
066600           AND IN-EKH-KDEKSHT = '301')                                    
066700           OR (IN-EKH-KDEKHHT = '303'                                     
066800           AND IN-EKH-KDEKSHT = '301')                                    
066900           OR (IN-EKH-KDEKHHT = '303'                                     
067000           AND IN-EKH-KDEKSHT = '307')                                    
067100           OR (IN-EKH-KDEKHHT = '303'                                     
067200           AND IN-EKH-KDEKSHT = '361')                                    
067300           OR (IN-EKH-KDEKHHT = '303'                                     
067400           AND IN-EKH-KDEKSHT = '371')                                    
067500           OR (IN-EKH-KDEKHHT = '303'                                     
067600           AND IN-EKH-KDEKSHT = '3XX')                                    
067700             CONTINUE                                                     
067800           ELSE                                                           
067900             PERFORM CA-CREATE-WRITE-HEADER-GL                            
068000           END-IF                                                         
068100         END-IF                                                           
068200       END-IF                                                             
068300     END-IF                                                               
068400                                                                          
068500**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
068600     MOVE WS-STATUS TO STATUS-WS                                          
068700     PERFORM UNTIL SEGMENT-SAKNAS                                         
068800       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
068900                                                                          
069000* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
069100       IF SYST-IDPTYP = '610'                                             
069200         PERFORM CD-BUILD-COMMON-610-PART                                 
069300         PERFORM CE-SCHEDULE-LINE-GL                                      
069400       ELSE                                                               
069500         IF SYST-IDPTYP = '210'                                           
069600           PERFORM CF-BUILD-COMMON-210-PART                               
069700           PERFORM CG-SCHEDULE-LINE-AP                                    
069800         ELSE                                                             
069900           IF SYST-IDPTYP = '310'                                         
070000             PERFORM CH-BUILD-COMMON-310-PART                             
070100             PERFORM CI-SCHEDULE-LINE-AR                                  
070200           END-IF                                                         
070300         END-IF                                                           
070400       END-IF                                                             
070500       PERFORM IMS-GNP-WDH531                                             
070600     END-PERFORM                                                          
070700     .                                                                    
070800     EJECT                                                                
070900                                                                          
071000 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
071100     MOVE SPACE                   TO R3-HEAD-R3                           
071200     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
071300     MOVE 'AE01'                  TO R3-HEAD-COMPANY-CODE                 
071400     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
071500     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
071600     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
071700     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
071800     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
071900       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
072000     ELSE                                                                 
072100       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
072200     END-IF                                                               
072300     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
072400     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
072500     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
072600     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
072700     IF WS-KDVALISO = 'USD'                                               
072800       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
072900     ELSE                                                                 
073000       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
073100       MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                     
073200       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
073300       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
073400       IF CURR-KDSVAR = ' '                                               
073500         IF IN-EKH-IDDISTR > ZERO                                         
073600           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
073700         ELSE                                                             
073800           MOVE 1               TO WS-PRKURS                              
073900         END-IF                                                           
074000       ELSE                                                               
074100         MOVE 1                 TO WS-PRKURS                              
074200       END-IF                                                             
074300       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
074400                                       CURR-REVALUTA-TO                   
074500       IF CURR-REVALUTA-TO = +1                                           
074600         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
074700       END-IF                                                             
074800       IF CURR-REVALUTA-TO = +10                                          
074900         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
075000       END-IF                                                             
075100       IF CURR-REVALUTA-TO = +100                                         
075200         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
075300       END-IF                                                             
075400     END-IF                                                               
075500     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
075600     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
075700     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
075800     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
075900     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
076000     MOVE JA                      TO WS-HEADER-SW                         
076100     MOVE NEJ                     TO WS-LINE-SW                           
076200                                                                          
076300* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
076400     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
076500     OR (IN-EKH-KDEKHHT = '303'                                           
076600     AND IN-EKH-KDEKSHT = '391')                                          
076700     OR (IN-EKH-KDEKHHT = '102'                                           
076800     AND IN-EKH-KDEKSHT = '121')                                          
076900     OR (IN-EKH-KDEKHHT = '102'                                           
077000     AND IN-EKH-KDEKSHT = '131')                                          
077100       PERFORM S004-WRITE-W57053A-HEAD                                    
077200     ELSE                                                                 
077300       PERFORM S002-WRITE-W57051A-HEAD                                    
077400     END-IF                                                               
077500     .                                                                    
077600     EJECT                                                                
077700                                                                          
077800 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
077900     MOVE SPACE                   TO R3-HEAD-R3                           
078000     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
078100     MOVE 'AE01'                  TO R3-HEAD-COMPANY-CODE                 
078200                                     R3-HEAD-CONTROL-AREA                 
078300     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
078400     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
078500     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
078600     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
078700     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
078800     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
078900       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
079000     ELSE                                                                 
079100       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
079200     END-IF                                                               
079300     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
079400     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
079500     IF (IN-EKH-KDEKHHT = '103'                                           
079600     AND IN-EKH-KDEKSHT = '102')                                          
079700     OR (IN-EKH-KDEKHHT = '103'                                           
079800     AND IN-EKH-KDEKSHT = '106')                                          
079900     OR (IN-EKH-KDEKHHT = '103'                                           
080000     AND IN-EKH-KDEKSHT = '107')                                          
080100       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
080200       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
080300     ELSE                                                                 
080400       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
080500       MOVE WS-PRKURS-USD2        TO R3-HEAD-EXCHANGE-RATE                
080600       MOVE 'USD'                 TO CURR-KDVALISO-ROW                    
080700       IF IN-FIL-IDPGM = 'W4183300'                                       
080800         IF IN-EKH-DAAVIDAT > ZERO                                        
080900           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
081000           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
081100         ELSE                                                             
081200           MOVE WS-TIAA              TO WS-TIAA-CR                        
081300           MOVE WS-TIMM              TO WS-TIMM-CR                        
081400         END-IF                                                           
081500       ELSE                                                               
081600         MOVE WS-TIAA                TO WS-TIAA-CR                        
081700         MOVE WS-TIMM                TO WS-TIMM-CR                        
081800       END-IF                                                             
081900       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
082000       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
082100       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
082200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
082300       IF CURR-KDSVAR = ' '                                               
082400         IF IN-EKH-IDDISTR > ZERO                                         
082500           MOVE CURR-PRKURS-NEW TO WS-PRKURS-USD                          
082600         ELSE                                                             
082700           IF WS-PRKURS = ZERO                                            
082800             MOVE 1             TO WS-PRKURS-USD                          
082900           END-IF                                                         
083000         END-IF                                                           
083100       ELSE                                                               
083200         MOVE 1                 TO WS-PRKURS-USD                          
083300       END-IF                                                             
083400       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-USD *                    
083500                                       CURR-REVALUTA-TO                   
083600       END-COMPUTE                                                        
083700       IF CURR-REVALUTA-TO = +1                                           
083800         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
083900       END-IF                                                             
084000       IF CURR-REVALUTA-TO = +10                                          
084100         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
084200       END-IF                                                             
084300       IF CURR-REVALUTA-TO = +100                                         
084400         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
084500       END-IF                                                             
084600     END-IF                                                               
084700     IF (IN-EKH-KDEKHHT = '102'                                           
084800     AND IN-EKH-KDEKSHT = '120')                                          
084900     OR (IN-EKH-KDEKHHT = '102'                                           
085000     AND IN-EKH-KDEKSHT = '124')                                          
085100     OR (IN-EKH-KDEKHHT = '102'                                           
085200     AND IN-EKH-KDEKSHT = '125')                                          
085300     OR (IN-EKH-KDEKHHT = '102'                                           
085400     AND IN-EKH-KDEKSHT = '130')                                          
085500     OR (IN-EKH-KDEKHHT = '102'                                           
085600     AND IN-EKH-KDEKSHT = '134')                                          
085700     OR (IN-EKH-KDEKHHT = '303'                                           
085800     AND IN-EKH-KDEKSHT = '301')                                          
085900     OR (IN-EKH-KDEKHHT = '303'                                           
086000     AND IN-EKH-KDEKSHT = '307')                                          
086100     OR (IN-EKH-KDEKHHT = '303'                                           
086200     AND IN-EKH-KDEKSHT = '361')                                          
086300     OR (IN-EKH-KDEKHHT = '303'                                           
086400     AND IN-EKH-KDEKSHT = '3XX')                                          
086500       MOVE 'USD'                 TO R3-HEAD-CURRENCY                     
086600       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
086700     END-IF                                                               
086800     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
086900     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
087000     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
087100     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
087200     MOVE JA                      TO WS-HEADER-SW                         
087300     MOVE NEJ                     TO WS-LINE-SW                           
087400                                                                          
087500* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57053A                       
087600       PERFORM S004-WRITE-W57053A-HEAD                                    
087700     .                                                                    
087800     EJECT                                                                
087900                                                                          
088000 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
088100     MOVE SPACE                   TO R3-HEAD-R3                           
088200     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
088300     MOVE 'AE01'                  TO R3-HEAD-COMPANY-CODE                 
088400                                     R3-HEAD-CONTROL-AREA                 
088500     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
088600     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
088700     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
088800     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
088900     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
089000     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
089100       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
089200     ELSE                                                                 
089300       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
089400     END-IF                                                               
089500     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
089600     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
089700     IF (IN-EKH-KDEKHHT = '204'                                           
089800     AND IN-EKH-KDEKSHT = '301')                                          
089900       MOVE 'USD'                 TO R3-HEAD-CURRENCY                     
090000       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
090100     ELSE                                                                 
090200       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
090300       MOVE WS-PRKURS-USD2        TO R3-HEAD-EXCHANGE-RATE                
090400       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
090500       IF IN-FIL-IDPGM = 'W4183300'                                       
090600         IF IN-EKH-DAAVIDAT > ZERO                                        
090700           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
090800           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
090900         ELSE                                                             
091000           MOVE WS-TIAA              TO WS-TIAA-CR                        
091100           MOVE WS-TIMM              TO WS-TIMM-CR                        
091200         END-IF                                                           
091300       ELSE                                                               
091400         MOVE WS-TIAA                TO WS-TIAA-CR                        
091500         MOVE WS-TIMM                TO WS-TIMM-CR                        
091600       END-IF                                                             
091700       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
091800       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
091900       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
092000       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
092100       IF CURR-KDSVAR = ' '                                               
092200         IF IN-EKH-IDDISTR > ZERO                                         
092300           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
092400         ELSE                                                             
092500           IF WS-PRKURS = ZERO                                            
092600             MOVE 1             TO WS-PRKURS                              
092700           END-IF                                                         
092800         END-IF                                                           
092900       ELSE                                                               
093000         MOVE 1                 TO WS-PRKURS                              
093100       END-IF                                                             
093200       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
093300                                       CURR-REVALUTA-TO                   
093400       END-COMPUTE                                                        
093500       IF CURR-REVALUTA-TO = +1                                           
093600         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
093700       END-IF                                                             
093800       IF CURR-REVALUTA-TO = +10                                          
093900         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
094000       END-IF                                                             
094100       IF CURR-REVALUTA-TO = +100                                         
094200         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
094300       END-IF                                                             
094400     END-IF                                                               
094500     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
094600     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
094700     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
094800     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
094900     MOVE JA                      TO WS-HEADER-SW                         
095000     MOVE NEJ                     TO WS-LINE-SW                           
095100                                                                          
095200* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57053A                       
095300       PERFORM S004-WRITE-W57053A-HEAD                                    
095400     .                                                                    
095500     EJECT                                                                
095600                                                                          
095700 CD-BUILD-COMMON-610-PART SECTION.                                        
095800     MOVE SPACE               TO R3-LINE-R3                               
095900     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
096000                                 R3-LINE-DUE-DATE                         
096100                                 R3-LINE-AMOUNT                           
096200                                 R3-LINE-AMOUNT-LC                        
096300                                 R3-LINE-TAX-AMOUNT                       
096400                                 R3-LINE-TAX-AMOUNT-LC                    
096500                                 R3-LINE-NUMBER-OF-DAYS                   
096600                                 R3-LINE-QUANTITY                         
096700                                 R3-LINE-SAMNR                            
096800     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
096900     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
097000     MOVE 'AE01'              TO R3-LINE-COMPANY-CODE                     
097100     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
097200     IF SYST-KDPOST = '50'                                                
097300       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
097400     ELSE                                                                 
097500       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
097600     END-IF                                                               
097700     IF SYST-IDPRCTR NOT = SPACE                                          
097800       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
097900       IF WS-PRCTR-PRODSL = '??'                                          
098000         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
098100         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
098200       END-IF                                                             
098300       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
098400     END-IF                                                               
098500     .                                                                    
098600     EJECT                                                                
098700                                                                          
098800 CE-SCHEDULE-LINE-GL SECTION.                                             
098900     MOVE NEJ                     TO WS-HEADER-SW                         
099000     MOVE JA                      TO WS-LINE-SW                           
099100     EVALUATE IN-EKH-KDEKHHT                                              
099200     WHEN '102'                                                           
099300          PERFORM CEB-MAIN-EVENT-102                                      
099400     WHEN '103'                                                           
099500          PERFORM CEC-MAIN-EVENT-103                                      
099600     WHEN '201'                                                           
099700          PERFORM CED-MAIN-EVENT-201                                      
099800     WHEN '203'                                                           
099900          PERFORM CEF-MAIN-EVENT-203                                      
100000     WHEN '204'                                                           
100100          PERFORM CEG-MAIN-EVENT-204                                      
100200     WHEN '302'                                                           
100300          PERFORM CEI-MAIN-EVENT-302                                      
100400     WHEN '303'                                                           
100500          PERFORM CEJ-MAIN-EVENT-303                                      
100600     WHEN '401'                                                           
100700          PERFORM CEK-MAIN-EVENT-401                                      
100800     WHEN '402'                                                           
100900          PERFORM CEL-MAIN-EVENT-402                                      
101000     WHEN '403'                                                           
101100          PERFORM CEM-MAIN-EVENT-403                                      
101200     WHEN '404'                                                           
101300          PERFORM CEN-MAIN-EVENT-404                                      
101400     END-EVALUATE                                                         
101500     .                                                                    
101600     EJECT                                                                
101700                                                                          
101800 CEB-MAIN-EVENT-102 SECTION.                                              
101900     EVALUATE IN-EKH-KDEKSHT                                              
102000     WHEN '102'                                                           
102100          PERFORM CEBB-SUB-EVENT-102-102                                  
102200     WHEN '120'                                                           
102300          PERFORM CEBD-SUB-EVENT-102-120                                  
102400     WHEN '121'                                                           
102500          PERFORM CEBD-SUB-EVENT-102-121                                  
102600     WHEN '122'                                                           
102700          PERFORM CEBD-SUB-EVENT-102-122                                  
102800     WHEN '123'                                                           
102900          PERFORM CEBD-SUB-EVENT-102-123                                  
103000     WHEN '124'                                                           
103100          PERFORM CEBD-SUB-EVENT-102-124                                  
103200     WHEN '125'                                                           
103300          PERFORM CEBD-SUB-EVENT-102-125                                  
103400     WHEN '130'                                                           
103500          PERFORM CEBE-SUB-EVENT-102-130                                  
103600     WHEN '131'                                                           
103700          PERFORM CEBE-SUB-EVENT-102-131                                  
103800     WHEN '132'                                                           
103900          PERFORM CEBE-SUB-EVENT-102-132                                  
104000     WHEN '134'                                                           
104100          PERFORM CEBE-SUB-EVENT-102-134                                  
104200     END-EVALUATE                                                         
104300     .                                                                    
104400     EJECT                                                                
104500                                                                          
104600 CEBB-SUB-EVENT-102-102 SECTION.                                          
104700     EVALUATE IN-EKH-KDEKNIVA                                             
104800     WHEN 'DET'                                                           
104900       IF SYST-IDSEKVNR = 1                                               
105000         IF IN-EKH-KVANTAL > 0                                            
105100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
105200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
105300           COMPUTE R3-LINE-AMOUNT-LC =                                    
105400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
105500           IF IN-EKH-KDVALISO = 'USD'                                     
105600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
105700           END-IF                                                         
105800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
105900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
106000           MOVE SPACE               TO WS-ALLOCATE-REF                    
106100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
106200           PERFORM S02-WRITE-W57051A                                      
106300         END-IF                                                           
106400       END-IF                                                             
106500                                                                          
106600       IF SYST-IDSEKVNR = 2                                               
106700         IF IN-EKH-KVANTAL < 0                                            
106800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
106900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
107000           COMPUTE R3-LINE-AMOUNT-LC =                                    
107100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
107200           IF IN-EKH-KDVALISO = 'USD'                                     
107300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
107400           END-IF                                                         
107500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
107600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
107700           MOVE SPACE               TO WS-ALLOCATE-REF                    
107800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
107900           PERFORM S02-WRITE-W57051A                                      
108000         END-IF                                                           
108100       END-IF                                                             
108200                                                                          
108300       IF SYST-IDSEKVNR = 3                                               
108400         IF IN-EKH-KVANTAL < 0                                            
108500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
108600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
108700           COMPUTE R3-LINE-AMOUNT-LC =                                    
108800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
108900           IF IN-EKH-KDVALISO = 'USD'                                     
109000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
109100           END-IF                                                         
109200           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
109300           PERFORM S02-WRITE-W57051A                                      
109400         END-IF                                                           
109500       END-IF                                                             
109600                                                                          
109700       IF SYST-IDSEKVNR = 4                                               
109800         IF IN-EKH-KVANTAL > 0                                            
109900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
110000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
110100           COMPUTE R3-LINE-AMOUNT-LC =                                    
110200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
110300           IF IN-EKH-KDVALISO = 'USD'                                     
110400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
110500           END-IF                                                         
110600           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
110700           PERFORM S02-WRITE-W57051A                                      
110800         END-IF                                                           
110900       END-IF                                                             
111000                                                                          
111100     END-EVALUATE                                                         
111200     .                                                                    
111300     EJECT                                                                
111400                                                                          
111500 CEBD-SUB-EVENT-102-120 SECTION.                                          
111600     EVALUATE IN-EKH-KDEKNIVA                                             
111700     WHEN 'DET'                                                           
111800       IF SYST-IDSEKVNR = 1                                               
111900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
112000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
112100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
112200          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1           
112300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
112400         PERFORM S03-WRITE-W57052                                         
112500       END-IF                                                             
112600                                                                          
112700     WHEN 'FÖRS'                                                          
112800     WHEN 'FRAKT'                                                         
112900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
113000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
113100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
113200               IN-EKH-SUBEL / WS-PRKURS-USD * -1                          
113300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
113400       PERFORM S04-WRITE-W57053A                                          
113500                                                                          
113600     WHEN 'EMB'                                                           
113700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
113800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
113900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
114000               IN-EKH-SUBEL / WS-PRKURS-USD * -1                          
114100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
114200       PERFORM S04-WRITE-W57053A                                          
114300                                                                          
114400     WHEN 'DDI'                                                           
114500       IF IN-EKH-SUBEL > ZERO                                             
114600         IF SYST-IDSEKVNR = 1                                             
114700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
114800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
114900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
115000                   IN-EKH-SUBEL                                           
115100           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
115200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
115300           PERFORM S04-WRITE-W57053A                                      
115400         END-IF                                                           
115500       ELSE                                                               
115600         IF SYST-IDSEKVNR = 2                                             
115700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
115800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
115900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
116000                   IN-EKH-SUBEL                                           
116100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
116200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
116300           PERFORM S04-WRITE-W57053A                                      
116400         END-IF                                                           
116500       END-IF                                                             
116600     END-EVALUATE                                                         
116700     .                                                                    
116800     EJECT                                                                
116900                                                                          
117000 CEBD-SUB-EVENT-102-121 SECTION.                                          
117100     EVALUATE IN-EKH-KDEKNIVA                                             
117200     WHEN 'DET'                                                           
117300       IF SYST-IDSEKVNR = 1                                               
117400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
117500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
117600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
117700             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-USD3           
117800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
117900         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-1                   
118000         MOVE SPACE               TO WS-ALLOCATE-DC                       
118100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
118200         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
118300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
118400         PERFORM S03-WRITE-W57052                                         
118500       END-IF                                                             
118600                                                                          
118700       IF SYST-IDSEKVNR = 2                                               
118800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
118900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
119000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
119100            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-USD3))         
119200            + (IN-EKH-KVANTAL *                                           
119300            (IN-EKH-PRARTNTO / WS-PRKURS-USD3) * WS-MARKUP)               
119400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
119500         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-2                   
119600         MOVE SPACE               TO WS-ALLOCATE-DC                       
119700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
119800         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
119900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
120000         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
120100         PERFORM S03-WRITE-W57052                                         
120200       END-IF                                                             
120300                                                                          
120400       IF SYST-IDSEKVNR = 3                                               
120500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
120600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
120700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
120800            WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
120900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
121000         MOVE SPACE               TO WS-ALLOCATE-DC                       
121100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
121200         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
121300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
121400         PERFORM S03-WRITE-W57052                                         
121500       END-IF                                                             
121600                                                                          
121700     WHEN 'FÖRS'                                                          
121800     WHEN 'FRAKT'                                                         
121900       IF SYST-IDSEKVNR = 1                                               
122000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
122100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
122200         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
122300                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
122400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
122500         MOVE SPACE               TO WS-ALLOCATE-DC                       
122600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
122700         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
122800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
122900         PERFORM S04-WRITE-W57053A                                        
123000       END-IF                                                             
123100                                                                          
123200       IF SYST-IDSEKVNR = 2                                               
123300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
123400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
123500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
123600                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
123700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
123800         MOVE SPACE               TO WS-ALLOCATE-DC                       
123900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
124000         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
124100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
124200         PERFORM S04-WRITE-W57053A                                        
124300       END-IF                                                             
124400                                                                          
124500     WHEN 'EMB'                                                           
124600       IF SYST-IDSEKVNR = 1                                               
124700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
124800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
124900         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
125000                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
125100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
125200         MOVE SPACE               TO WS-ALLOCATE-DC                       
125300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
125400         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
125500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
125600         PERFORM S04-WRITE-W57053A                                        
125700       END-IF                                                             
125800                                                                          
125900       IF SYST-IDSEKVNR = 2                                               
126000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
126100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
126200         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
126300                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
126400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
126500         MOVE SPACE               TO WS-ALLOCATE-DC                       
126600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
126700         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
126800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
126900         PERFORM S04-WRITE-W57053A                                        
127000       END-IF                                                             
127100     END-EVALUATE                                                         
127200                                                                          
127300     .                                                                    
127400     EJECT                                                                
127500                                                                          
127600 CEBD-SUB-EVENT-102-122 SECTION.                                          
127700     EVALUATE IN-EKH-KDEKNIVA                                             
127800     WHEN 'DET'                                                           
127900       IF IN-EKH-KVANTAL > 0                                              
128000         IF SYST-IDSEKVNR = 1                                             
128100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
128200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
128300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
128400            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-USD3))         
128500            + (IN-EKH-KVANTAL *                                           
128600            (IN-EKH-PRARTNTO / WS-PRKURS-USD3) * WS-MARKUP)               
128700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
128800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
128900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
129000           MOVE SPACE               TO WS-ALLOCATE-REF                    
129100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
129200           PERFORM S02-WRITE-W57051A                                      
129300         END-IF                                                           
129400                                                                          
129500         IF SYST-IDSEKVNR = 4                                             
129600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
129700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
129800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
129900            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-USD3))         
130000            +(IN-EKH-KVANTAL *                                            
130100            (IN-EKH-PRARTNTO / WS-PRKURS-USD3) * WS-MARKUP)               
130200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
130300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
130400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
130500           MOVE SPACE               TO WS-ALLOCATE-REF                    
130600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
130700           MOVE SPACE               TO R3-LINE-COST-CENTER                
130800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
130900           PERFORM S02-WRITE-W57051A                                      
131000         END-IF                                                           
131100       END-IF                                                             
131200                                                                          
131300                                                                          
131400       IF IN-EKH-KVANTAL < 0                                              
131500         IF SYST-IDSEKVNR = 2                                             
131600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
131700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
131800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
131900            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-USD3))         
132000            +(IN-EKH-KVANTAL *                                            
132100            (IN-EKH-PRARTNTO / WS-PRKURS-USD3) * WS-MARKUP)               
132200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
132300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
132400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
132500           MOVE SPACE               TO WS-ALLOCATE-REF                    
132600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
132700           MOVE SPACE             TO R3-LINE-COST-CENTER                  
132800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
132900           PERFORM S02-WRITE-W57051A                                      
133000         END-IF                                                           
133100                                                                          
133200         IF SYST-IDSEKVNR = 3                                             
133300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
133400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
133500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
133600            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-USD3))         
133700            +(IN-EKH-KVANTAL *                                            
133800            (IN-EKH-PRARTNTO / WS-PRKURS-USD3) * WS-MARKUP)               
133900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
134000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
134100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
134200           MOVE SPACE               TO WS-ALLOCATE-REF                    
134300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
134400           PERFORM S02-WRITE-W57051A                                      
134500         END-IF                                                           
134600       END-IF                                                             
134700                                                                          
134800     END-EVALUATE                                                         
134900     .                                                                    
135000     EJECT                                                                
135100                                                                          
135200 CEBD-SUB-EVENT-102-123 SECTION.                                          
135300     EVALUATE IN-EKH-KDEKNIVA                                             
135400     WHEN 'DET'                                                           
135500       IF SYST-IDSEKVNR = 1                                               
135600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
135700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
135800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
135900              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
136000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
136100         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
136200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
136300         MOVE SPACE               TO WS-ALLOCATE-REF                      
136400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
136500         PERFORM S02-WRITE-W57051A                                        
136600       END-IF                                                             
136700                                                                          
136800       IF SYST-IDSEKVNR = 2                                               
136900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
137000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
137100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
137200             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
137300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
137400         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
137500         PERFORM S02-WRITE-W57051A                                        
137600       END-IF                                                             
137700     END-EVALUATE                                                         
137800     .                                                                    
137900     EJECT                                                                
138000                                                                          
138100 CEBD-SUB-EVENT-102-124 SECTION.                                          
138200     EVALUATE IN-EKH-KDEKNIVA                                             
138300     WHEN 'DET'                                                           
138400       IF SYST-IDSEKVNR = 1                                               
138500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
138600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
138700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
138800         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1            
138900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
139000         MOVE SPACE               TO WS-ALLOCATE-DC                       
139100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
139200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
139300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
139400         PERFORM S03-WRITE-W57052                                         
139500       END-IF                                                             
139600                                                                          
139700       IF SYST-IDSEKVNR = 2                                               
139800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
139900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
140000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
140100         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1            
140200         * WS-MARKUP                                                      
140300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
140400         MOVE SPACE               TO WS-ALLOCATE-DC                       
140500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
140600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
140700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
140800         PERFORM S04-WRITE-W57053A                                        
140900       END-IF                                                             
141000                                                                          
141100       IF SYST-IDSEKVNR = 3                                               
141200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
141300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
141400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
141500         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1            
141600         * WS-MARKUP                                                      
141700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
141800         MOVE SPACE               TO WS-ALLOCATE-DC                       
141900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
142000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
142100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
142200         PERFORM S04-WRITE-W57053A                                        
142300       END-IF                                                             
142400                                                                          
142500     WHEN 'FÖRS'                                                          
142600     WHEN 'FRAKT'                                                         
142700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
142800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
142900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
143000               IN-EKH-SUBEL * -1                                          
143100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
143200       MOVE SPACE               TO WS-ALLOCATE-DC                         
143300       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
143400       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
143500       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
143600       PERFORM S04-WRITE-W57053A                                          
143700                                                                          
143800     WHEN 'EMB'                                                           
143900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
144000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
144100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
144200               (IN-EKH-SUBEL / WS-PRKURS-USD) * -1                        
144300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
144400       MOVE SPACE               TO WS-ALLOCATE-DC                         
144500       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
144600       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
144700       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
144800       PERFORM S04-WRITE-W57053A                                          
144900                                                                          
145000     WHEN 'DDI'                                                           
145100       IF IN-EKH-SUBEL > ZERO                                             
145200         IF SYST-IDSEKVNR = 1                                             
145300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
145400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
145500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
145600                   IN-EKH-SUBEL                                           
145700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
145800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
145900           MOVE SPACE               TO WS-ALLOCATE-DC                     
146000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
146100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
146200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
146300           PERFORM S04-WRITE-W57053A                                      
146400         END-IF                                                           
146500       ELSE                                                               
146600         IF SYST-IDSEKVNR = 2                                             
146700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
146800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
146900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
147000                   IN-EKH-SUBEL                                           
147100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
147200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
147300           MOVE SPACE               TO WS-ALLOCATE-DC                     
147400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
147500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
147600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
147700           PERFORM S04-WRITE-W57053A                                      
147800         END-IF                                                           
147900       END-IF                                                             
148000     END-EVALUATE                                                         
148100     .                                                                    
148200     EJECT                                                                
148300                                                                          
148400 CEBD-SUB-EVENT-102-125 SECTION.                                          
148500     EVALUATE IN-EKH-KDEKNIVA                                             
148600     WHEN 'DET'                                                           
148700       IF SYST-IDSEKVNR = 1                                               
148800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
148900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
149000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
149100         (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-USD)) +           
149200         (IN-EKH-KVANTAL *                                                
149300         (IN-EKH-PRARTNTO / WS-PRKURS-USD) * WS-MARKUP)                   
149400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
149500         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
149600         PERFORM S03-WRITE-W57052                                         
149700       END-IF                                                             
149800                                                                          
149900       IF SYST-IDSEKVNR = 2                                               
150000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
150100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
150200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
150300            (IN-EKH-KVANTAL *                                             
150400            (IN-EKH-PRARTNTO / WS-PRKURS-USD) * WS-MARKUP)                
150500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
150600         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
150700         PERFORM S03-WRITE-W57052                                         
150800       END-IF                                                             
150900                                                                          
151000     WHEN 'FÖRS'                                                          
151100     WHEN 'FRAKT'                                                         
151200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
151300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
151400       MOVE SPACE             TO R3-LINE-COST-CENTER                      
151500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
151600          IN-EKH-SUBEL / WS-PRKURS-USD * -1                               
151700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
151800       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
151900       PERFORM S04-WRITE-W57053A                                          
152000                                                                          
152100     WHEN 'EMB'                                                           
152200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
152300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
152400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
152500          IN-EKH-SUBEL / WS-PRKURS-USD * -1                               
152600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
152700       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
152800       PERFORM S04-WRITE-W57053A                                          
152900                                                                          
153000     WHEN 'DDI'                                                           
153100       IF SPAR-SUMMA-102-125 < ZERO                                       
153200         IF SYST-IDSEKVNR = 1                                             
153300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
153400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
153500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
153600                   SPAR-SUMMA-102-125                                     
153700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
153800           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
153900           PERFORM S04-WRITE-W57053A                                      
154000         END-IF                                                           
154100       END-IF                                                             
154200       IF SPAR-SUMMA-102-125 > ZERO                                       
154300         IF SYST-IDSEKVNR = 2                                             
154400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
154500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
154600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
154700                   SPAR-SUMMA-102-125                                     
154800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
154900           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
155000           PERFORM S04-WRITE-W57053A                                      
155100         END-IF                                                           
155200       END-IF                                                             
155300                                                                          
155400     END-EVALUATE                                                         
155500     .                                                                    
155600     EJECT                                                                
155700                                                                          
155800 CEBE-SUB-EVENT-102-130 SECTION.                                          
155900     EVALUATE IN-EKH-KDEKNIVA                                             
156000     WHEN 'DET'                                                           
156100       IF SYST-IDSEKVNR = 1                                               
156200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
156300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
156400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
156500         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1            
156600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
156700         PERFORM S03-WRITE-W57052                                         
156800       END-IF                                                             
156900                                                                          
157000     WHEN 'FÖRS'                                                          
157100     WHEN 'FRAKT'                                                         
157200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
157300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
157400       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
157500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
157600               IN-EKH-SUBEL / WS-PRKURS-USD * -1                          
157700       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
157800       PERFORM S04-WRITE-W57053A                                          
157900                                                                          
158000     WHEN 'EMB'                                                           
158100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
158200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
158300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
158400               IN-EKH-SUBEL / WS-PRKURS-USD * -1                          
158500       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
158600       PERFORM S04-WRITE-W57053A                                          
158700                                                                          
158800     WHEN 'DDI'                                                           
158900       IF IN-EKH-SUBEL > ZERO                                             
159000         IF SYST-IDSEKVNR = 1                                             
159100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
159200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
159300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
159400                   IN-EKH-SUBEL                                           
159500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
159600           PERFORM S04-WRITE-W57053A                                      
159700         END-IF                                                           
159800       ELSE                                                               
159900         IF SYST-IDSEKVNR = 2                                             
160000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
160100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
160200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
160300                   IN-EKH-SUBEL                                           
160400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
160500           PERFORM S04-WRITE-W57053A                                      
160600         END-IF                                                           
160700       END-IF                                                             
160800                                                                          
160900     END-EVALUATE                                                         
161000     .                                                                    
161100     EJECT                                                                
161200                                                                          
161300 CEBE-SUB-EVENT-102-131 SECTION.                                          
161400     EVALUATE IN-EKH-KDEKNIVA                                             
161500     WHEN 'DET'                                                           
161600       IF SYST-IDSEKVNR = 1                                               
161700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
161800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
161900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
162000            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3)           
162100         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-1                   
162200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
162300         PERFORM S03-WRITE-W57052                                         
162400       END-IF                                                             
162500                                                                          
162600       IF SYST-IDSEKVNR = 2                                               
162700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
162800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
162900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
163000            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3) +         
163100            (IN-EKH-KVANTAL *                                             
163200             IN-EKH-PRARTNTO / WS-PRKURS-USD3 * WS-MARKUP)                
163300         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-2                   
163400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
163500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
163600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
163700         MOVE SPACE               TO WS-ALLOCATE-REF                      
163800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
163900         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
164000         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
164100         PERFORM S03-WRITE-W57052                                         
164200       END-IF                                                             
164300                                                                          
164400       IF SYST-IDSEKVNR = 3                                               
164500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
164600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
164700         COMPUTE R3-LINE-AMOUNT-LC =                                      
164800                 WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1              
164900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
165000         PERFORM S03-WRITE-W57052                                         
165100       END-IF                                                             
165200                                                                          
165300     WHEN 'FÖRS'                                                          
165400     WHEN 'FRAKT'                                                         
165500       IF SYST-IDSEKVNR = 1                                               
165600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
165700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
165800         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
165900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
166000                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
166100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
166200         PERFORM S04-WRITE-W57053A                                        
166300       END-IF                                                             
166400                                                                          
166500       IF SYST-IDSEKVNR = 2                                               
166600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
166700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
166800         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
166900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
167000                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
167100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
167200         PERFORM S04-WRITE-W57053A                                        
167300       END-IF                                                             
167400                                                                          
167500     WHEN 'EMB'                                                           
167600       IF SYST-IDSEKVNR = 1                                               
167700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
167800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
167900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
168000                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
168100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
168200         PERFORM S04-WRITE-W57053A                                        
168300       END-IF                                                             
168400                                                                          
168500       IF SYST-IDSEKVNR = 2                                               
168600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
168700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
168800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
168900                 IN-EKH-SUBEL / WS-PRKURS-USD3                            
169000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
169100         PERFORM S04-WRITE-W57053A                                        
169200       END-IF                                                             
169300     END-EVALUATE                                                         
169400     .                                                                    
169500     EJECT                                                                
169600                                                                          
169700 CEBE-SUB-EVENT-102-132 SECTION.                                          
169800     EVALUATE IN-EKH-KDEKNIVA                                             
169900     WHEN 'DET'                                                           
170000       IF SYST-IDSEKVNR = 1                                               
170100         IF IN-EKH-KVANTAL > 0                                            
170200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
170300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
170400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
170500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3) +         
170600            (IN-EKH-KVANTAL *                                             
170700             IN-EKH-PRARTNTO / WS-PRKURS-USD3 * WS-MARKUP)                
170800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
170900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
171000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
171100           MOVE SPACE               TO WS-ALLOCATE-REF                    
171200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
171300           PERFORM S02-WRITE-W57051A                                      
171400         END-IF                                                           
171500       END-IF                                                             
171600                                                                          
171700       IF SYST-IDSEKVNR = 2                                               
171800         IF IN-EKH-KVANTAL < 0                                            
171900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
172000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
172100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
172200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3) +         
172300            (IN-EKH-KVANTAL *                                             
172400             IN-EKH-PRARTNTO / WS-PRKURS-USD3 * WS-MARKUP)                
172500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
172600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
172700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
172800           MOVE SPACE               TO WS-ALLOCATE-REF                    
172900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
173000           PERFORM S02-WRITE-W57051A                                      
173100         END-IF                                                           
173200       END-IF                                                             
173300                                                                          
173400       IF SYST-IDSEKVNR = 3                                               
173500         IF IN-EKH-KVANTAL < 0                                            
173600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
173700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
173800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
173900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3) +         
174000            (IN-EKH-KVANTAL *                                             
174100             IN-EKH-PRARTNTO / WS-PRKURS-USD3 * WS-MARKUP)                
174200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
174300           PERFORM S02-WRITE-W57051A                                      
174400         END-IF                                                           
174500       END-IF                                                             
174600                                                                          
174700       IF SYST-IDSEKVNR = 4                                               
174800         IF IN-EKH-KVANTAL > 0                                            
174900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
175000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
175100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
175200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3) +         
175300            (IN-EKH-KVANTAL *                                             
175400             IN-EKH-PRARTNTO / WS-PRKURS-USD3 * WS-MARKUP)                
175500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
175600           PERFORM S02-WRITE-W57051A                                      
175700         END-IF                                                           
175800       END-IF                                                             
175900     END-EVALUATE                                                         
176000     .                                                                    
176100     EJECT                                                                
176200                                                                          
176300 CEBE-SUB-EVENT-102-134 SECTION.                                          
176400     EVALUATE IN-EKH-KDEKNIVA                                             
176500     WHEN 'DET'                                                           
176600       IF SYST-IDSEKVNR = 1                                               
176700         IF IN-EKH-KVANTAL > 0                                            
176800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
176900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
177000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
177100           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1          
177200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
177300           MOVE SPACE               TO WS-ALLOCATE-DC                     
177400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
177500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
177600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
177700           PERFORM S03-WRITE-W57052                                       
177800         END-IF                                                           
177900       END-IF                                                             
178000                                                                          
178100       IF SYST-IDSEKVNR = 2                                               
178200         IF IN-EKH-KVANTAL < 0                                            
178300           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
178400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
178500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
178600           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1          
178700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
178800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
178900           MOVE SPACE               TO WS-ALLOCATE-DC                     
179000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
179100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
179200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
179300           PERFORM S03-WRITE-W57052                                       
179400         END-IF                                                           
179500       END-IF                                                             
179600                                                                          
179700     WHEN 'EMB'                                                           
179800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
179900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
180000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
180100               IN-EKH-SUBEL / WS-PRKURS-USD  * -1                         
180200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
180300       MOVE SPACE               TO WS-ALLOCATE-DC                         
180400       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
180500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
180600       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
180700       PERFORM S04-WRITE-W57053A                                          
180800                                                                          
180900     WHEN 'DDI'                                                           
181000       IF IN-EKH-SUBEL > ZERO                                             
181100         IF SYST-IDSEKVNR = 1                                             
181200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
181300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
181400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
181500                   IN-EKH-SUBEL                                           
181600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
181700           MOVE SPACE               TO WS-ALLOCATE-DC                     
181800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
181900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
182000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
182100           PERFORM S04-WRITE-W57053A                                      
182200         END-IF                                                           
182300       ELSE                                                               
182400         IF SYST-IDSEKVNR = 2                                             
182500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
182600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
182700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
182800                   IN-EKH-SUBEL                                           
182900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
183000           MOVE SPACE               TO WS-ALLOCATE-DC                     
183100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
183200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
183300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
183400           PERFORM S04-WRITE-W57053A                                      
183500         END-IF                                                           
183600       END-IF                                                             
183700                                                                          
183800     END-EVALUATE                                                         
183900     .                                                                    
184000     EJECT                                                                
184100                                                                          
184200 CEC-MAIN-EVENT-103 SECTION.                                              
184300     EVALUATE IN-EKH-KDEKSHT                                              
184400     WHEN '102'                                                           
184500          PERFORM CECB-SUB-EVENT-103-102                                  
184600     WHEN '106'                                                           
184700          PERFORM CECB-SUB-EVENT-103-106                                  
184800     WHEN '107'                                                           
184900          PERFORM CECB-SUB-EVENT-103-107                                  
185000     END-EVALUATE                                                         
185100     .                                                                    
185200     EJECT                                                                
185300                                                                          
185400 CECB-SUB-EVENT-103-102 SECTION.                                          
185500     EVALUATE IN-EKH-KDEKNIVA                                             
185600     WHEN 'DET'                                                           
185700       IF SYST-IDSEKVNR = 1                                               
185800         IF IN-EKH-KVANTAL < 0                                            
185900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
186000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
186100           COMPUTE R3-LINE-AMOUNT-LC =                                    
186200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
186300           IF IN-EKH-KDVALISO = 'USD'                                     
186400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
186500           END-IF                                                         
186600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
186700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
186800           MOVE SPACE               TO WS-ALLOCATE-REF                    
186900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
187000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
187100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
187200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
187300           PERFORM S04-WRITE-W57053A                                      
187400         END-IF                                                           
187500       END-IF                                                             
187600                                                                          
187700       IF SYST-IDSEKVNR = 2                                               
187800         IF IN-EKH-KVANTAL > 0                                            
187900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
188000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
188100           COMPUTE R3-LINE-AMOUNT-LC =                                    
188200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
188300           IF IN-EKH-KDVALISO = 'USD'                                     
188400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
188500           END-IF                                                         
188600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
188700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
188800           MOVE SPACE               TO WS-ALLOCATE-REF                    
188900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
189000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
189100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
189200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
189300           PERFORM S04-WRITE-W57053A                                      
189400         END-IF                                                           
189500       END-IF                                                             
189600                                                                          
189700     WHEN 'KALK'                                                          
189800       IF SYST-IDSEKVNR = 1                                               
189900         IF IN-EKH-SUBEL > 0                                              
190000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
190100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
190200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
190300           IF IN-EKH-KDVALISO = 'USD'                                     
190400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
190500           END-IF                                                         
190600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
190700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
190800           MOVE SPACE               TO WS-ALLOCATE-REF                    
190900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
191000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
191100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
191200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
191300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
191400           PERFORM S04-WRITE-W57053A                                      
191500         END-IF                                                           
191600       END-IF                                                             
191700                                                                          
191800       IF SYST-IDSEKVNR = 2                                               
191900         IF IN-EKH-SUBEL < 0                                              
192000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
192100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
192200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
192300           IF IN-EKH-KDVALISO = 'USD'                                     
192400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
192500           END-IF                                                         
192600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
192700           MOVE SPACE               TO WS-ALLOCATE-DC                     
192800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
192900           MOVE SPACE               TO WS-ALLOCATE-REF                    
193000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
193100           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
193200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
193300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
193400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
193500           PERFORM S04-WRITE-W57053A                                      
193600         END-IF                                                           
193700       END-IF                                                             
193800                                                                          
193900       IF SYST-IDSEKVNR = 3                                               
194000         IF IN-EKH-SUBEL > 0                                              
194100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
194200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
194300           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
194400           IF IN-EKH-KDVALISO = 'USD'                                     
194500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
194600           END-IF                                                         
194700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
194800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
194900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
195000           PERFORM S04-WRITE-W57053A                                      
195100         END-IF                                                           
195200       END-IF                                                             
195300                                                                          
195400       IF SYST-IDSEKVNR = 4                                               
195500         IF IN-EKH-SUBEL < 0                                              
195600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
195700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
195800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
195900           IF IN-EKH-KDVALISO = 'USD'                                     
196000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
196100           END-IF                                                         
196200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
196300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
196400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
196500           PERFORM S04-WRITE-W57053A                                      
196600         END-IF                                                           
196700       END-IF                                                             
196800                                                                          
196900     WHEN 'DDI'                                                           
197000       IF IN-EKH-SUBEL > ZERO                                             
197100         IF SYST-IDSEKVNR = 1                                             
197200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
197300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
197400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
197500                   IN-EKH-SUBEL                                           
197600           MOVE ZEROES              TO R3-LINE-AMOUNT                     
197700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
197800           MOVE SPACE               TO WS-ALLOCATE-DC                     
197900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
198000           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
198100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
198200           PERFORM S04-WRITE-W57053A                                      
198300         END-IF                                                           
198400       ELSE                                                               
198500         IF SYST-IDSEKVNR = 2                                             
198600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
198700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
198800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
198900                   IN-EKH-SUBEL                                           
199000           MOVE ZEROES              TO R3-LINE-AMOUNT                     
199100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
199200           MOVE SPACE               TO WS-ALLOCATE-DC                     
199300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
199400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
199500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
199600           PERFORM S04-WRITE-W57053A                                      
199700         END-IF                                                           
199800       END-IF                                                             
199900     END-EVALUATE                                                         
200000     .                                                                    
200100     EJECT                                                                
200200                                                                          
200300 CECB-SUB-EVENT-103-106 SECTION.                                          
200400     EVALUATE IN-EKH-KDEKNIVA                                             
200500     WHEN 'DET'                                                           
200600       IF SYST-IDSEKVNR = 1                                               
200700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
200800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
200900         COMPUTE R3-LINE-AMOUNT-LC =                                      
201000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
201100         IF IN-EKH-KDVALISO = 'USD'                                       
201200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
201300         END-IF                                                           
201400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
201500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
201600         MOVE SPACE               TO WS-ALLOCATE-REF                      
201700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
201800         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
201900         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
202000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
202100         PERFORM S04-WRITE-W57053A                                        
202200       END-IF                                                             
202300                                                                          
202400                                                                          
202500     WHEN 'KALK'                                                          
202600       IF SYST-IDSEKVNR = 1                                               
202700         IF IN-EKH-SUBEL > 0                                              
202800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
202900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
203000           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
203100           IF IN-EKH-KDVALISO = 'USD'                                     
203200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
203300           END-IF                                                         
203400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
203500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
203600           MOVE SPACE               TO WS-ALLOCATE-REF                    
203700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
203800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
203900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
204000           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
204100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
204200           PERFORM S04-WRITE-W57053A                                      
204300         END-IF                                                           
204400       END-IF                                                             
204500                                                                          
204600       IF SYST-IDSEKVNR = 2                                               
204700         IF IN-EKH-SUBEL < 0                                              
204800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
204900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
205000           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
205100           IF IN-EKH-KDVALISO = 'USD'                                     
205200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
205300           END-IF                                                         
205400           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
205500           MOVE SPACE               TO WS-ALLOCATE-DC                     
205600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
205700           MOVE SPACE               TO WS-ALLOCATE-REF                    
205800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
205900           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
206000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
206100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
206200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
206300           PERFORM S04-WRITE-W57053A                                      
206400         END-IF                                                           
206500       END-IF                                                             
206600                                                                          
206700       IF SYST-IDSEKVNR = 3                                               
206800         IF IN-EKH-SUBEL > 0                                              
206900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
207000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
207100           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
207200           IF IN-EKH-KDVALISO = 'USD'                                     
207300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
207400           END-IF                                                         
207500           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
207600           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
207700           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
207800           PERFORM S04-WRITE-W57053A                                      
207900         END-IF                                                           
208000       END-IF                                                             
208100                                                                          
208200       IF SYST-IDSEKVNR = 4                                               
208300         IF IN-EKH-SUBEL < 0                                              
208400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
208500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
208600           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
208700           IF IN-EKH-KDVALISO = 'USD'                                     
208800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
208900           END-IF                                                         
209000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
209100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
209200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
209300           PERFORM S04-WRITE-W57053A                                      
209400         END-IF                                                           
209500       END-IF                                                             
209600                                                                          
209700     END-EVALUATE                                                         
209800     .                                                                    
209900     EJECT                                                                
210000                                                                          
210100 CECB-SUB-EVENT-103-107 SECTION.                                          
210200     EVALUATE IN-EKH-KDEKNIVA                                             
210300     WHEN 'DET'                                                           
210400       IF SYST-IDSEKVNR = 1                                               
210500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
210600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
210700         COMPUTE R3-LINE-AMOUNT-LC =                                      
210800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
210900         IF IN-EKH-KDVALISO = 'USD'                                       
211000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
211100         END-IF                                                           
211200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
211300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
211400         MOVE SPACE               TO WS-ALLOCATE-REF                      
211500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
211600         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
211700         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
211800         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
211900         PERFORM S04-WRITE-W57053A                                        
212000       END-IF                                                             
212100                                                                          
212200     WHEN 'KALK'                                                          
212300       IF SYST-IDSEKVNR = 1                                               
212400         IF IN-EKH-SUBEL > 0                                              
212500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
212600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
212700           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
212800           IF IN-EKH-KDVALISO = 'USD'                                     
212900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
213000           END-IF                                                         
213100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
213200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
213300           MOVE SPACE               TO WS-ALLOCATE-REF                    
213400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
213500           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
213600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
213700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
213800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
213900           PERFORM S04-WRITE-W57053A                                      
214000         END-IF                                                           
214100       END-IF                                                             
214200                                                                          
214300       IF SYST-IDSEKVNR = 2                                               
214400         IF IN-EKH-SUBEL < 0                                              
214500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
214600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
214700           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
214800           IF IN-EKH-KDVALISO = 'USD'                                     
214900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
215000           END-IF                                                         
215100           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
215200           MOVE SPACE               TO WS-ALLOCATE-DC                     
215300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
215400           MOVE SPACE               TO WS-ALLOCATE-REF                    
215500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
215600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
215700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
215800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
215900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
216000           PERFORM S04-WRITE-W57053A                                      
216100         END-IF                                                           
216200       END-IF                                                             
216300                                                                          
216400       IF SYST-IDSEKVNR = 3                                               
216500         IF IN-EKH-SUBEL > 0                                              
216600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
216700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
216800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
216900           IF IN-EKH-KDVALISO = 'USD'                                     
217000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
217100           END-IF                                                         
217200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
217300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
217400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
217500           PERFORM S04-WRITE-W57053A                                      
217600         END-IF                                                           
217700       END-IF                                                             
217800                                                                          
217900       IF SYST-IDSEKVNR = 4                                               
218000         IF IN-EKH-SUBEL < 0                                              
218100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
218200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
218300           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
218400           IF IN-EKH-KDVALISO = 'USD'                                     
218500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
218600           END-IF                                                         
218700*          MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
218800*          PERFORM S11-ANALYSIS                                           
218900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
219000           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
219100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
219200           PERFORM S04-WRITE-W57053A                                      
219300         END-IF                                                           
219400       END-IF                                                             
219500                                                                          
219600     END-EVALUATE                                                         
219700     .                                                                    
219800     EJECT                                                                
219900                                                                          
220000 CED-MAIN-EVENT-201 SECTION.                                              
220100     EVALUATE IN-EKH-KDEKSHT                                              
220200     WHEN '201'                                                           
220300          PERFORM CEDA-SUB-EVENT-201-201                                  
220400     END-EVALUATE                                                         
220500     .                                                                    
220600     EJECT                                                                
220700                                                                          
220800 CEDA-SUB-EVENT-201-201 SECTION.                                          
220900     EVALUATE IN-EKH-KDEKNIVA                                             
221000     WHEN 'DET'                                                           
221100       IF SYST-IDSEKVNR = 1                                               
221200         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
221300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
221400         COMPUTE R3-LINE-AMOUNT-LC =                                      
221500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
221600         IF IN-EKH-KDVALISO = 'USD'                                       
221700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
221800         END-IF                                                           
221900         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
222000*        MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
222100         MOVE SPACES              TO R3-LINE-PA-CUSTOMER                  
222200         PERFORM S03-WRITE-W57052                                         
222300       END-IF                                                             
222400                                                                          
222500       IF SYST-IDSEKVNR = 2                                               
222600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
222700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
222800         COMPUTE R3-LINE-AMOUNT-LC =                                      
222900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
223000         IF IN-EKH-KDVALISO = 'USD'                                       
223100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
223200         END-IF                                                           
223300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
223400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
223500         MOVE SPACE               TO WS-ALLOCATE-REF                      
223600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
223700*        MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
223800         MOVE SPACES              TO R3-LINE-PA-CUSTOMER                  
223900         PERFORM S03-WRITE-W57052                                         
224000       END-IF                                                             
224100     END-EVALUATE                                                         
224200     .                                                                    
224300     EJECT                                                                
224400                                                                          
224500 CEF-MAIN-EVENT-203 SECTION.                                              
224600     EVALUATE IN-EKH-KDEKSHT                                              
224700     WHEN '201'                                                           
224800          PERFORM CEFA-SUB-EVENT-203-201                                  
224900     END-EVALUATE                                                         
225000     .                                                                    
225100     EJECT                                                                
225200                                                                          
225300 CEFA-SUB-EVENT-203-201 SECTION.                                          
225400     EVALUATE IN-EKH-KDEKNIVA                                             
225500     WHEN 'DET'                                                           
225600       IF SYST-IDSEKVNR = 1                                               
225700         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
225800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
225900         COMPUTE R3-LINE-AMOUNT-LC =                                      
226000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
226100         IF IN-EKH-KDVALISO = 'USD'                                       
226200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
226300         END-IF                                                           
226400         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
226500*        MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
226600         MOVE SPACES              TO R3-LINE-PA-CUSTOMER                  
226700         PERFORM S03-WRITE-W57052                                         
226800       END-IF                                                             
226900                                                                          
227000       IF SYST-IDSEKVNR = 2                                               
227100         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
227200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
227300         COMPUTE R3-LINE-AMOUNT-LC =                                      
227400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
227500         IF IN-EKH-KDVALISO = 'USD'                                       
227600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
227700         END-IF                                                           
227800         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
227900         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
228000         MOVE SPACE             TO WS-ALLOCATE-REF                        
228100         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
228200*        MOVE 0000409441        TO R3-LINE-PA-CUSTOMER                    
228300         MOVE SPACES            TO R3-LINE-PA-CUSTOMER                    
228400         PERFORM S03-WRITE-W57052                                         
228500       END-IF                                                             
228600     END-EVALUATE                                                         
228700     .                                                                    
228800     EJECT                                                                
228900                                                                          
229000 CEG-MAIN-EVENT-204 SECTION.                                              
229100     EVALUATE IN-EKH-KDEKSHT                                              
229200     WHEN '201'                                                           
229300          PERFORM CEGA-SUB-EVENT-204-201                                  
229400     WHEN '301'                                                           
229500          PERFORM CEGB-SUB-EVENT-204-301                                  
229600     END-EVALUATE                                                         
229700     .                                                                    
229800     EJECT                                                                
229900                                                                          
230000 CEGA-SUB-EVENT-204-201 SECTION.                                          
230100     EVALUATE IN-EKH-KDEKNIVA                                             
230200     WHEN 'DET'                                                           
230300         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
230400* R-FAKTURA                                                               
230500       IF SYST-IDSEKVNR = 1                                               
230600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
230700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
230800         COMPUTE R3-LINE-AMOUNT-LC =                                      
230900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
231000         IF IN-EKH-KDVALISO = 'USD'                                       
231100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
231200         END-IF                                                           
231300*        MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
231400         MOVE SPACES              TO R3-LINE-PA-CUSTOMER                  
231500         MOVE SPACE               TO WS-ALLOCATE-DC                       
231600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
231700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
231800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
231900         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
232000         PERFORM S03-WRITE-W57052                                         
232100       END-IF                                                             
232200                                                                          
232300       IF SYST-IDSEKVNR = 2                                               
232400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
232500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
232600         COMPUTE R3-LINE-AMOUNT-LC =                                      
232700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
232800         IF IN-EKH-KDVALISO = 'USD'                                       
232900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
233000         END-IF                                                           
233100         MOVE SPACE               TO WS-ALLOCATE-DC                       
233200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
233300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
233400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
233500*        MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
233600         MOVE SPACES              TO R3-LINE-PA-CUSTOMER                  
233700         PERFORM S03-WRITE-W57052                                         
233800       END-IF                                                             
233900                                                                          
234000       IF SYST-IDSEKVNR = 3                                               
234100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
234200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
234300         COMPUTE R3-LINE-AMOUNT-LC =                                      
234400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
234500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
234600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
234700         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
234800         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
234900         MOVE SPACE             TO WS-ALLOCATE-REF                        
235000         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
235100         PERFORM S03-WRITE-W57052                                         
235200       END-IF                                                             
235300                                                                          
235400     END-EVALUATE                                                         
235500     .                                                                    
235600     EJECT                                                                
235700                                                                          
235800 CEGB-SUB-EVENT-204-301 SECTION.                                          
235900     EVALUATE IN-EKH-KDEKNIVA                                             
236000     WHEN 'DET'                                                           
236100* R-FAKTURA                                                               
236200       IF SYST-IDSEKVNR = 1                                               
236300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
236400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
236500         COMPUTE R3-LINE-AMOUNT-LC =                                      
236600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
236700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
236800         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
236900         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
237000         PERFORM S03-WRITE-W57052                                         
237100       END-IF                                                             
237200                                                                          
237300       IF SYST-IDSEKVNR = 2                                               
237400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
237500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
237600         COMPUTE R3-LINE-AMOUNT-LC =                                      
237700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
237800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
237900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
238000         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
238100         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
238200         MOVE SPACE             TO WS-ALLOCATE-REF                        
238300         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
238400         PERFORM S03-WRITE-W57052                                         
238500       END-IF                                                             
238600                                                                          
238700       IF SYST-IDSEKVNR = 3                                               
238800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
238900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
239000         COMPUTE R3-LINE-AMOUNT-LC =                                      
239100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
239200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
239300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
239400         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
239500         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
239600         MOVE SPACE             TO WS-ALLOCATE-REF                        
239700         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
239800         PERFORM S03-WRITE-W57052                                         
239900       END-IF                                                             
240000                                                                          
240100     WHEN 'FÖRS'                                                          
240200     WHEN 'FRAKT'                                                         
240300     WHEN 'LEG'                                                           
240400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
240500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
240600       MOVE SYST-IDKST          TO WS-RED-IDKST                           
240700       IF WS-RED-IDKST > SPACE                                            
240800         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
240900       ELSE                                                               
241000         MOVE SPACE             TO R3-LINE-COST-CENTER                    
241100       END-IF                                                             
241200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
241300               IN-EKH-SUBEL * -1                                          
241400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
241500       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
241600       PERFORM S04-WRITE-W57053A                                          
241700                                                                          
241800     WHEN 'EMB'                                                           
241900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
242000       MOVE SYST-IDKST          TO WS-RED-IDKST                           
242100       IF WS-RED-IDKST > SPACE                                            
242200         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
242300       ELSE                                                               
242400         MOVE SPACE             TO R3-LINE-COST-CENTER                    
242500       END-IF                                                             
242600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
242700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
242800               IN-EKH-SUBEL * -1                                          
242900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
243000       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
243100       PERFORM S04-WRITE-W57053A                                          
243200                                                                          
243300     END-EVALUATE                                                         
243400     .                                                                    
243500     EJECT                                                                
243600                                                                          
243700 CEI-MAIN-EVENT-302 SECTION.                                              
243800     EVALUATE IN-EKH-KDEKSHT                                              
243900     WHEN '301'                                                           
244000          PERFORM CEIA-SUB-EVENT-302-301                                  
244100     WHEN '302'                                                           
244200          PERFORM CEIB-SUB-EVENT-302-302                                  
244300     END-EVALUATE                                                         
244400     .                                                                    
244500     EJECT                                                                
244600                                                                          
244700 CEIA-SUB-EVENT-302-301 SECTION.                                          
244800     EVALUATE IN-EKH-KDEKNIVA                                             
244900     WHEN 'DET'                                                           
245000       IF SYST-IDSEKVNR = 1                                               
245100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
245200         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
245300         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
245400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
245500         COMPUTE R3-LINE-AMOUNT-LC =                                      
245600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
245700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
245800         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
245900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
246000         MOVE SPACE               TO WS-ALLOCATE-REF                      
246100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
246200         PERFORM S02-WRITE-W57051A                                        
246300       END-IF                                                             
246400                                                                          
246500       IF SYST-IDSEKVNR = 2                                               
246600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
246700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
246800         MOVE SPACE           TO R3-LINE-COST-CENTER                      
246900         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
247000         COMPUTE R3-LINE-AMOUNT-LC =                                      
247100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
247200         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
247300         MOVE SPACE               TO WS-LINE-TEXT                         
247400         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
247500         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
247600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
247700         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
247800         PERFORM S02-WRITE-W57051A                                        
247900       END-IF                                                             
248000     END-EVALUATE                                                         
248100     .                                                                    
248200     EJECT                                                                
248300                                                                          
248400 CEIB-SUB-EVENT-302-302 SECTION.                                          
248500     EVALUATE IN-EKH-KDEKNIVA                                             
248600     WHEN 'DET'                                                           
248700       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
248800         IF SYST-IDSEKVNR = 1                                             
248900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
249000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
249100           IF BET-KDTRADP(3:2) NOT = SPACE                                
249200             MOVE '1'             TO WS-ACCOUNT-4                         
249300           ELSE                                                           
249400             MOVE '3'             TO WS-ACCOUNT-4                         
249500           END-IF                                                         
249600           COMPUTE R3-LINE-AMOUNT-LC  =                                   
249700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
249800           IF IN-EKH-KDVALISO = 'USD'                                     
249900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
250000           END-IF                                                         
250100*          MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
250200           MOVE SPACES              TO R3-LINE-PA-CUSTOMER                
250300           PERFORM S02-WRITE-W57051A                                      
250400         END-IF                                                           
250500                                                                          
250600         IF SYST-IDSEKVNR = 4                                             
250700           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
250800           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
250900           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
251000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
251100           COMPUTE R3-LINE-AMOUNT-LC  =                                   
251200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
251300           IF IN-EKH-KDVALISO = 'USD'                                     
251400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
251500           END-IF                                                         
251600           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
251700           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
251800           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
251900           MOVE SPACE             TO WS-ALLOCATE-REF                      
252000           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
252100*          MOVE 0000409441        TO R3-LINE-PA-CUSTOMER                  
252200           MOVE SPACES            TO R3-LINE-PA-CUSTOMER                  
252300           PERFORM S02-WRITE-W57051A                                      
252400         END-IF                                                           
252500       ELSE                                                               
252600         IF SYST-IDSEKVNR = 2                                             
252700           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
252800           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
252900           IF BET-KDTRADP(3:2) NOT = SPACE                                
253000             MOVE '1'             TO WS-ACCOUNT-4                         
253100           ELSE                                                           
253200             MOVE '3'             TO WS-ACCOUNT-4                         
253300           END-IF                                                         
253400           COMPUTE R3-LINE-AMOUNT-LC  =                                   
253500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
253600           IF IN-EKH-KDVALISO = 'USD'                                     
253700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
253800           END-IF                                                         
253900*          MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
254000           MOVE SPACES              TO R3-LINE-PA-CUSTOMER                
254100           PERFORM S02-WRITE-W57051A                                      
254200         END-IF                                                           
254300                                                                          
254400         IF SYST-IDSEKVNR = 3                                             
254500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
254600           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
254700           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
254800           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
254900           COMPUTE R3-LINE-AMOUNT-LC  =                                   
255000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
255100           IF IN-EKH-KDVALISO = 'USD'                                     
255200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
255300           END-IF                                                         
255400           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
255500           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
255600           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
255700           MOVE SPACE             TO WS-ALLOCATE-REF                      
255800           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
255900*          MOVE 0000409441        TO R3-LINE-PA-CUSTOMER                  
256000           MOVE SPACES            TO R3-LINE-PA-CUSTOMER                  
256100           PERFORM S02-WRITE-W57051A                                      
256200         END-IF                                                           
256300       END-IF                                                             
256400     END-EVALUATE                                                         
256500     .                                                                    
256600     EJECT                                                                
256700                                                                          
256800 CEJ-MAIN-EVENT-303 SECTION.                                              
256900     EVALUATE IN-EKH-KDEKSHT                                              
257000     WHEN '3XX'                                                           
257100          PERFORM CEJ301-SUB-EVENT-303-3XX                                
257200     WHEN '301'                                                           
257300          PERFORM CEJ301-SUB-EVENT-303-301                                
257400     WHEN '307'                                                           
257500          PERFORM CEJ307-SUB-EVENT-303-307                                
257600     WHEN '310'                                                           
257700          PERFORM CEJ310-SUB-EVENT-303-310                                
257800     WHEN '311'                                                           
257900          PERFORM CEJ311-SUB-EVENT-303-311                                
258000     WHEN '361'                                                           
258100          PERFORM CEJ307-SUB-EVENT-303-361                                
258200     WHEN '371'                                                           
258300          PERFORM CEJ371-SUB-EVENT-303-371                                
258400     WHEN '391'                                                           
258500          PERFORM CEJ301-SUB-EVENT-303-391                                
258600     END-EVALUATE                                                         
258700     .                                                                    
258800     EJECT                                                                
258900                                                                          
259000 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
259100     EVALUATE IN-EKH-KDEKNIVA                                             
259200                                                                          
259300     WHEN 'FÖRS'                                                          
259400     WHEN 'FRAKT'                                                         
259500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
259600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
259700       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
259800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
259900              (IN-EKH-SUBEL * -1) / WS-PRKURS-USD3                        
260000       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
260100       MOVE SPACE               TO WS-ALLOCATE-DC                         
260200       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
260300       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
260400       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
260500       PERFORM S03-WRITE-W57052                                           
260600                                                                          
260700     WHEN 'LAND'                                                          
260800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
260900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
261000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
261100              (IN-EKH-SUBEL * -1) / WS-PRKURS-USD3                        
261200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
261300       MOVE SPACE               TO WS-ALLOCATE-DC                         
261400       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
261500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
261600       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
261700       PERFORM S03-WRITE-W57052                                           
261800                                                                          
261900     WHEN 'DDI'                                                           
262000       IF IN-EKH-SUBEL > ZERO                                             
262100         IF SYST-IDSEKVNR = 1                                             
262200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
262300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
262400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
262500                   IN-EKH-SUBEL                                           
262600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
262700           MOVE SPACE               TO WS-ALLOCATE-DC                     
262800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
262900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
263000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
263100           PERFORM S04-WRITE-W57053A                                      
263200         END-IF                                                           
263300       ELSE                                                               
263400         IF SYST-IDSEKVNR = 2                                             
263500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
263600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
263700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
263800                   IN-EKH-SUBEL                                           
263900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
264000           MOVE SPACE               TO WS-ALLOCATE-DC                     
264100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
264200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
264300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
264400           PERFORM S04-WRITE-W57053A                                      
264500         END-IF                                                           
264600       END-IF                                                             
264700     END-EVALUATE                                                         
264800     .                                                                    
264900     EJECT                                                                
265000                                                                          
265100 CEJ301-SUB-EVENT-303-301 SECTION.                                        
265200     EVALUATE IN-EKH-KDEKNIVA                                             
265300     WHEN 'DET'                                                           
265400       IF SYST-IDSEKVNR = 1                                               
265500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
265600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
265700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
265800         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3 * -1           
265900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
266000         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
266100         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
266200         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
266300         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
266400         MOVE SPACE               TO WS-ALLOCATE-DC                       
266500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
266600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
266700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
266800         PERFORM S03-WRITE-W57052                                         
266900       END-IF                                                             
267000                                                                          
267100     END-EVALUATE                                                         
267200     .                                                                    
267300     EJECT                                                                
267400                                                                          
267500 CEJ307-SUB-EVENT-303-307 SECTION.                                        
267600     EVALUATE IN-EKH-KDEKNIVA                                             
267700     WHEN 'DET'                                                           
267800       IF SYST-IDSEKVNR = 1                                               
267900         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
268000         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
268100         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
268200         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3 * -1           
268300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
268400         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
268500         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
268600         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
268700         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
268800         MOVE SPACE               TO WS-ALLOCATE-DC                       
268900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
269000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
269100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
269200         PERFORM S03-WRITE-W57052                                         
269300       END-IF                                                             
269400                                                                          
269500     END-EVALUATE                                                         
269600     .                                                                    
269700     EJECT                                                                
269800                                                                          
269900 CEJ310-SUB-EVENT-303-310 SECTION.                                        
270000     EVALUATE IN-EKH-KDEKNIVA                                             
270100     WHEN 'DET'                                                           
270200       IF SYST-IDSEKVNR = 1                                               
270300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
270400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
270500         COMPUTE R3-LINE-AMOUNT-LC =                                      
270600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
270700         IF IN-EKH-KDVALISO = 'USD'                                       
270800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
270900         END-IF                                                           
271000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
271100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
271200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
271300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
271400         MOVE SPACE               TO WS-LINE-TEXT                         
271500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
271600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
271700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
271800         PERFORM S02-WRITE-W57051A                                        
271900       END-IF                                                             
272000                                                                          
272100       IF SYST-IDSEKVNR = 2                                               
272200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
272300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
272400         COMPUTE R3-LINE-AMOUNT-LC =                                      
272500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
272600         IF IN-EKH-KDVALISO = 'USD'                                       
272700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
272800         END-IF                                                           
272900         MOVE SPACE               TO WS-LINE-TEXT                         
273000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
273100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
273200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
273300         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
273400         MOVE SPACE               TO WS-ALLOCATE-DC                       
273500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
273600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
273700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
273800         PERFORM S02-WRITE-W57051A                                        
273900       END-IF                                                             
274000     END-EVALUATE                                                         
274100     .                                                                    
274200     EJECT                                                                
274300                                                                          
274400 CEJ311-SUB-EVENT-303-311 SECTION.                                        
274500     EVALUATE IN-EKH-KDEKNIVA                                             
274600     WHEN 'DET'                                                           
274700        IF SYST-IDSEKVNR = 1                                              
274800          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
274900          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
275000          COMPUTE R3-LINE-AMOUNT-LC =                                     
275100                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
275200          IF IN-EKH-KDVALISO = 'USD'                                      
275300            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
275400          END-IF                                                          
275500          MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER             
275600          MOVE SPACE               TO WS-LINE-TEXT                        
275700          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
275800          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
275900          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
276000          MOVE SPACE               TO WS-ALLOCATE-DC                      
276100          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
276200          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
276300          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
276400          PERFORM S02-WRITE-W57051A                                       
276500        END-IF                                                            
276600                                                                          
276700        IF SYST-IDSEKVNR = 2                                              
276800          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
276900          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
277000          COMPUTE R3-LINE-AMOUNT-LC =                                     
277100                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
277200          IF IN-EKH-KDVALISO = 'USD'                                      
277300            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
277400          END-IF                                                          
277500          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
277600          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
277700          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
277800          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
277900          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
278000          MOVE SPACE               TO WS-LINE-TEXT                        
278100          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
278200          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
278300          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
278400          PERFORM S02-WRITE-W57051A                                       
278500        END-IF                                                            
278600     END-EVALUATE                                                         
278700     .                                                                    
278800     EJECT                                                                
278900                                                                          
279000 CEJ307-SUB-EVENT-303-361 SECTION.                                        
279100     EVALUATE IN-EKH-KDEKNIVA                                             
279200     WHEN 'DET'                                                           
279300       IF SYST-IDSEKVNR = 1                                               
279400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
279500         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
279600         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
279700         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD * -1            
279800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
279900         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
280000         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
280100         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
280200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
280300         MOVE SPACE               TO WS-ALLOCATE-DC                       
280400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
280500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
280600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
280700         PERFORM S03-WRITE-W57052                                         
280800       END-IF                                                             
280900                                                                          
281000     WHEN 'LAND'                                                          
281100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
281200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
281300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
281400              IN-EKH-SUBEL / WS-PRKURS-USD * -1                           
281500       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
281600       MOVE SPACE               TO WS-ALLOCATE-DC                         
281700       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
281800       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
281900       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
282000       PERFORM S03-WRITE-W57052                                           
282100                                                                          
282200     END-EVALUATE                                                         
282300     .                                                                    
282400     EJECT                                                                
282500                                                                          
282600 CEJ371-SUB-EVENT-303-371 SECTION.                                        
282700     EVALUATE IN-EKH-KDEKNIVA                                             
282800     WHEN 'DET'                                                           
282900       IF SYST-IDSEKVNR = 1                                               
283000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
283100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
283200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
283300           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-USD3              
283400         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
283500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
283600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
283700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
283800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
283900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
284000         PERFORM S04-WRITE-W57053A                                        
284100       END-IF                                                             
284200                                                                          
284300     WHEN 'LAND'                                                          
284400       IF SYST-IDSEKVNR = 1                                               
284500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
284600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
284700         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
284800         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
284900               R3-LINE-AMOUNT-LC / WS-PRKURS-USD3                         
285000         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
285100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
285200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
285300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
285400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
285500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
285600         PERFORM S02-WRITE-W57051A                                        
285700       END-IF                                                             
285800                                                                          
285900     WHEN 'DDI'                                                           
286000       IF IN-EKH-SUBEL > ZERO                                             
286100         IF SYST-IDSEKVNR = 1                                             
286200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
286300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
286400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
286500                   IN-EKH-SUBEL                                           
286600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
286700           MOVE SPACE               TO WS-ALLOCATE-DC                     
286800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
286900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
287000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
287100           PERFORM S02-WRITE-W57051A                                      
287200         END-IF                                                           
287300       ELSE                                                               
287400         IF SYST-IDSEKVNR = 2                                             
287500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
287600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
287700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
287800                   IN-EKH-SUBEL                                           
287900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
288000           MOVE SPACE               TO WS-ALLOCATE-DC                     
288100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
288200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
288300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
288400           PERFORM S02-WRITE-W57051A                                      
288500         END-IF                                                           
288600       END-IF                                                             
288700     END-EVALUATE                                                         
288800     .                                                                    
288900     EJECT                                                                
289000                                                                          
289100 CEJ301-SUB-EVENT-303-391 SECTION.                                        
289200     EVALUATE IN-EKH-KDEKNIVA                                             
289300     WHEN 'DET'                                                           
289400       IF SYST-IDSEKVNR = 1                                               
289500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
289600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
289700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
289800              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
289900         IF IN-EKH-KDVALISO = 'USD'                                       
290000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
290100         END-IF                                                           
290200         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
290300         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
290400         MOVE SPACE               TO WS-LINE-TEXT                         
290500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
290600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
290700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
290800*        MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
290900         MOVE SPACE               TO WS-ALLOCATE-DC                       
291000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
291100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
291200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
291300         PERFORM S03-WRITE-W57052                                         
291400       END-IF                                                             
291500                                                                          
291600       IF SYST-IDSEKVNR = 2                                               
291700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
291800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
291900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
292000              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
292100         IF IN-EKH-KDVALISO = 'USD'                                       
292200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
292300         END-IF                                                           
292400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
292500         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
292600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
292700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
292800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
292900         MOVE SPACE               TO WS-LINE-TEXT                         
293000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
293100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
293200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
293300*        MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
293400         PERFORM S03-WRITE-W57052                                         
293500       END-IF                                                             
293600                                                                          
293700     END-EVALUATE                                                         
293800     .                                                                    
293900     EJECT                                                                
294000                                                                          
294100 CEK-MAIN-EVENT-401 SECTION.                                              
294200     EVALUATE IN-EKH-KDEKNIVA                                             
294300                                                                          
294400* PRISÄNDRING LÖPANDE                                                     
294500     WHEN 'DET'                                                           
294600       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
294700                           IN-EKH-PRARTSTD                                
294800       IF SYST-IDSEKVNR = 1                                               
294900* PRISHÖJNING                                                             
295000         IF WS-BELOPP > 0                                                 
295100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
295200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
295300           COMPUTE R3-LINE-AMOUNT-LC =                                    
295400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
295500           IF IN-EKH-KDVALISO = 'USD'                                     
295600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
295700           END-IF                                                         
295800           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
295900*          MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
296000           MOVE SPACES              TO R3-LINE-PA-CUSTOMER                
296100           PERFORM S02-WRITE-W57051A                                      
296200         END-IF                                                           
296300       END-IF                                                             
296400                                                                          
296500       IF SYST-IDSEKVNR = 2                                               
296600* PRISSÄKNING                                                             
296700         IF WS-BELOPP < 0                                                 
296800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
296900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
297000           COMPUTE R3-LINE-AMOUNT-LC =                                    
297100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
297200           IF IN-EKH-KDVALISO = 'USD'                                     
297300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
297400           END-IF                                                         
297500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
297600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
297700           MOVE SPACE               TO WS-ALLOCATE-REF                    
297800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
297900*          MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
298000           MOVE SPACES              TO R3-LINE-PA-CUSTOMER                
298100           PERFORM S02-WRITE-W57051A                                      
298200         END-IF                                                           
298300       END-IF                                                             
298400                                                                          
298500       IF SYST-IDSEKVNR = 3                                               
298600* PRISSÄNKNING                                                            
298700         IF WS-BELOPP < 0                                                 
298800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
298900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
299000           COMPUTE R3-LINE-AMOUNT-LC =                                    
299100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
299200           IF IN-EKH-KDVALISO = 'USD'                                     
299300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
299400           END-IF                                                         
299500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
299600*          MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
299700           MOVE SPACES              TO R3-LINE-PA-CUSTOMER                
299800           PERFORM S02-WRITE-W57051A                                      
299900         END-IF                                                           
300000       END-IF                                                             
300100                                                                          
300200       IF SYST-IDSEKVNR = 4                                               
300300* PRISHÖJNING                                                             
300400         IF WS-BELOPP > 0                                                 
300500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
300600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
300700           COMPUTE R3-LINE-AMOUNT-LC =                                    
300800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
300900           IF IN-EKH-KDVALISO = 'USD'                                     
301000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
301100           END-IF                                                         
301200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
301300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
301400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
301500           MOVE SPACE               TO WS-ALLOCATE-REF                    
301600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
301700*          MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
301800           MOVE SPACES              TO R3-LINE-PA-CUSTOMER                
301900           PERFORM S02-WRITE-W57051A                                      
302000         END-IF                                                           
302100       END-IF                                                             
302200     END-EVALUATE                                                         
302300     .                                                                    
302400     EJECT                                                                
302500                                                                          
302600 CEL-MAIN-EVENT-402 SECTION.                                              
302700     EVALUATE IN-EKH-KDEKNIVA                                             
302800     WHEN 'DET'                                                           
302900       IF SYST-IDSEKVNR = 1                                               
303000         IF IN-EKH-KVANTAL > 0                                            
303100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
303200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
303300           COMPUTE R3-LINE-AMOUNT-LC =                                    
303400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
303500           IF IN-EKH-KDVALISO = 'USD'                                     
303600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
303700           END-IF                                                         
303800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
303900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
304000           MOVE SPACE               TO WS-ALLOCATE-REF                    
304100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
304200           PERFORM S02-WRITE-W57051A                                      
304300         END-IF                                                           
304400       END-IF                                                             
304500                                                                          
304600       IF SYST-IDSEKVNR = 2                                               
304700         IF IN-EKH-KVANTAL < 0                                            
304800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
304900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
305000           COMPUTE R3-LINE-AMOUNT-LC =                                    
305100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
305200           IF IN-EKH-KDVALISO = 'USD'                                     
305300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
305400           END-IF                                                         
305500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
305600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
305700           MOVE SPACE               TO WS-ALLOCATE-REF                    
305800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
305900           PERFORM S02-WRITE-W57051A                                      
306000         END-IF                                                           
306100       END-IF                                                             
306200     END-EVALUATE                                                         
306300     .                                                                    
306400     EJECT                                                                
306500                                                                          
306600 CEM-MAIN-EVENT-403 SECTION.                                              
306700     EVALUATE IN-EKH-KDEKSHT                                              
306800     WHEN '401'                                                           
306900     WHEN '402'                                                           
307000     WHEN '403'                                                           
307100     WHEN '404'                                                           
307200     WHEN '405'                                                           
307300     WHEN '407'                                                           
307400     WHEN '408'                                                           
307500     WHEN '409'                                                           
307600          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
307700     END-EVALUATE                                                         
307800     .                                                                    
307900     EJECT                                                                
308000                                                                          
308100 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
308200     EVALUATE IN-EKH-KDEKNIVA                                             
308300     WHEN 'DET'                                                           
308400       IF IN-EKH-KVANTAL > 0                                              
308500         IF SYST-IDSEKVNR = 1                                             
308600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
308700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
308800           COMPUTE R3-LINE-AMOUNT-LC =                                    
308900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
309000           IF IN-EKH-KDVALISO = 'USD'                                     
309100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
309200           END-IF                                                         
309300           PERFORM S02-WRITE-W57051A                                      
309400         END-IF                                                           
309500                                                                          
309600         IF SYST-IDSEKVNR = 4                                             
309700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
309800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
309900           COMPUTE R3-LINE-AMOUNT-LC =                                    
310000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
310100           IF IN-EKH-KDVALISO = 'USD'                                     
310200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
310300           END-IF                                                         
310400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
310500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
310600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
310700           MOVE SPACE               TO WS-ALLOCATE-REF                    
310800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
310900           PERFORM S02-WRITE-W57051A                                      
311000         END-IF                                                           
311100       END-IF                                                             
311200                                                                          
311300       IF IN-EKH-KVANTAL < 0                                              
311400         IF SYST-IDSEKVNR = 2                                             
311500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
311600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
311700           COMPUTE R3-LINE-AMOUNT-LC =                                    
311800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
311900           IF IN-EKH-KDVALISO = 'USD'                                     
312000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
312100           END-IF                                                         
312200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
312300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
312400           MOVE SPACE               TO WS-ALLOCATE-REF                    
312500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
312600           PERFORM S02-WRITE-W57051A                                      
312700         END-IF                                                           
312800                                                                          
312900         IF SYST-IDSEKVNR = 3                                             
313000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
313100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
313200           COMPUTE R3-LINE-AMOUNT-LC =                                    
313300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
313400           IF IN-EKH-KDVALISO = 'USD'                                     
313500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
313600           END-IF                                                         
313700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
313800           PERFORM S02-WRITE-W57051A                                      
313900         END-IF                                                           
314000       END-IF                                                             
314100     END-EVALUATE                                                         
314200     .                                                                    
314300     EJECT                                                                
314400                                                                          
314500 CEN-MAIN-EVENT-404 SECTION.                                              
314600     EVALUATE IN-EKH-KDEKNIVA                                             
314700     WHEN 'DET'                                                           
314800       IF SYST-IDSEKVNR = 1                                               
314900* KONTO EJ MANUELLT REGISTRERAT                                           
315000         IF IN-EKH-IDKONTO = 0                                            
315100           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
315200           IF DIST18-SCRAP-NDC-SC                                         
315300           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
315400           OR DIST18-SCRAP-NDC-QUAL                                       
315500             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
315600             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
315700             COMPUTE R3-LINE-AMOUNT-LC =                                  
315800                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
315900             IF IN-EKH-KDVALISO = 'USD'                                   
316000               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
316100             END-IF                                                       
316200             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
316300             PERFORM S02-WRITE-W57051A                                    
316400           END-IF                                                         
316500         END-IF                                                           
316600       END-IF                                                             
316700                                                                          
316800       IF SYST-IDSEKVNR = 2                                               
316900* KONTO MANUELLT REGISTRERAT                                              
317000         IF IN-EKH-IDKONTO > 0                                            
317100           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
317200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
317300           COMPUTE R3-LINE-AMOUNT-LC =                                    
317400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
317500           IF IN-EKH-KDVALISO = 'USD'                                     
317600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
317700           END-IF                                                         
317800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
317900           PERFORM S02-WRITE-W57051A                                      
318000         END-IF                                                           
318100       END-IF                                                             
318200                                                                          
318300       IF SYST-IDSEKVNR = 3                                               
318400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
318500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
318600         COMPUTE R3-LINE-AMOUNT-LC =                                      
318700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
318800         IF IN-EKH-KDVALISO = 'USD'                                       
318900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
319000         END-IF                                                           
319100         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
319200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
319300         MOVE SPACE               TO WS-ALLOCATE-REF                      
319400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
319500         PERFORM S02-WRITE-W57051A                                        
319600       END-IF                                                             
319700                                                                          
319800                                                                          
319900     END-EVALUATE                                                         
320000     .                                                                    
320100     EJECT                                                                
320200                                                                          
320300 CF-BUILD-COMMON-210-PART SECTION.                                        
320400     MOVE SPACE              TO R3-LINE-R3                                
320500     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
320600                                R3-LINE-DUE-DATE                          
320700                                R3-LINE-AMOUNT                            
320800                                R3-LINE-AMOUNT-LC                         
320900                                R3-LINE-TAX-AMOUNT                        
321000                                R3-LINE-TAX-AMOUNT-LC                     
321100                                R3-LINE-NUMBER-OF-DAYS                    
321200                                R3-LINE-QUANTITY                          
321300                                R3-LINE-SAMNR                             
321400     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
321500     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
321600     MOVE 'AE01'             TO R3-LINE-COMPANY-CODE                      
321700     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
321800     IF SYST-KDPOST = '31'                                                
321900       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
322000     ELSE                                                                 
322100       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
322200     END-IF                                                               
322300     .                                                                    
322400     EJECT                                                                
322500                                                                          
322600 CG-SCHEDULE-LINE-AP SECTION.                                             
322700     MOVE NEJ                     TO WS-HEADER-SW                         
322800     MOVE JA                      TO WS-LINE-SW                           
322900     EVALUATE IN-EKH-KDEKHHT                                              
323000     WHEN '102'                                                           
323100       IF IN-EKH-KDEKSHT = '130'                                          
323200       OR IN-EKH-KDEKSHT = '134'                                          
323300         IF IN-EKH-KDEKSHT = '130'                                        
323400           PERFORM CGA-MAIN-EVENT-102-130                                 
323500         ELSE                                                             
323600           PERFORM CGA-MAIN-EVENT-102-134                                 
323700         END-IF                                                           
323800       ELSE                                                               
323900         IF IN-EKH-KDEKSHT = '120'                                        
324000         OR IN-EKH-KDEKSHT = '124'                                        
324100         OR IN-EKH-KDEKSHT = '125'                                        
324200           IF IN-EKH-KDEKSHT = '125'                                      
324300             PERFORM CGA-MAIN-EVENT-102-125                               
324400           ELSE                                                           
324500             PERFORM CGA-MAIN-EVENT-102-12X                               
324600           END-IF                                                         
324700         ELSE                                                             
324800           PERFORM CGA-MAIN-EVENT-102                                     
324900         END-IF                                                           
325000       END-IF                                                             
325100     WHEN '103'                                                           
325200         PERFORM CGA-MAIN-EVENT-103                                       
325300     WHEN '303'                                                           
325400       IF IN-EKH-KDEKSHT = '371'                                          
325500         PERFORM S81-GET-CURRENCY-RATE                                    
325600         PERFORM CGA-MAIN-EVENT-303-371                                   
325700       ELSE                                                               
325800         IF IN-EKH-KDEKSHT = '3XX'                                        
325900           PERFORM S81-GET-CURRENCY-RATE                                  
326000           PERFORM CGA-MAIN-EVENT-303                                     
326100         ELSE                                                             
326200           PERFORM CGA-MAIN-EVENT-303                                     
326300         END-IF                                                           
326400       END-IF                                                             
326500     END-EVALUATE                                                         
326600     .                                                                    
326700     EJECT                                                                
326800                                                                          
326900 CGA-MAIN-EVENT-102     SECTION.                                          
327000     EVALUATE IN-EKH-KDEKNIVA                                             
327100     WHEN 'SUM'                                                           
327200       IF IN-EKH-SUBEL > ZERO                                             
327300         IF SYST-IDSEKVNR = 1                                             
327400           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
327500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
327600            IN-EKH-SUBEL                                                  
327700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
327800           PERFORM S10-VATCODE                                            
327900           IF IN-EKH-SUVAT = ZERO                                         
328000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
328100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
328200           ELSE                                                           
328300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
328400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
328500           END-IF                                                         
328600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
328700                                                                          
328800           PERFORM S04-WRITE-W57053A                                      
328900         END-IF                                                           
329000       END-IF                                                             
329100                                                                          
329200       IF IN-EKH-SUBEL < ZERO                                             
329300         IF SYST-IDSEKVNR = 2                                             
329400           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
329500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
329600           IN-EKH-SUBEL                                                   
329700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
329800           PERFORM S10-VATCODE                                            
329900           IF IN-EKH-SUVAT = ZERO                                         
330000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
330100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
330200           ELSE                                                           
330300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
330400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
330500           END-IF                                                         
330600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
330700                                                                          
330800           PERFORM S04-WRITE-W57053A                                      
330900         END-IF                                                           
331000       END-IF                                                             
331100     END-EVALUATE                                                         
331200     .                                                                    
331300     EJECT                                                                
331400                                                                          
331500 CGA-MAIN-EVENT-102-12X SECTION.                                          
331600     EVALUATE IN-EKH-KDEKNIVA                                             
331700     WHEN 'SUM'                                                           
331800       IF IN-EKH-SUBEL > ZERO                                             
331900         IF SYST-IDSEKVNR = 1                                             
332000           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
332100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
332200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
332300                   R3-LINE-AMOUNT    / WS-PRKURS-USD * -1                 
332400           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
332500           MOVE 'F4'     TO R3-LINE-TAX-CODE                              
332600           IF IN-EKH-SUVAT = ZERO                                         
332700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
332800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
332900           ELSE                                                           
333000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
333100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
333200                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-USD * -1           
333300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
333400           END-IF                                                         
333500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
333600                                                                          
333700           PERFORM S04-WRITE-W57053A                                      
333800         END-IF                                                           
333900       END-IF                                                             
334000                                                                          
334100       IF IN-EKH-SUBEL < ZERO                                             
334200         IF SYST-IDSEKVNR = 2                                             
334300           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
334400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
334500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
334600                   R3-LINE-AMOUNT    / WS-PRKURS-USD * -1                 
334700           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
334800           MOVE 'F4'     TO R3-LINE-TAX-CODE                              
334900           IF IN-EKH-SUVAT = ZERO                                         
335000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
335100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
335200           ELSE                                                           
335300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
335400             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
335500                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-USD * -1           
335600             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
335700           END-IF                                                         
335800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
335900           MOVE SPACE           TO R3-LINE-COST-CENTER                    
336000                                                                          
336100           PERFORM S04-WRITE-W57053A                                      
336200         END-IF                                                           
336300       END-IF                                                             
336400     END-EVALUATE                                                         
336500     .                                                                    
336600     EJECT                                                                
336700                                                                          
336800                                                                          
336900 CGA-MAIN-EVENT-102-125 SECTION.                                          
337000     EVALUATE IN-EKH-KDEKNIVA                                             
337100     WHEN 'SUM'                                                           
337200       IF IN-EKH-SUBEL > ZERO                                             
337300         IF SYST-IDSEKVNR = 1                                             
337400           MOVE ZERO TO SPAR-SUMMA-102-125                                
337500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
337600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
337700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
337800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
337900                   R3-LINE-AMOUNT    / WS-PRKURS-USD * -1                 
338000           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
338100           MOVE 'F4'     TO R3-LINE-TAX-CODE                              
338200           IF IN-EKH-SUVAT = ZERO                                         
338300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
338400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
338500           ELSE                                                           
338600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
338700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
338800               R3-LINE-TAX-AMOUNT / WS-PRKURS-USD * -1                    
338900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
339000           END-IF                                                         
339100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
339200           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
339300                                                                          
339400           PERFORM S04-WRITE-W57053A                                      
339500         END-IF                                                           
339600       END-IF                                                             
339700                                                                          
339800       IF IN-EKH-SUBEL < ZERO                                             
339900         IF SYST-IDSEKVNR = 2                                             
340000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
340100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
340200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
340300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
340400                   R3-LINE-AMOUNT    / WS-PRKURS-USD * -1                 
340500           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
340600           MOVE 'F4'     TO R3-LINE-TAX-CODE                              
340700           IF IN-EKH-SUVAT = ZERO                                         
340800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
340900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
341000           ELSE                                                           
341100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
341200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
341300               R3-LINE-TAX-AMOUNT / WS-PRKURS-USD * -1                    
341400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
341500           END-IF                                                         
341600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
341700           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
341800                                                                          
341900           PERFORM S04-WRITE-W57053A                                      
342000         END-IF                                                           
342100       END-IF                                                             
342200     END-EVALUATE                                                         
342300     .                                                                    
342400     EJECT                                                                
342500                                                                          
342600 CGA-MAIN-EVENT-103     SECTION.                                          
342700     EVALUATE IN-EKH-KDEKNIVA                                             
342800     WHEN 'SUM'                                                           
342900       IF IN-EKH-SUBEL > ZERO                                             
343000         IF SYST-IDSEKVNR = 1                                             
343100           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
343200           PERFORM S10-VATCODE                                            
343300           IF IN-EKH-SUVAT = ZERO                                         
343400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
343500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
343600           ELSE                                                           
343700             IF IN-EKH-KDVALISO = 'USD'                                   
343800               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
343900                                      R3-LINE-TAX-AMOUNT-LC               
344000             ELSE                                                         
344100               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
344200                                      R3-LINE-TAX-AMOUNT-LC               
344300               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
344400                       R3-LINE-TAX-AMOUNT * WS-PRKURS                     
344500             END-IF                                                       
344600           END-IF                                                         
344700**** CALCULATE NEW SUM WITH VAT                                           
344800           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
344900                   IN-EKH-SUVAT                                           
345000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
345100           IF IN-EKH-KDVALISO = 'USD'                                     
345200             MOVE R3-LINE-AMOUNT TO R3-LINE-AMOUNT-LC                     
345300           ELSE                                                           
345400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
345500                     R3-LINE-AMOUNT * WS-PRKURS                           
345600           END-IF                                                         
345700           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
345800                                                                          
345900           PERFORM S04-WRITE-W57053A                                      
346000         END-IF                                                           
346100       END-IF                                                             
346200                                                                          
346300       IF IN-EKH-SUBEL < ZERO                                             
346400         IF SYST-IDSEKVNR = 1                                             
346500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
346600           MOVE 'P1'     TO R3-LINE-TAX-CODE                              
346700           COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.1              
346800           IF IN-EKH-SUVAT = ZERO                                         
346900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
347000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
347100           ELSE                                                           
347200             IF IN-EKH-KDVALISO = 'USD'                                   
347300               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
347400                                      R3-LINE-TAX-AMOUNT-LC               
347500             ELSE                                                         
347600               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
347700                                      R3-LINE-TAX-AMOUNT-LC               
347800               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
347900                       R3-LINE-TAX-AMOUNT * WS-PRKURS                     
348000             END-IF                                                       
348100           END-IF                                                         
348200**** CALCULATE NEW SUM WITH VAT                                           
348300           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
348400                   IN-EKH-SUVAT                                           
348500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
348600           IF IN-EKH-KDVALISO = 'USD'                                     
348700             MOVE R3-LINE-AMOUNT TO R3-LINE-AMOUNT-LC                     
348800           ELSE                                                           
348900             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
349000                     R3-LINE-AMOUNT * WS-PRKURS                           
349100           END-IF                                                         
349200           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
349300                                                                          
349400           PERFORM S04-WRITE-W57053A                                      
349500         END-IF                                                           
349600       END-IF                                                             
349700     END-EVALUATE                                                         
349800     .                                                                    
349900     EJECT                                                                
350000                                                                          
350100 CGA-MAIN-EVENT-102-130 SECTION.                                          
350200     EVALUATE IN-EKH-KDEKNIVA                                             
350300     WHEN 'SUM'                                                           
350400       IF IN-EKH-SUBEL > ZERO                                             
350500         IF SYST-IDSEKVNR = 1                                             
350600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
350700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
350800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
350900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
351000                   R3-LINE-AMOUNT    / WS-PRKURS-USD * -1                 
351100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
351200           PERFORM S10-VATCODE                                            
351300           IF IN-EKH-SUVAT = ZERO                                         
351400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
351500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
351600           ELSE                                                           
351700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
351800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
351900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-USD * -1           
352000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
352100           END-IF                                                         
352200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
352300                                                                          
352400           PERFORM S04-WRITE-W57053A                                      
352500         END-IF                                                           
352600       END-IF                                                             
352700                                                                          
352800       IF IN-EKH-SUBEL < ZERO                                             
352900         IF SYST-IDSEKVNR = 2                                             
353000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
353100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
353200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
353300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
353400                   R3-LINE-AMOUNT    / WS-PRKURS-USD                      
353500           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
353600           PERFORM S10-VATCODE                                            
353700           IF IN-EKH-SUVAT = ZERO                                         
353800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
353900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
354000           ELSE                                                           
354100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
354200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
354300                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-USD                
354400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
354500           END-IF                                                         
354600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
354700                                                                          
354800           PERFORM S04-WRITE-W57053A                                      
354900         END-IF                                                           
355000       END-IF                                                             
355100     END-EVALUATE                                                         
355200     .                                                                    
355300     EJECT                                                                
355400                                                                          
355500 CGA-MAIN-EVENT-102-134 SECTION.                                          
355600     EVALUATE IN-EKH-KDEKNIVA                                             
355700     WHEN 'SUM'                                                           
355800       IF IN-EKH-SUBEL > ZERO                                             
355900         IF SYST-IDSEKVNR = 1                                             
356000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
356100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
356200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
356300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
356400                   R3-LINE-AMOUNT    / WS-PRKURS-USD * -1                 
356500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
356600           PERFORM S10-VATCODE                                            
356700           IF IN-EKH-SUVAT = ZERO                                         
356800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
356900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
357000           ELSE                                                           
357100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
357200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
357300                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-USD * -1           
357400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
357500           END-IF                                                         
357600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
357700                                                                          
357800           PERFORM S04-WRITE-W57053A                                      
357900         END-IF                                                           
358000       END-IF                                                             
358100                                                                          
358200       IF IN-EKH-SUBEL < ZERO                                             
358300         IF SYST-IDSEKVNR = 2                                             
358400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
358500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
358600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
358700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
358800                   R3-LINE-AMOUNT    / WS-PRKURS-USD                      
358900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
359000           PERFORM S10-VATCODE                                            
359100           IF IN-EKH-SUVAT = ZERO                                         
359200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
359300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
359400           ELSE                                                           
359500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
359600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
359700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-USD                
359800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
359900           END-IF                                                         
360000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
360100                                                                          
360200           PERFORM S04-WRITE-W57053A                                      
360300         END-IF                                                           
360400       END-IF                                                             
360500     END-EVALUATE                                                         
360600     .                                                                    
360700     EJECT                                                                
360800                                                                          
360900 CGA-MAIN-EVENT-303 SECTION.                                              
361000     EVALUATE IN-EKH-KDEKNIVA                                             
361100     WHEN 'SUM'                                                           
361200       IF SYST-IDSEKVNR = 1                                               
361300         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
361400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
361500         (IN-EKH-SUBEL / WS-PRKURS-USD)                                   
361600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
361700         MOVE 'F4'     TO R3-LINE-TAX-CODE                                
361800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
361900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
362000                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-USD                    
362100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
362200                                                                          
362300         PERFORM S04-WRITE-W57053A                                        
362400       END-IF                                                             
362500     END-EVALUATE                                                         
362600     .                                                                    
362700     EJECT                                                                
362800                                                                          
362900 CGA-MAIN-EVENT-303-3XX SECTION.                                          
363000     EVALUATE IN-EKH-KDEKNIVA                                             
363100     WHEN 'SUM'                                                           
363200       IF SYST-IDSEKVNR = 1                                               
363300         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
363400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
363500         (IN-EKH-SUBEL / WS-PRKURS-USD3)                                  
363600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
363700         MOVE 'F4'     TO R3-LINE-TAX-CODE                                
363800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
363900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
364000                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-USD3                   
364100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
364200                                                                          
364300         PERFORM S04-WRITE-W57053A                                        
364400       END-IF                                                             
364500     END-EVALUATE                                                         
364600     .                                                                    
364700     EJECT                                                                
364800                                                                          
364900 CGA-MAIN-EVENT-303-371 SECTION.                                          
365000     EVALUATE IN-EKH-KDEKNIVA                                             
365100     WHEN 'SUM'                                                           
365200       IF SYST-IDSEKVNR = 1                                               
365300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
365400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
365500         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
365600         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
365700               R3-LINE-AMOUNT-LC / WS-PRKURS-USD3                         
365800         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
365900         PERFORM S10-VATCODE                                              
366000         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
366100         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
366200                 R3-LINE-TAX-AMOUNT-LC * WS-PRKURS-USD2                   
366300         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
366400                                                                          
366500         PERFORM S04-WRITE-W57053A                                        
366600       END-IF                                                             
366700     END-EVALUATE                                                         
366800     .                                                                    
366900     EJECT                                                                
367000                                                                          
367100 CH-BUILD-COMMON-310-PART SECTION.                                        
367200     MOVE SPACE              TO R3-LINE-R3                                
367300     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
367400                                R3-LINE-DUE-DATE                          
367500                                R3-LINE-AMOUNT                            
367600                                R3-LINE-AMOUNT-LC                         
367700                                R3-LINE-TAX-AMOUNT                        
367800                                R3-LINE-TAX-AMOUNT-LC                     
367900                                R3-LINE-NUMBER-OF-DAYS                    
368000                                R3-LINE-QUANTITY                          
368100                                R3-LINE-SAMNR                             
368200     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
368300     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
368400     MOVE 'AE01'             TO R3-LINE-COMPANY-CODE                      
368500     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
368600     IF SYST-KDPOST = '31'                                                
368700       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
368800     ELSE                                                                 
368900       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
369000     END-IF                                                               
369100     .                                                                    
369200     EJECT                                                                
369300                                                                          
369400 CI-SCHEDULE-LINE-AR SECTION.                                             
369500     MOVE NEJ                     TO WS-HEADER-SW                         
369600     MOVE JA                      TO WS-LINE-SW                           
369700     EVALUATE IN-EKH-KDEKHHT                                              
369800     WHEN '204'                                                           
369900         PERFORM CIA-MAIN-EVENT-204                                       
370000     END-EVALUATE                                                         
370100     .                                                                    
370200     EJECT                                                                
370300                                                                          
370400 CIA-MAIN-EVENT-204     SECTION.                                          
370500     EVALUATE IN-EKH-KDEKNIVA                                             
370600     WHEN 'SUM'                                                           
370700       IF IN-EKH-SUBEL > ZERO                                             
370800         IF SYST-IDSEKVNR = 1                                             
370900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
371000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
371100**** WE HAVE SOME ISSUE WITH THIS                                         
371200*          IF IN-EKH-KDVALISO = 'USD'                                     
371300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
371400*          END-IF                                                         
371500           PERFORM S10-VATCODE                                            
371600           IF IN-EKH-SUVAT = ZERO                                         
371700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
371800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
371900           ELSE                                                           
372000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
372100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
372200           END-IF                                                         
372300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
372400                                                                          
372500           PERFORM S04-WRITE-W57053A                                      
372600         END-IF                                                           
372700       END-IF                                                             
372800                                                                          
372900     END-EVALUATE                                                         
373000     .                                                                    
373100     EJECT                                                                
373200                                                                          
373300 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
373400     MOVE ZERO             TO LOGG-W57073                                 
373500     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
373600     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
373700     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
373800     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
373900     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
374000     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
374100     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
374200     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
374300     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
374400     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
374500     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
374600     MOVE 'AE01'           TO LOGG-KDTRADP                                
374700                                                                          
374800****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
374900     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
375000     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
375100     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
375200     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
375300     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
375400     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
375500     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
375600     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
375700     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
375800     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
375900     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
376000     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
376100     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
376200     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
376300     .                                                                    
376400     EJECT                                                                
376500                                                                          
376600 Z-FINI SECTION.                                                          
376700     CLOSE W57066                                                         
376800           W57058                                                         
376900           W57051A                                                        
377000           W57052A                                                        
377100           W57053A                                                        
377200           W57055                                                         
377300           W5705N                                                         
377400           W51350                                                         
377500                                                                          
377600     MOVE 'S' TO POSTSUM-OPKOD                                            
377700     CALL POSTSUM USING POSTSUM-PARM                                      
377800     .                                                                    
377900     EJECT                                                                
378000                                                                          
378100 S01-READ-W57066  SECTION.                                                
378200     READ W57066 INTO IN-AREA                                             
378300     AT END                                                               
378400        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
378500        SET END-OF-W57066 TO TRUE                                         
378600                                                                          
378700     NOT AT END                                                           
378800        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
378900        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
379000        MOVE 'W57058D1'   TO POSTSUM-DDNAMN2                              
379100        CALL POSTSUM USING POSTSUM-PARM                                   
379200     END-READ                                                             
379300     .                                                                    
379400                                                                          
379500 S02-WRITE-W57051A SECTION.                                               
379600     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
379700     MOVE SPACE                 TO 51LINE-POST                            
379800     IF WS-LINE-SW = JA                                                   
379900       IF IN-EKH-KDSORT = 'SW'                                            
380000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
380100         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
380200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
380300       ELSE                                                               
380400         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
380500         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
380600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
380700       END-IF                                                             
380800       WRITE 51LINE-POST        FROM R3-LINE-R3                           
380900       PERFORM S20-CREATE-WRITE-LOG                                       
381000     ELSE                                                                 
381100       WRITE 51HEAD-POST        FROM R3-HEAD-R3                           
381200     END-IF                                                               
381300                                                                          
381400     IF WS-LINE-SW = JA                                                   
381500       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
381600     ELSE                                                                 
381700       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
381800     END-IF                                                               
381900     MOVE 'W57051A'             TO POSTSUM-FDNAMN                         
382000     MOVE 'W57058D2'            TO POSTSUM-DDNAMN2                        
382100     CALL POSTSUM USING POSTSUM-PARM                                      
382200     .                                                                    
382300                                                                          
382400 S002-WRITE-W57051A-HEAD SECTION.                                         
382500     MOVE SPACE                 TO 51LINE-POST                            
382600     IF IN-EKH-KDSORT = 'SW'                                              
382700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
382800       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
382900       MOVE WS-TEXT           TO R3-LINE-TEXT                             
383000     ELSE                                                                 
383100       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
383200       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
383300       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
383400     END-IF                                                               
383500     WRITE 51HEAD-POST          FROM R3-HEAD-R3                           
383600                                                                          
383700     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
383800     MOVE 'W57051A'             TO POSTSUM-FDNAMN                         
383900     MOVE 'W57058D2'            TO POSTSUM-DDNAMN2                        
384000     CALL POSTSUM USING POSTSUM-PARM                                      
384100     .                                                                    
384200                                                                          
384300 S03-WRITE-W57052 SECTION.                                                
384400     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
384500     IF IN-EKH-KDSORT = 'SW'                                              
384600       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
384700       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
384800       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
384900     ELSE                                                                 
385000       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
385100       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
385200       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
385300     END-IF                                                               
385400     WRITE 52LINE-POST        FROM R3-LINE-R3                             
385500                                                                          
385600     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
385700     MOVE 'W57052A'           TO POSTSUM-FDNAMN                           
385800     MOVE 'W57058D3'          TO POSTSUM-DDNAMN2                          
385900     CALL POSTSUM USING POSTSUM-PARM                                      
386000                                                                          
386100     PERFORM S20-CREATE-WRITE-LOG                                         
386200     .                                                                    
386300                                                                          
386400 S04-WRITE-W57053A SECTION.                                               
386500     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
386600     MOVE SPACE                 TO 53LINE-POST                            
386700     IF WS-LINE-SW = JA                                                   
386800       IF IN-EKH-KDSORT = 'SW'                                            
386900         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
387000         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
387100         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
387200       ELSE                                                               
387300         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
387400         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
387500         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
387600       END-IF                                                             
387700       WRITE 53LINE-POST        FROM R3-LINE-R3                           
387800       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
387900     ELSE                                                                 
388000       WRITE 53HEAD-POST        FROM R3-HEAD-R3                           
388100       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
388200     END-IF                                                               
388300                                                                          
388400     MOVE 'W57053A'             TO POSTSUM-FDNAMN                         
388500     MOVE 'W57058D4'            TO POSTSUM-DDNAMN2                        
388600     CALL POSTSUM USING POSTSUM-PARM                                      
388700                                                                          
388800     IF WS-LINE-SW = JA                                                   
388900       PERFORM S20-CREATE-WRITE-LOG                                       
389000     END-IF                                                               
389100     .                                                                    
389200                                                                          
389300 S004-WRITE-W57053A-HEAD SECTION.                                         
389400     MOVE SPACE                 TO 53LINE-POST                            
389500     IF IN-EKH-KDSORT = 'SW'                                              
389600       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
389700       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
389800       MOVE WS-TEXT           TO R3-LINE-TEXT                             
389900     ELSE                                                                 
390000       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
390100       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
390200       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
390300     END-IF                                                               
390400     WRITE 53HEAD-POST          FROM R3-HEAD-R3                           
390500                                                                          
390600     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
390700     MOVE 'W57053A'             TO POSTSUM-FDNAMN                         
390800     MOVE 'W57058D4'            TO POSTSUM-DDNAMN2                        
390900     CALL POSTSUM USING POSTSUM-PARM                                      
391000     .                                                                    
391100                                                                          
391200 S10-VATCODE SECTION.                                                     
391300     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
391400     PERFORM IMS-GU-WDB601                                                
391500     IF DCS-KDDC = SPACE                                                  
391600       MOVE NEJ              TO WDB6-A-SW                                 
391700     ELSE                                                                 
391800       MOVE JA               TO WDB6-A-SW                                 
391900     END-IF                                                               
392000                                                                          
392100     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
392200     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
392300     IF IN-EKH-SUVAT = ZERO                                               
392400       MOVE '90'     TO R3-LINE-TAX-CODE                                  
392500     ELSE                                                                 
392600       MOVE 'F4'     TO R3-LINE-TAX-CODE                                  
392700     END-IF                                                               
392800     IF IN-EKH-BEVAT = 'XX'                                               
392900       MOVE 'F4'     TO R3-LINE-TAX-CODE                                  
393000     END-IF                                                               
393100     .                                                                    
393200     EJECT                                                                
393300                                                                          
393400 S11-ANALYSIS SECTION.                                                    
393500     MOVE WS-R3-ACCOUNT-10        TO W-IDKONTO-5122                       
393600     MOVE R3-LINE-PROFIT-CENTER   TO W-IDPRCTR-5122                       
393700     PERFORM IMS-GU-5122                                                  
393800     IF SEGMENT-SAKNAS                                                    
393900       MOVE '4516????????'        TO R3-LINE-ORDER                        
394000     ELSE                                                                 
394100       MOVE 5122-IDANALYS         TO R3-LINE-ORDER                        
394200     END-IF                                                               
394300     .                                                                    
394400     EJECT                                                                
394500                                                                          
394600 S12-PROFITCENTER SECTION.                                                
394700     MOVE IN-EKH-IDDISTR          TO WS-IDDISTR                           
394800     MOVE IN-EKH-IDKUNDNR         TO WS-IDKUNDNR                          
394900                                                                          
395000     PERFORM IMS-GU-5121                                                  
395100     MOVE +999999                 TO W-IDKONTO-5122-MIN                   
395200                                     W-IDKONTO-5122-MAX                   
395300     PERFORM IMS-GNP-5122                                                 
395400     PERFORM UNTIL SEGMENT-SAKNAS                                         
395500     OR 5122-IDANALYS = WS-IDDISTR-IDKUNDNR                               
395600       PERFORM IMS-GNP-5122                                               
395700     END-PERFORM                                                          
395800                                                                          
395900     IF SEGMENT-SAKNAS                                                    
396000       MOVE '??????????'          TO R3-LINE-PROFIT-CENTER                
396100     ELSE                                                                 
396200       MOVE 5122-IDPRCTR          TO R3-LINE-PROFIT-CENTER                
396300     END-IF                                                               
396400     .                                                                    
396500     EJECT                                                                
396600                                                                          
396700 S20-CREATE-WRITE-LOG SECTION.                                            
396800     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
396900     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
397000     IF SYST-IDPTYP = '610'                                               
397100       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
397200     ELSE                                                                 
397300       MOVE ZERO                      TO WS-IDLEVNR                       
397400       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
397500                          FOR CHARACTERS BEFORE INITIAL SPACE             
397600       IF WS-IDLEVNR   > ZERO                                             
397700          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
397800                                      TO LOGG-IDKONTO                     
397900       END-IF                                                             
398000     END-IF                                                               
398100     IF R3-LINE-COST-CENTER NOT = SPACE                                   
398200       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
398300     END-IF                                                               
398400     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
398500     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
398600     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
398700     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
398800     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
398900                                                                          
399000     PERFORM S21-WRITE-W57055                                             
399100     PERFORM S22-WRITE-W57058                                             
399200                                                                          
399300     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
399400       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
399500       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
399600       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
399700                                                                          
399800       PERFORM S21-WRITE-W57055                                           
399900     END-IF                                                               
400000     .                                                                    
400100     EJECT                                                                
400200                                                                          
400300 S21-WRITE-W57055 SECTION.                                                
400400     IF DCS-IDDC NOT = LOGG-IDDC                                          
400500        MOVE LOGG-IDDC TO W-IDDC-B6                                       
400600        PERFORM IMS-GU-WDB601                                             
400700     END-IF                                                               
400800     IF DCS-KDDC = SPACE                                                  
400900       MOVE NEJ              TO WDB6-A-SW                                 
401000     ELSE                                                                 
401100       MOVE JA               TO WDB6-A-SW                                 
401200     END-IF                                                               
401300                                                                          
401400     IF  WDB6-A-FINNS                                                     
401500     AND DCS-DDC                                                          
401600       MOVE 'N'       TO LOGG-FLLSBOK                                     
401700     END-IF                                                               
401800     WRITE LOGG-POST FROM LOGG-W57073                                     
401900                                                                          
402000     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
402100     MOVE 'W57055'    TO POSTSUM-FDNAMN                                   
402200     MOVE 'W57058D5'  TO POSTSUM-DDNAMN2                                  
402300     CALL POSTSUM USING POSTSUM-PARM                                      
402400     .                                                                    
402500                                                                          
402600 S22-WRITE-W57058 SECTION.                                                
402700     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
402800     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
402900     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
403000                                                                          
403100     IF R3-LINE-AMOUNT-SIGN = '+'                                         
403200       IF AVST-SUBEL < +0                                                 
403300         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
403400       END-IF                                                             
403500       IF AVST-KVANTAL < +0                                               
403600         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
403700       END-IF                                                             
403800     ELSE                                                                 
403900       IF AVST-SUBEL > +0                                                 
404000         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
404100       END-IF                                                             
404200       IF AVST-KVANTAL > +0                                               
404300         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
404400       END-IF                                                             
404500     END-IF                                                               
404600                                                                          
404700     IF DCS-IDDC NOT = AVST-IDDC                                          
404800        MOVE AVST-IDDC  TO W-IDDC-B6                                      
404900        PERFORM IMS-GU-WDB601                                             
405000     END-IF                                                               
405100     IF DCS-KDDC = SPACE                                                  
405200       MOVE NEJ              TO WDB6-A-SW                                 
405300     ELSE                                                                 
405400       MOVE JA               TO WDB6-A-SW                                 
405500     END-IF                                                               
405600                                                                          
405700     IF  WDB6-A-FINNS                                                     
405800     AND DCS-DDC                                                          
405900       MOVE 'N'                 TO AVST-FLLSBOK                           
406000     END-IF                                                               
406100                                                                          
406200     IF AVST-IDKONTO(1:4) = '1454'                                        
406300       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
406400       WRITE AVST-POST FROM AVST-W57070                                   
406500                                                                          
406600       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
406700       MOVE 'W57058'            TO POSTSUM-FDNAMN                         
406800       MOVE 'W57058D6'          TO POSTSUM-DDNAMN2                        
406900       CALL POSTSUM USING POSTSUM-PARM                                    
407000     END-IF                                                               
407100     .                                                                    
407200     EJECT                                                                
407300                                                                          
407400 S30-READ-DATABASE-B2-B1 SECTION.                                         
407500                                                                          
407600     IF IN-EKH-IDLEVNR = '1441'                                           
407700       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
407800     ELSE                                                                 
407900       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
408000       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
408100       PERFORM IMS-GU-WDB201                                              
408200       IF SEGMENT-SAKNAS                                                  
408300           MOVE 'AE99999'     TO W-WDB1-IDPARTNR                          
408400       ELSE                                                               
408500         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
408600       END-IF                                                             
408700     END-IF                                                               
408800     MOVE WC-IDFTG-AE       TO W-WDB1-IDFTG                               
408900     PERFORM IMS-GU-WDB101                                                
409000     IF SEGMENT-SAKNAS                                                    
409100       DISPLAY 'BETALARUPPG. SAKNAS '                                     
409200       DISPLAY IN-EKH-IDVERGL                                             
409300       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
409400       DISPLAY GMT-IDPARTNR                                               
409500                                                                          
409600       MOVE SPACE         TO BET-KDTRADP                                  
409700       MOVE ZERO          TO BET-IDPARTNR                                 
409800       MOVE '????'        TO WS-KDBETVIL                                  
409900       MOVE '???'         TO WS-KDVALISO-WDB1                             
410000     ELSE                                                                 
410100       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
410200     END-IF                                                               
410300     MOVE 'USD'           TO WS-KDVALISO-WDB1                             
410400                                                                          
410500     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
410600     MOVE ZERO TO TALLY                                                   
410700     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
410800                 FOR CHARACTERS BEFORE INITIAL SPACE                      
410900     IF TALLY = ZERO                                                      
411000       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
411100     ELSE                                                                 
411200       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
411300                                TO W-BET-IDPARTNR-NUM                     
411400     END-IF                                                               
411500     .                                                                    
411600     EJECT                                                                
411700                                                                          
411800 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
411900     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
412000     PERFORM IMS-GU-WDB601                                                
412100     IF DCS-KDDC = SPACE                                                  
412200       MOVE NEJ              TO WDB6-A-SW                                 
412300     ELSE                                                                 
412400       MOVE JA               TO WDB6-A-SW                                 
412500     END-IF                                                               
412600                                                                          
412700     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
412800       IF IN-EKH-KDEKSHT NOT = '406'                                      
412900         IF IN-EKH-FLDCET = NEJ                                           
413000           PERFORM S42-SKAPA-RW2-INV-POSTER                               
413100         END-IF                                                           
413200       END-IF                                                             
413300     END-IF                                                               
413400                                                                          
413500     IF IN-EKH-KDEKNIVA = 'DET'                                           
413600       IF  IN-EKH-KDEKHHT = '204'                                         
413700       AND (IN-EKH-KDEKSHT = '201')                                       
413800         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
413900       END-IF                                                             
414000                                                                          
414100       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
414200       AND (WDB6-A-FINNS                                                  
414300       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
414400       OR   DCS-DDC OR DCS-NDC-PF OR DCS-NDC-OTHERS))                     
414500         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
414600       END-IF                                                             
414700                                                                          
414800       IF IN-FIL-IDPGM = 'W4183000'                                       
414900       AND (WDB6-A-FINNS                                                  
415000       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
415100       OR   DCS-DDC OR DCS-NDC-PF OR DCS-NDC-OTHERS))                     
415200         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
415300       END-IF                                                             
415400     END-IF                                                               
415500     .                                                                    
415600     EJECT                                                                
415700                                                                          
415800 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
415900     MOVE 'RW2'              TO RW2-IDPTYP                                
416000     MOVE 'RW2'              TO WS-IDPTYP                                 
416100     MOVE ZERO               TO RW2-IDDISTR                               
416200     IF DCS-KDDC = SPACE OR DCS-DDC                                       
416300       MOVE WC-CDC-SE        TO RW2-IDDC                                  
416400     ELSE                                                                 
416500       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
416600     END-IF                                                               
416700     IF IN-EKH-KVANTAL < +0                                               
416800       MOVE '0422'           TO RW2-KDWRTYP                               
416900     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
417000     ELSE                                                                 
417100       MOVE '0421'           TO RW2-KDWRTYP                               
417200       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
417300     END-IF                                                               
417400                                                                          
417500     IF RW2-SUARTSTD NOT = +0                                             
417600       PERFORM S70-WRITE-W51350                                           
417700     END-IF                                                               
417800     .                                                                    
417900     EJECT                                                                
418000                                                                          
418100 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
418200     MOVE '0110'             TO RW1-KDWRTYP                               
418300     IF DCS-KDDC = SPACE OR DCS-DDC                                       
418400       MOVE WC-CDC-SE        TO RW1-IDDC                                  
418500     ELSE                                                                 
418600       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
418700     END-IF                                                               
418800     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
418900     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
419000     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
419100                                                                          
419200     IF RW1-SUARTSTD NOT = +0                                             
419300       MOVE 'RW1' TO WS-IDPTYP                                            
419400       PERFORM S70-WRITE-W51350                                           
419500     END-IF                                                               
419600     .                                                                    
419700     EJECT                                                                
419800                                                                          
419900 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
420000     MOVE '0110'             TO RW1-KDWRTYP                               
420100     IF DCS-KDDC = SPACE OR DCS-DDC                                       
420200       MOVE WC-CDC-SE        TO RW1-IDDC                                  
420300     ELSE                                                                 
420400       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
420500     END-IF                                                               
420600     IF IN-EKH-KDANMORS = '30'                                            
420700       MOVE ZERO             TO RW1-SUARTSTD                              
420800     ELSE                                                                 
420900      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
421000     END-IF                                                               
421100     IF IN-EKH-KDANMORS = '30' OR '80'                                    
421200       MOVE ZERO             TO RW1-SUARTSJK                              
421300     ELSE                                                                 
421400      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
421500     END-IF                                                               
421600     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
421700                                                                          
421800     IF RW1-SUARTSTD NOT = +0                                             
421900       MOVE 'RW1' TO WS-IDPTYP                                            
422000       PERFORM S70-WRITE-W51350                                           
422100     END-IF                                                               
422200     .                                                                    
422300     EJECT                                                                
422400                                                                          
422500 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
422600     MOVE '0110'             TO RW1-KDWRTYP                               
422700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
422800       MOVE WC-CDC-SE        TO RW1-IDDC                                  
422900     ELSE                                                                 
423000       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
423100     END-IF                                                               
423200     IF IN-EKH-KDEKSHT = '310'                                            
423300*** SKROTNING KDANMORS  13 O 23                                           
423400       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
423500     ELSE                                                                 
423600      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
423700     END-IF                                                               
423800                                                                          
423900     MOVE ZERO               TO RW1-SUARTSJK                              
424000                                RW1-SUARTFSG                              
424100     IF RW1-SUARTSTD NOT = +0                                             
424200       MOVE 'RW1' TO WS-IDPTYP                                            
424300       PERFORM S70-WRITE-W51350                                           
424400     END-IF                                                               
424500     .                                                                    
424600     EJECT                                                                
424700                                                                          
424800 S60-WRITE-W5705N SECTION.                                                
424900     WRITE SAPUT-POST  FROM IN-AREA                                       
425000                                                                          
425100     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
425200     MOVE 'W5705N'            TO POSTSUM-FDNAMN                           
425300     MOVE 'W57058D7'          TO POSTSUM-DDNAMN2                          
425400     CALL POSTSUM USING POSTSUM-PARM                                      
425500     .                                                                    
425600     EJECT                                                                
425700                                                                          
425800 S70-WRITE-W51350 SECTION.                                                
425900     IF WS-IDPTYP  = 'RW2'                                                
426000       IF DCS-KDDC = SPACE OR DCS-DDC                                     
426100         MOVE WC-CDC-SE        TO INV-IDDC                                
426200       ELSE                                                               
426300         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
426400       END-IF                                                             
426500       IF IN-EKH-KVANTAL < +0                                             
426600         MOVE '003'            TO INV-IDPTYP                              
426700       COMPUTE INV-SUARTSTD =                                             
426800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
426900       ELSE                                                               
427000         MOVE '002'            TO INV-IDPTYP                              
427100         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
427200       END-IF                                                             
427300       MOVE SPACE TO WS-IDPTYP                                            
427400       MOVE 0                  TO INV-ADLAGOMR                            
427500       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
427600       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
427700     END-IF                                                               
427800     IF WS-IDPTYP  = 'RW1'                                                
427900       IF DCS-KDDC = SPACE OR DCS-DDC                                     
428000         MOVE WC-CDC-SE        TO INV-IDDC                                
428100       ELSE                                                               
428200         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
428300       END-IF                                                             
428400       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
428500       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
428600       MOVE 0                  TO INV-ADLAGOMR                            
428700       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
428800       MOVE '001'              TO INV-IDPTYP                              
428900       MOVE SPACE              TO WS-IDPTYP                               
429000     END-IF                                                               
429100     WRITE INV-POST  FROM INV-W51310                                      
429200                                                                          
429300     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
429400     MOVE 'W51350'            TO POSTSUM-FDNAMN                           
429500     MOVE 'W57058D8'          TO POSTSUM-DDNAMN2                          
429600     CALL POSTSUM USING POSTSUM-PARM                                      
429700     .                                                                    
429800     EJECT                                                                
429900                                                                          
430000 S13-GET-LANDING-COST SECTION.                                            
430100                                                                          
430200     MOVE '87'                   TO W-IDDC-B6                             
430300     PERFORM IMS-GU-WDB601                                                
430400     IF SEGMENT-FINNS                                                     
430500       PERFORM IMS-GNP-WDB617                                             
430600       IF SEGMENT-FINNS                                                   
430700         IF PROC-TILANDCO >  IN-EKH-DAVERDAT                              
430800           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
430900         ELSE                                                             
431000           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
431100         END-IF                                                           
431200       END-IF                                                             
431300     END-IF                                                               
431400     .                                                                    
431500     EJECT                                                                
431600 S80-GET-CURRENCY-RATE SECTION.                                           
431700     MOVE +0                  TO W-ANT                                    
431800     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
431900             BEFORE INITIAL ' '                                           
432000     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
432100                                                                          
432200     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
432300     PERFORM IMS-GU-WDL601                                                
432400     IF SEGMENT-SAKNAS                                                    
432500       CONTINUE                                                           
432600     ELSE                                                                 
432700       PERFORM IMS-GNP-WDL611                                             
432800       IF SEGMENT-SAKNAS                                                  
432900         CONTINUE                                                         
433000       ELSE                                                               
433100         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
433200                                   - INL-DAINLEV                          
433300         MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                   
433400         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
433500         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
433600         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
433700         MOVE WS-KDVALISO-USD       TO CURR-KDVALISO-ROW                  
433800         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
433900         IF CURR-KDSVAR = ' '                                             
434000           MOVE CURR-PRKURS-NEW     TO WS-PRKURS-USD3                     
434100         ELSE                                                             
434200           MOVE +1                  TO WS-PRKURS-USD3                     
434300         END-IF                                                           
434400       END-IF                                                             
434500     END-IF                                                               
434600     .                                                                    
434700     EJECT                                                                
434800 S81-GET-CURRENCY-RATE SECTION.                                           
434900     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
435000     MOVE 'USD'               TO CURR-KDVALISO-ROW                        
435100     IF IN-FIL-IDPGM = 'W4183300'                                         
435200       IF IN-EKH-DAAVIDAT > ZERO                                          
435300         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
435400         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
435500       ELSE                                                               
435600         MOVE WS-TIAA            TO WS-TIAA-CR                            
435700         MOVE WS-TIMM            TO WS-TIMM-CR                            
435800       END-IF                                                             
435900     ELSE                                                                 
436000       MOVE WS-TIAA              TO WS-TIAA-CR                            
436100       MOVE WS-TIMM              TO WS-TIMM-CR                            
436200     END-IF                                                               
436300     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
436400     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
436500     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
436600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
436700     IF CURR-KDSVAR = ' '                                                 
436800       IF IN-EKH-IDDISTR > ZERO                                           
436900         MOVE CURR-PRKURS-NEW TO WS-PRKURS-USD3                           
437000       ELSE                                                               
437100         IF WS-PRKURS = ZERO                                              
437200           MOVE 1           TO WS-PRKURS-USD3                             
437300         END-IF                                                           
437400       END-IF                                                             
437500     ELSE                                                                 
437600       MOVE 1               TO WS-PRKURS-USD3                             
437700     END-IF                                                               
437800     .                                                                    
437900     EJECT                                                                
438000                                                                          
438100* --- IMS SECTIONS ---                                                    
438200                                                                          
438300 IMS-GU-WDH521 SECTION.                                                   
438400     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
438500          DELIMITED BY SIZE INTO SSA1                                     
438600     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
438700          DELIMITED BY SIZE INTO SSA2                                     
438800     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
438900          DELIMITED BY SIZE INTO SSA3                                     
439000     MOVE '  '              TO GODK-STATUSKODER                           
439100     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
439200                                                   SSA2                   
439300                                                   SSA3                   
439400     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
439500                                                                          
439600     PERFORM IMS-STATUS-CONTROL                                           
439700     .                                                                    
439800                                                                          
439900 IMS-GNP-WDH531 SECTION.                                                  
440000     MOVE 'WDH531  '        TO SSA1                                       
440100     MOVE '  GE'            TO GODK-STATUSKODER                           
440200     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
440300     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
440400                               WS-STATUS                                  
440500     PERFORM IMS-STATUS-CONTROL                                           
440600     .                                                                    
440700     EJECT                                                                
440800                                                                          
440900 IMS-GU-WDB201 SECTION.                                                   
441000     STRING 'WDB201  (IDGMT    =' W-IDGMT-KEY ')'                         
441100          DELIMITED BY SIZE INTO SSA1                                     
441200     MOVE '  GE'                 TO GODK-STATUSKODER                      
441300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
441400     MOVE WDB2-STATUS-CODE      TO STATUS-WS                              
441500     PERFORM IMS-STATUS-CONTROL                                           
441600     .                                                                    
441700     EJECT                                                                
441800                                                                          
441900 IMS-GU-WDB101 SECTION.                                                   
442000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
442100          DELIMITED BY SIZE INTO SSA1                                     
442200     MOVE '  GE'               TO GODK-STATUSKODER                        
442300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
442400     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
442500     PERFORM IMS-STATUS-CONTROL                                           
442600     .                                                                    
442700     EJECT                                                                
442800                                                                          
442900 IMS-GU-5122 SECTION.                                                     
443000     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
443100            DELIMITED BY SIZE INTO SSA1                                   
443200     STRING 'WDGX5122(KEY5122  =' W-WDGXKEY-5122-X ')'                    
443300            DELIMITED BY SIZE INTO SSA2                                   
443400     MOVE '  GE'           TO GODK-STATUSKODER                            
443500     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5122 SSA1 SSA2            
443600     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
443700     PERFORM IMS-STATUS-CONTROL                                           
443800     .                                                                    
443900                                                                          
444000 IMS-GU-5121 SECTION.                                                     
444100     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
444200            DELIMITED BY SIZE INTO SSA1                                   
444300     MOVE '    '           TO GODK-STATUSKODER                            
444400     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5121 SSA1                 
444500     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
444600     PERFORM IMS-STATUS-CONTROL                                           
444700     .                                                                    
444800                                                                          
444900 IMS-GNP-5122 SECTION.                                                    
445000     STRING 'WDGX5122(KEY5122 >=' W-WDGXKEY-5122-MIN-X                    
445100                    '&KEY5122 <=' W-WDGXKEY-5122-MAX-X ')'                
445200            DELIMITED BY SIZE INTO SSA1                                   
445300     MOVE '  GE'           TO GODK-STATUSKODER                            
445400     CALL CBLTDLI USING GNP 5121-PCB DLI-IO-WDGX5122 SSA1                 
445500     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
445600     PERFORM IMS-STATUS-CONTROL                                           
445700     .                                                                    
445800     EJECT                                                                
445900                                                                          
446000 IMS-GU-WDB601    SECTION.                                                
446100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
446200          DELIMITED BY SIZE INTO SSA1                                     
446300     MOVE '  GE' TO GODK-STATUSKODER                                      
446400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
446500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
446600     PERFORM IMS-STATUS-CONTROL                                           
446700     IF SEGMENT-SAKNAS                                                    
446800        MOVE SPACE TO DCS-KDDC                                            
446900     END-IF                                                               
447000     .                                                                    
447100     EJECT                                                                
447200                                                                          
447300 IMS-GNP-WDB617 SECTION.                                                  
447400     MOVE 'WDB617   ' TO SSA1                                             
447500     MOVE '  GE'        TO GODK-STATUSKODER                               
447600     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
447700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
447800     PERFORM IMS-STATUS-CONTROL                                           
447900     .                                                                    
448000     SKIP3                                                                
448100                                                                          
448200 IMS-GU-WDL601   SECTION.                                                 
448300     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
448400          DELIMITED BY SIZE INTO SSA1                                     
448500     MOVE '  GE' TO GODK-STATUSKODER                                      
448600     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
448700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
448800     PERFORM IMS-STATUS-CONTROL                                           
448900     .                                                                    
449000     SKIP3                                                                
449100                                                                          
449200 IMS-GNP-WDL611   SECTION.                                                
449300     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
449400          DELIMITED BY SIZE INTO SSA1                                     
449500     MOVE '  GE' TO GODK-STATUSKODER                                      
449600     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
449700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
449800     PERFORM IMS-STATUS-CONTROL                                           
449900     .                                                                    
450000     SKIP3                                                                
450100 IMS-STATUS-CONTROL SECTION.                                              
450200     SET STATUS-IX TO 1                                                   
450300     SEARCH GODK-STATUS                                                   
450400       AT END                                                             
450500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
450600           DELIMITED BY SIZE INTO FELTEXT                                 
450700         DISPLAY FELTEXT                                                  
450800         CALL FELLOG                                                      
450900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
451000         CONTINUE                                                         
451100     END-SEARCH                                                           
451200     .                                                                    
