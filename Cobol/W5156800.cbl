000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5156800.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20170823.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       -PGM LÄSER KONTROLLERADE/KOMPLETTERADE EKONOMISKA                 
000900*        HÄNDELSETRANSAKTIONER OCH MATCHAR DESSA MOT                      
001000*        EKONOMISKA STYRPARAMETRAR FÖR ATT I SLUTÄNDEN                    
001100*        PRODUCERA POSTER TILL R3 I FORM AV                               
001200*        1 "LINE RECORD" HUVUDBOK               (PTYP 610)                
001300*        2 "LINE RECORD" KUNDRESKONTRA          (PTYP 310)                
001400*        3 "LINE RECORD" LEVERANTÖRSRESKONTRA   (PTYP 210)                
001500*                                                                         
001600*       -PGM SKAPAR/SKRIVER ÄVEN FÖLJANDE POSTER TILL R3                  
001700*        1 "HEADER RECORD" HUVUDBOK             (PTYP 600)                
001800*        2 "HEADER RECORD" KUNDRESKONTRA        (PTYP 300)                
001900*        3 "HEADER RECORD" LEVERANTÖRSRESKONTRA (PTYP 200)                
002000*                                                                         
002100*       -PGM PLOCKAR UNDAN NY MÅNADS POSTER VID MÅNADSSKIFTE              
002200*        FÖR ATT TA IN DESSA VID NÄSTA KÖRNING.                           
002300*        (NY MÅNADS POSTER = DATUMKORTS MÅNAD + 1, OM DENNA ÄR            
002400*         LIKA MED IN-POSTENS DAVERDAT'S MÅNAD,                           
002500*         SKRIVS POSTEN PÅ UTFIL FÖR AT TAS IN NÄSTA KÖRNING).            
002600*                                                                         
002700*       -PROGRAMMET LÄSER      WDH5                                       
002800*                              WLBETC (WDB1)                              
002900*                              WLGMTA (WDB2)                              
003000*                              WL5121 (WDR1)                              
003100*                         MÅNADSKURSER WDG2                               
003200*                           DCREGISTER WDB6                               
003300*                                                                         
003400*    ABENDKODER:                                                          
003500*        U0016 -  . . . .                                                 
003600*        U1000 -  . . . .                                                 
003700*                                                                         
003800                                                                          
003900 ENVIRONMENT DIVISION.                                                    
004000                                                                          
004100 INPUT-OUTPUT SECTION.                                                    
004200                                                                          
004300 FILE-CONTROL.                                                            
004400*          --- KONTR./KOMPL. HÄNDELSETRANSAKTIONER                        
004500     SELECT W51566                     ASSIGN TO W51568D1.                
004600                                                                          
004700*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
004800     SELECT W51571A                    ASSIGN TO W51568D2.                
004900                                                                          
005000*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005100     SELECT W51572A                    ASSIGN TO W51568D3.                
005200                                                                          
005300*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005400     SELECT W51573A                    ASSIGN TO W51568D4.                
005500                                                                          
005600*          --- LOGG TILL ON-DEMAND                                        
005700     SELECT W51575                     ASSIGN TO W51568D5.                
005800                                                                          
005900*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006000     SELECT W51570                     ASSIGN TO W51568D6.                
006100                                                                          
006200*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006300     SELECT W5156N                     ASSIGN TO W51568D7.                
006400                                                                          
006500*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006600     SELECT W51310                     ASSIGN TO W51568D8.                
006700     EJECT                                                                
006800                                                                          
006900 DATA DIVISION.                                                           
007000                                                                          
007100 FILE SECTION.                                                            
007200 FD  W51566                                                               
007300     RECORDING       F                                                    
007400     BLOCK CONTAINS  0.                                                   
007500 01  SAP-POST.                                                            
007600*    03  -COPY WDR801        -L.                                          
007700     03 FILLER                   PIC X(6).                                
007800                                                                          
007900 FD  W51571A                                                              
008000     RECORDING       V                                                    
008100     BLOCK CONTAINS  0.                                                   
008200*01  71INIT-POST -COPY R3INIT20               -L.                         
008300*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008400*01  71LINE-POST -COPY R3LINE20               -L.                         
008500                                                                          
008600 FD  W51572A                                                              
008700     RECORDING       F                                                    
008800     BLOCK CONTAINS  0.                                                   
008900*01  72LINE-POST -COPY R3LINE20               -L.                         
009000                                                                          
009100 FD  W51573A                                                              
009200     RECORDING       V                                                    
009300     BLOCK CONTAINS  0.                                                   
009400*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009500*01  73LINE-POST -COPY R3LINE20               -L.                         
009600                                                                          
009700 FD  W51575                                                               
009800     RECORDING       F                                                    
009900     BLOCK CONTAINS  0.                                                   
010000*01  LOGG-POST   -COPY W51573                 -L.                         
010100                                                                          
010200 FD  W51570                                                               
010300     RECORDING       F                                                    
010400     BLOCK CONTAINS  0.                                                   
010500*01  AVST-POST   -COPY W51570                 -L.                         
010600                                                                          
010700 FD  W5156N                                                               
010800     RECORDING       F                                                    
010900     BLOCK CONTAINS  0.                                                   
011000                                                                          
011100 01  SAPUT-POST.                                                          
011200*    03  -COPY WDR801        -L.                                          
011300     03 FILLER                   PIC X(6).                                
011400                                                                          
011500 FD  W51310                                                               
011600     RECORDING       F                                                    
011700     BLOCK CONTAINS  0.                                                   
011800*01  POST -COPY W51310  -PRE  INV-   -L.                                  
011900                                                                          
012000     EJECT                                                                
012100 WORKING-STORAGE SECTION.                                                 
012200*    -- CHECKED BY WY2000                                                 
012300 77  IDPGM                        PIC X(8)    VALUE 'W5156800'.           
012400 77  JA                           PIC X       VALUE 'J'.                  
012500 77  NEJ                          PIC X       VALUE 'N'.                  
012600 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
012700 77  W51566-EOF-SW                PIC X       VALUE 'N'.                  
012800     88  END-OF-W51566                        VALUE 'J'.                  
012900 77  WS-HEADER-SW                 PIC X       VALUE 'N'.                  
013000 77  WS-LINE-SW                   PIC X       VALUE 'N'.                  
013100 77  WS-STATUS                    PIC XX      VALUE '  '.                 
013200 77  WS-LINE-AMOUNT               PIC S9(13)V99 COMP-3.                   
013300 77  SPAR-SUMMA-102-121           PIC S9(9)V99  COMP-3 VALUE ZERO.        
013400 77  SPAR-SUMMA-102-131           PIC S9(9)V99  COMP-3 VALUE ZERO.        
013500 77  SPAR-SUMMA-102-125           PIC S9(9)V99  COMP-3 VALUE ZERO.        
013600 77  WS-BELOPP                    PIC S9(7)V99  COMP-3.                   
013700 77  WS-LOP                       PIC 9       VALUE ZERO.                 
013800 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
013900 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
014000 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
014100 77  SPAR-LINE-ORDER              PIC X(12).                              
014200 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
014300 77  WS-RED-IDKST                 PIC X(10).                              
014400 77  WS-IDPTYP                    PIC X(3).                               
014500 77  WS-FAKTURA-DATUM             PIC X(16).                              
014600 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
014700 77  SPAR-SUMMA                   PIC S9(9)V99  COMP-3 VALUE ZERO.        
014800 77  SPAR-PRDMTRL                 PIC S9(9)V99  COMP-3 VALUE ZERO.        
014900 77  SPAR-PROVRPAL                PIC S9(9)V99  COMP-3 VALUE ZERO.        
015000 77  SPAR-PRDIRLON                PIC S9(9)V99  COMP-3 VALUE ZERO.        
015100 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
015200 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
015300 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
015400 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
015500 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
015600 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
015700 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
015800 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
015900                                                                          
016000 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
016100       88  WDB6-A-FINNS                       VALUE 'J'.                  
016200       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
016300                                                                          
016400*01  -COPY WWPRODSL                                                       
016500                                                                          
016600*01  -COPY WWDCKONS                                                       
016700     EJECT                                                                
016800                                                                          
016900 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
017000*01  -COPY WWIDFTG                                                        
017100     EJECT                                                                
017200                                                                          
017300 01  FELTEXT                      PIC X(80).                              
017400 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
017500*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
017600     EJECT                                                                
017700                                                                          
017800 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
017900 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
018000     EJECT                                                                
018100 01  WS-IDDISTR-IDKUNDNR.                                                 
018200     03  FILLER                   PIC X(2)    VALUE SPACE.                
018300     03  WS-IDDISTR               PIC 9(4).                               
018400     03  WS-IDKUNDNR              PIC 9(6).                               
018500                                                                          
018600 01  WS-KDBETVIL                  PIC X(4).                               
018700 01  WS-KDVALISO-WDB1             PIC X(3).                               
018800 01  WS-KDVALISO                  PIC X(3).                               
018900 01  WS-KDVALISO-IN               PIC X(3) VALUE 'INR'.                   
019000 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
019100 01  WS-PRKURS-IN                 PIC S9(6)V9(5) COMP-3.                  
019200 01  WS-PRKURS-IN2                PIC S9(6)V9(5) COMP-3.                  
019300 01  WS-PRKURS-IN3                PIC S9(6)V9(5) COMP-3.                  
019400 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
019500 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
019600                                                                          
019700 01  WS-ALLOCATE.                                                         
019800     03  WS-ALLOCATE-DC           PIC X(2).                               
019900     03  WS-ALLOCATE-DISTR        PIC X(5).                               
020000     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
020100     03  FILLER                   PIC X(4)    VALUE SPACE.                
020200                                                                          
020300 01  WS-TEXT.                                                             
020400     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
020500     03  WS-TEXT-KDEKHHT          PIC X(3).                               
020600     03  WS-TEXT-KDEKSHT          PIC X(3).                               
020700     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
020800     03  FILLER                   PIC X(7)    VALUE SPACE.                
020900                                                                          
021000 01  WS-LINE-TEXT.                                                        
021100     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
021200     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
021300     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
021400     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
021500     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
021600     03  FILLER                   PIC X(22)   VALUE SPACE.                
021700                                                                          
021800 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
021900 01  WS-PRCTR.                                                            
022000     03  WS-PRCTR-PRODSL          PIC X(2).                               
022100     03  FILLER                   PIC X(1).                               
022200     03  FILLER                   PIC X(7).                               
022300                                                                          
022400 01  WS-R3-ACCOUNT.                                                       
022500     03  WS-R3-ACCOUNT-ALFA.                                              
022600         05 FILLER                PIC X(4).                               
022700         05 WS-R3-ACCOUNT-6       PIC X(6).                               
022800     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
022900         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
023000                                                                          
023100 01  WS-ACCOUNT.                                                          
023200     03  FILLER                   PIC X(7).                               
023300     03  WS-ACCOUNT-4             PIC X(1).                               
023400     03  FILLER                   PIC X(2).                               
023500                                                                          
023600 01  SPAR-AREA.                                                           
023700     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
023800     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
023900     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
024000     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
024100                                                                          
024200 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
024300 01  FILLER REDEFINES DAGENS-DATUM.                                       
024400     03  DAGENS-DATUM-AAR         PIC 9(2).                               
024500     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
024600     03  DAGENS-DATUM-DAG         PIC 9(2).                               
024700                                                                          
024800 01  WS-NEW-MONTH                 PIC 9(2).                               
024900                                                                          
025000 01  WS-DAREGDAT.                                                         
025100     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
025200     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
025300                                                                          
025400 01  WS-TIREGDAT-TOT.                                                     
025500     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
025600     03  WS-TIREGDAT              PIC 9(6).                               
025700                                                                          
025800 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
025900 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
026000     EJECT                                                                
026100                                                                          
026200 01  DYNAMISKA-SUBPROGRAM.                                                
026300     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
026400     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
026500     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
026600     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
026700     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
026800     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
026900                                                                          
027000*    --- PARAMETRAR TILL ABEND                                            
027100 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
027200 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
027300 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
027400     EJECT                                                                
027500                                                                          
027600*    --- PARAMETRAR TILL DATKORT                                          
027700 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W51568'.             
027800                                                                          
027900 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
028000*01  -COPY WDATKORT                                                       
028100     EJECT                                                                
028200                                                                          
028300*    --- PARAMETRAR TILL POSTSUM                                          
028400*01  -COPY W0005   -PRE  POSTSUM-                                         
028500     EJECT                                                                
028600                                                                          
028700 01  FILLER                       PIC X(16) VALUE 'W510CURR-AREA'.        
028800*01  -COPY W510CURR                                                       
028900     EJECT                                                                
029000                                                                          
029100 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
029200*01  AREA -COPY WDR801           -PRE IN-                                 
029300*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
029400         05   IN-EKH-IDSYSMOT     PIC X(6).                               
029500                                                                          
029600     EJECT                                                                
029700 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
029800                                                                          
029900*01  -COPY R3LINE20              -PRE R3-                                 
030000*01  -COPY R3HEAD20              -PRE R3-                                 
030100*01  -COPY R3INIT20              -PRE R3-                                 
030200*01  -COPY W51573                -PRE LOGG-                               
030300*01  -COPY W51570                -PRE AVST-                               
030400*01  -COPY W517RW1               -PRE RW1-                                
030500*01  -COPY W517RW2               -PRE RW2-                                
030600*01  -COPY W51310                -PRE INV-                                
030700     EJECT                                                                
030800                                                                          
030900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031000 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
031100                                                                          
031200 01  NYCKLAR-TILL-DLI.                                                    
031300     03  W-WDH501KY-X.                                                    
031400         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
031500         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
031600     03  W-KDEKSHT-X.                                                     
031700         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
031800     03  W-KDEKNIVA-X.                                                    
031900         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
032000     03  W-WDH531KY-X.                                                    
032100         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
032200         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
032300     03  W-IDRADNR-X.                                                     
032400         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
032500                                                                          
032600     03  W-IDGMT-KEY.                                                     
032700         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
032800         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
032900                                                                          
033000     03  W-WDB101KY-X.                                                    
033100         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
033200         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
033300                                                                          
033400     03  W-WDGXKEY-5121-X.                                                
033500         05  FILLER               PIC X(4)    VALUE '5121'.               
033600         05  FILLER               PIC X(2)    VALUE '61'.                 
033700         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
033800     03  W-WDGXKEY-5122-X.                                                
033900         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
034000         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
034100     03  W-WDGXKEY-5122-MIN-X.                                            
034200         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
034300         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
034400     03  W-WDGXKEY-5122-MAX-X.                                            
034500         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
034600         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
034700                                                                          
034800     03  W-IDDC-B6-X.                                                     
034900         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
035000                                                                          
035100     03  W-IDARTNR-X.                                                     
035200         05 W-IDARTNR             PIC S9(9) COMP-3.                       
035300                                                                          
035400     03  W-IDFAKT-X.                                                      
035500         05 W-IDFAKT              PIC S9(7) COMP-3.                       
035600                                                                          
035700     EJECT                                                                
035800                                                                          
035900*    --- STATUS-KOD FRÅN IMS                                              
036000 01  STATUS-WS                    PIC XX.                                 
036100     88  SEGMENT-FINNS                        VALUE '  '.                 
036200     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
036300                                                                          
036400 01  GODK-STATUSKODER.                                                    
036500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036600                                                                          
036700 01  SSA1                         PIC X(128).                             
036800 01  SSA2                         PIC X(64).                              
036900 01  SSA3                         PIC X(64).                              
037000     EJECT                                                                
037100                                                                          
037200*    --- IMS FUNKTIONSKODER                                               
037300*01  -COPY W0003                                                          
037400     EJECT                                                                
037500                                                                          
037600*    ---  DLI INPUT-OUTPUT AREA                                           
037700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
037800 01  DLI-IO-WDH501.                                                       
037900*    03  -COPY WDH501                                                     
038000     EJECT                                                                
038100                                                                          
038200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
038300 01  DLI-IO-WDH511.                                                       
038400*    03  -COPY WDH511                                                     
038500     EJECT                                                                
038600                                                                          
038700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
038800 01  DLI-IO-WDH521.                                                       
038900*    03  -COPY WDH521                                                     
039000     EJECT                                                                
039100                                                                          
039200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
039300 01  DLI-IO-WDH531.                                                       
039400*    03  -COPY WDH531                                                     
039500     EJECT                                                                
039600                                                                          
039700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
039800 01  DLI-IO-WLBETC01.                                                     
039900*    03  -COPY WDB101                                                     
040000     EJECT                                                                
040100                                                                          
040200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
040300 01  DLI-IO-WLGMTA01.                                                     
040400*    03  -COPY WDB201                                                     
040500     EJECT                                                                
040600                                                                          
040700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
040800 01  DLI-IO-WDGX5121.                                                     
040900*    03  -COPY WDGX5121                                                   
041000     EJECT                                                                
041100                                                                          
041200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
041300 01  DLI-IO-WDGX5122.                                                     
041400*    03  -COPY WDGX5122                                                   
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
041800 01   DLI-IO-AREA-B601.                                                   
041900*     03  -COPY WDB601                                                    
042000     EJECT                                                                
042100 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
042200 01   DLI-IO-AREA-B617.                                                   
042300*     03  -COPY WDB617                                                    
042400     EJECT                                                                
042500 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
042600 01   DLI-IO-AREA-L601.                                                   
042700*     03  -COPY WDL601                                                    
042800     EJECT                                                                
042900 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
043000 01   DLI-IO-AREA-L611.                                                   
043100*     03  -COPY WDL611                                                    
043200     EJECT                                                                
043300 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6A1'.                
043400     SKIP3                                                                
043500     EJECT                                                                
043600                                                                          
043700 LINKAGE SECTION.                                                         
043800*01  -COPY W0008  -PRE WDH5-                                              
043900     05  FILLER                  PIC X.                                   
044000                                                                          
044100*01  -COPY W0008  -PRE GMTA-                                              
044200     05  FILLER                  PIC X.                                   
044300                                                                          
044400*01  -COPY W0008  -PRE BETC-                                              
044500     05  FILLER                  PIC X.                                   
044600                                                                          
044700*01  -COPY W0008  -PRE 5121-                                              
044800     05  FILLER                  PIC X.                                   
044900                                                                          
045000*01  -COPY W0008  -PRE WDG2-                                              
045100     05  FILLER                  PIC X.                                   
045200                                                                          
045300*01  -COPY W0008  -PRE WDB6-                                              
045400     05  FILLER                  PIC X.                                   
045500                                                                          
045600*01  -COPY W0008  -PRE WDL6-                                              
045700     05  FILLER                  PIC X.                                   
045800                                                                          
045900                                                                          
046000     EJECT                                                                
046100                                                                          
046200 PROCEDURE DIVISION  USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
046300                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
046400 MAIN SECTION.                                                            
046500     ENTRY 'DLITCBL' USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
046600                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
046700                                                                          
046800     PERFORM A-INIT                                                       
046900                                                                          
047000     PERFORM S01-READ-W51566                                              
047100     PERFORM UNTIL END-OF-W51566                                          
047200*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
047300       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
047400       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
047500       AND WS-NEW-MONTH > 01                                              
047600         PERFORM S60-WRITE-W5156N                                         
047700       ELSE                                                               
047800         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
047900         PERFORM S30-READ-DATABASE-B2-B1                                  
048000         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
048100           PERFORM C-EXECUTE                                              
048200         END-IF                                                           
048300       END-IF                                                             
048400       PERFORM S01-READ-W51566                                            
048500     END-PERFORM                                                          
048600                                                                          
048700     PERFORM Z-FINI                                                       
048800                                                                          
048900     MOVE ZERO TO RETURN-CODE                                             
049000     GOBACK                                                               
049100     .                                                                    
049200     EJECT                                                                
049300                                                                          
049400 A-INIT SECTION.                                                          
049500     OPEN INPUT  W51566                                                   
049600                                                                          
049700     OPEN OUTPUT W51570                                                   
049800                 W51571A                                                  
049900                 W51572A                                                  
050000                 W51573A                                                  
050100                 W51575                                                   
050200                 W5156N                                                   
050300                 W51310                                                   
050400                                                                          
050500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
050600     MOVE 20               TO RW1-DAVVREG(1:2)                            
050700     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
050800                              RW1-DAVVREG(3:2)                            
050900                              W-DATE-AAMM(1:2)                            
051000                              WS-TIAA                                     
051100     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
051200                              W-DATE-AAMM(3:2)                            
051300                              WS-NEW-MONTH                                
051400                              WS-TIMM                                     
051500     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
051600     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
051700     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
051800                                                                          
051900*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
052000*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
052100     IF WS-NEW-MONTH = 12                                                 
052200       MOVE 1              TO WS-NEW-MONTH                                
052300     ELSE                                                                 
052400       ADD 1               TO WS-NEW-MONTH                                
052500*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
052600***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
052700***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
052800***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
052900***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
053000       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
053100       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
053200         ADD 1             TO WS-NEW-MONTH                                
053300         ADD 1             TO WS-TIMM                                     
053400         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
053500       END-IF                                                             
053600     END-IF                                                               
053700                                                                          
053800     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
053900                                                                          
054000     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
054100                                                                          
054200     ACCEPT DAGENS-KLOCKA FROM TIME                                       
054300     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
054400                                                                          
054500     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
054600     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
054700     MOVE 'M'                   TO CURR-KDVALTYP                          
054800     MOVE WS-KDVALISO-IN        TO CURR-KDVALISO-ROW                      
054900     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
055000     IF CURR-KDSVAR = ' '                                                 
055100       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-IN                           
055200     ELSE                                                                 
055300       MOVE 1                   TO WS-PRKURS-IN                           
055400     END-IF                                                               
055500     COMPUTE WS-PRKURS-IN2 ROUNDED = 1 / WS-PRKURS-IN                     
055600     MOVE WS-PRKURS-IN          TO WS-PRKURS-IN3                          
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000 C-EXECUTE SECTION.                                                       
056100     MOVE WC-IDFTG-IN           TO W-IDFTG                                
056200     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
056300     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
056400     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
056500     PERFORM IMS-GU-WDH521                                                
056600     PERFORM IMS-GNP-WDH531                                               
056700                                                                          
056800     PERFORM S13-GET-LANDING-COST                                         
056900                                                                          
057000     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
057100     IF IN-EKH-IDDISTR > ZERO                                             
057200       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
057300     ELSE                                                                 
057400       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
057500     END-IF                                                               
057600     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
057700                                                                          
057800* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
057900* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
058000* BOKFÖRINGEN I SAP SKER DOCK ALLTID I INR, DÄRFÖR BYTET HÄR:             
058100*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
058200*      MOVE 'INR'               TO WS-KDVALISO                            
058300*    END-IF                                                               
058400                                                                          
058500     IF  ((IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                               
058600     AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                                
058700     OR (IN-EKH-KDEKHHT = '303'                                           
058800     AND IN-EKH-KDEKSHT = '301')                                          
058900     OR (IN-EKH-KDEKHHT = '303'                                           
059000     AND IN-EKH-KDEKSHT = '307'))                                         
059100       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
059200     ELSE                                                                 
059300       MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                             
059400       MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                             
059500       IF WS-LOP = 9                                                      
059600         MOVE ZERO  TO WS-LOP                                             
059700       ELSE                                                               
059800         ADD +1     TO WS-LOP                                             
059900       END-IF                                                             
060000       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
060100     END-IF                                                               
060200* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
060300     IF SYST-IDPTYP = '210'                                               
060400       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
060500     ELSE                                                                 
060600       IF SYST-IDPTYP = '310'                                             
060700         PERFORM CC-CREATE-WRITE-HEADER-AR                                
060800       ELSE                                                               
060900* TEST OM BRYTNING PÅ VERIFIKATION                                        
061000         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
061100         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
061200         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
061300         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
061400           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
061500           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
061600           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
061700           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
061800           IF (IN-EKH-KDEKHHT = '102'                                     
061900           AND IN-EKH-KDEKSHT = '121')                                    
062000           OR (IN-EKH-KDEKHHT = '102'                                     
062100           AND IN-EKH-KDEKSHT = '122')                                    
062200           OR (IN-EKH-KDEKHHT = '102'                                     
062300           AND IN-EKH-KDEKSHT = '131')                                    
062400           OR (IN-EKH-KDEKHHT = '102'                                     
062500           AND IN-EKH-KDEKSHT = '132')                                    
062600             PERFORM S80-GET-CURRENCY-RATE                                
062700           END-IF                                                         
062800           IF (IN-EKH-KDEKHHT = '303'                                     
062900           AND IN-EKH-KDEKSHT = '371')                                    
063000             PERFORM S81-GET-CURRENCY-RATE                                
063100           END-IF                                                         
063200*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
063300*   HEADER-POST TILL HUVUDBOKEN                                           
063400           IF (IN-EKH-KDEKHHT = '102'                                     
063500           AND IN-EKH-KDEKSHT = '120')                                    
063600           OR (IN-EKH-KDEKHHT = '102'                                     
063700           AND IN-EKH-KDEKSHT = '124')                                    
063800           OR (IN-EKH-KDEKHHT = '102'                                     
063900           AND IN-EKH-KDEKSHT = '125')                                    
064000           OR (IN-EKH-KDEKHHT = '102'                                     
064100           AND IN-EKH-KDEKSHT = '130')                                    
064200           OR (IN-EKH-KDEKHHT = '102'                                     
064300           AND IN-EKH-KDEKSHT = '134')                                    
064400           OR (IN-EKH-KDEKHHT = '103'                                     
064500           AND IN-EKH-KDEKSHT = '102')                                    
064600           OR (IN-EKH-KDEKHHT = '204'                                     
064700           AND IN-EKH-KDEKSHT = '301')                                    
064800           OR (IN-EKH-KDEKHHT = '303'                                     
064900           AND IN-EKH-KDEKSHT = '301')                                    
065000           OR (IN-EKH-KDEKHHT = '303'                                     
065100           AND IN-EKH-KDEKSHT = '307')                                    
065200           OR (IN-EKH-KDEKHHT = '303'                                     
065300           AND IN-EKH-KDEKSHT = '371')                                    
065400           OR (IN-EKH-KDEKHHT = '303'                                     
065500           AND IN-EKH-KDEKSHT = '3XX')                                    
065600             CONTINUE                                                     
065700           ELSE                                                           
065800             PERFORM CA-CREATE-WRITE-HEADER-GL                            
065900           END-IF                                                         
066000         END-IF                                                           
066100       END-IF                                                             
066200     END-IF                                                               
066300                                                                          
066400**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
066500     MOVE WS-STATUS TO STATUS-WS                                          
066600     PERFORM UNTIL SEGMENT-SAKNAS                                         
066700       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
066800                                                                          
066900* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
067000       IF SYST-IDPTYP = '610'                                             
067100         PERFORM CD-BUILD-COMMON-610-PART                                 
067200         PERFORM CE-SCHEDULE-LINE-GL                                      
067300       ELSE                                                               
067400         IF SYST-IDPTYP = '210'                                           
067500           PERFORM CF-BUILD-COMMON-210-PART                               
067600           PERFORM CG-SCHEDULE-LINE-AP                                    
067700         ELSE                                                             
067800           IF SYST-IDPTYP = '310'                                         
067900             PERFORM CH-BUILD-COMMON-310-PART                             
068000             PERFORM CI-SCHEDULE-LINE-AR                                  
068100           END-IF                                                         
068200         END-IF                                                           
068300       END-IF                                                             
068400       PERFORM IMS-GNP-WDH531                                             
068500     END-PERFORM                                                          
068600     .                                                                    
068700     EJECT                                                                
068800                                                                          
068900 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
069000     MOVE SPACE                   TO R3-HEAD-R3                           
069100     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
069200     MOVE 'IN07'                  TO R3-HEAD-COMPANY-CODE                 
069300     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
069400     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
069500     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
069600     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
069700     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
069800       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
069900     ELSE                                                                 
070000       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
070100     END-IF                                                               
070200     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
070300     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
070400     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
070500     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
070600     IF WS-KDVALISO = 'INR'                                               
070700       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
070800     ELSE                                                                 
070900       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
071000       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
071100       IF CURR-KDSVAR = ' '                                               
071200         IF IN-EKH-IDDISTR > ZERO                                         
071300           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
071400         ELSE                                                             
071500           MOVE 1               TO WS-PRKURS                              
071600         END-IF                                                           
071700       ELSE                                                               
071800         MOVE 1                 TO WS-PRKURS                              
071900       END-IF                                                             
072000       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
072100                                       CURR-REVALUTA-TO                   
072200       IF CURR-REVALUTA-TO = +1                                           
072300         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
072400       END-IF                                                             
072500       IF CURR-REVALUTA-TO = +10                                          
072600         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
072700       END-IF                                                             
072800       IF CURR-REVALUTA-TO = +100                                         
072900         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
073000       END-IF                                                             
073100     END-IF                                                               
073200     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
073300     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
073400     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
073500     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
073600     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
073700     MOVE JA                      TO WS-HEADER-SW                         
073800     MOVE NEJ                     TO WS-LINE-SW                           
073900                                                                          
074000* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
074100     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
074200     OR (IN-EKH-KDEKHHT = '303'                                           
074300     AND IN-EKH-KDEKSHT = '391')                                          
074400     OR (IN-EKH-KDEKHHT = '102'                                           
074500     AND IN-EKH-KDEKSHT = '121')                                          
074600     OR (IN-EKH-KDEKHHT = '102'                                           
074700     AND IN-EKH-KDEKSHT = '131')                                          
074800       PERFORM S004-WRITE-W51573A-HEAD                                    
074900     ELSE                                                                 
075000       PERFORM S002-WRITE-W51571A-HEAD                                    
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
075600     MOVE SPACE                   TO R3-HEAD-R3                           
075700     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
075800     MOVE 'IN07'                  TO R3-HEAD-COMPANY-CODE                 
075900                                     R3-HEAD-CONTROL-AREA                 
076000     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
076100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
076200     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
076300     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
076400     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
076500     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
076600       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
076700     ELSE                                                                 
076800       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
076900     END-IF                                                               
077000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
077100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
077200     IF (IN-EKH-KDEKHHT = '102'                                           
077300     AND IN-EKH-KDEKSHT = '120')                                          
077400     OR (IN-EKH-KDEKHHT = '102'                                           
077500     AND IN-EKH-KDEKSHT = '124')                                          
077600     OR (IN-EKH-KDEKHHT = '102'                                           
077700     AND IN-EKH-KDEKSHT = '125')                                          
077800     OR (IN-EKH-KDEKHHT = '102'                                           
077900     AND IN-EKH-KDEKSHT = '130')                                          
078000     OR (IN-EKH-KDEKHHT = '102'                                           
078100     AND IN-EKH-KDEKSHT = '134')                                          
078200     OR (IN-EKH-KDEKHHT = '103'                                           
078300     AND IN-EKH-KDEKSHT = '102')                                          
078400       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
078500       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
078600     ELSE                                                                 
078700       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
078800       MOVE WS-PRKURS-IN2         TO R3-HEAD-EXCHANGE-RATE                
078900       MOVE 'INR'                 TO CURR-KDVALISO-ROW                    
079000       IF IN-FIL-IDPGM = 'W4183300'                                       
079100         IF IN-EKH-DAAVIDAT > ZERO                                        
079200           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
079300           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
079400         ELSE                                                             
079500           MOVE WS-TIAA              TO WS-TIAA-CR                        
079600           MOVE WS-TIMM              TO WS-TIMM-CR                        
079700         END-IF                                                           
079800       ELSE                                                               
079900         MOVE WS-TIAA                TO WS-TIAA-CR                        
080000         MOVE WS-TIMM                TO WS-TIMM-CR                        
080100       END-IF                                                             
080200       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
080300       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
080400       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
080500       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
080600       IF CURR-KDSVAR = ' '                                               
080700         IF IN-EKH-IDDISTR > ZERO                                         
080800           MOVE CURR-PRKURS-NEW TO WS-PRKURS-IN                           
080900         ELSE                                                             
081000           IF WS-PRKURS = ZERO                                            
081100             MOVE 1             TO WS-PRKURS-IN                           
081200           END-IF                                                         
081300         END-IF                                                           
081400       ELSE                                                               
081500         MOVE 1                 TO WS-PRKURS-IN                           
081600       END-IF                                                             
081700       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-IN *                     
081800                                       CURR-REVALUTA-TO                   
081900       END-COMPUTE                                                        
082000       IF CURR-REVALUTA-TO = +1                                           
082100         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
082200       END-IF                                                             
082300       IF CURR-REVALUTA-TO = +10                                          
082400         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
082500       END-IF                                                             
082600       IF CURR-REVALUTA-TO = +100                                         
082700         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
082800       END-IF                                                             
082900     END-IF                                                               
083000     IF (IN-EKH-KDEKHHT = '102'                                           
083100     AND IN-EKH-KDEKSHT = '120')                                          
083200     OR (IN-EKH-KDEKHHT = '102'                                           
083300     AND IN-EKH-KDEKSHT = '124')                                          
083400     OR (IN-EKH-KDEKHHT = '102'                                           
083500     AND IN-EKH-KDEKSHT = '125')                                          
083600     OR (IN-EKH-KDEKHHT = '102'                                           
083700     AND IN-EKH-KDEKSHT = '130')                                          
083800     OR (IN-EKH-KDEKHHT = '102'                                           
083900     AND IN-EKH-KDEKSHT = '134')                                          
084000     OR (IN-EKH-KDEKHHT = '303'                                           
084100     AND IN-EKH-KDEKSHT = '301')                                          
084200     OR (IN-EKH-KDEKHHT = '303'                                           
084300     AND IN-EKH-KDEKSHT = '307')                                          
084400     OR (IN-EKH-KDEKHHT = '303'                                           
084500     AND IN-EKH-KDEKSHT = '371')                                          
084600     OR (IN-EKH-KDEKHHT = '303'                                           
084700     AND IN-EKH-KDEKSHT = '3XX')                                          
084800       MOVE 'INR'                 TO R3-HEAD-CURRENCY                     
084900       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
085000     END-IF                                                               
085100     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
085200     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
085300     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
085400     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
085500     MOVE JA                      TO WS-HEADER-SW                         
085600     MOVE NEJ                     TO WS-LINE-SW                           
085700                                                                          
085800* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W51573A                       
085900       PERFORM S004-WRITE-W51573A-HEAD                                    
086000     .                                                                    
086100     EJECT                                                                
086200                                                                          
086300 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
086400     MOVE SPACE                   TO R3-HEAD-R3                           
086500     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
086600     MOVE 'IN07'                  TO R3-HEAD-COMPANY-CODE                 
086700                                     R3-HEAD-CONTROL-AREA                 
086800     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
086900     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
087000     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
087100     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
087200     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
087300     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
087400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
087500     ELSE                                                                 
087600       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
087700     END-IF                                                               
087800     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
087900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
088000     IF (IN-EKH-KDEKHHT = '204'                                           
088100     AND IN-EKH-KDEKSHT = '301')                                          
088200       MOVE 'INR'                 TO R3-HEAD-CURRENCY                     
088300       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
088400     ELSE                                                                 
088500       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
088600       MOVE WS-PRKURS-IN2         TO R3-HEAD-EXCHANGE-RATE                
088700       MOVE 'INR'                 TO CURR-KDVALISO-ROW                    
088800       IF IN-FIL-IDPGM = 'W4183300'                                       
088900         IF IN-EKH-DAAVIDAT > ZERO                                        
089000           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
089100           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
089200         ELSE                                                             
089300           MOVE WS-TIAA              TO WS-TIAA-CR                        
089400           MOVE WS-TIMM              TO WS-TIMM-CR                        
089500         END-IF                                                           
089600       ELSE                                                               
089700         MOVE WS-TIAA                TO WS-TIAA-CR                        
089800         MOVE WS-TIMM                TO WS-TIMM-CR                        
089900       END-IF                                                             
090000       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
090100       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
090200       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
090300       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
090400       IF CURR-KDSVAR = ' '                                               
090500         IF IN-EKH-IDDISTR > ZERO                                         
090600           MOVE CURR-PRKURS-NEW TO WS-PRKURS-IN                           
090700         ELSE                                                             
090800           IF WS-PRKURS = ZERO                                            
090900             MOVE 1             TO WS-PRKURS-IN                           
091000           END-IF                                                         
091100         END-IF                                                           
091200       ELSE                                                               
091300         MOVE 1                 TO WS-PRKURS-IN                           
091400       END-IF                                                             
091500       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-IN *                     
091600                                       CURR-REVALUTA-TO                   
091700       END-COMPUTE                                                        
091800       IF CURR-REVALUTA-TO = +1                                           
091900         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
092000       END-IF                                                             
092100       IF CURR-REVALUTA-TO = +10                                          
092200         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
092300       END-IF                                                             
092400       IF CURR-REVALUTA-TO = +100                                         
092500         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
092600       END-IF                                                             
092700     END-IF                                                               
092800     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
092900     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
093000     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
093100     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
093200     MOVE JA                      TO WS-HEADER-SW                         
093300     MOVE NEJ                     TO WS-LINE-SW                           
093400                                                                          
093500* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W51573A                       
093600       PERFORM S004-WRITE-W51573A-HEAD                                    
093700     .                                                                    
093800     EJECT                                                                
093900                                                                          
094000 CD-BUILD-COMMON-610-PART SECTION.                                        
094100     MOVE SPACE               TO R3-LINE-R3                               
094200     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
094300                                 R3-LINE-DUE-DATE                         
094400                                 R3-LINE-AMOUNT                           
094500                                 R3-LINE-AMOUNT-LC                        
094600                                 R3-LINE-TAX-AMOUNT                       
094700                                 R3-LINE-TAX-AMOUNT-LC                    
094800                                 R3-LINE-NUMBER-OF-DAYS                   
094900                                 R3-LINE-QUANTITY                         
095000                                 R3-LINE-SAMNR                            
095100     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
095200     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
095300     MOVE 'IN07'              TO R3-LINE-COMPANY-CODE                     
095400     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
095500     IF SYST-KDPOST = '50'                                                
095600       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
095700     ELSE                                                                 
095800       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
095900     END-IF                                                               
096000     IF SYST-IDPRCTR NOT = SPACE                                          
096100       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
096200       IF WS-PRCTR-PRODSL = '??'                                          
096300         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
096400         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
096500       END-IF                                                             
096600       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
096700     END-IF                                                               
096800     .                                                                    
096900     EJECT                                                                
097000                                                                          
097100 CE-SCHEDULE-LINE-GL SECTION.                                             
097200     MOVE NEJ                     TO WS-HEADER-SW                         
097300     MOVE JA                      TO WS-LINE-SW                           
097400     EVALUATE IN-EKH-KDEKHHT                                              
097500     WHEN '102'                                                           
097600          PERFORM CEB-MAIN-EVENT-102                                      
097700     WHEN '103'                                                           
097800          PERFORM CEC-MAIN-EVENT-103                                      
097900     WHEN '201'                                                           
098000          PERFORM CED-MAIN-EVENT-201                                      
098100     WHEN '203'                                                           
098200          PERFORM CEF-MAIN-EVENT-203                                      
098300     WHEN '204'                                                           
098400          PERFORM CEG-MAIN-EVENT-204                                      
098500     WHEN '302'                                                           
098600          PERFORM CEI-MAIN-EVENT-302                                      
098700     WHEN '303'                                                           
098800          PERFORM CEJ-MAIN-EVENT-303                                      
098900     WHEN '401'                                                           
099000          PERFORM CEK-MAIN-EVENT-401                                      
099100     WHEN '402'                                                           
099200          PERFORM CEL-MAIN-EVENT-402                                      
099300     WHEN '403'                                                           
099400          PERFORM CEM-MAIN-EVENT-403                                      
099500     WHEN '404'                                                           
099600          PERFORM CEN-MAIN-EVENT-404                                      
099700     END-EVALUATE                                                         
099800     .                                                                    
099900     EJECT                                                                
100000                                                                          
100100 CEB-MAIN-EVENT-102 SECTION.                                              
100200     EVALUATE IN-EKH-KDEKSHT                                              
100300     WHEN '102'                                                           
100400          PERFORM CEBB-SUB-EVENT-102-102                                  
100500     WHEN '120'                                                           
100600          PERFORM CEBD-SUB-EVENT-102-120                                  
100700     WHEN '121'                                                           
100800          PERFORM CEBD-SUB-EVENT-102-121                                  
100900     WHEN '122'                                                           
101000          PERFORM CEBD-SUB-EVENT-102-122                                  
101100     WHEN '123'                                                           
101200          PERFORM CEBD-SUB-EVENT-102-123                                  
101300     WHEN '124'                                                           
101400          PERFORM CEBD-SUB-EVENT-102-124                                  
101500     WHEN '125'                                                           
101600          PERFORM CEBD-SUB-EVENT-102-125                                  
101700     WHEN '130'                                                           
101800          PERFORM CEBE-SUB-EVENT-102-130                                  
101900     WHEN '131'                                                           
102000          PERFORM CEBE-SUB-EVENT-102-131                                  
102100     WHEN '132'                                                           
102200          PERFORM CEBE-SUB-EVENT-102-132                                  
102300     WHEN '134'                                                           
102400          PERFORM CEBE-SUB-EVENT-102-134                                  
102500     END-EVALUATE                                                         
102600     .                                                                    
102700     EJECT                                                                
102800                                                                          
102900 CEBB-SUB-EVENT-102-102 SECTION.                                          
103000     EVALUATE IN-EKH-KDEKNIVA                                             
103100     WHEN 'DET'                                                           
103200       IF SYST-IDSEKVNR = 1                                               
103300         IF IN-EKH-KVANTAL > 0                                            
103400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
103500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
103600           COMPUTE R3-LINE-AMOUNT-LC =                                    
103700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
103800           IF IN-EKH-KDVALISO = 'INR'                                     
103900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
104000           END-IF                                                         
104100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
104200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
104300           MOVE SPACE               TO WS-ALLOCATE-REF                    
104400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
104500           PERFORM S02-WRITE-W51571A                                      
104600         END-IF                                                           
104700       END-IF                                                             
104800                                                                          
104900       IF SYST-IDSEKVNR = 2                                               
105000         IF IN-EKH-KVANTAL > 0                                            
105100           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
105200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
105300           COMPUTE R3-LINE-AMOUNT-LC =                                    
105400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
105500           IF IN-EKH-KDVALISO = 'INR'                                     
105600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
105700           END-IF                                                         
105800           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
105900           IF WS-RED-IDKST > SPACE                                        
106000             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
106100             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
106200           ELSE                                                           
106300             MOVE SPACE             TO R3-LINE-COST-CENTER                
106400           END-IF                                                         
106500           PERFORM S02-WRITE-W51571A                                      
106600         END-IF                                                           
106700       END-IF                                                             
106800                                                                          
106900       IF SYST-IDSEKVNR = 3                                               
107000         IF IN-EKH-KVANTAL < 0                                            
107100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
107200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
107300           COMPUTE R3-LINE-AMOUNT-LC =                                    
107400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
107500           IF IN-EKH-KDVALISO = 'INR'                                     
107600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
107700           END-IF                                                         
107800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
107900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
108000           MOVE SPACE               TO WS-ALLOCATE-REF                    
108100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
108200           PERFORM S02-WRITE-W51571A                                      
108300         END-IF                                                           
108400       END-IF                                                             
108500                                                                          
108600       IF SYST-IDSEKVNR = 4                                               
108700         IF IN-EKH-KVANTAL < 0                                            
108800           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
108900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
109000           COMPUTE R3-LINE-AMOUNT-LC =                                    
109100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
109200           IF IN-EKH-KDVALISO = 'INR'                                     
109300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
109400           END-IF                                                         
109500           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
109600           IF WS-RED-IDKST > SPACE                                        
109700             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
109800             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
109900           ELSE                                                           
110000             MOVE SPACE             TO R3-LINE-COST-CENTER                
110100           END-IF                                                         
110200           PERFORM S02-WRITE-W51571A                                      
110300         END-IF                                                           
110400       END-IF                                                             
110500     END-EVALUATE                                                         
110600     .                                                                    
110700     EJECT                                                                
110800                                                                          
110900 CEBD-SUB-EVENT-102-120 SECTION.                                          
111000     EVALUATE IN-EKH-KDEKNIVA                                             
111100     WHEN 'DET'                                                           
111200       IF SYST-IDSEKVNR = 1                                               
111300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
111400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
111500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
111600          IN-EKH-PRARTNTO * -1                                            
111700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
111800         PERFORM S03-WRITE-W51572                                         
111900       END-IF                                                             
112000                                                                          
112100     WHEN 'AVDR'                                                          
112200     WHEN 'FÖRS'                                                          
112300     WHEN 'LEG'                                                           
112400     WHEN 'FRAKT'                                                         
112500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
112600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
112700       MOVE SYST-IDKST          TO WS-RED-IDKST                           
112800       IF WS-RED-IDKST > SPACE                                            
112900         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
113000         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
113100       ELSE                                                               
113200         MOVE SPACE             TO R3-LINE-COST-CENTER                    
113300       END-IF                                                             
113400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
113500               IN-EKH-SUBEL * -1                                          
113600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
113700       PERFORM S04-WRITE-W51573A                                          
113800                                                                          
113900     WHEN 'EMB'                                                           
114000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
114100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
114200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
114300               IN-EKH-SUBEL * -1                                          
114400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
114500       PERFORM S04-WRITE-W51573A                                          
114600                                                                          
114700     WHEN 'DDI'                                                           
114800       IF IN-EKH-SUBEL > ZERO                                             
114900         IF SYST-IDSEKVNR = 1                                             
115000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
115100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
115200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
115300                   IN-EKH-SUBEL                                           
115400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
115500           PERFORM S04-WRITE-W51573A                                      
115600         END-IF                                                           
115700       ELSE                                                               
115800         IF SYST-IDSEKVNR = 2                                             
115900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
116000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
116100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
116200                   IN-EKH-SUBEL                                           
116300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
116400           PERFORM S04-WRITE-W51573A                                      
116500         END-IF                                                           
116600       END-IF                                                             
116700     END-EVALUATE                                                         
116800     .                                                                    
116900     EJECT                                                                
117000                                                                          
117100 CEBD-SUB-EVENT-102-121 SECTION.                                          
117200     EVALUATE IN-EKH-KDEKNIVA                                             
117300     WHEN 'DET'                                                           
117400       IF SYST-IDSEKVNR = 1                                               
117500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
117600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
117700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
117800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
117900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
118000         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-121                        
118100         PERFORM S03-WRITE-W51572                                         
118200       END-IF                                                             
118300                                                                          
118400       IF SYST-IDSEKVNR = 2                                               
118500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
118600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
118700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
118800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
118900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
119000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
119100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
119200         MOVE SPACE               TO WS-ALLOCATE-REF                      
119300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
119400         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
119500         IF WS-RED-IDKST > SPACE                                          
119600           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
119700           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
119800         ELSE                                                             
119900           MOVE SPACE             TO R3-LINE-COST-CENTER                  
120000         END-IF                                                           
120100         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
120200         PERFORM S03-WRITE-W51572                                         
120300       END-IF                                                             
120400                                                                          
120500       IF SYST-IDSEKVNR = 3                                               
120600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
120700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
120800         COMPUTE R3-LINE-AMOUNT-LC =                                      
120900      (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3                  
121000                      * WS-MARKUP                                         
121100         IF IN-EKH-KDVALISO = 'INR'                                       
121200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
121300         END-IF                                                           
121400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
121500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
121600         MOVE SPACE               TO WS-ALLOCATE-REF                      
121700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
121800         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
121900         IF WS-RED-IDKST > SPACE                                          
122000           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
122100           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
122200         ELSE                                                             
122300           MOVE SPACE             TO R3-LINE-COST-CENTER                  
122400         END-IF                                                           
122500         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
122600         PERFORM S03-WRITE-W51572                                         
122700       END-IF                                                             
122800                                                                          
122900       IF SYST-IDSEKVNR = 4                                               
123000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
123100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
123200         COMPUTE R3-LINE-AMOUNT-LC =                                      
123300      (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3                  
123400                      * WS-MARKUP                                         
123500         IF IN-EKH-KDVALISO = 'INR'                                       
123600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
123700         END-IF                                                           
123800         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
123900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
124000         MOVE SPACE               TO WS-ALLOCATE-REF                      
124100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
124200         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
124300         IF WS-RED-IDKST > SPACE                                          
124400           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
124500           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
124600         ELSE                                                             
124700           MOVE SPACE             TO R3-LINE-COST-CENTER                  
124800         END-IF                                                           
124900         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
125000         PERFORM S03-WRITE-W51572                                         
125100       END-IF                                                             
125200                                                                          
125300     WHEN 'AVDR'                                                          
125400     WHEN 'FÖRS'                                                          
125500     WHEN 'LEG'                                                           
125600     WHEN 'FRAKT'                                                         
125700       IF SYST-IDSEKVNR = 1                                               
125800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
125900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
126000         MOVE SYST-IDKST        TO WS-RED-IDKST                           
126100         IF WS-RED-IDKST > SPACE                                          
126200           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
126300           MOVE WS-RED-IDKST(1:5)                                         
126400                                TO R3-LINE-COST-CENTER(3:5)               
126500         ELSE                                                             
126600           MOVE SPACE           TO R3-LINE-COST-CENTER                    
126700         END-IF                                                           
126800         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
126900                 IN-EKH-SUBEL                                             
127000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
127100         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-121                        
127200         PERFORM S04-WRITE-W51573A                                        
127300       END-IF                                                             
127400                                                                          
127500       IF SYST-IDSEKVNR = 2                                               
127600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
127700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
127800         MOVE SYST-IDKST        TO WS-RED-IDKST                           
127900         IF WS-RED-IDKST > SPACE                                          
128000           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
128100           MOVE WS-RED-IDKST(1:5)                                         
128200                                TO R3-LINE-COST-CENTER(3:5)               
128300         ELSE                                                             
128400           MOVE SPACE           TO R3-LINE-COST-CENTER                    
128500         END-IF                                                           
128600         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
128700                 IN-EKH-SUBEL                                             
128800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
128900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
129000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
129100         MOVE SPACE               TO WS-ALLOCATE-REF                      
129200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
129300         PERFORM S04-WRITE-W51573A                                        
129400       END-IF                                                             
129500                                                                          
129600     WHEN 'EMB'                                                           
129700       IF SYST-IDSEKVNR = 1                                               
129800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
129900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
130000         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
130100                 IN-EKH-SUBEL                                             
130200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
130300         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-121                        
130400         PERFORM S04-WRITE-W51573A                                        
130500       END-IF                                                             
130600                                                                          
130700       IF SYST-IDSEKVNR = 2                                               
130800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
130900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
131000         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
131100                 IN-EKH-SUBEL                                             
131200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
131300         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
131400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
131500         MOVE SPACE               TO WS-ALLOCATE-REF                      
131600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
131700         MOVE SYST-IDKST        TO WS-RED-IDKST                           
131800         IF WS-RED-IDKST > SPACE                                          
131900           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
132000           MOVE WS-RED-IDKST(1:5)                                         
132100                                TO R3-LINE-COST-CENTER(3:5)               
132200         ELSE                                                             
132300           MOVE SPACE           TO R3-LINE-COST-CENTER                    
132400         END-IF                                                           
132500         PERFORM S04-WRITE-W51573A                                        
132600       END-IF                                                             
132700                                                                          
132800     WHEN 'SUM'                                                           
132900       IF SYST-IDSEKVNR = 1                                               
133000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
133100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
133200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
133300          SPAR-SUMMA-102-121 * 0.13                                       
133400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
133500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
133600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
133700         MOVE SPACE               TO WS-ALLOCATE-REF                      
133800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
133900         MOVE SYST-IDKST        TO WS-RED-IDKST                           
134000         IF WS-RED-IDKST > SPACE                                          
134100           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
134200           MOVE WS-RED-IDKST(1:5)                                         
134300                                TO R3-LINE-COST-CENTER(3:5)               
134400         ELSE                                                             
134500           MOVE SPACE           TO R3-LINE-COST-CENTER                    
134600         END-IF                                                           
134700         PERFORM S04-WRITE-W51573A                                        
134800       END-IF                                                             
134900                                                                          
135000       IF SYST-IDSEKVNR = 2                                               
135100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
135200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
135300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
135400          SPAR-SUMMA-102-121 * 0.13                                       
135500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
135600         PERFORM S04-WRITE-W51573A                                        
135700       END-IF                                                             
135800                                                                          
135900       IF SYST-IDSEKVNR = 3                                               
136000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
136100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
136200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
136300          SPAR-SUMMA-102-121 * 0.3164                                     
136400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
136500         PERFORM S04-WRITE-W51573A                                        
136600       END-IF                                                             
136700                                                                          
136800       IF SYST-IDSEKVNR = 4                                               
136900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
137000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
137100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
137200          SPAR-SUMMA-102-121 * 0.3164                                     
137300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
137400         PERFORM S04-WRITE-W51573A                                        
137500         MOVE ZERO                TO SPAR-SUMMA-102-121                   
137600       END-IF                                                             
137700     END-EVALUATE                                                         
137800     .                                                                    
137900     EJECT                                                                
138000                                                                          
138100 CEBD-SUB-EVENT-102-122 SECTION.                                          
138200     EVALUATE IN-EKH-KDEKNIVA                                             
138300     WHEN 'DET'                                                           
138400       IF SYST-IDSEKVNR = 1                                               
138500         IF IN-EKH-KVANTAL > 0                                            
138600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
138700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
138800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
138900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
139000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
139100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
139200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
139300           MOVE SPACE               TO WS-ALLOCATE-REF                    
139400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
139500           MOVE SYST-IDKST      TO WS-RED-IDKST                           
139600           IF WS-RED-IDKST > SPACE                                        
139700             MOVE 'HD'          TO R3-LINE-COST-CENTER(1:2)               
139800             MOVE WS-RED-IDKST(1:5)                                       
139900                                TO R3-LINE-COST-CENTER(3:5)               
140000           ELSE                                                           
140100             MOVE SPACE         TO R3-LINE-COST-CENTER                    
140200           END-IF                                                         
140300           PERFORM S02-WRITE-W51571A                                      
140400         END-IF                                                           
140500       END-IF                                                             
140600                                                                          
140700       IF SYST-IDSEKVNR = 2                                               
140800         IF IN-EKH-KVANTAL < 0                                            
140900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
141000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
141100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
141200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
141300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
141400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
141500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
141600           MOVE SPACE               TO WS-ALLOCATE-REF                    
141700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
141800           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
141900           IF WS-RED-IDKST > SPACE                                        
142000             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
142100             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
142200           ELSE                                                           
142300             MOVE SPACE             TO R3-LINE-COST-CENTER                
142400           END-IF                                                         
142500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
142600           PERFORM S02-WRITE-W51571A                                      
142700         END-IF                                                           
142800       END-IF                                                             
142900                                                                          
143000       IF SYST-IDSEKVNR = 3                                               
143100         IF IN-EKH-KVANTAL > 0                                            
143200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
143300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
143400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
143500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
143600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
143700           PERFORM S02-WRITE-W51571A                                      
143800         END-IF                                                           
143900       END-IF                                                             
144000                                                                          
144100       IF SYST-IDSEKVNR = 4                                               
144200         IF IN-EKH-KVANTAL < 0                                            
144300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
144400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
144500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
144600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
144700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
144800           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
144900           IF WS-RED-IDKST > SPACE                                        
145000             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
145100             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
145200           ELSE                                                           
145300             MOVE SPACE             TO R3-LINE-COST-CENTER                
145400           END-IF                                                         
145500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
145600           PERFORM S02-WRITE-W51571A                                      
145700         END-IF                                                           
145800       END-IF                                                             
145900                                                                          
146000       IF SYST-IDSEKVNR = 5                                               
146100         IF IN-EKH-KVANTAL > 0                                            
146200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
146300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
146400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
146500            (IN-EKH-KVANTAL *                                             
146600            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
146700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
146800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
146900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
147000           MOVE SPACE               TO WS-ALLOCATE-REF                    
147100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
147200           MOVE SYST-IDKST          TO WS-RED-IDKST                       
147300           IF WS-RED-IDKST > SPACE                                        
147400             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
147500             MOVE WS-RED-IDKST(1:5)                                       
147600                                    TO R3-LINE-COST-CENTER(3:5)           
147700           ELSE                                                           
147800             MOVE SPACE             TO R3-LINE-COST-CENTER                
147900           END-IF                                                         
148000           PERFORM S02-WRITE-W51571A                                      
148100         END-IF                                                           
148200       END-IF                                                             
148300                                                                          
148400       IF SYST-IDSEKVNR = 6                                               
148500         IF IN-EKH-KVANTAL < 0                                            
148600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
148700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
148800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
148900            (IN-EKH-KVANTAL *                                             
149000            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
149100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
149200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
149300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
149400           MOVE SPACE               TO WS-ALLOCATE-REF                    
149500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
149600           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
149700           IF WS-RED-IDKST > SPACE                                        
149800             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
149900             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
150000           ELSE                                                           
150100             MOVE SPACE             TO R3-LINE-COST-CENTER                
150200           END-IF                                                         
150300           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
150400           PERFORM S02-WRITE-W51571A                                      
150500         END-IF                                                           
150600       END-IF                                                             
150700                                                                          
150800       IF SYST-IDSEKVNR = 7                                               
150900         IF IN-EKH-KVANTAL > 0                                            
151000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
151100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
151200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
151300            (IN-EKH-KVANTAL *                                             
151400            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
151500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
151600           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
151700           IF WS-RED-IDKST > SPACE                                        
151800             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
151900             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
152000           ELSE                                                           
152100             MOVE SPACE             TO R3-LINE-COST-CENTER                
152200           END-IF                                                         
152300           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
152400           PERFORM S02-WRITE-W51571A                                      
152500         END-IF                                                           
152600       END-IF                                                             
152700                                                                          
152800       IF SYST-IDSEKVNR = 8                                               
152900         IF IN-EKH-KVANTAL < 0                                            
153000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
153100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
153200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
153300            (IN-EKH-KVANTAL *                                             
153400            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
153500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
153600           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
153700           IF WS-RED-IDKST > SPACE                                        
153800             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
153900             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
154000           ELSE                                                           
154100             MOVE SPACE             TO R3-LINE-COST-CENTER                
154200           END-IF                                                         
154300           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
154400           PERFORM S02-WRITE-W51571A                                      
154500         END-IF                                                           
154600       END-IF                                                             
154700                                                                          
154800     END-EVALUATE                                                         
154900     .                                                                    
155000     EJECT                                                                
155100                                                                          
155200 CEBD-SUB-EVENT-102-123 SECTION.                                          
155300     EVALUATE IN-EKH-KDEKNIVA                                             
155400     WHEN 'DET'                                                           
155500       IF SYST-IDSEKVNR = 1                                               
155600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
155700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
155800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
155900              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
156000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
156100         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
156200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
156300         MOVE SPACE               TO WS-ALLOCATE-REF                      
156400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
156500         PERFORM S02-WRITE-W51571A                                        
156600       END-IF                                                             
156700                                                                          
156800       IF SYST-IDSEKVNR = 2                                               
156900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
157000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
157100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
157200             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
157300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
157400         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
157500         IF WS-RED-IDKST > SPACE                                          
157600           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
157700           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
157800         ELSE                                                             
157900           MOVE SPACE             TO R3-LINE-COST-CENTER                  
158000         END-IF                                                           
158100         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
158200         PERFORM S02-WRITE-W51571A                                        
158300       END-IF                                                             
158400     END-EVALUATE                                                         
158500     .                                                                    
158600     EJECT                                                                
158700                                                                          
158800 CEBD-SUB-EVENT-102-124 SECTION.                                          
158900     EVALUATE IN-EKH-KDEKNIVA                                             
159000     WHEN 'DET'                                                           
159100       IF SYST-IDSEKVNR = 1                                               
159200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
159300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
159400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
159500         IN-EKH-PRARTNTO * -1                                             
159600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
159700         MOVE SPACE               TO WS-ALLOCATE-DC                       
159800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
159900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
160000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
160100         PERFORM S03-WRITE-W51572                                         
160200       END-IF                                                             
160300                                                                          
160400     WHEN 'AVDR'                                                          
160500     WHEN 'FÖRS'                                                          
160600     WHEN 'LEG'                                                           
160700     WHEN 'FRAKT'                                                         
160800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
160900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
161000       MOVE SYST-IDKST          TO WS-RED-IDKST                           
161100       IF WS-RED-IDKST > SPACE                                            
161200         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
161300         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
161400       ELSE                                                               
161500         MOVE SPACE             TO R3-LINE-COST-CENTER                    
161600       END-IF                                                             
161700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
161800               IN-EKH-SUBEL * -1                                          
161900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
162000       MOVE SPACE               TO WS-ALLOCATE-DC                         
162100       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
162200       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
162300       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
162400       PERFORM S04-WRITE-W51573A                                          
162500                                                                          
162600     WHEN 'EMB'                                                           
162700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
162800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
162900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
163000               IN-EKH-SUBEL * -1                                          
163100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
163200       MOVE SPACE               TO WS-ALLOCATE-DC                         
163300       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
163400       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
163500       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
163600       PERFORM S04-WRITE-W51573A                                          
163700                                                                          
163800     WHEN 'DDI'                                                           
163900       IF IN-EKH-SUBEL > ZERO                                             
164000         IF SYST-IDSEKVNR = 1                                             
164100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
164200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
164300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
164400                   IN-EKH-SUBEL                                           
164500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
164600           MOVE SPACE               TO WS-ALLOCATE-DC                     
164700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
164800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
164900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
165000           PERFORM S04-WRITE-W51573A                                      
165100         END-IF                                                           
165200       ELSE                                                               
165300         IF SYST-IDSEKVNR = 2                                             
165400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
165500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
165600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
165700                   IN-EKH-SUBEL                                           
165800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
165900           MOVE SPACE               TO WS-ALLOCATE-DC                     
166000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
166100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
166200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
166300           PERFORM S04-WRITE-W51573A                                      
166400         END-IF                                                           
166500       END-IF                                                             
166600     END-EVALUATE                                                         
166700     .                                                                    
166800     EJECT                                                                
166900                                                                          
167000 CEBD-SUB-EVENT-102-125 SECTION.                                          
167100     EVALUATE IN-EKH-KDEKNIVA                                             
167200     WHEN 'DET'                                                           
167300       IF SYST-IDSEKVNR = 1                                               
167400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
167500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
167600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
167700         IN-EKH-PRARTNTO * -1                                             
167800*        (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-IN3)) * -1        
167900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
168000         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
168100         PERFORM S04-WRITE-W51573A                                        
168200       END-IF                                                             
168300                                                                          
168400       IF SYST-IDSEKVNR = 2                                               
168500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
168600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
168700         COMPUTE R3-LINE-AMOUNT-LC =                                      
168800         IN-EKH-PRARTNTO * WS-MARKUP                                      
168900         IF IN-EKH-KDVALISO = 'INR'                                       
169000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
169100         END-IF                                                           
169200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
169300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
169400         MOVE SPACE               TO WS-ALLOCATE-REF                      
169500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
169600         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
169700         IF WS-RED-IDKST > SPACE                                          
169800           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
169900           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
170000         ELSE                                                             
170100           MOVE SPACE             TO R3-LINE-COST-CENTER                  
170200         END-IF                                                           
170300         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
170400         PERFORM S04-WRITE-W51573A                                        
170500       END-IF                                                             
170600                                                                          
170700       IF SYST-IDSEKVNR = 3                                               
170800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
170900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
171000         COMPUTE R3-LINE-AMOUNT-LC =                                      
171100         IN-EKH-PRARTNTO * WS-MARKUP                                      
171200         IF IN-EKH-KDVALISO = 'INR'                                       
171300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
171400         END-IF                                                           
171500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
171600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
171700         MOVE SPACE               TO WS-ALLOCATE-REF                      
171800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
171900         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
172000         IF WS-RED-IDKST > SPACE                                          
172100           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
172200           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
172300         ELSE                                                             
172400           MOVE SPACE             TO R3-LINE-COST-CENTER                  
172500         END-IF                                                           
172600         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
172700         PERFORM S04-WRITE-W51573A                                        
172800       END-IF                                                             
172900                                                                          
173000     WHEN 'AVDR'                                                          
173100     WHEN 'FÖRS'                                                          
173200     WHEN 'LEG'                                                           
173300     WHEN 'FRAKT'                                                         
173400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
173500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
173600       MOVE SYST-IDKST          TO WS-RED-IDKST                           
173700       IF WS-RED-IDKST > SPACE                                            
173800         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
173900         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
174000       ELSE                                                               
174100         MOVE SPACE             TO R3-LINE-COST-CENTER                    
174200       END-IF                                                             
174300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
174400               IN-EKH-SUBEL * -1                                          
174500*              (IN-EKH-SUBEL / WS-PRKURS-IN3) * -1                        
174600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
174700       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
174800       PERFORM S04-WRITE-W51573A                                          
174900                                                                          
175000     WHEN 'EMB'                                                           
175100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
175200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
175300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
175400               IN-EKH-SUBEL * -1                                          
175500*              (IN-EKH-SUBEL / WS-PRKURS-IN3) * -1                        
175600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
175700       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
175800       PERFORM S04-WRITE-W51573A                                          
175900                                                                          
176000     WHEN 'DDI'                                                           
176100       IF IN-EKH-SUBEL > ZERO                                             
176200         IF SYST-IDSEKVNR = 1                                             
176300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
176400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
176500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
176600                   IN-EKH-SUBEL                                           
176700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
176800           PERFORM S04-WRITE-W51573A                                      
176900         END-IF                                                           
177000       ELSE                                                               
177100         IF SYST-IDSEKVNR = 2                                             
177200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
177300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
177400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
177500                   IN-EKH-SUBEL                                           
177600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
177700           PERFORM S04-WRITE-W51573A                                      
177800         END-IF                                                           
177900       END-IF                                                             
178000                                                                          
178100     WHEN 'SUM'                                                           
178200*      IF IN-EKH-SUBEL > ZERO                                             
178300*        IF SYST-IDSEKVNR = 1                                             
178400*          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
178500*          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
178600*          COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
178700*           IN-EKH-SUBEL                                                  
178800*          MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
178900*          PERFORM S10-VATCODE                                            
179000*          IF IN-EKH-SUVAT = ZERO                                         
179100*            MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
179200*            MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
179300*          ELSE                                                           
179400*            MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
179500*            MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
179600*          END-IF                                                         
179700*          MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
179800*                                                                         
179900*          PERFORM S04-WRITE-W51573A                                      
180000*        END-IF                                                           
180100*      END-IF                                                             
180200                                                                          
180300       IF SYST-IDSEKVNR = 1                                               
180400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
180500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
180600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
180700          IN-EKH-SUBEL * 0.13                                             
180800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
180900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
181000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
181100         MOVE SPACE               TO WS-ALLOCATE-REF                      
181200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
181300         MOVE SYST-IDKST        TO WS-RED-IDKST                           
181400         IF WS-RED-IDKST > SPACE                                          
181500           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
181600           MOVE WS-RED-IDKST(1:5)                                         
181700                                TO R3-LINE-COST-CENTER(3:5)               
181800         ELSE                                                             
181900           MOVE SPACE           TO R3-LINE-COST-CENTER                    
182000         END-IF                                                           
182100         PERFORM S04-WRITE-W51573A                                        
182200       END-IF                                                             
182300                                                                          
182400       IF SYST-IDSEKVNR = 2                                               
182500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
182600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
182700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
182800          IN-EKH-SUBEL * 0.13                                             
182900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
183000         PERFORM S04-WRITE-W51573A                                        
183100       END-IF                                                             
183200                                                                          
183300       IF SYST-IDSEKVNR = 3                                               
183400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
183500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
183600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
183700          IN-EKH-SUBEL * 0.3164                                           
183800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
183900         PERFORM S04-WRITE-W51573A                                        
184000       END-IF                                                             
184100                                                                          
184200       IF SYST-IDSEKVNR = 4                                               
184300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
184400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
184500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
184600          IN-EKH-SUBEL * 0.3164                                           
184700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
184800         PERFORM S04-WRITE-W51573A                                        
184900         MOVE ZERO                TO SPAR-SUMMA-102-125                   
185000       END-IF                                                             
185100     END-EVALUATE                                                         
185200     .                                                                    
185300     EJECT                                                                
185400                                                                          
185500 CEBE-SUB-EVENT-102-130 SECTION.                                          
185600     EVALUATE IN-EKH-KDEKNIVA                                             
185700     WHEN 'DET'                                                           
185800       IF SYST-IDSEKVNR = 1                                               
185900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
186000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
186100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
186200          IN-EKH-PRARTNTO * -1                                            
186300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
186400         PERFORM S03-WRITE-W51572                                         
186500       END-IF                                                             
186600                                                                          
186700     WHEN 'AVDR'                                                          
186800     WHEN 'FÖRS'                                                          
186900     WHEN 'LEG'                                                           
187000     WHEN 'FRAKT'                                                         
187100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
187200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
187300       MOVE SYST-IDKST          TO WS-RED-IDKST                           
187400       IF WS-RED-IDKST > SPACE                                            
187500         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
187600         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
187700       ELSE                                                               
187800         MOVE SPACE             TO R3-LINE-COST-CENTER                    
187900       END-IF                                                             
188000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
188100               IN-EKH-SUBEL * -1                                          
188200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
188300       PERFORM S04-WRITE-W51573A                                          
188400                                                                          
188500     WHEN 'EMB'                                                           
188600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
188700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
188800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
188900               IN-EKH-SUBEL * -1                                          
189000       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
189100       PERFORM S04-WRITE-W51573A                                          
189200                                                                          
189300     WHEN 'DDI'                                                           
189400       IF IN-EKH-SUBEL > ZERO                                             
189500         IF SYST-IDSEKVNR = 1                                             
189600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
189700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
189800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
189900                   IN-EKH-SUBEL                                           
190000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
190100           PERFORM S04-WRITE-W51573A                                      
190200         END-IF                                                           
190300       ELSE                                                               
190400         IF SYST-IDSEKVNR = 2                                             
190500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
190600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
190700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
190800                   IN-EKH-SUBEL                                           
190900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
191000           PERFORM S04-WRITE-W51573A                                      
191100         END-IF                                                           
191200       END-IF                                                             
191300     END-EVALUATE                                                         
191400     .                                                                    
191500     EJECT                                                                
191600                                                                          
191700 CEBE-SUB-EVENT-102-131 SECTION.                                          
191800                                                                          
191900     EVALUATE IN-EKH-KDEKNIVA                                             
192000     WHEN 'DET'                                                           
192100       IF SYST-IDSEKVNR = 1                                               
192200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
192300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
192400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
192500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
192600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
192700         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-131                        
192800         PERFORM S03-WRITE-W51572                                         
192900       END-IF                                                             
193000                                                                          
193100       IF SYST-IDSEKVNR = 2                                               
193200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
193300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
193400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
193500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
193600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
193700         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
193800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
193900         MOVE SPACE               TO WS-ALLOCATE-REF                      
194000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
194100         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
194200         IF WS-RED-IDKST > SPACE                                          
194300           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
194400           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
194500         ELSE                                                             
194600           MOVE SPACE             TO R3-LINE-COST-CENTER                  
194700         END-IF                                                           
194800         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
194900         PERFORM S03-WRITE-W51572                                         
195000       END-IF                                                             
195100                                                                          
195200       IF SYST-IDSEKVNR = 3                                               
195300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
195400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
195500         COMPUTE R3-LINE-AMOUNT-LC =                                      
195600      (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3                  
195700                      * WS-MARKUP                                         
195800         IF IN-EKH-KDVALISO = 'INR'                                       
195900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
196000         END-IF                                                           
196100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
196200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
196300         MOVE SPACE               TO WS-ALLOCATE-REF                      
196400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
196500         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
196600         IF WS-RED-IDKST > SPACE                                          
196700           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
196800           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
196900         ELSE                                                             
197000           MOVE SPACE             TO R3-LINE-COST-CENTER                  
197100         END-IF                                                           
197200         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
197300         PERFORM S03-WRITE-W51572                                         
197400       END-IF                                                             
197500                                                                          
197600       IF SYST-IDSEKVNR = 4                                               
197700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
197800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
197900         COMPUTE R3-LINE-AMOUNT-LC =                                      
198000      (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3                  
198100                      * WS-MARKUP                                         
198200         IF IN-EKH-KDVALISO = 'INR'                                       
198300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
198400         END-IF                                                           
198500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
198600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
198700         MOVE SPACE               TO WS-ALLOCATE-REF                      
198800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
198900         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
199000         IF WS-RED-IDKST > SPACE                                          
199100           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
199200           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
199300         ELSE                                                             
199400           MOVE SPACE             TO R3-LINE-COST-CENTER                  
199500         END-IF                                                           
199600         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
199700         PERFORM S03-WRITE-W51572                                         
199800       END-IF                                                             
199900                                                                          
200000     WHEN 'AVDR'                                                          
200100     WHEN 'FÖRS'                                                          
200200     WHEN 'LEG'                                                           
200300     WHEN 'FRAKT'                                                         
200400       IF SYST-IDSEKVNR = 1                                               
200500         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
200600         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
200700         MOVE SYST-IDKST        TO WS-RED-IDKST                           
200800         IF WS-RED-IDKST > SPACE                                          
200900           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
201000           MOVE WS-RED-IDKST(1:5)                                         
201100                                TO R3-LINE-COST-CENTER(3:5)               
201200         ELSE                                                             
201300           MOVE SPACE           TO R3-LINE-COST-CENTER                    
201400         END-IF                                                           
201500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
201600                 IN-EKH-SUBEL                                             
201700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
201800         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-131                        
201900         PERFORM S04-WRITE-W51573A                                        
202000       END-IF                                                             
202100                                                                          
202200       IF SYST-IDSEKVNR = 2                                               
202300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
202400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
202500         MOVE SYST-IDKST        TO WS-RED-IDKST                           
202600         IF WS-RED-IDKST > SPACE                                          
202700           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
202800           MOVE WS-RED-IDKST(1:5)                                         
202900                                TO R3-LINE-COST-CENTER(3:5)               
203000         ELSE                                                             
203100           MOVE SPACE           TO R3-LINE-COST-CENTER                    
203200         END-IF                                                           
203300         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
203400                 IN-EKH-SUBEL                                             
203500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
203600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
203700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
203800         MOVE SPACE               TO WS-ALLOCATE-REF                      
203900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
204000         PERFORM S04-WRITE-W51573A                                        
204100       END-IF                                                             
204200                                                                          
204300     WHEN 'EMB'                                                           
204400       IF SYST-IDSEKVNR = 1                                               
204500         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
204600         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
204700         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
204800                 IN-EKH-SUBEL                                             
204900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
205000         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-131                        
205100         PERFORM S04-WRITE-W51573A                                        
205200       END-IF                                                             
205300                                                                          
205400       IF SYST-IDSEKVNR = 2                                               
205500         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
205600         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
205700         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
205800                 IN-EKH-SUBEL                                             
205900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
206000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
206100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
206200         MOVE SPACE               TO WS-ALLOCATE-REF                      
206300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
206400         MOVE SYST-IDKST        TO WS-RED-IDKST                           
206500         IF WS-RED-IDKST > SPACE                                          
206600           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
206700           MOVE WS-RED-IDKST(1:5)                                         
206800                                TO R3-LINE-COST-CENTER(3:5)               
206900         ELSE                                                             
207000           MOVE SPACE           TO R3-LINE-COST-CENTER                    
207100         END-IF                                                           
207200         PERFORM S04-WRITE-W51573A                                        
207300       END-IF                                                             
207400                                                                          
207500     WHEN 'SUM'                                                           
207600       IF SYST-IDSEKVNR = 1                                               
207700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
207800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
207900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
208000          SPAR-SUMMA-102-131 * 0.13                                       
208100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
208200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
208300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
208400         MOVE SPACE               TO WS-ALLOCATE-REF                      
208500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
208600         MOVE SYST-IDKST        TO WS-RED-IDKST                           
208700         IF WS-RED-IDKST > SPACE                                          
208800           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
208900           MOVE WS-RED-IDKST(1:5)                                         
209000                                TO R3-LINE-COST-CENTER(3:5)               
209100         ELSE                                                             
209200           MOVE SPACE           TO R3-LINE-COST-CENTER                    
209300         END-IF                                                           
209400         PERFORM S04-WRITE-W51573A                                        
209500       END-IF                                                             
209600                                                                          
209700       IF SYST-IDSEKVNR = 2                                               
209800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
209900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
210000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
210100          SPAR-SUMMA-102-131 * 0.13                                       
210200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
210300         PERFORM S04-WRITE-W51573A                                        
210400       END-IF                                                             
210500                                                                          
210600       IF SYST-IDSEKVNR = 3                                               
210700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
210800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
210900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
211000          SPAR-SUMMA-102-131 * 0.3164                                     
211100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
211200         PERFORM S04-WRITE-W51573A                                        
211300       END-IF                                                             
211400                                                                          
211500       IF SYST-IDSEKVNR = 4                                               
211600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
211700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
211800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
211900          SPAR-SUMMA-102-131 * 0.3164                                     
212000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
212100         PERFORM S04-WRITE-W51573A                                        
212200         MOVE ZERO                TO SPAR-SUMMA-102-131                   
212300       END-IF                                                             
212400     END-EVALUATE                                                         
212500     .                                                                    
212600     EJECT                                                                
212700                                                                          
212800 CEBE-SUB-EVENT-102-132 SECTION.                                          
212900     EVALUATE IN-EKH-KDEKNIVA                                             
213000     WHEN 'DET'                                                           
213100       IF SYST-IDSEKVNR = 1                                               
213200         IF IN-EKH-KVANTAL > 0                                            
213300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
213400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
213500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
213600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
213700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
213800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
213900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
214000           MOVE SPACE               TO WS-ALLOCATE-REF                    
214100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
214200           MOVE SYST-IDKST      TO WS-RED-IDKST                           
214300           IF WS-RED-IDKST > SPACE                                        
214400             MOVE 'HD'          TO R3-LINE-COST-CENTER(1:2)               
214500             MOVE WS-RED-IDKST(1:5)                                       
214600                                TO R3-LINE-COST-CENTER(3:5)               
214700           ELSE                                                           
214800             MOVE SPACE         TO R3-LINE-COST-CENTER                    
214900           END-IF                                                         
215000           PERFORM S02-WRITE-W51571A                                      
215100         END-IF                                                           
215200       END-IF                                                             
215300                                                                          
215400       IF SYST-IDSEKVNR = 2                                               
215500         IF IN-EKH-KVANTAL < 0                                            
215600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
215700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
215800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
215900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
216000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
216100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
216200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
216300           MOVE SPACE               TO WS-ALLOCATE-REF                    
216400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
216500           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
216600           IF WS-RED-IDKST > SPACE                                        
216700             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
216800             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
216900           ELSE                                                           
217000             MOVE SPACE             TO R3-LINE-COST-CENTER                
217100           END-IF                                                         
217200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
217300           PERFORM S02-WRITE-W51571A                                      
217400         END-IF                                                           
217500       END-IF                                                             
217600                                                                          
217700       IF SYST-IDSEKVNR = 3                                               
217800         IF IN-EKH-KVANTAL > 0                                            
217900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
218000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
218100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
218200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
218300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
218400           PERFORM S02-WRITE-W51571A                                      
218500         END-IF                                                           
218600       END-IF                                                             
218700                                                                          
218800       IF SYST-IDSEKVNR = 4                                               
218900         IF IN-EKH-KVANTAL < 0                                            
219000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
219100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
219200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
219300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO) / WS-PRKURS-IN3            
219400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
219500           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
219600           IF WS-RED-IDKST > SPACE                                        
219700             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
219800             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
219900           ELSE                                                           
220000             MOVE SPACE             TO R3-LINE-COST-CENTER                
220100           END-IF                                                         
220200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
220300           PERFORM S02-WRITE-W51571A                                      
220400         END-IF                                                           
220500       END-IF                                                             
220600                                                                          
220700       IF SYST-IDSEKVNR = 5                                               
220800         IF IN-EKH-KVANTAL > 0                                            
220900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
221000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
221100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
221200            (IN-EKH-KVANTAL *                                             
221300            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
221400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
221500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
221600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
221700           MOVE SPACE               TO WS-ALLOCATE-REF                    
221800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
221900           MOVE SYST-IDKST          TO WS-RED-IDKST                       
222000           IF WS-RED-IDKST > SPACE                                        
222100             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
222200             MOVE WS-RED-IDKST(1:5)                                       
222300                                    TO R3-LINE-COST-CENTER(3:5)           
222400           ELSE                                                           
222500             MOVE SPACE             TO R3-LINE-COST-CENTER                
222600           END-IF                                                         
222700           PERFORM S02-WRITE-W51571A                                      
222800         END-IF                                                           
222900       END-IF                                                             
223000                                                                          
223100       IF SYST-IDSEKVNR = 6                                               
223200         IF IN-EKH-KVANTAL < 0                                            
223300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
223400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
223500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
223600            (IN-EKH-KVANTAL *                                             
223700            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
223800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
223900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
224000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
224100           MOVE SPACE               TO WS-ALLOCATE-REF                    
224200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
224300           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
224400           IF WS-RED-IDKST > SPACE                                        
224500             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
224600             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
224700           ELSE                                                           
224800             MOVE SPACE             TO R3-LINE-COST-CENTER                
224900           END-IF                                                         
225000           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
225100           PERFORM S02-WRITE-W51571A                                      
225200         END-IF                                                           
225300       END-IF                                                             
225400                                                                          
225500       IF SYST-IDSEKVNR = 7                                               
225600         IF IN-EKH-KVANTAL > 0                                            
225700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
225800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
225900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
226000            (IN-EKH-KVANTAL *                                             
226100            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
226200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
226300           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
226400           IF WS-RED-IDKST > SPACE                                        
226500             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
226600             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
226700           ELSE                                                           
226800             MOVE SPACE             TO R3-LINE-COST-CENTER                
226900           END-IF                                                         
227000           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
227100           PERFORM S02-WRITE-W51571A                                      
227200         END-IF                                                           
227300       END-IF                                                             
227400                                                                          
227500       IF SYST-IDSEKVNR = 8                                               
227600         IF IN-EKH-KVANTAL < 0                                            
227700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
227800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
227900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
228000            (IN-EKH-KVANTAL *                                             
228100            IN-EKH-PRARTNTO) / WS-PRKURS-IN3 * WS-MARKUP                  
228200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
228300           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
228400           IF WS-RED-IDKST > SPACE                                        
228500             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
228600             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
228700           ELSE                                                           
228800             MOVE SPACE             TO R3-LINE-COST-CENTER                
228900           END-IF                                                         
229000           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
229100           PERFORM S02-WRITE-W51571A                                      
229200         END-IF                                                           
229300       END-IF                                                             
229400                                                                          
229500     END-EVALUATE                                                         
229600     .                                                                    
229700     EJECT                                                                
229800                                                                          
229900 CEBE-SUB-EVENT-102-134 SECTION.                                          
230000     EVALUATE IN-EKH-KDEKNIVA                                             
230100     WHEN 'DET'                                                           
230200       IF SYST-IDSEKVNR = 1                                               
230300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
230400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
230500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
230600         IN-EKH-PRARTNTO * -1                                             
230700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
230800         MOVE SPACE               TO WS-ALLOCATE-DC                       
230900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
231000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
231100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
231200         PERFORM S03-WRITE-W51572                                         
231300       END-IF                                                             
231400                                                                          
231500     WHEN 'AVDR'                                                          
231600     WHEN 'FÖRS'                                                          
231700     WHEN 'LEG'                                                           
231800     WHEN 'FRAKT'                                                         
231900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
232000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
232100       MOVE SYST-IDKST          TO WS-RED-IDKST                           
232200       IF WS-RED-IDKST > SPACE                                            
232300         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
232400         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
232500       ELSE                                                               
232600         MOVE SPACE             TO R3-LINE-COST-CENTER                    
232700       END-IF                                                             
232800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
232900               IN-EKH-SUBEL * -1                                          
233000       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
233100       MOVE SPACE               TO WS-ALLOCATE-DC                         
233200       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
233300       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
233400       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
233500       PERFORM S04-WRITE-W51573A                                          
233600                                                                          
233700     WHEN 'EMB'                                                           
233800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
233900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
234000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
234100               IN-EKH-SUBEL * -1                                          
234200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
234300       MOVE SPACE               TO WS-ALLOCATE-DC                         
234400       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
234500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
234600       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
234700       PERFORM S04-WRITE-W51573A                                          
234800                                                                          
234900     WHEN 'DDI'                                                           
235000       IF IN-EKH-SUBEL > ZERO                                             
235100         IF SYST-IDSEKVNR = 1                                             
235200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
235300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
235400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
235500                   IN-EKH-SUBEL                                           
235600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
235700           MOVE SPACE               TO WS-ALLOCATE-DC                     
235800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
235900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
236000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
236100           PERFORM S04-WRITE-W51573A                                      
236200         END-IF                                                           
236300       ELSE                                                               
236400         IF SYST-IDSEKVNR = 2                                             
236500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
236600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
236700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
236800                   IN-EKH-SUBEL                                           
236900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
237000           MOVE SPACE               TO WS-ALLOCATE-DC                     
237100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
237200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
237300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
237400           PERFORM S04-WRITE-W51573A                                      
237500         END-IF                                                           
237600       END-IF                                                             
237700     END-EVALUATE                                                         
237800     .                                                                    
237900     EJECT                                                                
238000                                                                          
238100 CEC-MAIN-EVENT-103 SECTION.                                              
238200     EVALUATE IN-EKH-KDEKSHT                                              
238300     WHEN '102'                                                           
238400          PERFORM CECB-SUB-EVENT-103-102                                  
238500     END-EVALUATE                                                         
238600     .                                                                    
238700     EJECT                                                                
238800                                                                          
238900 CECB-SUB-EVENT-103-102 SECTION.                                          
239000     EVALUATE IN-EKH-KDEKNIVA                                             
239100     WHEN 'DET'                                                           
239200       IF SYST-IDSEKVNR = 1                                               
239300         IF IN-EKH-KVANTAL < 0                                            
239400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
239500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
239600           COMPUTE R3-LINE-AMOUNT-LC =                                    
239700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
239800           IF IN-EKH-KDVALISO = 'INR'                                     
239900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
240000           END-IF                                                         
240100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
240200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
240300           MOVE SPACE               TO WS-ALLOCATE-REF                    
240400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
240500           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
240600           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
240700           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
240800           PERFORM S04-WRITE-W51573A                                      
240900         END-IF                                                           
241000       END-IF                                                             
241100                                                                          
241200       IF SYST-IDSEKVNR = 2                                               
241300         IF IN-EKH-KVANTAL > 0                                            
241400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
241500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
241600           COMPUTE R3-LINE-AMOUNT-LC =                                    
241700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
241800           IF IN-EKH-KDVALISO = 'INR'                                     
241900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
242000           END-IF                                                         
242100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
242200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
242300           MOVE SPACE               TO WS-ALLOCATE-REF                    
242400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
242500           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
242600           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
242700           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
242800           PERFORM S04-WRITE-W51573A                                      
242900         END-IF                                                           
243000       END-IF                                                             
243100                                                                          
243200     WHEN 'KALK'                                                          
243300       IF SYST-IDSEKVNR = 1                                               
243400         IF IN-EKH-SUBEL > 0                                              
243500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
243600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
243700           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
243800           IF IN-EKH-KDVALISO = 'INR'                                     
243900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
244000           END-IF                                                         
244100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
244200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
244300           MOVE SPACE               TO WS-ALLOCATE-REF                    
244400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
244500           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
244600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
244700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
244800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
244900           PERFORM S04-WRITE-W51573A                                      
245000         END-IF                                                           
245100       END-IF                                                             
245200                                                                          
245300       IF SYST-IDSEKVNR = 2                                               
245400         IF IN-EKH-SUBEL < 0                                              
245500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
245600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
245700           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
245800           IF IN-EKH-KDVALISO = 'INR'                                     
245900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
246000           END-IF                                                         
246100           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
246200           MOVE SPACE               TO WS-ALLOCATE-DC                     
246300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
246400           MOVE SPACE               TO WS-ALLOCATE-REF                    
246500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
246600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
246700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
246800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
246900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
247000           PERFORM S04-WRITE-W51573A                                      
247100         END-IF                                                           
247200       END-IF                                                             
247300                                                                          
247400       IF SYST-IDSEKVNR = 3                                               
247500         IF IN-EKH-SUBEL > 0                                              
247600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
247700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
247800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
247900           IF IN-EKH-KDVALISO = 'INR'                                     
248000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
248100           END-IF                                                         
248200*          MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
248300*          PERFORM S11-ANALYSIS                                           
248400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
248500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
248600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
248700           PERFORM S04-WRITE-W51573A                                      
248800         END-IF                                                           
248900       END-IF                                                             
249000                                                                          
249100       IF SYST-IDSEKVNR = 4                                               
249200         IF IN-EKH-SUBEL < 0                                              
249300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
249400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
249500           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
249600           IF IN-EKH-KDVALISO = 'INR'                                     
249700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
249800           END-IF                                                         
249900*          MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
250000*          PERFORM S11-ANALYSIS                                           
250100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
250200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
250300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
250400           PERFORM S04-WRITE-W51573A                                      
250500         END-IF                                                           
250600       END-IF                                                             
250700                                                                          
250800     WHEN 'HEMT'                                                          
250900       IF SYST-IDSEKVNR = 1                                               
251000         IF IN-EKH-SUBEL > 0                                              
251100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
251200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
251300           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
251400           IF IN-EKH-KDVALISO = 'INR'                                     
251500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
251600           END-IF                                                         
251700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
251800           MOVE SPACE               TO WS-ALLOCATE-DC                     
251900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
252000           MOVE SPACE               TO WS-ALLOCATE-REF                    
252100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
252200           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
252300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
252400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
252500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
252600           PERFORM S04-WRITE-W51573A                                      
252700         END-IF                                                           
252800       END-IF                                                             
252900                                                                          
253000       IF SYST-IDSEKVNR = 2                                               
253100         IF IN-EKH-SUBEL < 0                                              
253200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
253300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
253400           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
253500           IF IN-EKH-KDVALISO = 'INR'                                     
253600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
253700           END-IF                                                         
253800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
253900           MOVE SPACE               TO WS-ALLOCATE-DC                     
254000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
254100           MOVE SPACE               TO WS-ALLOCATE-REF                    
254200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
254300           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
254400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
254500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
254600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
254700           PERFORM S04-WRITE-W51573A                                      
254800         END-IF                                                           
254900       END-IF                                                             
255000                                                                          
255100       IF SYST-IDSEKVNR = 3                                               
255200         IF IN-EKH-SUBEL > 0                                              
255300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
255400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
255500           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
255600           IF IN-EKH-KDVALISO = 'INR'                                     
255700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
255800           END-IF                                                         
255900*          MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
256000*          PERFORM S11-ANALYSIS                                           
256100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
256200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
256300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
256400           PERFORM S04-WRITE-W51573A                                      
256500         END-IF                                                           
256600       END-IF                                                             
256700                                                                          
256800       IF SYST-IDSEKVNR = 4                                               
256900         IF IN-EKH-SUBEL < 0                                              
257000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
257100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
257200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
257300           IF IN-EKH-KDVALISO = 'INR'                                     
257400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
257500           END-IF                                                         
257600*          MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
257700*          PERFORM S11-ANALYSIS                                           
257800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
257900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
258000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
258100           PERFORM S04-WRITE-W51573A                                      
258200         END-IF                                                           
258300       END-IF                                                             
258400                                                                          
258500     WHEN 'DDI'                                                           
258600       IF IN-EKH-SUBEL > ZERO                                             
258700         IF SYST-IDSEKVNR = 1                                             
258800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
258900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
259000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
259100                   IN-EKH-SUBEL                                           
259200           MOVE ZEROES              TO R3-LINE-AMOUNT                     
259210           IF IN-EKH-KDVALISO = 'INR'                                     
259220             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
259230           END-IF                                                         
259300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
259700           MOVE SPACE               TO R3-LINE-ALLOCATE                   
259800           PERFORM S04-WRITE-W51573A                                      
259900         END-IF                                                           
260000       ELSE                                                               
260100         IF SYST-IDSEKVNR = 2                                             
260200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
260300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
260400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
260500                   IN-EKH-SUBEL                                           
260600           MOVE ZEROES              TO R3-LINE-AMOUNT                     
260610           IF IN-EKH-KDVALISO = 'INR'                                     
260620             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
260630           END-IF                                                         
260700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
261100           MOVE SPACE               TO R3-LINE-ALLOCATE                   
261200           PERFORM S04-WRITE-W51573A                                      
261300         END-IF                                                           
261400       END-IF                                                             
261500     END-EVALUATE                                                         
261600     .                                                                    
261700     EJECT                                                                
261800                                                                          
261900 CED-MAIN-EVENT-201 SECTION.                                              
262000     EVALUATE IN-EKH-KDEKSHT                                              
262100     WHEN '201'                                                           
262200          PERFORM CEDA-SUB-EVENT-201-201                                  
262300     END-EVALUATE                                                         
262400     .                                                                    
262500     EJECT                                                                
262600                                                                          
262700 CEDA-SUB-EVENT-201-201 SECTION.                                          
262800     EVALUATE IN-EKH-KDEKNIVA                                             
262900     WHEN 'DET'                                                           
263000       IF SYST-IDSEKVNR = 1                                               
263100         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
263200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
263300         COMPUTE R3-LINE-AMOUNT-LC =                                      
263400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
263500         IF IN-EKH-KDVALISO = 'INR'                                       
263600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
263700         END-IF                                                           
263800         PERFORM S03-WRITE-W51572                                         
263900       END-IF                                                             
264000                                                                          
264100       IF SYST-IDSEKVNR = 2                                               
264200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
264300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
264400         COMPUTE R3-LINE-AMOUNT-LC =                                      
264500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
264600         IF IN-EKH-KDVALISO = 'INR'                                       
264700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
264800         END-IF                                                           
264900         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
265000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
265100         MOVE SPACE               TO WS-ALLOCATE-REF                      
265200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
265300         PERFORM S03-WRITE-W51572                                         
265400       END-IF                                                             
265500     END-EVALUATE                                                         
265600     .                                                                    
265700     EJECT                                                                
265800                                                                          
265900 CEF-MAIN-EVENT-203 SECTION.                                              
266000     EVALUATE IN-EKH-KDEKSHT                                              
266100     WHEN '201'                                                           
266200          PERFORM CEFA-SUB-EVENT-203-201                                  
266300     END-EVALUATE                                                         
266400     .                                                                    
266500     EJECT                                                                
266600                                                                          
266700 CEFA-SUB-EVENT-203-201 SECTION.                                          
266800     EVALUATE IN-EKH-KDEKNIVA                                             
266900     WHEN 'DET'                                                           
267000       IF SYST-IDSEKVNR = 1                                               
267100         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
267200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
267300         IF IN-EKH-PRARTSTD > 0                                           
267400           COMPUTE R3-LINE-AMOUNT-LC =                                    
267500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
267600         ELSE                                                             
267700           COMPUTE R3-LINE-AMOUNT-LC =                                    
267800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
267900         END-IF                                                           
268000         IF IN-EKH-KDVALISO = 'INR'                                       
268100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
268200         END-IF                                                           
268300         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
268400         IF WS-RED-IDKST > SPACE                                          
268500           MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)             
268600           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
268700         ELSE                                                             
268800           MOVE SPACE             TO R3-LINE-COST-CENTER                  
268900         END-IF                                                           
269000         PERFORM S03-WRITE-W51572                                         
269100       END-IF                                                             
269200                                                                          
269300       IF SYST-IDSEKVNR = 2                                               
269400         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
269500         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
269600         COMPUTE R3-LINE-AMOUNT-LC =                                      
269700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
269800         IF IN-EKH-KDVALISO = 'INR'                                       
269900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
270000         END-IF                                                           
270100         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
270200         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
270300         MOVE SPACE             TO WS-ALLOCATE-REF                        
270400         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
270500         PERFORM S03-WRITE-W51572                                         
270600       END-IF                                                             
270700     END-EVALUATE                                                         
270800     .                                                                    
270900     EJECT                                                                
271000                                                                          
271100 CEG-MAIN-EVENT-204 SECTION.                                              
271200     EVALUATE IN-EKH-KDEKSHT                                              
271300     WHEN '201'                                                           
271400          PERFORM CEGA-SUB-EVENT-204-201                                  
271500     WHEN '301'                                                           
271600          PERFORM CEGB-SUB-EVENT-204-301                                  
271700     END-EVALUATE                                                         
271800     .                                                                    
271900     EJECT                                                                
272000                                                                          
272100 CEGA-SUB-EVENT-204-201 SECTION.                                          
272200     EVALUATE IN-EKH-KDEKNIVA                                             
272300     WHEN 'DET'                                                           
272400         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
272500* R-FAKTURA                                                               
272600       IF SYST-IDSEKVNR = 1                                               
272700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
272800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
272900         COMPUTE R3-LINE-AMOUNT-LC =                                      
273000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
273100         IF IN-EKH-KDVALISO = 'INR'                                       
273200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
273300         END-IF                                                           
273400         MOVE 0000411122          TO R3-LINE-PA-CUSTOMER                  
273500         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
273600         MOVE SPACE               TO WS-ALLOCATE-DC                       
273700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
273800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
273900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
274000         PERFORM S03-WRITE-W51572                                         
274100       END-IF                                                             
274200                                                                          
274300       IF SYST-IDSEKVNR = 2                                               
274400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
274500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
274600         COMPUTE R3-LINE-AMOUNT-LC =                                      
274700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
274800         IF IN-EKH-KDVALISO = 'INR'                                       
274900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
275000         END-IF                                                           
275100         MOVE 0000411122          TO R3-LINE-PA-CUSTOMER                  
275200         MOVE SPACE               TO WS-ALLOCATE-DC                       
275300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
275400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
275500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
275600         PERFORM S03-WRITE-W51572                                         
275700       END-IF                                                             
275800     END-EVALUATE                                                         
275900     .                                                                    
276000     EJECT                                                                
276100                                                                          
276200 CEGB-SUB-EVENT-204-301 SECTION.                                          
276300     EVALUATE IN-EKH-KDEKNIVA                                             
276400     WHEN 'DET'                                                           
276500* R-FAKTURA                                                               
276600       IF SYST-IDSEKVNR = 1                                               
276700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
276800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
276900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
277000         IF BET-KDTRADP(3:2) NOT = SPACE                                  
277100            MOVE '1'             TO WS-ACCOUNT-4                          
277200            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
277300         ELSE                                                             
277400            MOVE '3'             TO WS-ACCOUNT-4                          
277500            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
277600         END-IF                                                           
277700         COMPUTE R3-LINE-AMOUNT-LC =                                      
277800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
277900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
278000         PERFORM S03-WRITE-W51572                                         
278100       END-IF                                                             
278200                                                                          
278300       IF SYST-IDSEKVNR = 2                                               
278400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
278500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
278600         COMPUTE R3-LINE-AMOUNT-LC =                                      
278700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
278800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
278900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
279000         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
279100         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
279200         MOVE SPACE             TO WS-ALLOCATE-REF                        
279300         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
279400         PERFORM S03-WRITE-W51572                                         
279500       END-IF                                                             
279600                                                                          
279700       IF SYST-IDSEKVNR = 3                                               
279800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
279900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
280000         IF BET-KDTRADP(3:2) NOT = SPACE                                  
280100            MOVE '1'             TO WS-ACCOUNT-4                          
280200            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
280300         ELSE                                                             
280400            MOVE '3'             TO WS-ACCOUNT-4                          
280500            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
280600         END-IF                                                           
280700         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
280800         COMPUTE R3-LINE-AMOUNT-LC =                                      
280900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
281000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
281100         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
281200         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
281300         MOVE SPACE             TO WS-ALLOCATE-REF                        
281400         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
281500         PERFORM S03-WRITE-W51572                                         
281600       END-IF                                                             
281700                                                                          
281800     WHEN 'FÖRS'                                                          
281900     WHEN 'FRAKT'                                                         
282000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
282100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
282200       MOVE SYST-IDKST          TO WS-RED-IDKST                           
282300       IF WS-RED-IDKST > SPACE                                            
282400         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
282500         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
282600       ELSE                                                               
282700         MOVE SPACE             TO R3-LINE-COST-CENTER                    
282800       END-IF                                                             
282900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
283000               IN-EKH-SUBEL * -1                                          
283100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
283200       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
283300       PERFORM S04-WRITE-W51573A                                          
283400                                                                          
283500     WHEN 'EMB'                                                           
283600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
283700       MOVE SYST-IDKST          TO WS-RED-IDKST                           
283800       IF WS-RED-IDKST > SPACE                                            
283900         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
284000         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
284100       ELSE                                                               
284200         MOVE SPACE             TO R3-LINE-COST-CENTER                    
284300       END-IF                                                             
284400       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
284500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
284600               IN-EKH-SUBEL * -1                                          
284700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
284800       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
284900       PERFORM S04-WRITE-W51573A                                          
285000                                                                          
285100     END-EVALUATE                                                         
285200     .                                                                    
285300     EJECT                                                                
285400                                                                          
285500 CEI-MAIN-EVENT-302 SECTION.                                              
285600     EVALUATE IN-EKH-KDEKSHT                                              
285700     WHEN '302'                                                           
285800          PERFORM CEIB-SUB-EVENT-302-302                                  
285900     END-EVALUATE                                                         
286000     .                                                                    
286100     EJECT                                                                
286200                                                                          
286300 CEIB-SUB-EVENT-302-302 SECTION.                                          
286400     EVALUATE IN-EKH-KDEKNIVA                                             
286500     WHEN 'DET'                                                           
286600       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
286700         IF SYST-IDSEKVNR = 1                                             
286800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
286900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
287000           IF BET-KDTRADP(3:2) NOT = SPACE                                
287100             MOVE '1'             TO WS-ACCOUNT-4                         
287200             MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER              
287300           ELSE                                                           
287400             MOVE '3'             TO WS-ACCOUNT-4                         
287500             MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER             
287600           END-IF                                                         
287700           COMPUTE R3-LINE-AMOUNT-LC  =                                   
287800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
287900           IF IN-EKH-KDVALISO = 'INR'                                     
288000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
288100           END-IF                                                         
288200           PERFORM S02-WRITE-W51571A                                      
288300         END-IF                                                           
288400                                                                          
288500         IF SYST-IDSEKVNR = 2                                             
288600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
288700           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
288800           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
288900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
289000           COMPUTE R3-LINE-AMOUNT-LC  =                                   
289100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
289200           IF IN-EKH-KDVALISO = 'INR'                                     
289300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
289400           END-IF                                                         
289500           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
289600           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
289700           MOVE SPACE               TO WS-ALLOCATE-REF                    
289800           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
289900           PERFORM S02-WRITE-W51571A                                      
290000         END-IF                                                           
290100       ELSE                                                               
290200         IF SYST-IDSEKVNR = 3                                             
290300           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
290400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
290500           IF BET-KDTRADP(3:2) NOT = SPACE                                
290600             MOVE '1'             TO WS-ACCOUNT-4                         
290700             MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER              
290800           ELSE                                                           
290900             MOVE '3'             TO WS-ACCOUNT-4                         
291000             MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER             
291100           END-IF                                                         
291200           COMPUTE R3-LINE-AMOUNT-LC  =                                   
291300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
291400           IF IN-EKH-KDVALISO = 'INR'                                     
291500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
291600           END-IF                                                         
291700           PERFORM S02-WRITE-W51571A                                      
291800         END-IF                                                           
291900                                                                          
292000         IF SYST-IDSEKVNR = 4                                             
292100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
292200           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
292300           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
292400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
292500           COMPUTE R3-LINE-AMOUNT-LC  =                                   
292600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
292700           IF IN-EKH-KDVALISO = 'INR'                                     
292800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
292900           END-IF                                                         
293000           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
293100           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
293200           MOVE SPACE               TO WS-ALLOCATE-REF                    
293300           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
293400           PERFORM S02-WRITE-W51571A                                      
293500         END-IF                                                           
293600       END-IF                                                             
293700     END-EVALUATE                                                         
293800     .                                                                    
293900     EJECT                                                                
294000                                                                          
294100 CEJ-MAIN-EVENT-303 SECTION.                                              
294200     EVALUATE IN-EKH-KDEKSHT                                              
294300     WHEN '3XX'                                                           
294400          PERFORM CEJ301-SUB-EVENT-303-3XX                                
294500     WHEN '301'                                                           
294600          PERFORM CEJ301-SUB-EVENT-303-301                                
294700     WHEN '307'                                                           
294800          PERFORM CEJ307-SUB-EVENT-303-307                                
294900     WHEN '310'                                                           
295000          PERFORM CEJ310-SUB-EVENT-303-310                                
295100     WHEN '311'                                                           
295200          PERFORM CEJ311-SUB-EVENT-303-311                                
295300     WHEN '371'                                                           
295400          PERFORM CEJ371-SUB-EVENT-303-371                                
295500     WHEN '391'                                                           
295600          PERFORM CEJ301-SUB-EVENT-303-391                                
295700     END-EVALUATE                                                         
295800     .                                                                    
295900     EJECT                                                                
296000                                                                          
296100 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
296200     EVALUATE IN-EKH-KDEKNIVA                                             
296300                                                                          
296400     WHEN 'FÖRS'                                                          
296500     WHEN 'LEG'                                                           
296600     WHEN 'FRAKT'                                                         
296700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
296800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
296900       MOVE SYST-IDKST          TO WS-RED-IDKST                           
297000       IF WS-RED-IDKST > SPACE                                            
297100         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
297200         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
297300       ELSE                                                               
297400         MOVE SPACE             TO R3-LINE-COST-CENTER                    
297500       END-IF                                                             
297600       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
297700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
297800               IN-EKH-SUBEL * -1                                          
297900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
298000       MOVE SPACE               TO WS-ALLOCATE-DC                         
298100       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
298200       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
298300       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
298400       PERFORM S03-WRITE-W51572                                           
298500                                                                          
298600     WHEN 'LAND'                                                          
298700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
298800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
298900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
299000               IN-EKH-SUBEL * -1                                          
299100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
299200       MOVE SPACE               TO WS-ALLOCATE-DC                         
299300       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
299400       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
299500       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
299600       PERFORM S03-WRITE-W51572                                           
299700                                                                          
299800     WHEN 'DDI'                                                           
299900       IF IN-EKH-SUBEL > ZERO                                             
300000         IF SYST-IDSEKVNR = 1                                             
300100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
300200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
300300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
300400                   IN-EKH-SUBEL                                           
300500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
300600           MOVE SPACE               TO WS-ALLOCATE-DC                     
300700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
300800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
300900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
301000           PERFORM S04-WRITE-W51573A                                      
301100         END-IF                                                           
301200       ELSE                                                               
301300         IF SYST-IDSEKVNR = 2                                             
301400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
301500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
301600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
301700                   IN-EKH-SUBEL                                           
301800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
301900           MOVE SPACE               TO WS-ALLOCATE-DC                     
302000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
302100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
302200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
302300           PERFORM S04-WRITE-W51573A                                      
302400         END-IF                                                           
302500       END-IF                                                             
302600     END-EVALUATE                                                         
302700     .                                                                    
302800     EJECT                                                                
302900                                                                          
303000 CEJ301-SUB-EVENT-303-301 SECTION.                                        
303100     EVALUATE IN-EKH-KDEKNIVA                                             
303200     WHEN 'DET'                                                           
303300       IF SYST-IDSEKVNR = 1                                               
303400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
303500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
303600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
303700         IN-EKH-PRARTNTO * -1                                             
303800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
303900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
303910         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
304000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
304100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
304200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
304300         MOVE SPACE               TO WS-ALLOCATE-DC                       
304400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
304500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
304600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
304700         PERFORM S03-WRITE-W51572                                         
304800       END-IF                                                             
304900                                                                          
305000     END-EVALUATE                                                         
305100     .                                                                    
305200     EJECT                                                                
305300                                                                          
305400 CEJ307-SUB-EVENT-303-307 SECTION.                                        
305500     EVALUATE IN-EKH-KDEKNIVA                                             
305600     WHEN 'DET'                                                           
305700       IF SYST-IDSEKVNR = 1                                               
305800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
305900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
306000         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
306100         IN-EKH-PRARTNTO * -1                                             
306200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
306300         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
306310         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
306400         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
306500         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
306600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
306700         MOVE SPACE               TO WS-ALLOCATE-DC                       
306800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
306900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
307000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
307100         PERFORM S03-WRITE-W51572                                         
307200       END-IF                                                             
307300                                                                          
307400                                                                          
307500     END-EVALUATE                                                         
307600     .                                                                    
307700     EJECT                                                                
307800                                                                          
307900 CEJ310-SUB-EVENT-303-310 SECTION.                                        
308000     EVALUATE IN-EKH-KDEKNIVA                                             
308100     WHEN 'DET'                                                           
308200       IF SYST-IDSEKVNR = 1                                               
308300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
308400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
308500         COMPUTE R3-LINE-AMOUNT-LC =                                      
308600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
308700         IF IN-EKH-KDVALISO = 'INR'                                       
308800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
308900         END-IF                                                           
309000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
309100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
309200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
309300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
309310         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
309400         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
309500         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
309600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
309700         PERFORM S02-WRITE-W51571A                                        
309800       END-IF                                                             
309900                                                                          
310000       IF SYST-IDSEKVNR = 2                                               
310100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
310200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
310300         COMPUTE R3-LINE-AMOUNT-LC =                                      
310400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
310500         IF IN-EKH-KDVALISO = 'INR'                                       
310600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
310700         END-IF                                                           
310710         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
310800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
310900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
311000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
311100         MOVE SPACE               TO WS-ALLOCATE-DC                       
311200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
311300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
311400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
311500         PERFORM S02-WRITE-W51571A                                        
311600       END-IF                                                             
311700     END-EVALUATE                                                         
311800     .                                                                    
311900     EJECT                                                                
312000                                                                          
312100 CEJ311-SUB-EVENT-303-311 SECTION.                                        
312200     EVALUATE IN-EKH-KDEKNIVA                                             
312300     WHEN 'DET'                                                           
312400        IF SYST-IDSEKVNR = 1                                              
312500          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
312600          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
312700          COMPUTE R3-LINE-AMOUNT-LC =                                     
312800                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
312900          IF IN-EKH-KDVALISO = 'INR'                                      
313000            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
313100          END-IF                                                          
313200          MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER             
313210          MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                        
313300          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
313400          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
313500          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
313600          MOVE SPACE               TO WS-ALLOCATE-DC                      
313700          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
313800          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
313900          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
314000          PERFORM S02-WRITE-W51571A                                       
314100        END-IF                                                            
314200                                                                          
314300        IF SYST-IDSEKVNR = 2                                              
314400          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
314500          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
314600          COMPUTE R3-LINE-AMOUNT-LC =                                     
314700                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
314800          IF IN-EKH-KDVALISO = 'INR'                                      
314900            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
315000          END-IF                                                          
315100          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
315200          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
315300          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
315400          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
315410          MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                        
315500          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
315600          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
315700          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
315800          PERFORM S02-WRITE-W51571A                                       
315900        END-IF                                                            
316000     END-EVALUATE                                                         
316100     .                                                                    
316200     EJECT                                                                
316300                                                                          
316400 CEJ371-SUB-EVENT-303-371 SECTION.                                        
316500     EVALUATE IN-EKH-KDEKNIVA                                             
316600     WHEN 'DET'                                                           
316700       IF SYST-IDSEKVNR = 1                                               
316800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
316900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
317000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
317100           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-IN3               
317200         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
317300         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
317400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
317500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
317600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
317700         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
317800         PERFORM S04-WRITE-W51573A                                        
317900       END-IF                                                             
318000                                                                          
318100     WHEN 'LAND'                                                          
318200       IF SYST-IDSEKVNR = 1                                               
318300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
318400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
318500         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
318600         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
318700               R3-LINE-AMOUNT-LC / WS-PRKURS-IN3                          
318800         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
318900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
319000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
319100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
319200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
319300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
319400         PERFORM S03-WRITE-W51572                                         
319500       END-IF                                                             
319600                                                                          
319700     WHEN 'DDI'                                                           
319800       IF IN-EKH-SUBEL > ZERO                                             
319900         IF SYST-IDSEKVNR = 1                                             
320000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
320100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
320200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
320300                   IN-EKH-SUBEL                                           
320400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
320500           MOVE SPACE               TO WS-ALLOCATE-DC                     
320600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
320700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
320800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
320900           PERFORM S03-WRITE-W51572                                       
321000         END-IF                                                           
321100       ELSE                                                               
321200         IF SYST-IDSEKVNR = 2                                             
321300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
321400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
321500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
321600                   IN-EKH-SUBEL                                           
321700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
321800           MOVE SPACE               TO WS-ALLOCATE-DC                     
321900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
322000           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
322100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
322200           PERFORM S03-WRITE-W51572                                       
322300         END-IF                                                           
322400       END-IF                                                             
322500     END-EVALUATE                                                         
322600     .                                                                    
322700     EJECT                                                                
322800                                                                          
322900 CEJ301-SUB-EVENT-303-391 SECTION.                                        
323000     EVALUATE IN-EKH-KDEKNIVA                                             
323100     WHEN 'DET'                                                           
323200       IF SYST-IDSEKVNR = 1                                               
323300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
323400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
323500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
323600              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
323700         IF IN-EKH-KDVALISO = 'INR'                                       
323800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
323900         END-IF                                                           
324000         MOVE 0000411122          TO R3-LINE-PA-CUSTOMER                  
324100         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
324110         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
324200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
324300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
324400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
324500         MOVE SPACE               TO WS-ALLOCATE-DC                       
324600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
324700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
324800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
324900         PERFORM S03-WRITE-W51572                                         
325000       END-IF                                                             
325100                                                                          
325200       IF SYST-IDSEKVNR = 2                                               
325300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
325400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
325500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
325600              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
325700         IF IN-EKH-KDVALISO = 'INR'                                       
325800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
325900         END-IF                                                           
326000         MOVE 0000411122          TO R3-LINE-PA-CUSTOMER                  
326100         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
326200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
326300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
326400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
326410         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
326500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
326600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
326700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
326800         PERFORM S03-WRITE-W51572                                         
326900       END-IF                                                             
327000                                                                          
327100     WHEN 'FÖRS'                                                          
327200     WHEN 'LEG'                                                           
327300     WHEN 'FRAKT'                                                         
327400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
327500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
327600       MOVE SYST-IDKST          TO WS-RED-IDKST                           
327700       IF WS-RED-IDKST > SPACE                                            
327800         MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)               
327900         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
328000       ELSE                                                               
328100         MOVE SPACE             TO R3-LINE-COST-CENTER                    
328200       END-IF                                                             
328300       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
328400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
328500               IN-EKH-SUBEL * -1                                          
328600       IF IN-EKH-KDVALISO = 'INR'                                         
328700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
328800       END-IF                                                             
328900       MOVE SPACE               TO WS-ALLOCATE-DC                         
329000       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
329100       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
329200       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
329300       PERFORM S03-WRITE-W51572                                           
329400                                                                          
329500     WHEN 'LAND'                                                          
329600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
329700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
329800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
329900               IN-EKH-SUBEL * -1                                          
330000       IF IN-EKH-KDVALISO = 'INR'                                         
330100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
330200       END-IF                                                             
330300       MOVE SPACE               TO WS-ALLOCATE-DC                         
330400       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
330500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
330600       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
330700       PERFORM S03-WRITE-W51572                                           
330800                                                                          
330900     END-EVALUATE                                                         
331000     .                                                                    
331100     EJECT                                                                
331200                                                                          
331300 CEK-MAIN-EVENT-401 SECTION.                                              
331400     EVALUATE IN-EKH-KDEKNIVA                                             
331500                                                                          
331600* PRISÄNDRING LÖPANDE                                                     
331700     WHEN 'DET'                                                           
331800       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
331900                           IN-EKH-PRARTSTD                                
332000       IF SYST-IDSEKVNR = 1                                               
332100* PRISHÖJNING                                                             
332200         IF WS-BELOPP > 0                                                 
332300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
332400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
332500           COMPUTE R3-LINE-AMOUNT-LC =                                    
332600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
332700           IF IN-EKH-KDVALISO = 'INR'                                     
332800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
332900           END-IF                                                         
333000           MOVE 0000411122          TO R3-LINE-PA-CUSTOMER                
333100           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
333200           PERFORM S02-WRITE-W51571A                                      
333300         END-IF                                                           
333400       END-IF                                                             
333500                                                                          
333600       IF SYST-IDSEKVNR = 2                                               
333700* PRISHÖJNING                                                             
333800         IF WS-BELOPP > 0                                                 
333900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
334000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
334100           COMPUTE R3-LINE-AMOUNT-LC =                                    
334200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
334300           IF IN-EKH-KDVALISO = 'INR'                                     
334400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
334500           END-IF                                                         
334600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
334700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
334800           MOVE SPACE               TO WS-ALLOCATE-REF                    
334900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
335000           PERFORM S02-WRITE-W51571A                                      
335100         END-IF                                                           
335200       END-IF                                                             
335300                                                                          
335400       IF SYST-IDSEKVNR = 3                                               
335500* PRISSÄNKNING                                                            
335600         IF WS-BELOPP < 0                                                 
335700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
335800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
335900           COMPUTE R3-LINE-AMOUNT-LC =                                    
336000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
336100           IF IN-EKH-KDVALISO = 'INR'                                     
336200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
336300           END-IF                                                         
336400           MOVE 0000411122          TO R3-LINE-PA-CUSTOMER                
336500           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
336600           PERFORM S02-WRITE-W51571A                                      
336700         END-IF                                                           
336800       END-IF                                                             
336900                                                                          
337000       IF SYST-IDSEKVNR = 4                                               
337100* PRISSÄNKNING                                                            
337200         IF WS-BELOPP < 0                                                 
337300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
337400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
337500           COMPUTE R3-LINE-AMOUNT-LC =                                    
337600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
337700           IF IN-EKH-KDVALISO = 'INR'                                     
337800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
337900           END-IF                                                         
338000           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
338100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
338200           MOVE SPACE               TO WS-ALLOCATE-REF                    
338300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
338400           PERFORM S02-WRITE-W51571A                                      
338500         END-IF                                                           
338600       END-IF                                                             
338700     END-EVALUATE                                                         
338800     .                                                                    
338900     EJECT                                                                
339000                                                                          
339100 CEL-MAIN-EVENT-402 SECTION.                                              
339200     EVALUATE IN-EKH-KDEKNIVA                                             
339300     WHEN 'DET'                                                           
339400       IF SYST-IDSEKVNR = 1                                               
339500         IF IN-EKH-KVANTAL > 0                                            
339600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
339700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
339800           COMPUTE R3-LINE-AMOUNT-LC =                                    
339900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
340000           IF IN-EKH-KDVALISO = 'INR'                                     
340100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
340200           END-IF                                                         
340300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
340400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
340500           MOVE SPACE               TO WS-ALLOCATE-REF                    
340600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
340700           PERFORM S02-WRITE-W51571A                                      
340800         END-IF                                                           
340900       END-IF                                                             
341000                                                                          
341100       IF SYST-IDSEKVNR = 2                                               
341200         IF IN-EKH-KVANTAL < 0                                            
341300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
341400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
341500           COMPUTE R3-LINE-AMOUNT-LC =                                    
341600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
341700           IF IN-EKH-KDVALISO = 'INR'                                     
341800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
341900           END-IF                                                         
342000           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
342100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
342200           MOVE SPACE               TO WS-ALLOCATE-REF                    
342300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
342400           PERFORM S02-WRITE-W51571A                                      
342500         END-IF                                                           
342600       END-IF                                                             
342700     END-EVALUATE                                                         
342800     .                                                                    
342900     EJECT                                                                
343000                                                                          
343100 CEM-MAIN-EVENT-403 SECTION.                                              
343200     EVALUATE IN-EKH-KDEKSHT                                              
343300     WHEN '401'                                                           
343400     WHEN '402'                                                           
343500     WHEN '403'                                                           
343600     WHEN '404'                                                           
343700     WHEN '405'                                                           
343800     WHEN '407'                                                           
343900     WHEN '408'                                                           
344000     WHEN '409'                                                           
344100          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
344200     END-EVALUATE                                                         
344300     .                                                                    
344400     EJECT                                                                
344500                                                                          
344600 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
344700     EVALUATE IN-EKH-KDEKNIVA                                             
344800     WHEN 'DET'                                                           
344900       IF SYST-IDSEKVNR = 1                                               
345000         IF IN-EKH-KVANTAL > 0                                            
345100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
345200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
345300           COMPUTE R3-LINE-AMOUNT-LC =                                    
345400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
345500           IF IN-EKH-KDVALISO = 'INR'                                     
345600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
345700           END-IF                                                         
345800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
345900           PERFORM S02-WRITE-W51571A                                      
346000         END-IF                                                           
346100       END-IF                                                             
346200                                                                          
346300       IF SYST-IDSEKVNR = 2                                               
346400         IF IN-EKH-KVANTAL > 0                                            
346500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
346600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
346700           COMPUTE R3-LINE-AMOUNT-LC =                                    
346800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
346900           IF IN-EKH-KDVALISO = 'INR'                                     
347000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
347100           END-IF                                                         
347200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
347300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
347400           MOVE SPACE               TO WS-ALLOCATE-REF                    
347500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
347600           PERFORM S02-WRITE-W51571A                                      
347700         END-IF                                                           
347800       END-IF                                                             
347900                                                                          
348000       IF SYST-IDSEKVNR = 3                                               
348100         IF IN-EKH-KVANTAL < 0                                            
348200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
348300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
348400           COMPUTE R3-LINE-AMOUNT-LC =                                    
348500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
348600           IF IN-EKH-KDVALISO = 'INR'                                     
348700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
348800           END-IF                                                         
348900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
349000           PERFORM S02-WRITE-W51571A                                      
349100         END-IF                                                           
349200       END-IF                                                             
349300                                                                          
349400       IF SYST-IDSEKVNR = 4                                               
349500         IF IN-EKH-KVANTAL < 0                                            
349600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
349700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
349800           COMPUTE R3-LINE-AMOUNT-LC =                                    
349900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
350000           IF IN-EKH-KDVALISO = 'INR'                                     
350100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
350200           END-IF                                                         
350300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
350400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
350500           MOVE SPACE               TO WS-ALLOCATE-REF                    
350600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
350700           PERFORM S02-WRITE-W51571A                                      
350800         END-IF                                                           
350900       END-IF                                                             
351000     END-EVALUATE                                                         
351100     .                                                                    
351200     EJECT                                                                
351300                                                                          
351400 CEN-MAIN-EVENT-404 SECTION.                                              
351500     EVALUATE IN-EKH-KDEKNIVA                                             
351600     WHEN 'DET'                                                           
351700       IF SYST-IDSEKVNR = 1                                               
351800* KONTO EJ MANUELLT REGISTRERAT                                           
351900         IF IN-EKH-IDKONTO = 0                                            
352000           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
352100           IF DIST18-SCRAP-NDC-SC                                         
352200           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
352300           OR DIST18-SCRAP-NDC-QUAL                                       
352400*          IF DIST18-SCRAP-NDC-QUAL                                       
352500             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
352600             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
352700             COMPUTE R3-LINE-AMOUNT-LC =                                  
352800                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
352900             IF IN-EKH-KDVALISO = 'INR'                                   
353000               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
353100             END-IF                                                       
353200             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
353300             PERFORM S02-WRITE-W51571A                                    
353400           END-IF                                                         
353500         END-IF                                                           
353600       END-IF                                                             
353700                                                                          
353800       IF SYST-IDSEKVNR = 2                                               
353900* KONTO MANUELLT REGISTRERAT                                              
354000         IF IN-EKH-IDKONTO > 0                                            
354100           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
354200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
354300           COMPUTE R3-LINE-AMOUNT-LC =                                    
354400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
354500           IF IN-EKH-KDVALISO = 'INR'                                     
354600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
354700           END-IF                                                         
354800           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
354900           IF WS-RED-IDKST > SPACE                                        
355000             MOVE 'HD'              TO R3-LINE-COST-CENTER(1:2)           
355100             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
355200           ELSE                                                           
355300             MOVE SPACE             TO R3-LINE-COST-CENTER                
355400           END-IF                                                         
355500           PERFORM S02-WRITE-W51571A                                      
355600         END-IF                                                           
355700       END-IF                                                             
355800                                                                          
355900       IF SYST-IDSEKVNR = 3                                               
356000         IF IN-EKH-IDKUNDRF = 'OBJ'                                       
356100           CONTINUE                                                       
356200         ELSE                                                             
356300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
356400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
356500           COMPUTE R3-LINE-AMOUNT-LC =                                    
356600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
356700           IF IN-EKH-KDVALISO = 'INR'                                     
356800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
356900           END-IF                                                         
357000           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
357100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
357200           MOVE SPACE               TO WS-ALLOCATE-REF                    
357300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
357400           PERFORM S02-WRITE-W51571A                                      
357500         END-IF                                                           
357600       END-IF                                                             
357700                                                                          
357800       IF SYST-IDSEKVNR = 4                                               
357900         IF IN-EKH-IDKUNDRF = 'OBJ'                                       
358000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
358100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
358200           COMPUTE R3-LINE-AMOUNT-LC =                                    
358300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
358400           IF IN-EKH-KDVALISO = 'INR'                                     
358500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
358600           END-IF                                                         
358700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
358800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
358900           MOVE SPACE               TO WS-ALLOCATE-REF                    
359000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
359100           PERFORM S02-WRITE-W51571A                                      
359200         END-IF                                                           
359300       END-IF                                                             
359400                                                                          
359500*      IF SYST-IDSEKVNR = 4                                               
359600* KONTO EJ MANUELLT REGISTRERAT                                           
359700*        IF IN-EKH-IDKONTO = 0                                            
359800*          MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
359900*          IF DIST18-SCRAP-NDC-SC                                         
360000*          OR DIST18-SCRAP-NDC-SC-LOCAL                                   
360100*            MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
360200*            MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
360300*            COMPUTE R3-LINE-AMOUNT    =                                  
360400*                    IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
360500*            PERFORM S11-ANALYSIS                                         
360600*            PERFORM S02-WRITE-W51571A                                    
360700*          END-IF                                                         
360800*        END-IF                                                           
360900*      END-IF                                                             
361000                                                                          
361100     END-EVALUATE                                                         
361200     .                                                                    
361300     EJECT                                                                
361400                                                                          
361500 CF-BUILD-COMMON-210-PART SECTION.                                        
361600     MOVE SPACE              TO R3-LINE-R3                                
361700     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
361800                                R3-LINE-DUE-DATE                          
361900                                R3-LINE-AMOUNT                            
362000                                R3-LINE-AMOUNT-LC                         
362100                                R3-LINE-TAX-AMOUNT                        
362200                                R3-LINE-TAX-AMOUNT-LC                     
362300                                R3-LINE-NUMBER-OF-DAYS                    
362400                                R3-LINE-QUANTITY                          
362500                                R3-LINE-SAMNR                             
362600     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
362700     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
362800     MOVE 'IN07'             TO R3-LINE-COMPANY-CODE                      
362900     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
363000     IF SYST-KDPOST = '31'                                                
363100       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
363200     ELSE                                                                 
363300       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
363400     END-IF                                                               
363500     .                                                                    
363600     EJECT                                                                
363700                                                                          
363800 CG-SCHEDULE-LINE-AP SECTION.                                             
363900     MOVE NEJ                     TO WS-HEADER-SW                         
364000     MOVE JA                      TO WS-LINE-SW                           
364100     EVALUATE IN-EKH-KDEKHHT                                              
364200     WHEN '102'                                                           
364300*      IF IN-EKH-KDEKSHT = '130'                                          
364400*      OR IN-EKH-KDEKSHT = '134'                                          
364500*        IF IN-EKH-KDEKSHT = '130'                                        
364600*          PERFORM CGA-MAIN-EVENT-102-130                                 
364700*        ELSE                                                             
364800*          PERFORM CGA-MAIN-EVENT-102-134                                 
364900*        END-IF                                                           
365000*      ELSE                                                               
365100*        IF IN-EKH-KDEKSHT = '120'                                        
365200*        OR IN-EKH-KDEKSHT = '124'                                        
365300*        OR IN-EKH-KDEKSHT = '125'                                        
365400           IF IN-EKH-KDEKSHT = '125'                                      
365500             PERFORM CGA-MAIN-EVENT-102-125                               
365600           ELSE                                                           
365700             PERFORM CGA-MAIN-EVENT-102                                   
365800           END-IF                                                         
365900*        END-IF                                                           
366000*      END-IF                                                             
366100     WHEN '103'                                                           
366200         PERFORM CGA-MAIN-EVENT-103                                       
366300     WHEN '303'                                                           
366400       IF IN-EKH-KDEKSHT = '371'                                          
366500         PERFORM S81-GET-CURRENCY-RATE                                    
366600         PERFORM CGA-MAIN-EVENT-303-371                                   
366700       ELSE                                                               
366800         PERFORM CGA-MAIN-EVENT-303                                       
366900       END-IF                                                             
367000     END-EVALUATE                                                         
367100     .                                                                    
367200     EJECT                                                                
367300                                                                          
367400 CGA-MAIN-EVENT-102     SECTION.                                          
367500     EVALUATE IN-EKH-KDEKNIVA                                             
367600     WHEN 'SUM'                                                           
367700       IF IN-EKH-SUBEL > ZERO                                             
367800         IF SYST-IDSEKVNR = 1                                             
367900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
368000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
368100            IN-EKH-SUBEL                                                  
368200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
368300           PERFORM S10-VATCODE                                            
368400           IF IN-EKH-SUVAT = ZERO                                         
368500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
368600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
368700           ELSE                                                           
368800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
368900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
369000           END-IF                                                         
369100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
369200                                                                          
369300           PERFORM S04-WRITE-W51573A                                      
369400         END-IF                                                           
369500       END-IF                                                             
369600                                                                          
369700       IF IN-EKH-SUBEL < ZERO                                             
369800         IF SYST-IDSEKVNR = 2                                             
369900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
370000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
370100           IN-EKH-SUBEL                                                   
370200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
370300           PERFORM S10-VATCODE                                            
370400           IF IN-EKH-SUVAT = ZERO                                         
370500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
370600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
370700           ELSE                                                           
370800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
370900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
371000           END-IF                                                         
371100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
371200                                                                          
371300           PERFORM S04-WRITE-W51573A                                      
371400         END-IF                                                           
371500       END-IF                                                             
371600     END-EVALUATE                                                         
371700     .                                                                    
371800     EJECT                                                                
371900                                                                          
372000 CGA-MAIN-EVENT-102-125 SECTION.                                          
372100     EVALUATE IN-EKH-KDEKNIVA                                             
372200     WHEN 'SUM'                                                           
372300       IF IN-EKH-SUBEL > ZERO                                             
372400         IF SYST-IDSEKVNR = 1                                             
372500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
372600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
372700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
372800            IN-EKH-SUBEL                                                  
372900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
373000           PERFORM S10-VATCODE                                            
373100           IF IN-EKH-SUVAT = ZERO                                         
373200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
373300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
373400           ELSE                                                           
373500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
373600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
373700           END-IF                                                         
373800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
373900                                                                          
374000           PERFORM S04-WRITE-W51573A                                      
374100         END-IF                                                           
374200       END-IF                                                             
374300                                                                          
374400       IF SYST-IDSEKVNR = 2                                               
374500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
374600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
374700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
374800          IN-EKH-SUBEL * 0.13                                             
374900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
375000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
375100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
375200         MOVE SPACE               TO WS-ALLOCATE-REF                      
375300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
375400         MOVE SYST-IDKST        TO WS-RED-IDKST                           
375500         IF WS-RED-IDKST > SPACE                                          
375600           MOVE 'HD'            TO R3-LINE-COST-CENTER(1:2)               
375700           MOVE WS-RED-IDKST(1:5)                                         
375800                                TO R3-LINE-COST-CENTER(3:5)               
375900         ELSE                                                             
376000           MOVE SPACE           TO R3-LINE-COST-CENTER                    
376100         END-IF                                                           
376200         PERFORM S04-WRITE-W51573A                                        
376300       END-IF                                                             
376400                                                                          
376500       IF SYST-IDSEKVNR = 3                                               
376600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
376700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
376800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
376900          IN-EKH-SUBEL * 0.13                                             
377000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
377100         PERFORM S04-WRITE-W51573A                                        
377200       END-IF                                                             
377300                                                                          
377400       IF SYST-IDSEKVNR = 4                                               
377500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
377600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
377700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
377800          IN-EKH-SUBEL * 0.3164                                           
377900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
378000         PERFORM S04-WRITE-W51573A                                        
378100       END-IF                                                             
378200                                                                          
378300       IF SYST-IDSEKVNR = 5                                               
378400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
378500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
378600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
378700          IN-EKH-SUBEL * 0.3164                                           
378800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
378900         PERFORM S04-WRITE-W51573A                                        
379000         MOVE ZERO                TO SPAR-SUMMA-102-125                   
379100       END-IF                                                             
379200     END-EVALUATE                                                         
379300     .                                                                    
379400     EJECT                                                                
379500                                                                          
379600 CGA-MAIN-EVENT-103     SECTION.                                          
379700     EVALUATE IN-EKH-KDEKNIVA                                             
379800     WHEN 'SUM'                                                           
379900       IF IN-EKH-SUBEL > ZERO                                             
380000         IF SYST-IDSEKVNR = 1                                             
380100           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
380200           PERFORM S10-VATCODE                                            
380300           IF IN-EKH-SUVAT = ZERO                                         
380400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
380500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
380600           ELSE                                                           
380700             IF IN-EKH-KDVALISO = 'INR'                                   
380800               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
380900                                      R3-LINE-TAX-AMOUNT-LC               
381000             ELSE                                                         
381100               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
381200                                      R3-LINE-TAX-AMOUNT-LC               
381300               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
381400                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
381500             END-IF                                                       
381600           END-IF                                                         
381700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
381800**** CALCULATE NEW SUM WITH VAT                                           
381900           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
382000                   IN-EKH-SUVAT                                           
382100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
382200           IF IN-EKH-KDVALISO = 'INR'                                     
382300             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
382400           ELSE                                                           
382500             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
382600                     R3-LINE-AMOUNT * WS-PRKURS                           
382700           END-IF                                                         
382800                                                                          
382900           PERFORM S04-WRITE-W51573A                                      
383000         END-IF                                                           
383100       END-IF                                                             
383200                                                                          
383300       IF IN-EKH-SUBEL < ZERO                                             
383400         IF SYST-IDSEKVNR = 2                                             
383500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
383600           PERFORM S10-VATCODE                                            
383700           IF IN-EKH-SUVAT = ZERO                                         
383800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
383900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
384000           ELSE                                                           
384100             IF IN-EKH-KDVALISO = 'INR'                                   
384200               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
384300                                      R3-LINE-TAX-AMOUNT-LC               
384400             ELSE                                                         
384500               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
384600                                      R3-LINE-TAX-AMOUNT-LC               
384700               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
384800                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
384900             END-IF                                                       
385000           END-IF                                                         
385100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
385200                                                                          
385300**** CALCULATE NEW SUM WITH VAT                                           
385400           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
385500                   IN-EKH-SUVAT                                           
385600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
385700           IF IN-EKH-KDVALISO = 'INR'                                     
385800             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
385900           ELSE                                                           
386000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
386100                     R3-LINE-AMOUNT * WS-PRKURS                           
386200           END-IF                                                         
386300           PERFORM S04-WRITE-W51573A                                      
386400         END-IF                                                           
386500       END-IF                                                             
386600     END-EVALUATE                                                         
386700     .                                                                    
386800     EJECT                                                                
386900                                                                          
387000 CGA-MAIN-EVENT-102-130 SECTION.                                          
387100     EVALUATE IN-EKH-KDEKNIVA                                             
387200     WHEN 'SUM'                                                           
387300       IF IN-EKH-SUBEL > ZERO                                             
387400         IF SYST-IDSEKVNR = 1                                             
387500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
387600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
387700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
387800                   R3-LINE-AMOUNT    / WS-PRKURS-IN  * -1                 
387900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
388000           PERFORM S10-VATCODE                                            
388100           IF IN-EKH-SUVAT = ZERO                                         
388200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
388300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
388400           ELSE                                                           
388500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
388600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
388700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-IN  * -1           
388800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
388900           END-IF                                                         
389000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
389100                                                                          
389200           PERFORM S04-WRITE-W51573A                                      
389300         END-IF                                                           
389400       END-IF                                                             
389500                                                                          
389600       IF IN-EKH-SUBEL < ZERO                                             
389700         IF SYST-IDSEKVNR = 2                                             
389800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
389900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
390000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
390100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
390200                   R3-LINE-AMOUNT    / WS-PRKURS-IN                       
390300           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
390400           PERFORM S10-VATCODE                                            
390500           IF IN-EKH-SUVAT = ZERO                                         
390600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
390700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
390800           ELSE                                                           
390900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
391000             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
391100                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-IN                 
391200             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
391300           END-IF                                                         
391400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
391500                                                                          
391600           PERFORM S04-WRITE-W51573A                                      
391700         END-IF                                                           
391800       END-IF                                                             
391900     END-EVALUATE                                                         
392000     .                                                                    
392100     EJECT                                                                
392200                                                                          
392300 CGA-MAIN-EVENT-102-134 SECTION.                                          
392400     EVALUATE IN-EKH-KDEKNIVA                                             
392500     WHEN 'SUM'                                                           
392600       IF IN-EKH-SUBEL > ZERO                                             
392700         IF SYST-IDSEKVNR = 1                                             
392800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
392900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
393000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
393100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
393200                   R3-LINE-AMOUNT    / WS-PRKURS-IN  * -1                 
393300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
393400           PERFORM S10-VATCODE                                            
393500           IF IN-EKH-SUVAT = ZERO                                         
393600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
393700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
393800           ELSE                                                           
393900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
394000             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
394100                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-IN  * -1           
394200             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
394300           END-IF                                                         
394400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
394500                                                                          
394600           PERFORM S04-WRITE-W51573A                                      
394700         END-IF                                                           
394800       END-IF                                                             
394900                                                                          
395000       IF IN-EKH-SUBEL < ZERO                                             
395100         IF SYST-IDSEKVNR = 2                                             
395200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
395300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
395400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
395500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
395600                   R3-LINE-AMOUNT    / WS-PRKURS-IN                       
395700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
395800           PERFORM S10-VATCODE                                            
395900           IF IN-EKH-SUVAT = ZERO                                         
396000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
396100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
396200           ELSE                                                           
396300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
396400             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
396500                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-IN                 
396600             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
396700           END-IF                                                         
396800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
396900                                                                          
397000           PERFORM S04-WRITE-W51573A                                      
397100         END-IF                                                           
397200       END-IF                                                             
397300     END-EVALUATE                                                         
397400     .                                                                    
397500     EJECT                                                                
397600                                                                          
397700 CGA-MAIN-EVENT-303 SECTION.                                              
397800     EVALUATE IN-EKH-KDEKNIVA                                             
397900     WHEN 'SUM'                                                           
398000       IF SYST-IDSEKVNR = 1                                               
398100         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
398200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
398300         IN-EKH-SUBEL                                                     
398400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
398500         PERFORM S10-VATCODE                                              
398600         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
398700         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
398800                 R3-LINE-TAX-AMOUNT-LC                                    
398900         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
399000                                                                          
399100         PERFORM S04-WRITE-W51573A                                        
399200       END-IF                                                             
399300     END-EVALUATE                                                         
399400     .                                                                    
399500     EJECT                                                                
399600                                                                          
399700 CGA-MAIN-EVENT-303-371 SECTION.                                          
399800     EVALUATE IN-EKH-KDEKNIVA                                             
399900     WHEN 'SUM'                                                           
400000       IF SYST-IDSEKVNR = 1                                               
400100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
400200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
400300         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
400400         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
400500               R3-LINE-AMOUNT-LC / WS-PRKURS-IN3                          
400600         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
400700         PERFORM S10-VATCODE                                              
400800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
400900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
401000                 R3-LINE-TAX-AMOUNT-LC * WS-PRKURS-IN2                    
401100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
401200                                                                          
401300         PERFORM S04-WRITE-W51573A                                        
401400       END-IF                                                             
401500     END-EVALUATE                                                         
401600     .                                                                    
401700     EJECT                                                                
401800                                                                          
401900 CH-BUILD-COMMON-310-PART SECTION.                                        
402000     MOVE SPACE              TO R3-LINE-R3                                
402100     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
402200                                R3-LINE-DUE-DATE                          
402300                                R3-LINE-AMOUNT                            
402400                                R3-LINE-AMOUNT-LC                         
402500                                R3-LINE-TAX-AMOUNT                        
402600                                R3-LINE-TAX-AMOUNT-LC                     
402700                                R3-LINE-NUMBER-OF-DAYS                    
402800                                R3-LINE-QUANTITY                          
402900                                R3-LINE-SAMNR                             
403000     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
403100     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
403200     MOVE 'IN07'             TO R3-LINE-COMPANY-CODE                      
403300     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
403400     IF SYST-KDPOST = '31'                                                
403500       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
403600     ELSE                                                                 
403700       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
403800     END-IF                                                               
403900     .                                                                    
404000     EJECT                                                                
404100                                                                          
404200 CI-SCHEDULE-LINE-AR SECTION.                                             
404300     MOVE NEJ                     TO WS-HEADER-SW                         
404400     MOVE JA                      TO WS-LINE-SW                           
404500     EVALUATE IN-EKH-KDEKHHT                                              
404600     WHEN '204'                                                           
404700         PERFORM CIA-MAIN-EVENT-204                                       
404800     END-EVALUATE                                                         
404900     .                                                                    
405000     EJECT                                                                
405100                                                                          
405200 CIA-MAIN-EVENT-204     SECTION.                                          
405300     EVALUATE IN-EKH-KDEKNIVA                                             
405400     WHEN 'SUM'                                                           
405500       IF IN-EKH-SUBEL > ZERO                                             
405600         IF SYST-IDSEKVNR = 1                                             
405700           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
405800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
405900           IF IN-EKH-KDVALISO = 'INR'                                     
406000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
406100           END-IF                                                         
406200           PERFORM S10-VATCODE                                            
406300           IF IN-EKH-SUVAT = ZERO                                         
406400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
406500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
406600           ELSE                                                           
406700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
406800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
406900           END-IF                                                         
407000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
407100                                                                          
407200           PERFORM S04-WRITE-W51573A                                      
407300         END-IF                                                           
407400       END-IF                                                             
407500                                                                          
407600     END-EVALUATE                                                         
407700     .                                                                    
407800     EJECT                                                                
407900                                                                          
408000 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
408100     MOVE ZERO             TO LOGG-W51573                                 
408200     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
408300     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
408400     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
408500     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
408600     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
408700     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
408800     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
408900     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
409000     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
409100     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
409200     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
409300                                                                          
409400****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
409500     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
409600     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
409700     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
409800     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
409900     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
410000     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
410100     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
410200     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
410300     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
410400     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
410500     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
410600     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
410700     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
410800     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
410900     .                                                                    
411000     EJECT                                                                
411100                                                                          
411200 Z-FINI SECTION.                                                          
411300     CLOSE W51566                                                         
411400           W51570                                                         
411500           W51571A                                                        
411600           W51572A                                                        
411700           W51573A                                                        
411800           W51575                                                         
411900           W5156N                                                         
412000           W51310                                                         
412100                                                                          
412200     MOVE 'S' TO POSTSUM-OPKOD                                            
412300     CALL POSTSUM USING POSTSUM-PARM                                      
412400     .                                                                    
412500     EJECT                                                                
412600                                                                          
412700 S01-READ-W51566  SECTION.                                                
412800     READ W51566 INTO IN-AREA                                             
412900     AT END                                                               
413000        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
413100        SET END-OF-W51566 TO TRUE                                         
413200                                                                          
413300     NOT AT END                                                           
413400        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
413500        MOVE 'W51566'     TO POSTSUM-FDNAMN                               
413600        MOVE 'W51568D1'   TO POSTSUM-DDNAMN2                              
413700        CALL POSTSUM USING POSTSUM-PARM                                   
413800     END-READ                                                             
413900     .                                                                    
414000                                                                          
414100 S02-WRITE-W51571A SECTION.                                               
414200     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
414300     MOVE SPACE                 TO 71LINE-POST                            
414400     IF WS-LINE-SW = JA                                                   
414500       IF IN-EKH-KDSORT = 'SW'                                            
414600         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
414700         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
414800         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
414900       ELSE                                                               
415000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
415100         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
415200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
415300       END-IF                                                             
415400       WRITE 71LINE-POST        FROM R3-LINE-R3                           
415500       PERFORM S20-CREATE-WRITE-LOG                                       
415600     ELSE                                                                 
415700       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
415800     END-IF                                                               
415900                                                                          
416000     IF WS-LINE-SW = JA                                                   
416100       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
416200     ELSE                                                                 
416300       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
416400     END-IF                                                               
416500     MOVE 'W51571A'             TO POSTSUM-FDNAMN                         
416600     MOVE 'W51568D2'            TO POSTSUM-DDNAMN2                        
416700     CALL POSTSUM USING POSTSUM-PARM                                      
416800     .                                                                    
416900                                                                          
417000 S002-WRITE-W51571A-HEAD SECTION.                                         
417100     MOVE SPACE                 TO 71LINE-POST                            
417200     IF IN-EKH-KDSORT = 'SW'                                              
417300       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
417400       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
417500       MOVE WS-TEXT           TO R3-LINE-TEXT                             
417600     ELSE                                                                 
417700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
417800       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
417900       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
418000     END-IF                                                               
418100     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
418200                                                                          
418300     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
418400     MOVE 'W51571A'             TO POSTSUM-FDNAMN                         
418500     MOVE 'W51568D2'            TO POSTSUM-DDNAMN2                        
418600     CALL POSTSUM USING POSTSUM-PARM                                      
418700     .                                                                    
418800                                                                          
418900 S03-WRITE-W51572 SECTION.                                                
419000     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
419100     IF IN-EKH-KDSORT = 'SW'                                              
419200       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
419300       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
419400       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
419500     ELSE                                                                 
419600       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
419700       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
419800       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
419900     END-IF                                                               
420000     WRITE 72LINE-POST        FROM R3-LINE-R3                             
420100                                                                          
420200     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
420300     MOVE 'W51572A'           TO POSTSUM-FDNAMN                           
420400     MOVE 'W51568D3'          TO POSTSUM-DDNAMN2                          
420500     CALL POSTSUM USING POSTSUM-PARM                                      
420600                                                                          
420700     PERFORM S20-CREATE-WRITE-LOG                                         
420800     .                                                                    
420900                                                                          
421000 S04-WRITE-W51573A SECTION.                                               
421100     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
421200     MOVE SPACE                 TO 73LINE-POST                            
421300     IF WS-LINE-SW = JA                                                   
421400       IF IN-EKH-KDSORT = 'SW'                                            
421500         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
421600         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
421700         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
421800       ELSE                                                               
421900         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
422000         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
422100         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
422200       END-IF                                                             
422300       WRITE 73LINE-POST        FROM R3-LINE-R3                           
422400       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
422500     ELSE                                                                 
422600       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
422700       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
422800     END-IF                                                               
422900                                                                          
423000     MOVE 'W51573A'             TO POSTSUM-FDNAMN                         
423100     MOVE 'W51568D4'            TO POSTSUM-DDNAMN2                        
423200     CALL POSTSUM USING POSTSUM-PARM                                      
423300                                                                          
423400     IF WS-LINE-SW = JA                                                   
423500       PERFORM S20-CREATE-WRITE-LOG                                       
423600     END-IF                                                               
423700     .                                                                    
423800                                                                          
423900 S004-WRITE-W51573A-HEAD SECTION.                                         
424000     MOVE SPACE                 TO 73LINE-POST                            
424100     IF IN-EKH-KDSORT = 'SW'                                              
424200       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
424300       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
424400       MOVE WS-TEXT           TO R3-LINE-TEXT                             
424500     ELSE                                                                 
424600       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
424700       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
424800       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
424900     END-IF                                                               
425000     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
425100                                                                          
425200     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
425300     MOVE 'W51573A'             TO POSTSUM-FDNAMN                         
425400     MOVE 'W51568D4'            TO POSTSUM-DDNAMN2                        
425500     CALL POSTSUM USING POSTSUM-PARM                                      
425600     .                                                                    
425700                                                                          
425800 S10-VATCODE SECTION.                                                     
425900     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
426000     PERFORM IMS-GU-WDB601                                                
426100     IF DCS-KDDC = SPACE                                                  
426200       MOVE NEJ              TO WDB6-A-SW                                 
426300     ELSE                                                                 
426400       MOVE JA               TO WDB6-A-SW                                 
426500     END-IF                                                               
426600                                                                          
426700     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
426800     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
426900     IF IN-EKH-SUVAT = ZERO                                               
427000       MOVE 'I1'     TO R3-LINE-TAX-CODE                                  
427100     ELSE                                                                 
427200       MOVE 'A1'     TO R3-LINE-TAX-CODE                                  
427300     END-IF                                                               
427400     IF IN-EKH-BEVAT = 'XX'                                               
427500       MOVE 'I1'     TO R3-LINE-TAX-CODE                                  
427600     END-IF                                                               
427700     .                                                                    
427800     EJECT                                                                
427900                                                                          
428000 S20-CREATE-WRITE-LOG SECTION.                                            
428100     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
428200     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
428300     IF SYST-IDPTYP = '610'                                               
428400       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
428500     ELSE                                                                 
428600       MOVE ZERO                      TO WS-IDLEVNR                       
428700       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
428800                          FOR CHARACTERS BEFORE INITIAL SPACE             
428900       IF WS-IDLEVNR   > ZERO                                             
429000          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
429100                                      TO LOGG-IDKONTO                     
429200       END-IF                                                             
429300     END-IF                                                               
429400     IF R3-LINE-COST-CENTER NOT = SPACE                                   
429500       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
429600     END-IF                                                               
429700     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
429800     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
429900     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
430000     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
430100     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
430200                                                                          
430300     PERFORM S21-WRITE-W51575                                             
430400     PERFORM S22-WRITE-W51570                                             
430500                                                                          
430600     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
430700       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
430800       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
430900       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
431000                                                                          
431100       PERFORM S21-WRITE-W51575                                           
431200     END-IF                                                               
431300     .                                                                    
431400     EJECT                                                                
431500                                                                          
431600 S21-WRITE-W51575 SECTION.                                                
431700     IF DCS-IDDC NOT = LOGG-IDDC                                          
431800        MOVE LOGG-IDDC TO W-IDDC-B6                                       
431900        PERFORM IMS-GU-WDB601                                             
432000     END-IF                                                               
432100     IF DCS-KDDC = SPACE                                                  
432200       MOVE NEJ              TO WDB6-A-SW                                 
432300     ELSE                                                                 
432400       MOVE JA               TO WDB6-A-SW                                 
432500     END-IF                                                               
432600                                                                          
432700     IF  WDB6-A-FINNS                                                     
432800     AND DCS-DDC                                                          
432900       MOVE 'N'       TO LOGG-FLLSBOK                                     
433000     END-IF                                                               
433100     WRITE LOGG-POST FROM LOGG-W51573                                     
433200                                                                          
433300     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
433400     MOVE 'W51575'    TO POSTSUM-FDNAMN                                   
433500     MOVE 'W51568D5'  TO POSTSUM-DDNAMN2                                  
433600     CALL POSTSUM USING POSTSUM-PARM                                      
433700     .                                                                    
433800                                                                          
433900 S22-WRITE-W51570 SECTION.                                                
434000     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
434100     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
434200     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
434300                                                                          
434400     IF R3-LINE-AMOUNT-SIGN = '+'                                         
434500       IF AVST-SUBEL < +0                                                 
434600         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
434700       END-IF                                                             
434800       IF AVST-KVANTAL < +0                                               
434900         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
435000       END-IF                                                             
435100     ELSE                                                                 
435200       IF AVST-SUBEL > +0                                                 
435300         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
435400       END-IF                                                             
435500       IF AVST-KVANTAL > +0                                               
435600         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
435700       END-IF                                                             
435800     END-IF                                                               
435900                                                                          
436000     IF DCS-IDDC NOT = AVST-IDDC                                          
436100        MOVE AVST-IDDC  TO W-IDDC-B6                                      
436200        PERFORM IMS-GU-WDB601                                             
436300     END-IF                                                               
436400     IF DCS-KDDC = SPACE                                                  
436500       MOVE NEJ              TO WDB6-A-SW                                 
436600     ELSE                                                                 
436700       MOVE JA               TO WDB6-A-SW                                 
436800     END-IF                                                               
436900                                                                          
437000     IF  WDB6-A-FINNS                                                     
437100     AND DCS-DDC                                                          
437200       MOVE 'N'                 TO AVST-FLLSBOK                           
437300     END-IF                                                               
437400                                                                          
437500     IF AVST-IDKONTO(1:4) = '1454'                                        
437600       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
437700       WRITE AVST-POST FROM AVST-W51570                                   
437800                                                                          
437900       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
438000       MOVE 'W51570'            TO POSTSUM-FDNAMN                         
438100       MOVE 'W51568D6'          TO POSTSUM-DDNAMN2                        
438200       CALL POSTSUM USING POSTSUM-PARM                                    
438300     END-IF                                                               
438400     .                                                                    
438500     EJECT                                                                
438600                                                                          
438700 S30-READ-DATABASE-B2-B1 SECTION.                                         
438800     MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                             
438900     MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                            
439000     PERFORM IMS-GU-WDB201                                                
439100     IF SEGMENT-SAKNAS                                                    
439200** OM MAN SKICKAR PÅ EXPORT SÅ KAN TILLÄGGSKOSTNADERNA HAMNA PÅ           
439300** KUND 0 OCH OM DEN SAKNAS SÅ SÄTTER VI EN ANNAN DEFAULT                 
439400*      IF IN-EKH-IDDISTR = 9111                                           
439500       IF IN-EKH-IDLEVNR = '1441'                                         
439600*        MOVE '1441'          TO W-WDB1-IDPARTNR                          
439700         MOVE IN-EKH-IDLEVNR  TO W-WDB1-IDPARTNR                          
439800       ELSE                                                               
439900         MOVE 'IN99999'       TO W-WDB1-IDPARTNR                          
440000       END-IF                                                             
440100     ELSE                                                                 
440200       MOVE GMT-IDPARTNR      TO W-WDB1-IDPARTNR                          
440300       IF IN-EKH-IDLEVNR = '1441'                                         
440400         MOVE IN-EKH-IDLEVNR  TO W-WDB1-IDPARTNR                          
440500       END-IF                                                             
440600     END-IF                                                               
440700     MOVE WC-IDFTG-IN       TO W-WDB1-IDFTG                               
440800     PERFORM IMS-GU-WDB101                                                
440900     IF SEGMENT-SAKNAS                                                    
441000       DISPLAY 'BETALARUPPG. SAKNAS '                                     
441100       DISPLAY IN-EKH-IDVERGL                                             
441200       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
441300       DISPLAY GMT-IDPARTNR                                               
441400                                                                          
441500       MOVE SPACE         TO BET-KDTRADP                                  
441600       MOVE ZERO          TO BET-IDPARTNR                                 
441700       MOVE '????'        TO WS-KDBETVIL                                  
441800       MOVE '???'         TO WS-KDVALISO-WDB1                             
441900     ELSE                                                                 
442000       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
442100     END-IF                                                               
442200     MOVE 'INR'           TO WS-KDVALISO-WDB1                             
442300     DISPLAY 'BETVIL ' WS-KDBETVIL                                        
442400                                                                          
442500                                                                          
442600     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
442700     MOVE ZERO TO TALLY                                                   
442800     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
442900                 FOR CHARACTERS BEFORE INITIAL SPACE                      
443000     IF TALLY = ZERO                                                      
443100       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
443200     ELSE                                                                 
443300       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
443400                                TO W-BET-IDPARTNR-NUM                     
443500     END-IF                                                               
443600     .                                                                    
443700     EJECT                                                                
443800                                                                          
443900 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
444000     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
444100     PERFORM IMS-GU-WDB601                                                
444200     IF DCS-KDDC = SPACE                                                  
444300       MOVE NEJ              TO WDB6-A-SW                                 
444400     ELSE                                                                 
444500       MOVE JA               TO WDB6-A-SW                                 
444600     END-IF                                                               
444700                                                                          
444800     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
444900       IF IN-EKH-KDEKSHT NOT = '406'                                      
445000         IF IN-EKH-FLDCET = NEJ                                           
445100           PERFORM S42-SKAPA-RW2-INV-POSTER                               
445200         END-IF                                                           
445300       END-IF                                                             
445400     END-IF                                                               
445500                                                                          
445600     IF IN-EKH-KDEKNIVA = 'DET'                                           
445700       IF  IN-EKH-KDEKHHT = '204'                                         
445800       AND (IN-EKH-KDEKSHT = '201')                                       
445900         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
446000       END-IF                                                             
446100                                                                          
446200       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
446300       AND (WDB6-A-FINNS                                                  
446400       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
446500       OR   DCS-DDC OR DCS-NDC-PF))                                       
446600         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
446700       END-IF                                                             
446800                                                                          
446900       IF IN-FIL-IDPGM = 'W4183000'                                       
447000       AND (WDB6-A-FINNS                                                  
447100       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
447200       OR   DCS-DDC OR DCS-NDC-PF))                                       
447300         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
447400       END-IF                                                             
447500     END-IF                                                               
447600     .                                                                    
447700     EJECT                                                                
447800                                                                          
447900 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
448000     MOVE 'RW2'              TO RW2-IDPTYP                                
448100     MOVE 'RW2'              TO WS-IDPTYP                                 
448200     MOVE ZERO               TO RW2-IDDISTR                               
448300     IF DCS-KDDC = SPACE OR DCS-DDC                                       
448400       MOVE WC-CDC-SE        TO RW2-IDDC                                  
448500     ELSE                                                                 
448600       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
448700     END-IF                                                               
448800     IF IN-EKH-KVANTAL < +0                                               
448900       MOVE '0422'           TO RW2-KDWRTYP                               
449000     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
449100     ELSE                                                                 
449200       MOVE '0421'           TO RW2-KDWRTYP                               
449300       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
449400     END-IF                                                               
449500                                                                          
449600     IF RW2-SUARTSTD NOT = +0                                             
449700       PERFORM S70-WRITE-W51310                                           
449800     END-IF                                                               
449900     .                                                                    
450000     EJECT                                                                
450100                                                                          
450200 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
450300     MOVE '0110'             TO RW1-KDWRTYP                               
450400     IF DCS-KDDC = SPACE OR DCS-DDC                                       
450500       MOVE WC-CDC-SE        TO RW1-IDDC                                  
450600     ELSE                                                                 
450700       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
450800     END-IF                                                               
450900     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
451000     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
451100     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
451200                                                                          
451300     IF RW1-SUARTSTD NOT = +0                                             
451400       MOVE 'RW1' TO WS-IDPTYP                                            
451500       PERFORM S70-WRITE-W51310                                           
451600     END-IF                                                               
451700     .                                                                    
451800     EJECT                                                                
451900                                                                          
452000 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
452100     MOVE '0110'             TO RW1-KDWRTYP                               
452200     IF DCS-KDDC = SPACE OR DCS-DDC                                       
452300       MOVE WC-CDC-SE        TO RW1-IDDC                                  
452400     ELSE                                                                 
452500       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
452600     END-IF                                                               
452700     IF IN-EKH-KDANMORS = '30'                                            
452800       MOVE ZERO             TO RW1-SUARTSTD                              
452900     ELSE                                                                 
453000      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
453100     END-IF                                                               
453200     IF IN-EKH-KDANMORS = '30' OR '80'                                    
453300       MOVE ZERO             TO RW1-SUARTSJK                              
453400     ELSE                                                                 
453500      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
453600     END-IF                                                               
453700     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
453800                                                                          
453900     IF RW1-SUARTSTD NOT = +0                                             
454000       MOVE 'RW1' TO WS-IDPTYP                                            
454100       PERFORM S70-WRITE-W51310                                           
454200     END-IF                                                               
454300     .                                                                    
454400     EJECT                                                                
454500                                                                          
454600 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
454700     MOVE '0110'             TO RW1-KDWRTYP                               
454800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
454900       MOVE WC-CDC-SE        TO RW1-IDDC                                  
455000     ELSE                                                                 
455100       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
455200     END-IF                                                               
455300     IF IN-EKH-KDEKSHT = '310'                                            
455400*** SKROTNING KDANMORS  13 O 23                                           
455500       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
455600     ELSE                                                                 
455700      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
455800     END-IF                                                               
455900                                                                          
456000     MOVE ZERO               TO RW1-SUARTSJK                              
456100                                RW1-SUARTFSG                              
456200     IF RW1-SUARTSTD NOT = +0                                             
456300       MOVE 'RW1' TO WS-IDPTYP                                            
456400       PERFORM S70-WRITE-W51310                                           
456500     END-IF                                                               
456600     .                                                                    
456700     EJECT                                                                
456800                                                                          
456900 S60-WRITE-W5156N SECTION.                                                
457000     WRITE SAPUT-POST  FROM IN-AREA                                       
457100                                                                          
457200     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
457300     MOVE 'W5156N'            TO POSTSUM-FDNAMN                           
457400     MOVE 'W51568D7'          TO POSTSUM-DDNAMN2                          
457500     CALL POSTSUM USING POSTSUM-PARM                                      
457600     .                                                                    
457700     EJECT                                                                
457800                                                                          
457900 S70-WRITE-W51310 SECTION.                                                
458000     IF WS-IDPTYP  = 'RW2'                                                
458100       IF DCS-KDDC = SPACE OR DCS-DDC                                     
458200         MOVE WC-CDC-SE        TO INV-IDDC                                
458300       ELSE                                                               
458400         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
458500       END-IF                                                             
458600       IF IN-EKH-KVANTAL < +0                                             
458700         MOVE '003'            TO INV-IDPTYP                              
458800       COMPUTE INV-SUARTSTD =                                             
458900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
459000       ELSE                                                               
459100         MOVE '002'            TO INV-IDPTYP                              
459200         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
459300       END-IF                                                             
459400       MOVE SPACE TO WS-IDPTYP                                            
459500       MOVE 0                  TO INV-ADLAGOMR                            
459600       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
459700       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
459800     END-IF                                                               
459900     IF WS-IDPTYP  = 'RW1'                                                
460000       IF DCS-KDDC = SPACE OR DCS-DDC                                     
460100         MOVE WC-CDC-SE        TO INV-IDDC                                
460200       ELSE                                                               
460300         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
460400       END-IF                                                             
460500       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
460600       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
460700       MOVE 0                  TO INV-ADLAGOMR                            
460800       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
460900       MOVE '001'              TO INV-IDPTYP                              
461000       MOVE SPACE              TO WS-IDPTYP                               
461100     END-IF                                                               
461200     WRITE INV-POST  FROM INV-W51310                                      
461300                                                                          
461400     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
461500     MOVE 'W51310'            TO POSTSUM-FDNAMN                           
461600     MOVE 'W51568D8'          TO POSTSUM-DDNAMN2                          
461700     CALL POSTSUM USING POSTSUM-PARM                                      
461800     .                                                                    
461900     EJECT                                                                
462000                                                                          
462100 S13-GET-LANDING-COST SECTION.                                            
462200     MOVE '67'                   TO W-IDDC-B6                             
462300     PERFORM IMS-GU-WDB601                                                
462400     IF SEGMENT-FINNS                                                     
462500       PERFORM IMS-GNP-WDB617                                             
462600       IF SEGMENT-FINNS                                                   
462700         IF PROC-TILANDCO >  IN-EKH-DAVERDAT                              
462800           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
462900         ELSE                                                             
463000           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
463100         END-IF                                                           
463200       END-IF                                                             
463300     END-IF                                                               
463400     .                                                                    
463500     EJECT                                                                
463600 S80-GET-CURRENCY-RATE SECTION.                                           
463700     MOVE +0                  TO W-ANT                                    
463800     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
463900             BEFORE INITIAL ' '                                           
464000     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
464100     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
464200     PERFORM IMS-GU-WDL601                                                
464300     IF SEGMENT-SAKNAS                                                    
464400       CONTINUE                                                           
464500     ELSE                                                                 
464600       PERFORM IMS-GNP-WDL611                                             
464700       IF SEGMENT-SAKNAS                                                  
464800         CONTINUE                                                         
464900       ELSE                                                               
465000         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
465100                                   - INL-DAINLEV                          
465200         MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                   
465300         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
465400         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
465500         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
465600         MOVE WS-KDVALISO-IN        TO CURR-KDVALISO-ROW                  
465700         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
465800         IF CURR-KDSVAR = ' '                                             
465900           MOVE CURR-PRKURS-NEW     TO WS-PRKURS-IN3                      
466000         ELSE                                                             
466100           MOVE +1                  TO WS-PRKURS-IN3                      
466200         END-IF                                                           
466300       END-IF                                                             
466400     END-IF                                                               
466500     .                                                                    
466600     EJECT                                                                
466700                                                                          
466800 S81-GET-CURRENCY-RATE SECTION.                                           
466900     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
467000     MOVE 'INR'               TO CURR-KDVALISO-ROW                        
467100     IF IN-FIL-IDPGM = 'W4183300'                                         
467200       IF IN-EKH-DAAVIDAT > ZERO                                          
467300         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
467400         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
467500       ELSE                                                               
467600         MOVE WS-TIAA            TO WS-TIAA-CR                            
467700         MOVE WS-TIMM            TO WS-TIMM-CR                            
467800       END-IF                                                             
467900     ELSE                                                                 
468000       MOVE WS-TIAA              TO WS-TIAA-CR                            
468100       MOVE WS-TIMM              TO WS-TIMM-CR                            
468200     END-IF                                                               
468300     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
468400     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
468500     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
468600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
468700     IF CURR-KDSVAR = ' '                                                 
468800       IF IN-EKH-IDDISTR > ZERO                                           
468900         MOVE CURR-PRKURS-NEW TO WS-PRKURS-IN3                            
469000       ELSE                                                               
469100         IF WS-PRKURS = ZERO                                              
469200           MOVE 1           TO WS-PRKURS-IN3                              
469300         END-IF                                                           
469400       END-IF                                                             
469500     ELSE                                                                 
469600       MOVE 1               TO WS-PRKURS-IN3                              
469700     END-IF                                                               
469800     .                                                                    
469900     EJECT                                                                
470000* --- IMS SECTIONS ---                                                    
470100                                                                          
470200 IMS-GU-WDH521 SECTION.                                                   
470300     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
470400          DELIMITED BY SIZE INTO SSA1                                     
470500     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
470600          DELIMITED BY SIZE INTO SSA2                                     
470700     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
470800          DELIMITED BY SIZE INTO SSA3                                     
470900     MOVE '  '              TO GODK-STATUSKODER                           
471000     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
471100                                                   SSA2                   
471200                                                   SSA3                   
471300     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
471400                                                                          
471500     PERFORM IMS-STATUS-CONTROL                                           
471600     .                                                                    
471700                                                                          
471800 IMS-GNP-WDH531 SECTION.                                                  
471900     MOVE 'WDH531  '        TO SSA1                                       
472000     MOVE '  GE'            TO GODK-STATUSKODER                           
472100     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
472200     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
472300                               WS-STATUS                                  
472400     PERFORM IMS-STATUS-CONTROL                                           
472500     .                                                                    
472600     EJECT                                                                
472700                                                                          
472800 IMS-GU-WDB201 SECTION.                                                   
472900     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-KEY ')'                         
473000          DELIMITED BY SIZE INTO SSA1                                     
473100     MOVE '  GE'                 TO GODK-STATUSKODER                      
473200     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
473300      MOVE GMTA-STATUS-CODE      TO STATUS-WS                             
473400     PERFORM IMS-STATUS-CONTROL                                           
473500     .                                                                    
473600     EJECT                                                                
473700                                                                          
473800 IMS-GU-WDB101 SECTION.                                                   
473900     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
474000          DELIMITED BY SIZE INTO SSA1                                     
474100     MOVE '  GE'               TO GODK-STATUSKODER                        
474200     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
474300     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
474400     PERFORM IMS-STATUS-CONTROL                                           
474500     .                                                                    
474600     EJECT                                                                
474700                                                                          
474800 IMS-GU-5122 SECTION.                                                     
474900     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
475000            DELIMITED BY SIZE INTO SSA1                                   
475100     STRING 'WDGX5122(KEY5122  =' W-WDGXKEY-5122-X ')'                    
475200            DELIMITED BY SIZE INTO SSA2                                   
475300     MOVE '  GE'           TO GODK-STATUSKODER                            
475400     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5122 SSA1 SSA2            
475500     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
475600     PERFORM IMS-STATUS-CONTROL                                           
475700     .                                                                    
475800                                                                          
475900 IMS-GU-5121 SECTION.                                                     
476000     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
476100            DELIMITED BY SIZE INTO SSA1                                   
476200     MOVE '    '           TO GODK-STATUSKODER                            
476300     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5121 SSA1                 
476400     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
476500     PERFORM IMS-STATUS-CONTROL                                           
476600     .                                                                    
476700                                                                          
476800 IMS-GNP-5122 SECTION.                                                    
476900     STRING 'WDGX5122(KEY5122 >=' W-WDGXKEY-5122-MIN-X                    
477000                    '&KEY5122 <=' W-WDGXKEY-5122-MAX-X ')'                
477100            DELIMITED BY SIZE INTO SSA1                                   
477200     MOVE '  GE'           TO GODK-STATUSKODER                            
477300     CALL CBLTDLI USING GNP 5121-PCB DLI-IO-WDGX5122 SSA1                 
477400     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
477500     PERFORM IMS-STATUS-CONTROL                                           
477600     .                                                                    
477700     EJECT                                                                
477800                                                                          
477900 IMS-GU-WDB601    SECTION.                                                
478000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
478100          DELIMITED BY SIZE INTO SSA1                                     
478200     MOVE '  GE' TO GODK-STATUSKODER                                      
478300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
478400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
478500     PERFORM IMS-STATUS-CONTROL                                           
478600     IF SEGMENT-SAKNAS                                                    
478700        MOVE SPACE TO DCS-KDDC                                            
478800     END-IF                                                               
478900     .                                                                    
479000     EJECT                                                                
479100                                                                          
479200 IMS-GNP-WDB617 SECTION.                                                  
479300     MOVE 'WDB617   ' TO SSA1                                             
479400     MOVE '  GE'        TO GODK-STATUSKODER                               
479500     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
479600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
479700     PERFORM IMS-STATUS-CONTROL                                           
479800     .                                                                    
479900     SKIP3                                                                
480000                                                                          
480100 IMS-GU-WDL601   SECTION.                                                 
480200     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
480300          DELIMITED BY SIZE INTO SSA1                                     
480400     MOVE '  GE' TO GODK-STATUSKODER                                      
480500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
480600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
480700     PERFORM IMS-STATUS-CONTROL                                           
480800     .                                                                    
480900     SKIP3                                                                
481000                                                                          
481100 IMS-GNP-WDL611   SECTION.                                                
481200     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
481300          DELIMITED BY SIZE INTO SSA1                                     
481400     MOVE '  GE' TO GODK-STATUSKODER                                      
481500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
481600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
481700     PERFORM IMS-STATUS-CONTROL                                           
481800     .                                                                    
481900     SKIP3                                                                
482000                                                                          
482100 IMS-STATUS-CONTROL SECTION.                                              
482200     SET STATUS-IX TO 1                                                   
482300     SEARCH GODK-STATUS                                                   
482400       AT END                                                             
482500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
482600           DELIMITED BY SIZE INTO FELTEXT                                 
482700         DISPLAY FELTEXT                                                  
482800         CALL FELLOG                                                      
482900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
483000         CONTINUE                                                         
483100     END-SEARCH                                                           
483200     .                                                                    
