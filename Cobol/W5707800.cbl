000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5707800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   20190911.                                                
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
003000*                              WLBETC (WDB1)                              
003100*                              WLGMTA (WDB2)                              
003200*                              WL5121 (WDR1)                              
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
004700     SELECT W57066                     ASSIGN TO W57078D1.                
004800                                                                          
004900*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
005000     SELECT W57091A                    ASSIGN TO W57078D2.                
005100                                                                          
005200*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005300     SELECT W57092A                    ASSIGN TO W57078D3.                
005400                                                                          
005500*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005600     SELECT W57093A                    ASSIGN TO W57078D4.                
005700                                                                          
005800*          --- LOGG TILL ON-DEMAND                                        
005900     SELECT W57095                     ASSIGN TO W57078D5.                
006000                                                                          
006100*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006200     SELECT W57090                     ASSIGN TO W57078D6.                
006300                                                                          
006400*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006500     SELECT W5709N                     ASSIGN TO W57078D7.                
006600                                                                          
006700*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006800     SELECT W51390                     ASSIGN TO W57078D8.                
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
008100 FD  W57091A                                                              
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400*01  71INIT-POST -COPY R3INIT20               -L.                         
008500*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008600*01  71LINE-POST -COPY R3LINE20               -L.                         
008700                                                                          
008800 FD  W57092A                                                              
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100*01  72LINE-POST -COPY R3LINE20               -L.                         
009200                                                                          
009300 FD  W57093A                                                              
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009700*01  73LINE-POST -COPY R3LINE20               -L.                         
009800                                                                          
009900 FD  W57095                                                               
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200*01  LOGG-POST   -COPY W57073                 -L.                         
010300                                                                          
010400 FD  W57090                                                               
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700*01  AVST-POST   -COPY W57070                 -L.                         
010800                                                                          
010900 FD  W5709N                                                               
011000     RECORDING       F                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300 01  SAPUT-POST.                                                          
011400*    03  -COPY WDR801        -L.                                          
011500     03 FILLER                   PIC X(6).                                
011600                                                                          
011700 FD  W51390                                                               
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012100                                                                          
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400*    -- CHECKED BY WY2000                                                 
012500 77  IDPGM                        PIC X(8)    VALUE 'W5707800'.           
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
013700 77  WS-LINE-AMOUNT-131-1         PIC S9(13)V99 COMP-3.                   
013800 77  WS-LINE-AMOUNT-131-2         PIC S9(13)V99 COMP-3.                   
013900 77  SPAR-SUMMA-102-125         PIC S9(13)V99  COMP-3 VALUE ZERO.         
014000 77  WS-BELOPP                  PIC S9(13)V99  COMP-3.                    
014100 77  WS-LOP                       PIC 9       VALUE ZERO.                 
014200 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
014300 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
014400 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
014500 77  SPAR-LINE-ORDER              PIC X(12).                              
014600 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
014700 77  WS-RED-IDKST                 PIC X(10).                              
014800 77  WS-IDPTYP                    PIC X(3).                               
014900 77  WS-FAKTURA-DATUM             PIC X(16).                              
015000 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
015100 77  SPAR-SUMMA                 PIC S9(13)V99  COMP-3 VALUE ZERO.         
015200 77  SPAR-PRDMTRL               PIC S9(13)V99  COMP-3 VALUE ZERO.         
015300 77  SPAR-PROVRPAL              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015400 77  SPAR-PRDIRLON              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015500 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
015600 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
015700 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
015800 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
015900 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
016000 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
016100 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
016200 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
016300 77  WS-KDPRODSL-SAVE             PIC X(2)    VALUE SPACE.                
016400                                                                          
016500 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
016600       88  WDB6-A-FINNS                       VALUE 'J'.                  
016700       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
016800                                                                          
016900*01  -COPY WWPRODSL                                                       
017000                                                                          
017100*01  -COPY WWDCKONS                                                       
017200     EJECT                                                                
017300                                                                          
017400 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
017500*01  -COPY WWIDFTG                                                        
017600     EJECT                                                                
017700                                                                          
017800 01  FELTEXT                      PIC X(80).                              
017900 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
018000*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
018100     EJECT                                                                
018200                                                                          
018300 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
018400 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
018500     EJECT                                                                
018600 01  WS-IDDISTR-IDKUNDNR.                                                 
018700     03  FILLER                   PIC X(2)    VALUE SPACE.                
018800     03  WS-IDDISTR               PIC 9(4).                               
018900     03  WS-IDKUNDNR              PIC 9(6).                               
019000                                                                          
019100 01  WS-KDBETVIL                  PIC X(4).                               
019200 01  WS-KDVALISO-WDB1             PIC X(3).                               
019300 01  WS-KDVALISO                  PIC X(3).                               
019400 01  WS-KDVALISO-KR               PIC X(3) VALUE 'KRW'.                   
019500 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
019600 01  WS-PRKURS-KR                 PIC S9(6)V9(5) COMP-3.                  
019700 01  WS-PRKURS-KR2                PIC S9(6)V9(5) COMP-3.                  
019800 01  WS-PRKURS-KR3                PIC S9(6)V9(5) COMP-3.                  
019900 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
020000 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
020100                                                                          
020200 01  WS-ALLOCATE.                                                         
020300     03  WS-ALLOCATE-DC           PIC X(2).                               
020400     03  WS-ALLOCATE-DISTR        PIC X(5).                               
020500     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
020600     03  FILLER                   PIC X(4)    VALUE SPACE.                
020700                                                                          
020800 01  WS-TEXT.                                                             
020900     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
021000     03  WS-TEXT-KDEKHHT          PIC X(3).                               
021100     03  WS-TEXT-KDEKSHT          PIC X(3).                               
021200     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
021300     03  FILLER                   PIC X(7)    VALUE SPACE.                
021400                                                                          
021500 01  WS-LINE-TEXT.                                                        
021600     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
021700     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
021800     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
021900     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
022000     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
022100     03  FILLER                   PIC X(22)   VALUE SPACE.                
022200                                                                          
022300 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
022400 01  WS-PRCTR.                                                            
022500     03  WS-PRCTR-PRODSL          PIC X(2).                               
022600     03  FILLER                   PIC X(1).                               
022700     03  FILLER                   PIC X(7).                               
022800                                                                          
022900 01  WS-R3-ACCOUNT.                                                       
023000     03  WS-R3-ACCOUNT-ALFA.                                              
023100         05 FILLER                PIC X(4).                               
023200         05 WS-R3-ACCOUNT-6       PIC X(6).                               
023300     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
023400         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
023500                                                                          
023600 01  WS-ACCOUNT.                                                          
023700     03  FILLER                   PIC X(7).                               
023800     03  WS-ACCOUNT-4             PIC X(1).                               
023900     03  FILLER                   PIC X(2).                               
024000                                                                          
024100 01  SPAR-AREA.                                                           
024200     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
024300     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
024400     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
024500     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
024600                                                                          
024700 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
024800 01  FILLER REDEFINES DAGENS-DATUM.                                       
024900     03  DAGENS-DATUM-AAR         PIC 9(2).                               
025000     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
025100     03  DAGENS-DATUM-DAG         PIC 9(2).                               
025200                                                                          
025300 01  WS-NEW-MONTH                 PIC 9(2).                               
025400                                                                          
025500 01  WS-DAREGDAT.                                                         
025600     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
025700     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
025800                                                                          
025900 01  WS-TIREGDAT-TOT.                                                     
026000     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
026100     03  WS-TIREGDAT              PIC 9(6).                               
026200                                                                          
026300 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
026400 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
026500     EJECT                                                                
026600                                                                          
026700 01  DYNAMISKA-SUBPROGRAM.                                                
026800     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
026900     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
027000     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
027100     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
027200     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
027300     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
027400                                                                          
027500*    --- PARAMETRAR TILL ABEND                                            
027600 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
027700 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
027800 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
027900     EJECT                                                                
028000                                                                          
028100*    --- PARAMETRAR TILL DATKORT                                          
028200 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57078'.             
028300                                                                          
028400 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
028500*01  -COPY WDATKORT                                                       
028600     EJECT                                                                
028700                                                                          
028800*    --- PARAMETRAR TILL POSTSUM                                          
028900*01  -COPY W0005   -PRE  POSTSUM-                                         
029000     EJECT                                                                
029100                                                                          
029200 01  FILLER                          PIC X(16) VALUE 'W510CURR '.         
029300*01  -COPY W510CURR                                                       
029400     EJECT                                                                
029500                                                                          
029600 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
029700*01  AREA -COPY WDR801           -PRE IN-                                 
029800*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
029900         05   IN-EKH-IDSYSMOT     PIC X(6).                               
030000                                                                          
030100     EJECT                                                                
030200 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
030300                                                                          
030400*01  -COPY R3LINE20              -PRE R3-                                 
030500*01  -COPY R3HEAD20              -PRE R3-                                 
030600*01  -COPY R3INIT20              -PRE R3-                                 
030700*01  -COPY W57073                -PRE LOGG-                               
030800*01  -COPY W57070                -PRE AVST-                               
030900*01  -COPY W517RW1               -PRE RW1-                                
031000*01  -COPY W517RW2               -PRE RW2-                                
031100*01  -COPY W51310                -PRE INV-                                
031200     EJECT                                                                
031300                                                                          
031400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031500 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
031600                                                                          
031700 01  NYCKLAR-TILL-DLI.                                                    
031800     03  W-WDH501KY-X.                                                    
031900         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
032000         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
032100     03  W-KDEKSHT-X.                                                     
032200         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
032300     03  W-KDEKNIVA-X.                                                    
032400         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
032500     03  W-WDH531KY-X.                                                    
032600         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
032700         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
032800     03  W-IDRADNR-X.                                                     
032900         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
033000                                                                          
033100     03  W-IDGMT-KEY.                                                     
033200         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
033300         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
033400                                                                          
033500     03  W-WDB101KY-X.                                                    
033600         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
033700         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
033800                                                                          
033900     03  W-WDGXKEY-5121-X.                                                
034000         05  FILLER               PIC X(4)    VALUE '5121'.               
034100         05  FILLER               PIC X(2)    VALUE '61'.                 
034200         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
034300     03  W-WDGXKEY-5122-X.                                                
034400         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
034500         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
034600     03  W-WDGXKEY-5122-MIN-X.                                            
034700         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
034800         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
034900     03  W-WDGXKEY-5122-MAX-X.                                            
035000         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
035100         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
035200                                                                          
035300     03  W-IDDC-B6-X.                                                     
035400         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
035500                                                                          
035600     03  W-IDARTNR-X.                                                     
035700         05 W-IDARTNR             PIC S9(9) COMP-3.                       
035800                                                                          
035900     03  W-IDFAKT-X.                                                      
036000         05 W-IDFAKT              PIC S9(7) COMP-3.                       
036100                                                                          
036200     03  W-IDLEVNR-X.                                                     
036300         05  W-IDLEVNR            PIC X(5)    VALUE SPACE.                
036400                                                                          
036500     EJECT                                                                
036600                                                                          
036700*    --- STATUS-KOD FRÅN IMS                                              
036800 01  STATUS-WS                    PIC XX.                                 
036900     88  SEGMENT-FINNS                        VALUE '  '.                 
037000     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
037100                                                                          
037200 01  GODK-STATUSKODER.                                                    
037300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037400                                                                          
037500 01  SSA1                         PIC X(128).                             
037600 01  SSA2                         PIC X(64).                              
037700 01  SSA3                         PIC X(64).                              
037800     EJECT                                                                
037900                                                                          
038000*    --- IMS FUNKTIONSKODER                                               
038100*01  -COPY W0003                                                          
038200     EJECT                                                                
038300                                                                          
038400*    ---  DLI INPUT-OUTPUT AREA                                           
038500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
038600 01  DLI-IO-WDH501.                                                       
038700*    03  -COPY WDH501                                                     
038800     EJECT                                                                
038900                                                                          
039000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
039100 01  DLI-IO-WDH511.                                                       
039200*    03  -COPY WDH511                                                     
039300     EJECT                                                                
039400                                                                          
039500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
039600 01  DLI-IO-WDH521.                                                       
039700*    03  -COPY WDH521                                                     
039800     EJECT                                                                
039900                                                                          
040000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
040100 01  DLI-IO-WDH531.                                                       
040200*    03  -COPY WDH531                                                     
040300     EJECT                                                                
040400                                                                          
040500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
040600 01  DLI-IO-WLBETC01.                                                     
040700*    03  -COPY WDB101                                                     
040800     EJECT                                                                
040900                                                                          
041000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
041100 01  DLI-IO-WLGMTA01.                                                     
041200*    03  -COPY WDB201                                                     
041300     EJECT                                                                
041400                                                                          
041500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
041600 01  DLI-IO-WDGX5121.                                                     
041700*    03  -COPY WDGX5121                                                   
041800     EJECT                                                                
041900                                                                          
042000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
042100 01  DLI-IO-WDGX5122.                                                     
042200*    03  -COPY WDGX5122                                                   
042300     EJECT                                                                
042400                                                                          
042500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042600 01   DLI-IO-AREA-B601.                                                   
042700*     03  -COPY WDB601                                                    
042800     EJECT                                                                
042900 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
043000 01   DLI-IO-WDB617.                                                      
043100*     03  -COPY WDB617                                                    
043200     EJECT                                                                
043300 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
043400 01   DLI-IO-AREA-L601.                                                   
043500*     03  -COPY WDL601                                                    
043600     EJECT                                                                
043700 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
043800 01   DLI-IO-AREA-L611.                                                   
043900*     03  -COPY WDL611                                                    
044000     EJECT                                                                
044100 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6C1'.                
044200     SKIP3                                                                
044300 01  FILLER               PIC X(16)   VALUE 'WDF101 AREA'.                
044400 01  DLI-IO-WDF101.                                                       
044500*    03  -COPY WDF101                                                     
044600     EJECT                                                                
044700 01  FILLER               PIC X(16)   VALUE 'WDF106 AREA'.                
044800 01  DLI-IO-WDF106.                                                       
044900*    03  -COPY WDF106                                                     
045000     EJECT                                                                
045100 LINKAGE SECTION.                                                         
045200*01  -COPY W0008  -PRE WDH5-                                              
045300     05  FILLER                  PIC X.                                   
045400                                                                          
045500*01  -COPY W0008  -PRE GMTA-                                              
045600     05  FILLER                  PIC X.                                   
045700                                                                          
045800*01  -COPY W0008  -PRE BETC-                                              
045900     05  FILLER                  PIC X.                                   
046000                                                                          
046100*01  -COPY W0008  -PRE 5121-                                              
046200     05  FILLER                  PIC X.                                   
046300                                                                          
046400*01  -COPY W0008  -PRE WDG2-                                              
046500     05  FILLER                  PIC X.                                   
046600                                                                          
046700*01  -COPY W0008  -PRE WDB6-                                              
046800     05  FILLER                  PIC X.                                   
046900                                                                          
047000*01  -COPY W0008  -PRE WDL6-                                              
047100     05  FILLER                  PIC X.                                   
047200                                                                          
047300*01  -COPY W0008  -PRE WDF1-                                              
047400     05  FILLER                  PIC X.                                   
047500                                                                          
047600     EJECT                                                                
047700                                                                          
047800 PROCEDURE DIVISION  USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
047900                           WDG2-PCB WDB6-PCB WDL6-PCB WDF1-PCB.           
048000 MAIN SECTION.                                                            
048100     ENTRY 'DLITCBL' USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
048200                           WDG2-PCB WDB6-PCB WDL6-PCB WDF1-PCB.           
048300                                                                          
048400     PERFORM A-INIT                                                       
048500                                                                          
048600     PERFORM S01-READ-W57066                                              
048700     PERFORM UNTIL END-OF-W57066                                          
048800*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
048900       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
049000       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
049100       AND WS-NEW-MONTH > 01                                              
049200         PERFORM S60-WRITE-W5709N                                         
049300       ELSE                                                               
049400         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
049500         PERFORM S30-READ-DATABASE-B2-B1                                  
049600         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
049700           PERFORM C-EXECUTE                                              
049800         END-IF                                                           
049900       END-IF                                                             
050000       PERFORM S01-READ-W57066                                            
050100     END-PERFORM                                                          
050200                                                                          
050300     PERFORM Z-FINI                                                       
050400                                                                          
050500     MOVE ZERO TO RETURN-CODE                                             
050600     GOBACK                                                               
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000 A-INIT SECTION.                                                          
051100     OPEN INPUT  W57066                                                   
051200                                                                          
051300     OPEN OUTPUT W57090                                                   
051400                 W57091A                                                  
051500                 W57092A                                                  
051600                 W57093A                                                  
051700                 W57095                                                   
051800                 W5709N                                                   
051900                 W51390                                                   
052000                                                                          
052100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
052200     MOVE 20               TO RW1-DAVVREG(1:2)                            
052300     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
052400                              RW1-DAVVREG(3:2)                            
052500                              W-DATE-AAMM(1:2)                            
052600                              WS-TIAA                                     
052700     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
052800                              W-DATE-AAMM(3:2)                            
052900                              WS-TIMM                                     
053000                              WS-NEW-MONTH                                
053100     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
053200     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
053300     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
053400                                                                          
053500*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
053600*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
053700     IF WS-NEW-MONTH = 12                                                 
053800       MOVE 1              TO WS-NEW-MONTH                                
053900     ELSE                                                                 
054000       ADD 1               TO WS-NEW-MONTH                                
054100*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
054200***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
054300***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
054400***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
054500***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
054600       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
054700       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
054800         ADD 1             TO WS-NEW-MONTH                                
054900         ADD 1             TO WS-TIMM                                     
055000         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
055100       END-IF                                                             
055200     END-IF                                                               
055300                                                                          
055400     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
055500                                                                          
055600     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
055700                                                                          
055800     ACCEPT DAGENS-KLOCKA FROM TIME                                       
055900     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
056000                                                                          
056100     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
056200     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
056300     MOVE 'M'                   TO CURR-KDVALTYP                          
056400                                                                          
056500     MOVE WS-KDVALISO-KR        TO CURR-KDVALISO-ROW                      
056600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
056700     IF CURR-KDSVAR = ' '                                                 
056800       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-KR                           
056900     ELSE                                                                 
057000       MOVE 1                   TO WS-PRKURS-KR                           
057100     END-IF                                                               
057200     COMPUTE WS-PRKURS-KR2 ROUNDED = 1 / WS-PRKURS-KR                     
057300     MOVE WS-PRKURS-KR          TO WS-PRKURS-KR3                          
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700 C-EXECUTE SECTION.                                                       
057800     MOVE WC-IDFTG-KR           TO W-IDFTG                                
057900     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
058000     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
058100     IF IN-EKH-KDEKNIVA = 'TDET'                                          
058200       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
058300     END-IF                                                               
058400     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
058500     PERFORM IMS-GU-WDH521                                                
058600     PERFORM IMS-GNP-WDH531                                               
058700                                                                          
058800     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
058900     IF IN-EKH-IDDISTR > ZERO                                             
059000       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
059100     ELSE                                                                 
059200       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
059300     END-IF                                                               
059400     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
059500                                                                          
059600* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
059700* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
059800* BOKFÖRINGEN I SAP SKER DOCK ALLTID I KRW, DÄRFÖR BYTET HÄR:             
059900*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
060000*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '106')               
060100*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '107')               
060200*      MOVE 'KRW'               TO WS-KDVALISO                            
060300*    END-IF                                                               
060400                                                                          
060500     IF  ((IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                               
060600     AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                                
060700     OR (IN-EKH-KDEKHHT = '303'                                           
060800     AND IN-EKH-KDEKSHT = '301')                                          
060900     OR (IN-EKH-KDEKHHT = '303'                                           
061000     AND IN-EKH-KDEKSHT = '307'))                                         
061100       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
061200     ELSE                                                                 
061300       MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                             
061400       MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                             
061500       IF WS-LOP = 9                                                      
061600         MOVE ZERO  TO WS-LOP                                             
061700       ELSE                                                               
061800         ADD +1     TO WS-LOP                                             
061900       END-IF                                                             
062000       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
062100     END-IF                                                               
062200* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
062300     IF SYST-IDPTYP = '210'                                               
062400       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
062500     ELSE                                                                 
062600       IF SYST-IDPTYP = '310'                                             
062700         PERFORM CC-CREATE-WRITE-HEADER-AR                                
062800       ELSE                                                               
062900* TEST OM BRYTNING PÅ VERIFIKATION                                        
063000         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
063100         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
063200         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
063300         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
063400           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
063500           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
063600           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
063700           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
063800           IF (IN-EKH-KDEKHHT = '102'                                     
063900           AND IN-EKH-KDEKSHT = '121')                                    
064000           OR (IN-EKH-KDEKHHT = '102'                                     
064100           AND IN-EKH-KDEKSHT = '122')                                    
064200           OR (IN-EKH-KDEKHHT = '102'                                     
064300           AND IN-EKH-KDEKSHT = '131')                                    
064400           OR (IN-EKH-KDEKHHT = '102'                                     
064500           AND IN-EKH-KDEKSHT = '132')                                    
064600             PERFORM S80-GET-CURRENCY-RATE                                
064700           END-IF                                                         
064800           IF (IN-EKH-KDEKHHT = '303'                                     
064900           AND IN-EKH-KDEKSHT = '301')                                    
064910           OR (IN-EKH-KDEKHHT = '303'                                     
064920           AND IN-EKH-KDEKSHT = '307')                                    
064930           OR (IN-EKH-KDEKHHT = '303'                                     
064940           AND IN-EKH-KDEKSHT = '371')                                    
064950           OR (IN-EKH-KDEKHHT = '303'                                     
064960           AND IN-EKH-KDEKSHT = '3XX')                                    
065000             PERFORM S81-GET-CURRENCY-RATE                                
065100           END-IF                                                         
065200*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
065300*   HEADER-POST TILL HUVUDBOKEN                                           
065400           IF (IN-EKH-KDEKHHT = '102'                                     
065500           AND IN-EKH-KDEKSHT = '120')                                    
065600           OR (IN-EKH-KDEKHHT = '102'                                     
065700           AND IN-EKH-KDEKSHT = '124')                                    
065800           OR (IN-EKH-KDEKHHT = '102'                                     
065900           AND IN-EKH-KDEKSHT = '125')                                    
066000           OR (IN-EKH-KDEKHHT = '102'                                     
066100           AND IN-EKH-KDEKSHT = '130')                                    
066200           OR (IN-EKH-KDEKHHT = '102'                                     
066300           AND IN-EKH-KDEKSHT = '134')                                    
066400           OR (IN-EKH-KDEKHHT = '103'                                     
066500           AND IN-EKH-KDEKSHT = '102')                                    
066600           OR (IN-EKH-KDEKHHT = '103'                                     
066700           AND IN-EKH-KDEKSHT = '106')                                    
066800           OR (IN-EKH-KDEKHHT = '103'                                     
066900           AND IN-EKH-KDEKSHT = '107')                                    
067000           OR (IN-EKH-KDEKHHT = '204'                                     
067100           AND IN-EKH-KDEKSHT = '301')                                    
067200           OR (IN-EKH-KDEKHHT = '303'                                     
067300           AND IN-EKH-KDEKSHT = '301')                                    
067400           OR (IN-EKH-KDEKHHT = '303'                                     
067500           AND IN-EKH-KDEKSHT = '307')                                    
067600           OR (IN-EKH-KDEKHHT = '303'                                     
067700           AND IN-EKH-KDEKSHT = '3XX')                                    
067800           OR (IN-EKH-KDEKHHT = '303'                                     
067900           AND IN-EKH-KDEKSHT = '371')                                    
068000             CONTINUE                                                     
068100           ELSE                                                           
068200             PERFORM CA-CREATE-WRITE-HEADER-GL                            
068300           END-IF                                                         
068400         END-IF                                                           
068500       END-IF                                                             
068600     END-IF                                                               
068700                                                                          
068800**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
068900     MOVE WS-STATUS TO STATUS-WS                                          
069000     PERFORM UNTIL SEGMENT-SAKNAS                                         
069100       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
069200                                                                          
069300* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
069400       IF SYST-IDPTYP = '610'                                             
069500         PERFORM CD-BUILD-COMMON-610-PART                                 
069600         PERFORM CE-SCHEDULE-LINE-GL                                      
069700       ELSE                                                               
069800         IF SYST-IDPTYP = '210'                                           
069900           PERFORM CF-BUILD-COMMON-210-PART                               
070000           PERFORM CG-SCHEDULE-LINE-AP                                    
070100         ELSE                                                             
070200           IF SYST-IDPTYP = '310'                                         
070300             PERFORM CH-BUILD-COMMON-310-PART                             
070400             PERFORM CI-SCHEDULE-LINE-AR                                  
070500           END-IF                                                         
070600         END-IF                                                           
070700       END-IF                                                             
070800       PERFORM IMS-GNP-WDH531                                             
070900     END-PERFORM                                                          
071000     .                                                                    
071100     EJECT                                                                
071200                                                                          
071300 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
071400     MOVE SPACE                   TO R3-HEAD-R3                           
071500     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
071600     MOVE 'KR02'                  TO R3-HEAD-COMPANY-CODE                 
071700     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
071800     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
071900     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
072000     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
072100     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
072200       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
072300     ELSE                                                                 
072400       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
072500     END-IF                                                               
072600     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
072700     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
072800     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
072900     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
073000     IF WS-KDVALISO = 'KRW'                                               
073100       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
073200     ELSE                                                                 
073300       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
073400       MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                     
073500       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
073600       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
073700       IF CURR-KDSVAR = ' '                                               
073800         IF IN-EKH-IDDISTR > ZERO                                         
073900           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
074000         ELSE                                                             
074100           MOVE 1               TO WS-PRKURS                              
074200         END-IF                                                           
074300       ELSE                                                               
074400         MOVE 1                 TO WS-PRKURS                              
074500       END-IF                                                             
074600       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
074700                                       CURR-REVALUTA-TO                   
074800       IF CURR-REVALUTA-TO = +1                                           
074900         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
075000       END-IF                                                             
075100       IF CURR-REVALUTA-TO = +10                                          
075200         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
075300       END-IF                                                             
075400       IF CURR-REVALUTA-TO = +100                                         
075500         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
075600       END-IF                                                             
075700     END-IF                                                               
075800     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
075900     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
076000     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
076100     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
076200     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
076300     MOVE JA                      TO WS-HEADER-SW                         
076400     MOVE NEJ                     TO WS-LINE-SW                           
076500                                                                          
076600* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
076700     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
076800     OR (IN-EKH-KDEKHHT = '303'                                           
076900     AND IN-EKH-KDEKSHT = '391')                                          
077000     OR (IN-EKH-KDEKHHT = '102'                                           
077100     AND IN-EKH-KDEKSHT = '121')                                          
077200     OR (IN-EKH-KDEKHHT = '102'                                           
077300     AND IN-EKH-KDEKSHT = '131')                                          
077400       PERFORM S004-WRITE-W57093A-HEAD                                    
077500     ELSE                                                                 
077600       PERFORM S002-WRITE-W57091A-HEAD                                    
077700     END-IF                                                               
077800     .                                                                    
077900     EJECT                                                                
078000                                                                          
078100 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
078200     MOVE SPACE                   TO R3-HEAD-R3                           
078300     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
078400     MOVE 'KR02'                  TO R3-HEAD-COMPANY-CODE                 
078500                                     R3-HEAD-CONTROL-AREA                 
078600     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
078700     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
078800     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
078900     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
079000     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
079100     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
079200       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
079300     ELSE                                                                 
079400       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
079500     END-IF                                                               
079600     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
079700     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
079800     IF (IN-EKH-KDEKHHT = '103'                                           
079900     AND IN-EKH-KDEKSHT = '102')                                          
080000     OR (IN-EKH-KDEKHHT = '103'                                           
080100     AND IN-EKH-KDEKSHT = '106')                                          
080200     OR (IN-EKH-KDEKHHT = '103'                                           
080300     AND IN-EKH-KDEKSHT = '107')                                          
080400       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
080500       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
080600     ELSE                                                                 
080700       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
080800       MOVE WS-PRKURS-KR2         TO R3-HEAD-EXCHANGE-RATE                
080900       MOVE 'KRW'                 TO CURR-KDVALISO-ROW                    
081000       IF IN-FIL-IDPGM = 'W4183300'                                       
081100         IF IN-EKH-DAAVIDAT > ZERO                                        
081200           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
081300           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
081400         ELSE                                                             
081500           MOVE WS-TIAA              TO WS-TIAA-CR                        
081600           MOVE WS-TIMM              TO WS-TIMM-CR                        
081700         END-IF                                                           
081800       ELSE                                                               
081900         MOVE WS-TIAA                TO WS-TIAA-CR                        
082000         MOVE WS-TIMM                TO WS-TIMM-CR                        
082100       END-IF                                                             
082200       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
082300       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
082400       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
082500       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
082600       IF CURR-KDSVAR = ' '                                               
082700         IF IN-EKH-IDDISTR > ZERO                                         
082800           MOVE CURR-PRKURS-NEW TO WS-PRKURS-KR                           
082900         ELSE                                                             
083000           IF WS-PRKURS = ZERO                                            
083100             MOVE 1             TO WS-PRKURS-KR                           
083200           END-IF                                                         
083300         END-IF                                                           
083400       ELSE                                                               
083500         MOVE 1                 TO WS-PRKURS-KR                           
083600       END-IF                                                             
083700       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-KR *                     
083800                                       CURR-REVALUTA-TO                   
083900       END-COMPUTE                                                        
084000       IF CURR-REVALUTA-TO = +1                                           
084100         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
084200       END-IF                                                             
084300       IF CURR-REVALUTA-TO = +10                                          
084400         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
084500       END-IF                                                             
084600       IF CURR-REVALUTA-TO = +100                                         
084700         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
084800       END-IF                                                             
084900     END-IF                                                               
085000     IF (IN-EKH-KDEKHHT = '102'                                           
085100     AND IN-EKH-KDEKSHT = '120')                                          
085200     OR (IN-EKH-KDEKHHT = '102'                                           
085300     AND IN-EKH-KDEKSHT = '124')                                          
085400     OR (IN-EKH-KDEKHHT = '102'                                           
085500     AND IN-EKH-KDEKSHT = '125')                                          
085600     OR (IN-EKH-KDEKHHT = '102'                                           
085700     AND IN-EKH-KDEKSHT = '130')                                          
085800     OR (IN-EKH-KDEKHHT = '102'                                           
085900     AND IN-EKH-KDEKSHT = '134')                                          
086000     OR (IN-EKH-KDEKHHT = '303'                                           
086100     AND IN-EKH-KDEKSHT = '301')                                          
086200     OR (IN-EKH-KDEKHHT = '303'                                           
086300     AND IN-EKH-KDEKSHT = '307')                                          
086400     OR (IN-EKH-KDEKHHT = '303'                                           
086500     AND IN-EKH-KDEKSHT = '371')                                          
086600     OR (IN-EKH-KDEKHHT = '303'                                           
086700     AND IN-EKH-KDEKSHT = '3XX')                                          
086800       MOVE 'KRW'                 TO R3-HEAD-CURRENCY                     
086900       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
087000     END-IF                                                               
087100     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
087200     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
087300     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
087400     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
087500     MOVE JA                      TO WS-HEADER-SW                         
087600     MOVE NEJ                     TO WS-LINE-SW                           
087700                                                                          
087800* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57093A                       
087900       PERFORM S004-WRITE-W57093A-HEAD                                    
088000     .                                                                    
088100     EJECT                                                                
088200                                                                          
088300 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
088400     MOVE SPACE                   TO R3-HEAD-R3                           
088500     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
088600     MOVE 'KR02'                  TO R3-HEAD-COMPANY-CODE                 
088700                                     R3-HEAD-CONTROL-AREA                 
088800     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
088900     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
089000     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
089100     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
089200     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
089300     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
089400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
089500     ELSE                                                                 
089600       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
089700     END-IF                                                               
089800     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
089900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
090000     IF (IN-EKH-KDEKHHT = '204'                                           
090100     AND IN-EKH-KDEKSHT = '301')                                          
090200       MOVE 'KRW'                 TO R3-HEAD-CURRENCY                     
090300       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
090400     ELSE                                                                 
090500       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
090600       MOVE WS-PRKURS-KR2         TO R3-HEAD-EXCHANGE-RATE                
090700       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
090800       IF IN-FIL-IDPGM = 'W4183300'                                       
090900         IF IN-EKH-DAAVIDAT > ZERO                                        
091000           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
091100           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
091200         ELSE                                                             
091300           MOVE WS-TIAA              TO WS-TIAA-CR                        
091400           MOVE WS-TIMM              TO WS-TIMM-CR                        
091500         END-IF                                                           
091600       ELSE                                                               
091700         MOVE WS-TIAA                TO WS-TIAA-CR                        
091800         MOVE WS-TIMM                TO WS-TIMM-CR                        
091900       END-IF                                                             
092000       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
092100       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
092200       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
092300       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
092400       IF CURR-KDSVAR = ' '                                               
092500         IF IN-EKH-IDDISTR > ZERO                                         
092600           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
092700         ELSE                                                             
092800           IF WS-PRKURS = ZERO                                            
092900             MOVE 1             TO WS-PRKURS                              
093000           END-IF                                                         
093100         END-IF                                                           
093200       ELSE                                                               
093300         MOVE 1                 TO WS-PRKURS                              
093400       END-IF                                                             
093500       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
093600                                       CURR-REVALUTA-TO                   
093700       END-COMPUTE                                                        
093800       IF CURR-REVALUTA-TO = +1                                           
093900         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
094000       END-IF                                                             
094100       IF CURR-REVALUTA-TO = +10                                          
094200         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
094300       END-IF                                                             
094400       IF CURR-REVALUTA-TO = +100                                         
094500         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
094600       END-IF                                                             
094700     END-IF                                                               
094800     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
094900     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
095000     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
095100     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
095200     MOVE JA                      TO WS-HEADER-SW                         
095300     MOVE NEJ                     TO WS-LINE-SW                           
095400                                                                          
095500* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57093A                       
095600       PERFORM S004-WRITE-W57093A-HEAD                                    
095700     .                                                                    
095800     EJECT                                                                
095900                                                                          
096000 CD-BUILD-COMMON-610-PART SECTION.                                        
096100     MOVE SPACE               TO R3-LINE-R3                               
096200     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
096300                                 R3-LINE-DUE-DATE                         
096400                                 R3-LINE-AMOUNT                           
096500                                 R3-LINE-AMOUNT-LC                        
096600                                 R3-LINE-TAX-AMOUNT                       
096700                                 R3-LINE-TAX-AMOUNT-LC                    
096800                                 R3-LINE-NUMBER-OF-DAYS                   
096900                                 R3-LINE-QUANTITY                         
097000                                 R3-LINE-SAMNR                            
097100     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
097200     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
097300     MOVE 'KR02'              TO R3-LINE-COMPANY-CODE                     
097400     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
097500     IF SYST-KDPOST = '50'                                                
097600       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
097700     ELSE                                                                 
097800       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
097900     END-IF                                                               
098000     IF SYST-IDPRCTR NOT = SPACE                                          
098100       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
098200       IF WS-PRCTR-PRODSL = '??'                                          
098300         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
098400         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
098500       END-IF                                                             
098600       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 CE-SCHEDULE-LINE-GL SECTION.                                             
099200     MOVE NEJ                     TO WS-HEADER-SW                         
099300     MOVE JA                      TO WS-LINE-SW                           
099400     EVALUATE IN-EKH-KDEKHHT                                              
099500     WHEN '102'                                                           
099600          PERFORM CEB-MAIN-EVENT-102                                      
099700     WHEN '103'                                                           
099800          PERFORM CEC-MAIN-EVENT-103                                      
099900     WHEN '201'                                                           
100000          PERFORM CED-MAIN-EVENT-201                                      
100100     WHEN '203'                                                           
100200          PERFORM CEF-MAIN-EVENT-203                                      
100300     WHEN '204'                                                           
100400          PERFORM CEG-MAIN-EVENT-204                                      
100500     WHEN '302'                                                           
100600          PERFORM CEI-MAIN-EVENT-302                                      
100700     WHEN '303'                                                           
100800          PERFORM CEJ-MAIN-EVENT-303                                      
100900     WHEN '401'                                                           
101000          PERFORM CEK-MAIN-EVENT-401                                      
101100     WHEN '402'                                                           
101200          PERFORM CEL-MAIN-EVENT-402                                      
101300     WHEN '403'                                                           
101400          PERFORM CEM-MAIN-EVENT-403                                      
101500     WHEN '404'                                                           
101600          PERFORM CEN-MAIN-EVENT-404                                      
101700     END-EVALUATE                                                         
101800     .                                                                    
101900     EJECT                                                                
102000                                                                          
102100 CEB-MAIN-EVENT-102 SECTION.                                              
102200     EVALUATE IN-EKH-KDEKSHT                                              
102300     WHEN '102'                                                           
102400          PERFORM CEBB-SUB-EVENT-102-102                                  
102500     WHEN '120'                                                           
102600          PERFORM CEBD-SUB-EVENT-102-120                                  
102700     WHEN '121'                                                           
102800          PERFORM CEBD-SUB-EVENT-102-121                                  
102900     WHEN '122'                                                           
103000          PERFORM CEBD-SUB-EVENT-102-122                                  
103100     WHEN '123'                                                           
103200          PERFORM CEBD-SUB-EVENT-102-123                                  
103300     WHEN '124'                                                           
103400          PERFORM CEBD-SUB-EVENT-102-124                                  
103500     WHEN '125'                                                           
103600          PERFORM CEBD-SUB-EVENT-102-125                                  
103700     WHEN '130'                                                           
103800          PERFORM CEBE-SUB-EVENT-102-130                                  
103900     WHEN '131'                                                           
104000          PERFORM CEBE-SUB-EVENT-102-131                                  
104100     WHEN '132'                                                           
104200          PERFORM CEBE-SUB-EVENT-102-132                                  
104300     WHEN '134'                                                           
104400          PERFORM CEBE-SUB-EVENT-102-134                                  
104500     END-EVALUATE                                                         
104600     .                                                                    
104700     EJECT                                                                
104800                                                                          
104900 CEBB-SUB-EVENT-102-102 SECTION.                                          
105000     EVALUATE IN-EKH-KDEKNIVA                                             
105100     WHEN 'DET'                                                           
105200       IF SYST-IDSEKVNR = 1                                               
105300         IF IN-EKH-KVANTAL > 0                                            
105400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
105500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
105600           COMPUTE R3-LINE-AMOUNT-LC =                                    
105700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
105800           IF IN-EKH-KDVALISO = 'KRW'                                     
105900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
106000           END-IF                                                         
106100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
106200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
106300           MOVE SPACE               TO WS-ALLOCATE-REF                    
106400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
106500           PERFORM S02-WRITE-W57091A                                      
106600         END-IF                                                           
106700       END-IF                                                             
106800                                                                          
106900       IF SYST-IDSEKVNR = 2                                               
107000         IF IN-EKH-KVANTAL < 0                                            
107100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
107200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
107300           COMPUTE R3-LINE-AMOUNT-LC =                                    
107400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
107500           IF IN-EKH-KDVALISO = 'KRW'                                     
107600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
107700           END-IF                                                         
107800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
107900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
108000           MOVE SPACE               TO WS-ALLOCATE-REF                    
108100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
108200           PERFORM S02-WRITE-W57091A                                      
108300         END-IF                                                           
108400       END-IF                                                             
108500                                                                          
108600       IF SYST-IDSEKVNR = 3                                               
108700         IF IN-EKH-KVANTAL < 0                                            
108800           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
108900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
109000           COMPUTE R3-LINE-AMOUNT-LC =                                    
109100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
109200           IF IN-EKH-KDVALISO = 'KRW'                                     
109300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
109400           END-IF                                                         
109500           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
109600           PERFORM S02-WRITE-W57091A                                      
109700         END-IF                                                           
109800       END-IF                                                             
109900                                                                          
110000       IF SYST-IDSEKVNR = 4                                               
110100         IF IN-EKH-KVANTAL > 0                                            
110200           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
110300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
110400           COMPUTE R3-LINE-AMOUNT-LC =                                    
110500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
110600           IF IN-EKH-KDVALISO = 'KRW'                                     
110700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
110800           END-IF                                                         
110900           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
111000           PERFORM S02-WRITE-W57091A                                      
111100         END-IF                                                           
111200       END-IF                                                             
111300                                                                          
111400     END-EVALUATE                                                         
111500     .                                                                    
111600     EJECT                                                                
111700                                                                          
111800 CEBD-SUB-EVENT-102-120 SECTION.                                          
111900     EVALUATE IN-EKH-KDEKNIVA                                             
112000     WHEN 'DET'                                                           
112100       IF SYST-IDSEKVNR = 1                                               
112200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
112300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
112400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
112500          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR * -1            
112600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
112700         PERFORM S03-WRITE-W57072                                         
112800       END-IF                                                             
112900                                                                          
113000     WHEN 'FÖRS'                                                          
113100     WHEN 'FRAKT'                                                         
113200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
113300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
113400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
113500               IN-EKH-SUBEL / WS-PRKURS-KR  * -1                          
113600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
113700       PERFORM S04-WRITE-W57093A                                          
113800                                                                          
113900     WHEN 'EMB'                                                           
114000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
114100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
114200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
114300               IN-EKH-SUBEL / WS-PRKURS-KR  * -1                          
114400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
114500       PERFORM S04-WRITE-W57093A                                          
114600                                                                          
114700     WHEN 'DDI'                                                           
114800       IF IN-EKH-SUBEL > ZERO                                             
114900         IF SYST-IDSEKVNR = 1                                             
115000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
115100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
115200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
115300                   IN-EKH-SUBEL                                           
115400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
115500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
115600           PERFORM S04-WRITE-W57093A                                      
115700         END-IF                                                           
115800       ELSE                                                               
115900         IF SYST-IDSEKVNR = 2                                             
116000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
116100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
116200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
116300                   IN-EKH-SUBEL                                           
116400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
116500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
116600           PERFORM S04-WRITE-W57093A                                      
116700         END-IF                                                           
116800       END-IF                                                             
116900     END-EVALUATE                                                         
117000     .                                                                    
117100     EJECT                                                                
117200                                                                          
117300 CEBD-SUB-EVENT-102-121 SECTION.                                          
117400     EVALUATE IN-EKH-KDEKNIVA                                             
117500     WHEN 'DET'                                                           
117600       IF SYST-IDSEKVNR = 1                                               
117700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
117800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
117900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
118000             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-KR3            
118100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
118200         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-1                      
118300         PERFORM S03-WRITE-W57072                                         
118400       END-IF                                                             
118500                                                                          
118600       IF SYST-IDSEKVNR = 2                                               
118700         PERFORM S13-GET-LANDING-COST                                     
118800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
118900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
119000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
119100            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
119200            (IN-EKH-KVANTAL *                                             
119300            IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                  
119400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
119500         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-2                      
119600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
119700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
119800         MOVE SPACE               TO WS-ALLOCATE-REF                      
119900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
120000         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
120100         PERFORM S03-WRITE-W57072                                         
120200       END-IF                                                             
120300                                                                          
120400       IF SYST-IDSEKVNR = 3                                               
120500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
120600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
120700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
120800            WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
120900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
121000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
121100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
121200         MOVE SPACE               TO WS-ALLOCATE-REF                      
121300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
121400         PERFORM S03-WRITE-W57072                                         
121500       END-IF                                                             
121600                                                                          
121700     WHEN 'FÖRS'                                                          
121800     WHEN 'FRAKT'                                                         
121900       IF SYST-IDSEKVNR = 1                                               
122000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
122100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
122200         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
122300                 IN-EKH-SUBEL / WS-PRKURS-KR3                             
122400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
122500         PERFORM S04-WRITE-W57093A                                        
122600       END-IF                                                             
122700                                                                          
122800       IF SYST-IDSEKVNR = 2                                               
122900         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
123000         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
123100         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
123200                 IN-EKH-SUBEL / WS-PRKURS-KR3                             
123300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
123400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
123500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
123600         MOVE SPACE               TO WS-ALLOCATE-REF                      
123700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
123800         PERFORM S04-WRITE-W57093A                                        
123900       END-IF                                                             
124000                                                                          
124100     WHEN 'EMB'                                                           
124200       IF SYST-IDSEKVNR = 1                                               
124300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
124400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
124500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
124600                 IN-EKH-SUBEL / WS-PRKURS-KR3                             
124700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
124800         PERFORM S04-WRITE-W57093A                                        
124900       END-IF                                                             
125000                                                                          
125100       IF SYST-IDSEKVNR = 2                                               
125200         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
125300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
125400         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
125500                 IN-EKH-SUBEL / WS-PRKURS-KR3                             
125600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
125700         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
125800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
125900         MOVE SPACE               TO WS-ALLOCATE-REF                      
126000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
126100         PERFORM S04-WRITE-W57093A                                        
126200       END-IF                                                             
126300     END-EVALUATE                                                         
126400                                                                          
126500     .                                                                    
126600     EJECT                                                                
126700                                                                          
126800 CEBD-SUB-EVENT-102-122 SECTION.                                          
126900     EVALUATE IN-EKH-KDEKNIVA                                             
127000     WHEN 'DET'                                                           
127100       IF IN-EKH-KVANTAL > 0                                              
127200         IF SYST-IDSEKVNR = 1                                             
127300           PERFORM S13-GET-LANDING-COST                                   
127400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
127500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
127600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
127700            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
127800            (IN-EKH-KVANTAL *                                             
127900             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
128000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
128100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
128200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
128300           MOVE SPACE               TO WS-ALLOCATE-REF                    
128400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
128500           PERFORM S02-WRITE-W57091A                                      
128600         END-IF                                                           
128700                                                                          
128800         IF SYST-IDSEKVNR = 4                                             
128900           PERFORM S13-GET-LANDING-COST                                   
129000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
129100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
129200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
129300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
129400            (IN-EKH-KVANTAL *                                             
129500             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
129600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
129700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
129800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
129900           MOVE SPACE               TO WS-ALLOCATE-REF                    
130000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
130100           MOVE SPACE               TO R3-LINE-COST-CENTER                
130200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
130300           PERFORM S02-WRITE-W57091A                                      
130400         END-IF                                                           
130500       END-IF                                                             
130600                                                                          
130700                                                                          
130800       IF IN-EKH-KVANTAL < 0                                              
130900         PERFORM S13-GET-LANDING-COST                                     
131000         IF SYST-IDSEKVNR = 2                                             
131100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
131200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
131300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
131400            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
131500            (IN-EKH-KVANTAL *                                             
131600             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
131700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
131800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
131900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
132000           MOVE SPACE               TO WS-ALLOCATE-REF                    
132100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
132200           MOVE SPACE             TO R3-LINE-COST-CENTER                  
132300           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
132400           PERFORM S02-WRITE-W57091A                                      
132500         END-IF                                                           
132600                                                                          
132700         IF SYST-IDSEKVNR = 3                                             
132800           PERFORM S13-GET-LANDING-COST                                   
132900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
133000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
133100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
133200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
133300            (IN-EKH-KVANTAL *                                             
133400             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
133500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
133600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
133700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
133800           MOVE SPACE               TO WS-ALLOCATE-REF                    
133900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
134000           PERFORM S02-WRITE-W57091A                                      
134100         END-IF                                                           
134200       END-IF                                                             
134300                                                                          
134400     END-EVALUATE                                                         
134500     .                                                                    
134600     EJECT                                                                
134700                                                                          
134800 CEBD-SUB-EVENT-102-123 SECTION.                                          
134900     EVALUATE IN-EKH-KDEKNIVA                                             
135000     WHEN 'DET'                                                           
135100       IF SYST-IDSEKVNR = 1                                               
135200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
135300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
135400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
135500              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
135600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
135700         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
135800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
135900         MOVE SPACE               TO WS-ALLOCATE-REF                      
136000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
136100         PERFORM S02-WRITE-W57091A                                        
136200       END-IF                                                             
136300                                                                          
136400       IF SYST-IDSEKVNR = 2                                               
136500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
136600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
136700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
136800             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
136900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
137000         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
137100         PERFORM S02-WRITE-W57091A                                        
137200       END-IF                                                             
137300     END-EVALUATE                                                         
137400     .                                                                    
137500     EJECT                                                                
137600                                                                          
137700 CEBD-SUB-EVENT-102-124 SECTION.                                          
137800     EVALUATE IN-EKH-KDEKNIVA                                             
137900     WHEN 'DET'                                                           
138000       IF SYST-IDSEKVNR = 1                                               
138100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
138200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
138300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
138400         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR  * -1            
138500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
138600         MOVE SPACE               TO WS-ALLOCATE-DC                       
138700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
138800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
138900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
139000         PERFORM S03-WRITE-W57072                                         
139100       END-IF                                                             
139200                                                                          
139300     WHEN 'FÖRS'                                                          
139400     WHEN 'FRAKT'                                                         
139500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
139600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
139700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
139800               IN-EKH-SUBEL * -1                                          
139900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
140000       MOVE SPACE               TO WS-ALLOCATE-DC                         
140100       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
140200       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
140300       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
140400       PERFORM S04-WRITE-W57093A                                          
140500                                                                          
140600     WHEN 'EMB'                                                           
140700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
140800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
140900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
141000               (IN-EKH-SUBEL / WS-PRKURS-KR) * -1                         
141100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
141200       MOVE SPACE               TO WS-ALLOCATE-DC                         
141300       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
141400       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
141500       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
141600       PERFORM S04-WRITE-W57093A                                          
141700                                                                          
141800     WHEN 'DDI'                                                           
141900       IF IN-EKH-SUBEL > ZERO                                             
142000         IF SYST-IDSEKVNR = 1                                             
142100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
142200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
142300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
142400                   IN-EKH-SUBEL                                           
142500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
142600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
142700           MOVE SPACE               TO WS-ALLOCATE-DC                     
142800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
142900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
143000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
143100           PERFORM S04-WRITE-W57093A                                      
143200         END-IF                                                           
143300       ELSE                                                               
143400         IF SYST-IDSEKVNR = 2                                             
143500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
143600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
143700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
143800                   IN-EKH-SUBEL                                           
143900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
144000           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
144100           MOVE SPACE               TO WS-ALLOCATE-DC                     
144200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
144300           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
144400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
144500           PERFORM S04-WRITE-W57093A                                      
144600         END-IF                                                           
144700       END-IF                                                             
144800     END-EVALUATE                                                         
144900     .                                                                    
145000     EJECT                                                                
145100                                                                          
145200 CEBD-SUB-EVENT-102-125 SECTION.                                          
145300     EVALUATE IN-EKH-KDEKNIVA                                             
145400     WHEN 'DET'                                                           
145500       IF SYST-IDSEKVNR = 1                                               
145600         PERFORM S13-GET-LANDING-COST                                     
145700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
145800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
145900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
146000         (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR) +              
146100         (IN-EKH-KVANTAL *                                                
146200          IN-EKH-PRARTNTO / WS-PRKURS-KR * WS-MARKUP)                     
146300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
146400         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
146500         PERFORM S03-WRITE-W57072                                         
146600       END-IF                                                             
146700                                                                          
146800       IF SYST-IDSEKVNR = 2                                               
146900         PERFORM S13-GET-LANDING-COST                                     
147000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
147100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
147200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
147300            (IN-EKH-KVANTAL *                                             
147400             IN-EKH-PRARTNTO / WS-PRKURS-KR * WS-MARKUP)                  
147500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
147600         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
147700         PERFORM S03-WRITE-W57072                                         
147800       END-IF                                                             
147900                                                                          
148000     WHEN 'FÖRS'                                                          
148100     WHEN 'FRAKT'                                                         
148200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
148300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
148400       MOVE SPACE             TO R3-LINE-COST-CENTER                      
148500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
148600          IN-EKH-SUBEL / WS-PRKURS-KR  * -1                               
148700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
148800       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
148900       PERFORM S04-WRITE-W57093A                                          
149000                                                                          
149100     WHEN 'EMB'                                                           
149200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
149300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
149400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
149500          IN-EKH-SUBEL / WS-PRKURS-KR  * -1                               
149600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
149700       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
149800       PERFORM S04-WRITE-W57093A                                          
149900                                                                          
150000     WHEN 'DDI'                                                           
150100       IF SPAR-SUMMA-102-125 < ZERO                                       
150200         IF SYST-IDSEKVNR = 1                                             
150300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
150400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
150500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
150600                   SPAR-SUMMA-102-125                                     
150700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
150800           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
150900           PERFORM S04-WRITE-W57093A                                      
151000         END-IF                                                           
151100       END-IF                                                             
151200       IF SPAR-SUMMA-102-125 > ZERO                                       
151300         IF SYST-IDSEKVNR = 2                                             
151400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
151500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
151600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
151700                   SPAR-SUMMA-102-125                                     
151800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
151900           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
152000           PERFORM S04-WRITE-W57093A                                      
152100         END-IF                                                           
152200       END-IF                                                             
152300                                                                          
152400     END-EVALUATE                                                         
152500     .                                                                    
152600     EJECT                                                                
152700                                                                          
152800 CEBE-SUB-EVENT-102-130 SECTION.                                          
152900     EVALUATE IN-EKH-KDEKNIVA                                             
153000     WHEN 'DET'                                                           
153100       IF SYST-IDSEKVNR = 1                                               
153200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
153300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
153400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
153500          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR * -1            
153600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
153700         PERFORM S03-WRITE-W57072                                         
153800       END-IF                                                             
153900                                                                          
154000     WHEN 'EMB'                                                           
154100     WHEN 'FÖRS'                                                          
154200     WHEN 'FRAKT'                                                         
154300       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
154400       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
154500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
154600               IN-EKH-SUBEL / WS-PRKURS-KR  * -1                          
154700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
154800       PERFORM S04-WRITE-W57093A                                          
154900                                                                          
155000     WHEN 'DDI'                                                           
155100       IF IN-EKH-SUBEL > ZERO                                             
155200         IF SYST-IDSEKVNR = 1                                             
155300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
155400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
155500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
155600                   IN-EKH-SUBEL                                           
155700           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
155800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
155900           PERFORM S04-WRITE-W57093A                                      
156000         END-IF                                                           
156100       ELSE                                                               
156200         IF SYST-IDSEKVNR = 2                                             
156300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
156400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
156500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
156600                   IN-EKH-SUBEL                                           
156700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
156800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
156900           PERFORM S04-WRITE-W57093A                                      
157000         END-IF                                                           
157100       END-IF                                                             
157200     END-EVALUATE                                                         
157300     .                                                                    
157400     EJECT                                                                
157500                                                                          
157600 CEBE-SUB-EVENT-102-131 SECTION.                                          
157700     EVALUATE IN-EKH-KDEKNIVA                                             
157800     WHEN 'DET'                                                           
157900       IF SYST-IDSEKVNR = 1                                               
158000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
158100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
158200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
158300             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-KR3            
158400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
158500         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-131-1                      
158600         PERFORM S03-WRITE-W57072                                         
158700       END-IF                                                             
158800                                                                          
158900       IF SYST-IDSEKVNR = 2                                               
159000         PERFORM S13-GET-LANDING-COST                                     
159100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
159200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
159300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
159400            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
159500            (IN-EKH-KVANTAL *                                             
159600            IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                  
159700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
159800         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-131-2                      
159900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
160000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
160100         MOVE SPACE               TO WS-ALLOCATE-REF                      
160200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
160300         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
160400         PERFORM S03-WRITE-W57072                                         
160500       END-IF                                                             
160600                                                                          
160700       IF SYST-IDSEKVNR = 3                                               
160800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
160900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
161000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
161100            WS-LINE-AMOUNT-131-2 - WS-LINE-AMOUNT-131-1                   
161200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
161300         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
161400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
161500         MOVE SPACE               TO WS-ALLOCATE-REF                      
161600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
161700         PERFORM S03-WRITE-W57072                                         
161800       END-IF                                                             
161900                                                                          
162000     WHEN 'EMB'                                                           
162100     WHEN 'FÖRS'                                                          
162200     WHEN 'FRAKT'                                                         
162300       IF SYST-IDSEKVNR = 1                                               
162400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
162500         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
162600         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
162700                 IN-EKH-SUBEL / WS-PRKURS-KR3                             
162800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
162900         PERFORM S04-WRITE-W57093A                                        
163000       END-IF                                                             
163100                                                                          
163200       IF SYST-IDSEKVNR = 2                                               
163300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
163400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
163500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
163600                 IN-EKH-SUBEL / WS-PRKURS-KR3                             
163700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
163800         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
163900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
164000         MOVE SPACE               TO WS-ALLOCATE-REF                      
164100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
164200         PERFORM S04-WRITE-W57093A                                        
164300       END-IF                                                             
164400                                                                          
164500     END-EVALUATE                                                         
164600     .                                                                    
164700     EJECT                                                                
164800                                                                          
164900 CEBE-SUB-EVENT-102-132 SECTION.                                          
165000     EVALUATE IN-EKH-KDEKNIVA                                             
165100     WHEN 'DET'                                                           
165200       IF IN-EKH-KVANTAL > 0                                              
165300         IF SYST-IDSEKVNR = 1                                             
165400           PERFORM S13-GET-LANDING-COST                                   
165500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
165600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
165700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
165800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
165900            (IN-EKH-KVANTAL *                                             
166000             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
166100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
166200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
166300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
166400           MOVE SPACE               TO WS-ALLOCATE-REF                    
166500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
166600           PERFORM S02-WRITE-W57091A                                      
166700         END-IF                                                           
166800                                                                          
166900         IF SYST-IDSEKVNR = 4                                             
167000           PERFORM S13-GET-LANDING-COST                                   
167100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
167200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
167300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
167400            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
167500            (IN-EKH-KVANTAL *                                             
167600             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
167700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
167800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
167900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
168000           MOVE SPACE               TO WS-ALLOCATE-REF                    
168100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
168200           MOVE SPACE               TO R3-LINE-COST-CENTER                
168300           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
168400           PERFORM S02-WRITE-W57091A                                      
168500         END-IF                                                           
168600       END-IF                                                             
168700                                                                          
168800                                                                          
168900       IF IN-EKH-KVANTAL < 0                                              
169000         PERFORM S13-GET-LANDING-COST                                     
169100         IF SYST-IDSEKVNR = 2                                             
169200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
169300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
169400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
169500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
169600            (IN-EKH-KVANTAL *                                             
169700             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
169800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
169900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
170000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
170100           MOVE SPACE               TO WS-ALLOCATE-REF                    
170200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
170300           MOVE SPACE             TO R3-LINE-COST-CENTER                  
170400           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
170500           PERFORM S02-WRITE-W57091A                                      
170600         END-IF                                                           
170700                                                                          
170800         IF SYST-IDSEKVNR = 3                                             
170900           PERFORM S13-GET-LANDING-COST                                   
171000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
171100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
171200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
171300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3) +          
171400            (IN-EKH-KVANTAL *                                             
171500             IN-EKH-PRARTNTO / WS-PRKURS-KR3 * WS-MARKUP)                 
171600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
171700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
171800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
171900           MOVE SPACE               TO WS-ALLOCATE-REF                    
172000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
172100           PERFORM S02-WRITE-W57091A                                      
172200         END-IF                                                           
172300       END-IF                                                             
172400                                                                          
172500     END-EVALUATE                                                         
172600     .                                                                    
172700     EJECT                                                                
172800                                                                          
172900 CEBE-SUB-EVENT-102-134 SECTION.                                          
173000     EVALUATE IN-EKH-KDEKNIVA                                             
173100     WHEN 'DET'                                                           
173200       IF SYST-IDSEKVNR = 1                                               
173300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
173400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
173500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
173600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR  * -1            
173700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
173800         MOVE SPACE               TO WS-ALLOCATE-DC                       
173900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
174000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
174100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
174200         PERFORM S03-WRITE-W57072                                         
174300       END-IF                                                             
174400                                                                          
174500     WHEN 'FÖRS'                                                          
174600     WHEN 'FRAKT'                                                         
174700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
174800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
174900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
175000               IN-EKH-SUBEL * -1                                          
175100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
175200       MOVE SPACE               TO WS-ALLOCATE-DC                         
175300       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
175400       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
175500       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
175600       PERFORM S04-WRITE-W57093A                                          
175700                                                                          
175800     WHEN 'EMB'                                                           
175900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
176000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
176100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
176200               (IN-EKH-SUBEL / WS-PRKURS-KR) * -1                         
176300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
176400       MOVE SPACE               TO WS-ALLOCATE-DC                         
176500       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
176600       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
176700       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
176800       PERFORM S04-WRITE-W57093A                                          
176900                                                                          
177000     WHEN 'DDI'                                                           
177100       IF IN-EKH-SUBEL > ZERO                                             
177200         IF SYST-IDSEKVNR = 1                                             
177300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
177400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
177500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
177600                   IN-EKH-SUBEL                                           
177700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
177800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
177900           MOVE SPACE               TO WS-ALLOCATE-DC                     
178000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
178100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
178200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
178300           PERFORM S04-WRITE-W57093A                                      
178400         END-IF                                                           
178500       ELSE                                                               
178600         IF SYST-IDSEKVNR = 2                                             
178700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
178800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
178900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
179000                   IN-EKH-SUBEL                                           
179100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
179200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
179300           MOVE SPACE               TO WS-ALLOCATE-DC                     
179400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
179500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
179600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
179700           PERFORM S04-WRITE-W57093A                                      
179800         END-IF                                                           
179900       END-IF                                                             
180000     END-EVALUATE                                                         
180100     .                                                                    
180200     EJECT                                                                
180300 CEC-MAIN-EVENT-103 SECTION.                                              
180400     EVALUATE IN-EKH-KDEKSHT                                              
180500     WHEN '102'                                                           
180600          PERFORM CECB-SUB-EVENT-103-102                                  
180700     WHEN '106'                                                           
180800          PERFORM CECB-SUB-EVENT-103-106                                  
180900     WHEN '107'                                                           
181000          PERFORM CECB-SUB-EVENT-103-107                                  
181100     END-EVALUATE                                                         
181200     .                                                                    
181300     EJECT                                                                
181400                                                                          
181500 CECB-SUB-EVENT-103-102 SECTION.                                          
181600     EVALUATE IN-EKH-KDEKNIVA                                             
181700     WHEN 'DET'                                                           
181800       IF SYST-IDSEKVNR = 1                                               
181900         IF IN-EKH-KVANTAL < 0                                            
182000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
182100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
182200           COMPUTE R3-LINE-AMOUNT-LC =                                    
182300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
182400           IF IN-EKH-KDVALISO = 'KRW'                                     
182500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
182600           END-IF                                                         
182700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
182800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
182900           MOVE SPACE               TO WS-ALLOCATE-REF                    
183000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
183100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
183200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
183300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
183400           PERFORM S04-WRITE-W57093A                                      
183500         END-IF                                                           
183600       END-IF                                                             
183700                                                                          
183800       IF SYST-IDSEKVNR = 2                                               
183900         IF IN-EKH-KVANTAL > 0                                            
184000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
184100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
184200           COMPUTE R3-LINE-AMOUNT-LC =                                    
184300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
184400           IF IN-EKH-KDVALISO = 'KRW'                                     
184500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
184600           END-IF                                                         
184700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
184800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
184900           MOVE SPACE               TO WS-ALLOCATE-REF                    
185000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
185100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
185200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
185300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
185400           PERFORM S04-WRITE-W57093A                                      
185500         END-IF                                                           
185600       END-IF                                                             
185700                                                                          
185800     WHEN 'KALK'                                                          
185900       IF SYST-IDSEKVNR = 1                                               
186000         IF IN-EKH-SUBEL > 0                                              
186100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
186200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
186300           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
186400           IF IN-EKH-KDVALISO = 'KRW'                                     
186500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
186600           END-IF                                                         
186700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
186800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
186900           MOVE SPACE               TO WS-ALLOCATE-REF                    
187000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
187100           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
187200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
187300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
187400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
187500           PERFORM S04-WRITE-W57093A                                      
187600         END-IF                                                           
187700       END-IF                                                             
187800                                                                          
187900       IF SYST-IDSEKVNR = 2                                               
188000         IF IN-EKH-SUBEL < 0                                              
188100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
188200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
188300           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
188400           IF IN-EKH-KDVALISO = 'KRW'                                     
188500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
188600           END-IF                                                         
188700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
188800           MOVE SPACE               TO WS-ALLOCATE-DC                     
188900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
189000           MOVE SPACE               TO WS-ALLOCATE-REF                    
189100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
189200           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
189300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
189400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
189500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
189600           PERFORM S04-WRITE-W57093A                                      
189700         END-IF                                                           
189800       END-IF                                                             
189900                                                                          
190000       IF SYST-IDSEKVNR = 3                                               
190100         IF IN-EKH-SUBEL > 0                                              
190200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
190300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
190400           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
190500           IF IN-EKH-KDVALISO = 'KRW'                                     
190600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
190700           END-IF                                                         
190800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
190900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
191000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
191100           PERFORM S04-WRITE-W57093A                                      
191200         END-IF                                                           
191300       END-IF                                                             
191400                                                                          
191500       IF SYST-IDSEKVNR = 4                                               
191600         IF IN-EKH-SUBEL < 0                                              
191700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
191800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
191900           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
192000           IF IN-EKH-KDVALISO = 'KRW'                                     
192100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
192200           END-IF                                                         
192300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
192400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
192500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
192600           PERFORM S04-WRITE-W57093A                                      
192700         END-IF                                                           
192800       END-IF                                                             
192900                                                                          
193000     WHEN 'DDI'                                                           
193100       IF IN-EKH-SUBEL > ZERO                                             
193200         IF SYST-IDSEKVNR = 1                                             
193300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
193400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
193500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
193600                   IN-EKH-SUBEL                                           
193700           MOVE ZEROES              TO R3-LINE-AMOUNT                     
193710           IF IN-EKH-KDVALISO = 'KRW'                                     
193720             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
193730           END-IF                                                         
193800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
194200           MOVE SPACE               TO R3-LINE-ALLOCATE                   
194300           PERFORM S04-WRITE-W57093A                                      
194400         END-IF                                                           
194500       ELSE                                                               
194600         IF SYST-IDSEKVNR = 2                                             
194700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
194800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
194900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
195000                   IN-EKH-SUBEL                                           
195100           MOVE ZEROES              TO R3-LINE-AMOUNT                     
195110           IF IN-EKH-KDVALISO = 'KRW'                                     
195120             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
195130           END-IF                                                         
195200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
195600           MOVE SPACE               TO R3-LINE-ALLOCATE                   
195700           PERFORM S04-WRITE-W57093A                                      
195800         END-IF                                                           
195900       END-IF                                                             
196000     END-EVALUATE                                                         
196100     .                                                                    
196200     EJECT                                                                
196300                                                                          
196400 CECB-SUB-EVENT-103-106 SECTION.                                          
196500     EVALUATE IN-EKH-KDEKNIVA                                             
196600     WHEN 'DET'                                                           
196700       IF SYST-IDSEKVNR = 1                                               
196800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
196900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
197000         COMPUTE R3-LINE-AMOUNT-LC =                                      
197100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
197200         IF IN-EKH-KDVALISO = 'KRW'                                       
197300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
197400         END-IF                                                           
197500         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
197600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
197700         MOVE SPACE               TO WS-ALLOCATE-REF                      
197800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
197900         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
198000         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
198100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
198200         PERFORM S04-WRITE-W57093A                                        
198300       END-IF                                                             
198400                                                                          
198500                                                                          
198600     WHEN 'KALK'                                                          
198700       IF SYST-IDSEKVNR = 1                                               
198800         IF IN-EKH-SUBEL > 0                                              
198900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
199000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
199100           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
199200           IF IN-EKH-KDVALISO = 'KRW'                                     
199300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
199400           END-IF                                                         
199500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
199600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
199700           MOVE SPACE               TO WS-ALLOCATE-REF                    
199800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
199900           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
200000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
200100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
200200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
200300           PERFORM S04-WRITE-W57093A                                      
200400         END-IF                                                           
200500       END-IF                                                             
200600                                                                          
200700       IF SYST-IDSEKVNR = 2                                               
200800         IF IN-EKH-SUBEL < 0                                              
200900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
201000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
201100           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
201200           IF IN-EKH-KDVALISO = 'KRW'                                     
201300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
201400           END-IF                                                         
201500           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
201600           MOVE SPACE               TO WS-ALLOCATE-DC                     
201700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
201800           MOVE SPACE               TO WS-ALLOCATE-REF                    
201900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
202000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
202100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
202200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
202300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
202400           PERFORM S04-WRITE-W57093A                                      
202500         END-IF                                                           
202600       END-IF                                                             
202700                                                                          
202800       IF SYST-IDSEKVNR = 3                                               
202900         IF IN-EKH-SUBEL > 0                                              
203000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
203100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
203200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
203300           IF IN-EKH-KDVALISO = 'KRW'                                     
203400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
203500           END-IF                                                         
203600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
203700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
203800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
203900           PERFORM S04-WRITE-W57093A                                      
204000         END-IF                                                           
204100       END-IF                                                             
204200                                                                          
204300       IF SYST-IDSEKVNR = 4                                               
204400         IF IN-EKH-SUBEL < 0                                              
204500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
204600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
204700           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
204800           IF IN-EKH-KDVALISO = 'KRW'                                     
204900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
205000           END-IF                                                         
205100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
205200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
205300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
205400           PERFORM S04-WRITE-W57093A                                      
205500         END-IF                                                           
205600       END-IF                                                             
205601                                                                          
205610     WHEN 'DDI'                                                           
205620       IF IN-EKH-SUBEL < ZERO                                             
205630         IF SYST-IDSEKVNR = 1                                             
205640           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
205650           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
205660           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
205670                   IN-EKH-SUBEL                                           
205680           MOVE ZEROES              TO R3-LINE-AMOUNT                     
205681           IF IN-EKH-KDVALISO = 'KRW'                                     
205682             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
205683           END-IF                                                         
205690           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
205694           MOVE SPACE               TO R3-LINE-ALLOCATE                   
205695           PERFORM S04-WRITE-W57093A                                      
205696         END-IF                                                           
205697       ELSE                                                               
205698         IF SYST-IDSEKVNR = 2                                             
205699           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
205700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
205701           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
205702                   IN-EKH-SUBEL                                           
205703           MOVE ZEROES              TO R3-LINE-AMOUNT                     
205704           IF IN-EKH-KDVALISO = 'KRW'                                     
205705             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
205706           END-IF                                                         
205707           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
205711           MOVE SPACE               TO R3-LINE-ALLOCATE                   
205712           PERFORM S04-WRITE-W57093A                                      
205713         END-IF                                                           
205714       END-IF                                                             
205720                                                                          
205800     END-EVALUATE                                                         
205900     .                                                                    
206000     EJECT                                                                
206100                                                                          
206200 CECB-SUB-EVENT-103-107 SECTION.                                          
206300     EVALUATE IN-EKH-KDEKNIVA                                             
206400     WHEN 'DET'                                                           
206500       IF SYST-IDSEKVNR = 1                                               
206600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
206700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
206800         COMPUTE R3-LINE-AMOUNT-LC =                                      
206900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
207000         IF IN-EKH-KDVALISO = 'KRW'                                       
207100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
207200         END-IF                                                           
207300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
207400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
207500         MOVE SPACE               TO WS-ALLOCATE-REF                      
207600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
207700         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
207800         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
207900         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
208000         PERFORM S04-WRITE-W57093A                                        
208100       END-IF                                                             
208200                                                                          
208300     WHEN 'KALK'                                                          
208400       IF SYST-IDSEKVNR = 1                                               
208500         IF IN-EKH-SUBEL > 0                                              
208600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
208700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
208800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
208900           IF IN-EKH-KDVALISO = 'KRW'                                     
209000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
209100           END-IF                                                         
209200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
209300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
209400           MOVE SPACE               TO WS-ALLOCATE-REF                    
209500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
209600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
209700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
209800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
209900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
210000           PERFORM S04-WRITE-W57093A                                      
210100         END-IF                                                           
210200       END-IF                                                             
210300                                                                          
210400       IF SYST-IDSEKVNR = 2                                               
210500         IF IN-EKH-SUBEL < 0                                              
210600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
210700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
210800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
210900           IF IN-EKH-KDVALISO = 'KRW'                                     
211000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
211100           END-IF                                                         
211200           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
211300           MOVE SPACE               TO WS-ALLOCATE-DC                     
211400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
211500           MOVE SPACE               TO WS-ALLOCATE-REF                    
211600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
211700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
211800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
211900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
212000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
212100           PERFORM S04-WRITE-W57093A                                      
212200         END-IF                                                           
212300       END-IF                                                             
212400                                                                          
212500       IF SYST-IDSEKVNR = 3                                               
212600         IF IN-EKH-SUBEL > 0                                              
212700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
212800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
212900           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
213000           IF IN-EKH-KDVALISO = 'KRW'                                     
213100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
213200           END-IF                                                         
213300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
213400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
213500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
213600           PERFORM S04-WRITE-W57093A                                      
213700         END-IF                                                           
213800       END-IF                                                             
213900                                                                          
214000       IF SYST-IDSEKVNR = 4                                               
214100         IF IN-EKH-SUBEL < 0                                              
214200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
214300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
214400           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
214500           IF IN-EKH-KDVALISO = 'KRW'                                     
214600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
214700           END-IF                                                         
214800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
214900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
215000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
215100           PERFORM S04-WRITE-W57093A                                      
215200         END-IF                                                           
215300       END-IF                                                             
215301                                                                          
215310     WHEN 'DDI'                                                           
215320       IF IN-EKH-SUBEL < ZERO                                             
215330         IF SYST-IDSEKVNR = 1                                             
215340           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
215350           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
215360           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
215370                   IN-EKH-SUBEL                                           
215380           MOVE ZEROES              TO R3-LINE-AMOUNT                     
215390           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
215391           MOVE SPACE               TO WS-ALLOCATE-DC                     
215392           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
215393           MOVE SPACE               TO WS-ALLOCATE-REF                    
215394           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
215395           PERFORM S04-WRITE-W57093A                                      
215396         END-IF                                                           
215397       ELSE                                                               
215398         IF SYST-IDSEKVNR = 2                                             
215399           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
215400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
215401           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
215402                   IN-EKH-SUBEL                                           
215403           MOVE ZEROES              TO R3-LINE-AMOUNT                     
215404           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
215405           MOVE SPACE               TO WS-ALLOCATE-DC                     
215406           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
215407           MOVE SPACE               TO WS-ALLOCATE-REF                    
215408           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
215409           PERFORM S04-WRITE-W57093A                                      
215410         END-IF                                                           
215411       END-IF                                                             
215420                                                                          
215500     END-EVALUATE                                                         
215600     .                                                                    
215700     EJECT                                                                
215800                                                                          
215900 CED-MAIN-EVENT-201 SECTION.                                              
216000     EVALUATE IN-EKH-KDEKSHT                                              
216100     WHEN '201'                                                           
216200          PERFORM CEDA-SUB-EVENT-201-201                                  
216300     END-EVALUATE                                                         
216400     .                                                                    
216500     EJECT                                                                
216600                                                                          
216700 CEDA-SUB-EVENT-201-201 SECTION.                                          
216800     EVALUATE IN-EKH-KDEKNIVA                                             
216900     WHEN 'DET'                                                           
217000       IF SYST-IDSEKVNR = 1                                               
217100         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
217200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
217300         COMPUTE R3-LINE-AMOUNT-LC =                                      
217400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
217500         IF IN-EKH-KDVALISO = 'KRW'                                       
217600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
217700         END-IF                                                           
217800         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
217900         MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
218000         PERFORM S03-WRITE-W57072                                         
218100       END-IF                                                             
218200                                                                          
218300       IF SYST-IDSEKVNR = 2                                               
218400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
218500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
218600         COMPUTE R3-LINE-AMOUNT-LC =                                      
218700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
218800         IF IN-EKH-KDVALISO = 'KRW'                                       
218900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
219000         END-IF                                                           
219100         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
219200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
219300         MOVE SPACE               TO WS-ALLOCATE-REF                      
219400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
219500         MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
219600         PERFORM S03-WRITE-W57072                                         
219700       END-IF                                                             
219800     END-EVALUATE                                                         
219900     .                                                                    
220000     EJECT                                                                
220100                                                                          
220200 CEF-MAIN-EVENT-203 SECTION.                                              
220300     EVALUATE IN-EKH-KDEKSHT                                              
220400     WHEN '201'                                                           
220500          PERFORM CEFA-SUB-EVENT-203-201                                  
220600     END-EVALUATE                                                         
220700     .                                                                    
220800     EJECT                                                                
220900                                                                          
221000 CEFA-SUB-EVENT-203-201 SECTION.                                          
221100     EVALUATE IN-EKH-KDEKNIVA                                             
221200     WHEN 'DET'                                                           
221300       IF SYST-IDSEKVNR = 1                                               
221400         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
221500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
221600         COMPUTE R3-LINE-AMOUNT-LC =                                      
221700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
221800         IF IN-EKH-KDVALISO = 'KRW'                                       
221900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
222000         END-IF                                                           
222100         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
222200         MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
222300         PERFORM S03-WRITE-W57072                                         
222400       END-IF                                                             
222500                                                                          
222600       IF SYST-IDSEKVNR = 2                                               
222700         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
222800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
222900         COMPUTE R3-LINE-AMOUNT-LC =                                      
223000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
223100         IF IN-EKH-KDVALISO = 'KRW'                                       
223200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
223300         END-IF                                                           
223400         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
223500         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
223600         MOVE SPACE             TO WS-ALLOCATE-REF                        
223700         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
223800         MOVE 0000409441        TO R3-LINE-PA-CUSTOMER                    
223900         PERFORM S03-WRITE-W57072                                         
224000       END-IF                                                             
224100     END-EVALUATE                                                         
224200     .                                                                    
224300     EJECT                                                                
224400                                                                          
224500 CEG-MAIN-EVENT-204 SECTION.                                              
224600     EVALUATE IN-EKH-KDEKSHT                                              
224700     WHEN '201'                                                           
224800          PERFORM CEGA-SUB-EVENT-204-201                                  
224900     WHEN '301'                                                           
225000          PERFORM CEGB-SUB-EVENT-204-301                                  
225100     END-EVALUATE                                                         
225200     .                                                                    
225300     EJECT                                                                
225400                                                                          
225500 CEGA-SUB-EVENT-204-201 SECTION.                                          
225600     EVALUATE IN-EKH-KDEKNIVA                                             
225700     WHEN 'DET'                                                           
225800         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
225900* R-FAKTURA                                                               
226000       IF SYST-IDSEKVNR = 1                                               
226100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
226200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
226300         COMPUTE R3-LINE-AMOUNT-LC =                                      
226400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
226500         IF IN-EKH-KDVALISO = 'KRW'                                       
226600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
226700         END-IF                                                           
226800         MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
226900         MOVE SPACE               TO WS-ALLOCATE-DC                       
227000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
227100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
227200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
227300         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
227400         PERFORM S03-WRITE-W57072                                         
227500       END-IF                                                             
227600                                                                          
227700       IF SYST-IDSEKVNR = 2                                               
227800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
227900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
228000         COMPUTE R3-LINE-AMOUNT-LC =                                      
228100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
228200         IF IN-EKH-KDVALISO = 'KRW'                                       
228300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
228400         END-IF                                                           
228500         MOVE SPACE               TO WS-ALLOCATE-DC                       
228600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
228700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
228800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
228900         MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
229000         PERFORM S03-WRITE-W57072                                         
229100       END-IF                                                             
229200     END-EVALUATE                                                         
229300     .                                                                    
229400     EJECT                                                                
229500                                                                          
229600 CEGB-SUB-EVENT-204-301 SECTION.                                          
229700     EVALUATE IN-EKH-KDEKNIVA                                             
229800     WHEN 'DET'                                                           
229900* R-FAKTURA                                                               
230000       IF SYST-IDSEKVNR = 1                                               
230100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
230200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
230300         COMPUTE R3-LINE-AMOUNT-LC =                                      
230400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
230500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
230600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
230700         IF BET-KDTRADP(3:2) NOT = SPACE                                  
230800            MOVE '1'             TO WS-ACCOUNT-4                          
230900            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
231000         ELSE                                                             
231100            MOVE '3'             TO WS-ACCOUNT-4                          
231200            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
231300         END-IF                                                           
231400         PERFORM S03-WRITE-W57072                                         
231500       END-IF                                                             
231600                                                                          
231700       IF SYST-IDSEKVNR = 2                                               
231800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
231900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
232000         COMPUTE R3-LINE-AMOUNT-LC =                                      
232100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
232200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
232300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
232400         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
232500         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
232600         MOVE SPACE             TO WS-ALLOCATE-REF                        
232700         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
232800         PERFORM S03-WRITE-W57072                                         
232900       END-IF                                                             
233000                                                                          
233100       IF SYST-IDSEKVNR = 3                                               
233200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
233300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
233400         COMPUTE R3-LINE-AMOUNT-LC =                                      
233500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
233600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
233700         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
233800         IF BET-KDTRADP(3:2) NOT = SPACE                                  
233900            MOVE '1'             TO WS-ACCOUNT-4                          
234000            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
234100         ELSE                                                             
234200            MOVE '3'             TO WS-ACCOUNT-4                          
234300            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
234400         END-IF                                                           
234500         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
234600         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
234700         MOVE SPACE             TO WS-ALLOCATE-REF                        
234800         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
234900         PERFORM S03-WRITE-W57072                                         
235000       END-IF                                                             
235100                                                                          
235200     WHEN 'EMB'                                                           
235300     WHEN 'FÖRS'                                                          
235400     WHEN 'FRAKT'                                                         
235500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
235600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
235700       MOVE SYST-IDKST          TO WS-RED-IDKST                           
235800       IF WS-RED-IDKST > SPACE                                            
235900         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
236000       ELSE                                                               
236100         MOVE SPACE             TO R3-LINE-COST-CENTER                    
236200       END-IF                                                             
236300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
236400               IN-EKH-SUBEL * -1                                          
236500       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
236600       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
236700       PERFORM S04-WRITE-W57093A                                          
236800                                                                          
236900     END-EVALUATE                                                         
237000     .                                                                    
237100     EJECT                                                                
237200                                                                          
237300 CEI-MAIN-EVENT-302 SECTION.                                              
237400     EVALUATE IN-EKH-KDEKSHT                                              
237500     WHEN '301'                                                           
237600          PERFORM CEIA-SUB-EVENT-302-301                                  
237700     WHEN '302'                                                           
237800          PERFORM CEIB-SUB-EVENT-302-302                                  
237900     END-EVALUATE                                                         
238000     .                                                                    
238100     EJECT                                                                
238200                                                                          
238300 CEIA-SUB-EVENT-302-301 SECTION.                                          
238400     EVALUATE IN-EKH-KDEKNIVA                                             
238500     WHEN 'DET'                                                           
238600       IF SYST-IDSEKVNR = 1                                               
238700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
238800         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
238900         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
239000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
239100         COMPUTE R3-LINE-AMOUNT-LC =                                      
239200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
239300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
239400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
239500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
239600         MOVE SPACE               TO WS-ALLOCATE-REF                      
239700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
239800         PERFORM S02-WRITE-W57091A                                        
239900       END-IF                                                             
240000                                                                          
240100       IF SYST-IDSEKVNR = 2                                               
240200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
240300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
240400         MOVE SPACE           TO R3-LINE-COST-CENTER                      
240500         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
240600         COMPUTE R3-LINE-AMOUNT-LC =                                      
240700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
240800         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
240900         MOVE SPACE               TO WS-LINE-TEXT                         
241000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
241100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
241200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
241300         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
241400         PERFORM S02-WRITE-W57091A                                        
241500       END-IF                                                             
241600     END-EVALUATE                                                         
241700     .                                                                    
241800     EJECT                                                                
241900                                                                          
242000 CEIB-SUB-EVENT-302-302 SECTION.                                          
242100     EVALUATE IN-EKH-KDEKNIVA                                             
242200     WHEN 'DET'                                                           
242300       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
242400         IF SYST-IDSEKVNR = 1                                             
242500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
242600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
242700           IF BET-KDTRADP(3:2) NOT = SPACE                                
242800             MOVE '1'             TO WS-ACCOUNT-4                         
242900           ELSE                                                           
243000             MOVE '3'             TO WS-ACCOUNT-4                         
243100           END-IF                                                         
243200           COMPUTE R3-LINE-AMOUNT-LC  =                                   
243300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
243400           IF IN-EKH-KDVALISO = 'KRW'                                     
243500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
243600           END-IF                                                         
243700           MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
243800           PERFORM S02-WRITE-W57091A                                      
243900         END-IF                                                           
244000                                                                          
244100         IF SYST-IDSEKVNR = 4                                             
244200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
244300           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
244400           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
244500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
244600           COMPUTE R3-LINE-AMOUNT-LC  =                                   
244700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
244800           IF IN-EKH-KDVALISO = 'KRW'                                     
244900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
245000           END-IF                                                         
245100           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
245200           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
245300           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
245400           MOVE SPACE             TO WS-ALLOCATE-REF                      
245500           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
245600           MOVE 0000409441        TO R3-LINE-PA-CUSTOMER                  
245700           PERFORM S02-WRITE-W57091A                                      
245800         END-IF                                                           
245900       ELSE                                                               
246000         IF SYST-IDSEKVNR = 2                                             
246100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
246200           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
246300           IF BET-KDTRADP(3:2) NOT = SPACE                                
246400             MOVE '1'             TO WS-ACCOUNT-4                         
246500           ELSE                                                           
246600             MOVE '3'             TO WS-ACCOUNT-4                         
246700           END-IF                                                         
246800           COMPUTE R3-LINE-AMOUNT-LC  =                                   
246900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
247000           IF IN-EKH-KDVALISO = 'KRW'                                     
247100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
247200           END-IF                                                         
247300           MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
247400           PERFORM S02-WRITE-W57091A                                      
247500         END-IF                                                           
247600                                                                          
247700         IF SYST-IDSEKVNR = 3                                             
247800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
247900           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
248000           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
248100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
248200           COMPUTE R3-LINE-AMOUNT-LC  =                                   
248300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
248400           IF IN-EKH-KDVALISO = 'KRW'                                     
248500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
248600           END-IF                                                         
248700           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
248800           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
248900           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
249000           MOVE SPACE             TO WS-ALLOCATE-REF                      
249100           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
249200           MOVE 0000409441        TO R3-LINE-PA-CUSTOMER                  
249300           PERFORM S02-WRITE-W57091A                                      
249400         END-IF                                                           
249500       END-IF                                                             
249600     END-EVALUATE                                                         
249700     .                                                                    
249800     EJECT                                                                
249900                                                                          
250000 CEJ-MAIN-EVENT-303 SECTION.                                              
250100     EVALUATE IN-EKH-KDEKSHT                                              
250200     WHEN '3XX'                                                           
250300          PERFORM CEJ301-SUB-EVENT-303-3XX                                
250400     WHEN '301'                                                           
250500          PERFORM CEJ301-SUB-EVENT-303-301                                
250600     WHEN '307'                                                           
250700          PERFORM CEJ307-SUB-EVENT-303-307                                
250800     WHEN '310'                                                           
250900          PERFORM CEJ310-SUB-EVENT-303-310                                
251000     WHEN '311'                                                           
251100          PERFORM CEJ311-SUB-EVENT-303-311                                
251200     WHEN '391'                                                           
251300          PERFORM CEJ301-SUB-EVENT-303-391                                
251400     WHEN '371'                                                           
251500          PERFORM CEJ303-SUB-EVENT-303-371                                
251600     END-EVALUATE                                                         
251700     .                                                                    
251800     EJECT                                                                
251900                                                                          
252000 CEJ303-SUB-EVENT-303-371 SECTION.                                        
252100     EVALUATE IN-EKH-KDEKNIVA                                             
252200     WHEN 'DET'                                                           
252300       IF SYST-IDSEKVNR = 1                                               
252400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
252500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
252600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
252700           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3               
252800         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
252900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
253000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
253100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
253200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
253300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
253400         PERFORM S04-WRITE-W57093A                                        
253500       END-IF                                                             
253600                                                                          
253700     WHEN 'LAND'                                                          
253800       IF SYST-IDSEKVNR = 1                                               
253900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
254000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
254100         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
254200         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
254300               R3-LINE-AMOUNT-LC / WS-PRKURS-KR3                          
254400         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
254500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
254600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
254700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
254800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
254900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
255000         PERFORM S02-WRITE-W57091A                                        
255100       END-IF                                                             
255200                                                                          
255300     WHEN 'DDI'                                                           
255400       IF IN-EKH-SUBEL < ZERO                                             
255500         IF SYST-IDSEKVNR = 1                                             
255600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
255700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
255800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
255900                   IN-EKH-SUBEL                                           
256000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
256100           MOVE SPACE               TO WS-ALLOCATE-DC                     
256200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
256300           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
256400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
256500           PERFORM S02-WRITE-W57091A                                      
256600         END-IF                                                           
256700       ELSE                                                               
256800         IF SYST-IDSEKVNR = 2                                             
256900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
257000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
257100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
257200                   IN-EKH-SUBEL                                           
257300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
257400           MOVE SPACE               TO WS-ALLOCATE-DC                     
257500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
257600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
257700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
257800           PERFORM S02-WRITE-W57091A                                      
257900         END-IF                                                           
258000       END-IF                                                             
258100     END-EVALUATE                                                         
258200     .                                                                    
258300     EJECT                                                                
258400 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
258500     EVALUATE IN-EKH-KDEKNIVA                                             
258600                                                                          
258700     WHEN 'FÖRS'                                                          
258800     WHEN 'FRAKT'                                                         
258900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
259000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
259100       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
259200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
259300              (IN-EKH-SUBEL * -1) / WS-PRKURS-KR3                         
259400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
259500       MOVE SPACE               TO WS-ALLOCATE-DC                         
259600       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
259700       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
259800       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
259900       PERFORM S03-WRITE-W57072                                           
260000                                                                          
260100     WHEN 'LAND'                                                          
260200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
260300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
260400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
260500              (IN-EKH-SUBEL * -1) / WS-PRKURS-KR                          
260600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
260700       MOVE SPACE               TO WS-ALLOCATE-DC                         
260800       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
260900       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
261000       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
261100       PERFORM S03-WRITE-W57072                                           
261200                                                                          
261300     WHEN 'DDI'                                                           
261400       IF IN-EKH-SUBEL < ZERO                                             
261500         IF SYST-IDSEKVNR = 1                                             
261600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
261700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
261800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
261900                   IN-EKH-SUBEL                                           
262000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
262100           MOVE SPACE               TO WS-ALLOCATE-DC                     
262200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
262300           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
262400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
262500           PERFORM S04-WRITE-W57093A                                      
262600         END-IF                                                           
262700       ELSE                                                               
262800         IF SYST-IDSEKVNR = 2                                             
262900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
263000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
263100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
263200                   IN-EKH-SUBEL                                           
263300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
263400           MOVE SPACE               TO WS-ALLOCATE-DC                     
263500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
263600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
263700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
263800           PERFORM S04-WRITE-W57093A                                      
263900         END-IF                                                           
264000       END-IF                                                             
264100     END-EVALUATE                                                         
264200     .                                                                    
264300     EJECT                                                                
264400                                                                          
264500 CEJ301-SUB-EVENT-303-301 SECTION.                                        
264600     EVALUATE IN-EKH-KDEKNIVA                                             
264700     WHEN 'DET'                                                           
264800       IF SYST-IDSEKVNR = 1                                               
264900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
265000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
265100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
265200         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3 * -1            
265300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
265400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
265500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
265600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
265700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
265800         MOVE SPACE               TO WS-ALLOCATE-DC                       
265900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
266000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
266100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
266200         PERFORM S03-WRITE-W57072                                         
266300       END-IF                                                             
266400                                                                          
266500     END-EVALUATE                                                         
266600     .                                                                    
266700     EJECT                                                                
266800                                                                          
266900 CEJ307-SUB-EVENT-303-307 SECTION.                                        
267000     EVALUATE IN-EKH-KDEKNIVA                                             
267100     WHEN 'DET'                                                           
267200       IF SYST-IDSEKVNR = 1                                               
267300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
267400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
267500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
267600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-KR3 * -1            
267700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
267800         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
267900         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
268000         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
268100         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
268200         MOVE SPACE               TO WS-ALLOCATE-DC                       
268300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
268400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
268500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
268600         PERFORM S03-WRITE-W57072                                         
268700       END-IF                                                             
268800                                                                          
268900     END-EVALUATE                                                         
269000     .                                                                    
269100     EJECT                                                                
269200                                                                          
269300 CEJ310-SUB-EVENT-303-310 SECTION.                                        
269400     EVALUATE IN-EKH-KDEKNIVA                                             
269500     WHEN 'DET'                                                           
269600       IF SYST-IDSEKVNR = 1                                               
269700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
269800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
269900         COMPUTE R3-LINE-AMOUNT-LC =                                      
270000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
270100         IF IN-EKH-KDVALISO = 'KRW'                                       
270200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
270300         END-IF                                                           
270400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
270500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
270600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
270700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
270800         MOVE SPACE               TO WS-LINE-TEXT                         
270900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
271000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
271100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
271200         PERFORM S02-WRITE-W57091A                                        
271300       END-IF                                                             
271400                                                                          
271500       IF SYST-IDSEKVNR = 2                                               
271600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
271700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
271800         COMPUTE R3-LINE-AMOUNT-LC =                                      
271900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
272000         IF IN-EKH-KDVALISO = 'KRW'                                       
272100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
272200         END-IF                                                           
272300         MOVE SPACE               TO WS-LINE-TEXT                         
272400         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
272500         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
272600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
272700         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
272800         MOVE SPACE               TO WS-ALLOCATE-DC                       
272900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
273000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
273100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
273200         PERFORM S02-WRITE-W57091A                                        
273300       END-IF                                                             
273400     END-EVALUATE                                                         
273500     .                                                                    
273600     EJECT                                                                
273700                                                                          
273800 CEJ311-SUB-EVENT-303-311 SECTION.                                        
273900     EVALUATE IN-EKH-KDEKNIVA                                             
274000     WHEN 'DET'                                                           
274100        IF SYST-IDSEKVNR = 1                                              
274200          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
274300          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
274400          COMPUTE R3-LINE-AMOUNT-LC =                                     
274500                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
274600          IF IN-EKH-KDVALISO = 'KRW'                                      
274700            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
274800          END-IF                                                          
274900          MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER             
275000          MOVE SPACE               TO WS-LINE-TEXT                        
275100          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
275200          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
275300          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
275400          MOVE SPACE               TO WS-ALLOCATE-DC                      
275500          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
275600          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
275700          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
275800          PERFORM S02-WRITE-W57091A                                       
275900        END-IF                                                            
276000                                                                          
276100        IF SYST-IDSEKVNR = 2                                              
276200          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
276300          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
276400          COMPUTE R3-LINE-AMOUNT-LC =                                     
276500                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
276600          IF IN-EKH-KDVALISO = 'KRW'                                      
276700            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
276800          END-IF                                                          
276900          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
277000          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
277100          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
277200          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
277300          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
277400          MOVE SPACE               TO WS-LINE-TEXT                        
277500          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
277600          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
277700          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
277800          PERFORM S02-WRITE-W57091A                                       
277900        END-IF                                                            
278000     END-EVALUATE                                                         
278100     .                                                                    
278200     EJECT                                                                
278300                                                                          
278400 CEJ301-SUB-EVENT-303-391 SECTION.                                        
278500     EVALUATE IN-EKH-KDEKNIVA                                             
278600     WHEN 'DET'                                                           
278700       IF SYST-IDSEKVNR = 1                                               
278800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
278900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
279000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
279100              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
279200         IF IN-EKH-KDVALISO = 'KRW'                                       
279300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
279400         END-IF                                                           
279500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
279600         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
279700         MOVE SPACE               TO WS-LINE-TEXT                         
279800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
279900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
280000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
280100         MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
280200         MOVE SPACE               TO WS-ALLOCATE-DC                       
280300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
280400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
280500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
280600         PERFORM S03-WRITE-W57072                                         
280700       END-IF                                                             
280800                                                                          
280900       IF SYST-IDSEKVNR = 2                                               
281000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
281100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
281200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
281300              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
281400         IF IN-EKH-KDVALISO = 'KRW'                                       
281500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
281600         END-IF                                                           
281700         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
281800         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
281900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
282000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
282100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
282200         MOVE SPACE               TO WS-LINE-TEXT                         
282300         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
282400         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
282500         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
282600         MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                  
282700         PERFORM S03-WRITE-W57072                                         
282800       END-IF                                                             
282900                                                                          
283000     END-EVALUATE                                                         
283100     .                                                                    
283200     EJECT                                                                
283300                                                                          
283400 CEK-MAIN-EVENT-401 SECTION.                                              
283500     EVALUATE IN-EKH-KDEKNIVA                                             
283600                                                                          
283700* PRISÄNDRING LÖPANDE                                                     
283800     WHEN 'DET'                                                           
283900       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
284000                           IN-EKH-PRARTSTD                                
284100       IF SYST-IDSEKVNR = 1                                               
284200* PRISHÖJNING                                                             
284300         IF WS-BELOPP > 0                                                 
284400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
284500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
284600           COMPUTE R3-LINE-AMOUNT-LC =                                    
284700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
284800           IF IN-EKH-KDVALISO = 'KRW'                                     
284900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
285000           END-IF                                                         
285100           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
285200           MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
285300           PERFORM S02-WRITE-W57091A                                      
285400         END-IF                                                           
285500       END-IF                                                             
285600                                                                          
285700       IF SYST-IDSEKVNR = 2                                               
285800* PRISSÄKNING                                                             
285900         IF WS-BELOPP < 0                                                 
286000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
286100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
286200           COMPUTE R3-LINE-AMOUNT-LC =                                    
286300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
286400           IF IN-EKH-KDVALISO = 'KRW'                                     
286500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
286600           END-IF                                                         
286700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
286800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
286900           MOVE SPACE               TO WS-ALLOCATE-REF                    
287000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
287100           MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
287200           PERFORM S02-WRITE-W57091A                                      
287300         END-IF                                                           
287400       END-IF                                                             
287500                                                                          
287600       IF SYST-IDSEKVNR = 3                                               
287700* PRISSÄNKNING                                                            
287800         IF WS-BELOPP < 0                                                 
287900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
288000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
288100           COMPUTE R3-LINE-AMOUNT-LC =                                    
288200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
288300           IF IN-EKH-KDVALISO = 'KRW'                                     
288400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
288500           END-IF                                                         
288600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
288700           MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
288800           PERFORM S02-WRITE-W57091A                                      
288900         END-IF                                                           
289000       END-IF                                                             
289100                                                                          
289200       IF SYST-IDSEKVNR = 4                                               
289300* PRISHÖJNING                                                             
289400         IF WS-BELOPP > 0                                                 
289500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
289600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
289700           COMPUTE R3-LINE-AMOUNT-LC =                                    
289800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
289900           IF IN-EKH-KDVALISO = 'KRW'                                     
290000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
290100           END-IF                                                         
290200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
290300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
290400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
290500           MOVE SPACE               TO WS-ALLOCATE-REF                    
290600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
290700           MOVE 0000409441          TO R3-LINE-PA-CUSTOMER                
290800           PERFORM S02-WRITE-W57091A                                      
290900         END-IF                                                           
291000       END-IF                                                             
291100     END-EVALUATE                                                         
291200     .                                                                    
291300     EJECT                                                                
291400                                                                          
291500 CEL-MAIN-EVENT-402 SECTION.                                              
291600     EVALUATE IN-EKH-KDEKNIVA                                             
291700     WHEN 'DET'                                                           
291800       IF SYST-IDSEKVNR = 1                                               
291900         IF IN-EKH-KVANTAL > 0                                            
292000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
292100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
292200           COMPUTE R3-LINE-AMOUNT-LC =                                    
292300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
292400           IF IN-EKH-KDVALISO = 'KRW'                                     
292500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
292600           END-IF                                                         
292700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
292800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
292900           MOVE SPACE               TO WS-ALLOCATE-REF                    
293000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
293100           PERFORM S02-WRITE-W57091A                                      
293200         END-IF                                                           
293300       END-IF                                                             
293400                                                                          
293500       IF SYST-IDSEKVNR = 2                                               
293600         IF IN-EKH-KVANTAL < 0                                            
293700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
293800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
293900           COMPUTE R3-LINE-AMOUNT-LC =                                    
294000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
294100           IF IN-EKH-KDVALISO = 'KRW'                                     
294200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
294300           END-IF                                                         
294400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
294500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
294600           MOVE SPACE               TO WS-ALLOCATE-REF                    
294700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
294800           PERFORM S02-WRITE-W57091A                                      
294900         END-IF                                                           
295000       END-IF                                                             
295100     END-EVALUATE                                                         
295200     .                                                                    
295300     EJECT                                                                
295400                                                                          
295500 CEM-MAIN-EVENT-403 SECTION.                                              
295600     EVALUATE IN-EKH-KDEKSHT                                              
295700     WHEN '401'                                                           
295800     WHEN '402'                                                           
295900     WHEN '403'                                                           
296000     WHEN '404'                                                           
296100     WHEN '405'                                                           
296200     WHEN '407'                                                           
296300     WHEN '408'                                                           
296400     WHEN '409'                                                           
296500          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
296600     END-EVALUATE                                                         
296700     .                                                                    
296800     EJECT                                                                
296900                                                                          
297000 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
297100     EVALUATE IN-EKH-KDEKNIVA                                             
297200     WHEN 'DET'                                                           
297300       IF IN-EKH-KVANTAL > 0                                              
297400         IF SYST-IDSEKVNR = 1                                             
297500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
297600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
297700           COMPUTE R3-LINE-AMOUNT-LC =                                    
297800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
297900           IF IN-EKH-KDVALISO = 'KRW'                                     
298000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
298100           END-IF                                                         
298200           PERFORM S02-WRITE-W57091A                                      
298300         END-IF                                                           
298400                                                                          
298500         IF SYST-IDSEKVNR = 4                                             
298600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
298700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
298800           COMPUTE R3-LINE-AMOUNT-LC =                                    
298900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
299000           IF IN-EKH-KDVALISO = 'KRW'                                     
299100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
299200           END-IF                                                         
299300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
299400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
299500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
299600           MOVE SPACE               TO WS-ALLOCATE-REF                    
299700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
299800           PERFORM S02-WRITE-W57091A                                      
299900         END-IF                                                           
300000       END-IF                                                             
300100                                                                          
300200       IF IN-EKH-KVANTAL < 0                                              
300300         IF SYST-IDSEKVNR = 2                                             
300400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
300500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
300600           COMPUTE R3-LINE-AMOUNT-LC =                                    
300700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
300800           IF IN-EKH-KDVALISO = 'KRW'                                     
300900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
301000           END-IF                                                         
301100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
301200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
301300           MOVE SPACE               TO WS-ALLOCATE-REF                    
301400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
301500           PERFORM S02-WRITE-W57091A                                      
301600         END-IF                                                           
301700                                                                          
301800         IF SYST-IDSEKVNR = 3                                             
301900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
302000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
302100           COMPUTE R3-LINE-AMOUNT-LC =                                    
302200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
302300           IF IN-EKH-KDVALISO = 'KRW'                                     
302400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
302500           END-IF                                                         
302600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
302700           PERFORM S02-WRITE-W57091A                                      
302800         END-IF                                                           
302900       END-IF                                                             
303000     END-EVALUATE                                                         
303100     .                                                                    
303200     EJECT                                                                
303300                                                                          
303400 CEN-MAIN-EVENT-404 SECTION.                                              
303500     EVALUATE IN-EKH-KDEKNIVA                                             
303600     WHEN 'DET'                                                           
303700       IF SYST-IDSEKVNR = 1                                               
303800* KONTO EJ MANUELLT REGISTRERAT                                           
303900         IF IN-EKH-IDKONTO = 0                                            
304000           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
304100           IF DIST18-SCRAP-NDC-SC                                         
304200           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
304300           OR DIST18-SCRAP-NDC-QUAL                                       
304400             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
304500             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
304600             COMPUTE R3-LINE-AMOUNT-LC =                                  
304700                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
304800             IF IN-EKH-KDVALISO = 'KRW'                                   
304900               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
305000             END-IF                                                       
305100             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
305200             PERFORM S02-WRITE-W57091A                                    
305300           END-IF                                                         
305400         END-IF                                                           
305500       END-IF                                                             
305600                                                                          
305700       IF SYST-IDSEKVNR = 2                                               
305800* KONTO MANUELLT REGISTRERAT                                              
305900         IF IN-EKH-IDKONTO > 0                                            
306000           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
306100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
306200           COMPUTE R3-LINE-AMOUNT-LC =                                    
306300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
306400           IF IN-EKH-KDVALISO = 'KRW'                                     
306500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
306600           END-IF                                                         
306700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
306800           PERFORM S02-WRITE-W57091A                                      
306900         END-IF                                                           
307000       END-IF                                                             
307100                                                                          
307200       IF SYST-IDSEKVNR = 3                                               
307300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
307400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
307500         COMPUTE R3-LINE-AMOUNT-LC =                                      
307600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
307700         IF IN-EKH-KDVALISO = 'KRW'                                       
307800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
307900         END-IF                                                           
308000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
308100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
308200         MOVE SPACE               TO WS-ALLOCATE-REF                      
308300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
308400         PERFORM S02-WRITE-W57091A                                        
308500       END-IF                                                             
308600                                                                          
308700                                                                          
308800     END-EVALUATE                                                         
308900     .                                                                    
309000     EJECT                                                                
309100                                                                          
309200 CF-BUILD-COMMON-210-PART SECTION.                                        
309300     MOVE SPACE              TO R3-LINE-R3                                
309400     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
309500                                R3-LINE-DUE-DATE                          
309600                                R3-LINE-AMOUNT                            
309700                                R3-LINE-AMOUNT-LC                         
309800                                R3-LINE-TAX-AMOUNT                        
309900                                R3-LINE-TAX-AMOUNT-LC                     
310000                                R3-LINE-NUMBER-OF-DAYS                    
310100                                R3-LINE-QUANTITY                          
310200                                R3-LINE-SAMNR                             
310300     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
310400     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
310500     MOVE 'KR02'             TO R3-LINE-COMPANY-CODE                      
310600     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
310700     IF SYST-KDPOST = '31'                                                
310800       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
310900     ELSE                                                                 
311000       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
311100     END-IF                                                               
311200     .                                                                    
311300     EJECT                                                                
311400                                                                          
311500 CG-SCHEDULE-LINE-AP SECTION.                                             
311600     MOVE NEJ                     TO WS-HEADER-SW                         
311700     MOVE JA                      TO WS-LINE-SW                           
311800     EVALUATE IN-EKH-KDEKHHT                                              
311900     WHEN '102'                                                           
312000       IF IN-EKH-KDEKSHT = '130'                                          
312100       OR IN-EKH-KDEKSHT = '134'                                          
312200         IF IN-EKH-KDEKSHT = '130'                                        
312300           PERFORM CGA-MAIN-EVENT-102-130                                 
312400         ELSE                                                             
312500           PERFORM CGA-MAIN-EVENT-102-134                                 
312600         END-IF                                                           
312700       ELSE                                                               
312800         IF IN-EKH-KDEKSHT = '120'                                        
312900         OR IN-EKH-KDEKSHT = '124'                                        
313000         OR IN-EKH-KDEKSHT = '125'                                        
313100           IF IN-EKH-KDEKSHT = '125'                                      
313200             PERFORM CGA-MAIN-EVENT-102-125                               
313300           ELSE                                                           
313400             PERFORM CGA-MAIN-EVENT-102-12X                               
313500           END-IF                                                         
313600         ELSE                                                             
313700           PERFORM CGA-MAIN-EVENT-102                                     
313800         END-IF                                                           
313900       END-IF                                                             
314000     WHEN '103'                                                           
314100         PERFORM CGA-MAIN-EVENT-103                                       
314200     WHEN '303'                                                           
314300       IF IN-EKH-KDEKSHT = '371'                                          
314400         PERFORM S81-GET-CURRENCY-RATE                                    
314500         PERFORM CGA-MAIN-EVENT-303-371                                   
314600       ELSE                                                               
314610         IF IN-EKH-KDEKSHT = '3XX'                                        
314620           PERFORM S81-GET-CURRENCY-RATE                                  
314700           PERFORM CGA-MAIN-EVENT-303-3XX                                 
314701         ELSE                                                             
314710           PERFORM CGA-MAIN-EVENT-303                                     
314720         END-IF                                                           
314800       END-IF                                                             
314900     END-EVALUATE                                                         
315000     .                                                                    
315100     EJECT                                                                
315200                                                                          
315300                                                                          
315400 CGA-MAIN-EVENT-102     SECTION.                                          
315500     EVALUATE IN-EKH-KDEKNIVA                                             
315600     WHEN 'SUM'                                                           
315700       IF IN-EKH-SUBEL > ZERO                                             
315800         IF SYST-IDSEKVNR = 1                                             
315900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
316000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
316100            IN-EKH-SUBEL                                                  
316200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
316300           PERFORM S10-VATCODE                                            
316400           IF IN-EKH-SUVAT = ZERO                                         
316500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
316600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
316700           ELSE                                                           
316800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
316900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
317000           END-IF                                                         
317100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
317200                                                                          
317300           PERFORM S04-WRITE-W57093A                                      
317400         END-IF                                                           
317500       END-IF                                                             
317600                                                                          
317700       IF IN-EKH-SUBEL < ZERO                                             
317800         IF SYST-IDSEKVNR = 2                                             
317900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
318000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
318100           IN-EKH-SUBEL                                                   
318200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
318300           PERFORM S10-VATCODE                                            
318400           IF IN-EKH-SUVAT = ZERO                                         
318500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
318600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
318700           ELSE                                                           
318800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
318900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
319000           END-IF                                                         
319100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
319200                                                                          
319300           PERFORM S04-WRITE-W57093A                                      
319400         END-IF                                                           
319500       END-IF                                                             
319600     END-EVALUATE                                                         
319700     .                                                                    
319800     EJECT                                                                
319900                                                                          
320000 CGA-MAIN-EVENT-102-12X SECTION.                                          
320100     EVALUATE IN-EKH-KDEKNIVA                                             
320200     WHEN 'SUM'                                                           
320300       IF IN-EKH-SUBEL > ZERO                                             
320400         IF SYST-IDSEKVNR = 1                                             
320500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
320600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
320700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
320800                   R3-LINE-AMOUNT    / WS-PRKURS-KR  * -1                 
320900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
321000           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
321100           IF IN-EKH-SUVAT = ZERO                                         
321200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
321300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
321400           ELSE                                                           
321500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
321600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
321700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-KR  * -1           
321800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
321900           END-IF                                                         
322000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
322100                                                                          
322200           PERFORM S04-WRITE-W57093A                                      
322300         END-IF                                                           
322400       END-IF                                                             
322500                                                                          
322600       IF IN-EKH-SUBEL < ZERO                                             
322700         IF SYST-IDSEKVNR = 2                                             
322800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
322900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
323000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
323100                   R3-LINE-AMOUNT    / WS-PRKURS-KR  * -1                 
323200           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
323300           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
323400           IF IN-EKH-SUVAT = ZERO                                         
323500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
323600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
323700           ELSE                                                           
323800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
323900             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
324000                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-KR  * -1           
324100             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
324200           END-IF                                                         
324300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
324400           MOVE SPACE           TO R3-LINE-COST-CENTER                    
324500                                                                          
324600           PERFORM S04-WRITE-W57093A                                      
324700         END-IF                                                           
324800       END-IF                                                             
324900     END-EVALUATE                                                         
325000     .                                                                    
325100     EJECT                                                                
325200                                                                          
325300                                                                          
325400 CGA-MAIN-EVENT-102-125 SECTION.                                          
325500     EVALUATE IN-EKH-KDEKNIVA                                             
325600     WHEN 'SUM'                                                           
325700       IF IN-EKH-SUBEL > ZERO                                             
325800         IF SYST-IDSEKVNR = 1                                             
325900           MOVE ZERO TO SPAR-SUMMA-102-125                                
326000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
326100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
326200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
326300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
326400                   R3-LINE-AMOUNT    / WS-PRKURS-KR  * -1                 
326500           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
326600           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
326700           IF IN-EKH-SUVAT = ZERO                                         
326800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
326900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
327000           ELSE                                                           
327100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
327200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
327300               R3-LINE-TAX-AMOUNT / WS-PRKURS-KR  * -1                    
327400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
327500           END-IF                                                         
327600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
327700           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
327800                                                                          
327900           PERFORM S04-WRITE-W57093A                                      
328000         END-IF                                                           
328100       END-IF                                                             
328200                                                                          
328300       IF IN-EKH-SUBEL < ZERO                                             
328400         IF SYST-IDSEKVNR = 2                                             
328500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
328600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
328700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
328800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
328900                   R3-LINE-AMOUNT    / WS-PRKURS-KR  * -1                 
329000           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
329100           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
329200           IF IN-EKH-SUVAT = ZERO                                         
329300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
329400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
329500           ELSE                                                           
329600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
329700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
329800               R3-LINE-TAX-AMOUNT / WS-PRKURS-KR * -1                     
329900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
330000           END-IF                                                         
330100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
330200           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
330300                                                                          
330400           PERFORM S04-WRITE-W57093A                                      
330500         END-IF                                                           
330600       END-IF                                                             
330700     END-EVALUATE                                                         
330800     .                                                                    
330900     EJECT                                                                
331000 CGA-MAIN-EVENT-102-130 SECTION.                                          
331100     EVALUATE IN-EKH-KDEKNIVA                                             
331200     WHEN 'SUM'                                                           
331300       IF IN-EKH-SUBEL > ZERO                                             
331400         IF SYST-IDSEKVNR = 1                                             
331500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
331600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
331700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
331800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
331900                   R3-LINE-AMOUNT    / WS-PRKURS-KR  * -1                 
332000           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
332100           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
332200           IF IN-EKH-SUVAT = ZERO                                         
332300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
332400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
332500           ELSE                                                           
332600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
332700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
332800                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-KR  * -1           
332900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
333000           END-IF                                                         
333100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
333200                                                                          
333300           PERFORM S04-WRITE-W57093A                                      
333400         END-IF                                                           
333500       END-IF                                                             
333600                                                                          
333700       IF IN-EKH-SUBEL < ZERO                                             
333800         IF SYST-IDSEKVNR = 2                                             
333900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
334000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
334100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
334200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
334300                   R3-LINE-AMOUNT    / WS-PRKURS-KR                       
334400           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
334500           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
334600           IF IN-EKH-SUVAT = ZERO                                         
334700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
334800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
334900           ELSE                                                           
335000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
335100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
335200                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-KR                 
335300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
335400           END-IF                                                         
335500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
335600                                                                          
335700           PERFORM S04-WRITE-W57093A                                      
335800         END-IF                                                           
335900       END-IF                                                             
336000     END-EVALUATE                                                         
336100     .                                                                    
336200     EJECT                                                                
336300                                                                          
336400 CGA-MAIN-EVENT-102-134 SECTION.                                          
336500     EVALUATE IN-EKH-KDEKNIVA                                             
336600     WHEN 'SUM'                                                           
336700       IF IN-EKH-SUBEL > ZERO                                             
336800         IF SYST-IDSEKVNR = 1                                             
336900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
337000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
337100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
337200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
337300                   R3-LINE-AMOUNT    / WS-PRKURS-KR  * -1                 
337400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
337500           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
337600           IF IN-EKH-SUVAT = ZERO                                         
337700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
337800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
337900           ELSE                                                           
338000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
338100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
338200                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-KR  * -1           
338300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
338400           END-IF                                                         
338500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
338600                                                                          
338700           PERFORM S04-WRITE-W57093A                                      
338800         END-IF                                                           
338900       END-IF                                                             
339000                                                                          
339100       IF IN-EKH-SUBEL < ZERO                                             
339200         IF SYST-IDSEKVNR = 2                                             
339300           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
339400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
339500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
339600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
339700                   R3-LINE-AMOUNT    / WS-PRKURS-KR                       
339800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
339900           MOVE 'P7'     TO R3-LINE-TAX-CODE                              
340000           IF IN-EKH-SUVAT = ZERO                                         
340100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
340200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
340300           ELSE                                                           
340400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
340500             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
340600                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-KR                 
340700             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
340800           END-IF                                                         
340900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
341000                                                                          
341100           PERFORM S04-WRITE-W57093A                                      
341200         END-IF                                                           
341300       END-IF                                                             
341400     END-EVALUATE                                                         
341500     .                                                                    
341600     EJECT                                                                
341700                                                                          
341800 CGA-MAIN-EVENT-103     SECTION.                                          
341900     EVALUATE IN-EKH-KDEKNIVA                                             
342000     WHEN 'SUM'                                                           
342100       MOVE IN-EKH-IDLEVNR         TO W-IDLEVNR                           
342200       PERFORM IMS-GET-WDF101                                             
342300       PERFORM IMS-GNP-WDF106                                             
342400       IF IN-EKH-SUBEL > ZERO                                             
342500         IF SYST-IDSEKVNR = 1                                             
342600           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
342700           IF ADR-IDLANDX2 = 'KR'                                         
342800             PERFORM S10-VATCODE                                          
342900           ELSE                                                           
343000             MOVE 'P7'             TO R3-LINE-TAX-CODE                    
343100             MOVE ZERO             TO IN-EKH-SUVAT                        
343200           END-IF                                                         
343300           IF IN-EKH-SUVAT = ZERO                                         
343400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
343500                                      R3-LINE-TAX-AMOUNT-LC               
343600           ELSE                                                           
343700             IF IN-EKH-KDVALISO = 'KRW'                                   
343800               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
343900                                      R3-LINE-TAX-AMOUNT-LC               
344000             ELSE                                                         
344100               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
344200                                      R3-LINE-TAX-AMOUNT-LC               
344300               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
344400                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
344500             END-IF                                                       
344600           END-IF                                                         
344700**** CALCULATE NEW SUM WITH VAT                                           
344800           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
344900                   IN-EKH-SUVAT                                           
345000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
345100           IF IN-EKH-KDVALISO = 'KRW'                                     
345200             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
345300           ELSE                                                           
345400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
345500                     R3-LINE-AMOUNT * WS-PRKURS                           
345600           END-IF                                                         
345700           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
345800                                                                          
345900           PERFORM S04-WRITE-W57093A                                      
346000         END-IF                                                           
346100       END-IF                                                             
346200                                                                          
346300       IF IN-EKH-SUBEL < ZERO                                             
346400         IF SYST-IDSEKVNR = 1                                             
346500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
346600           IF ADR-IDLANDX2 = 'KR'                                         
346700             MOVE 'P1'             TO R3-LINE-TAX-CODE                    
346800             COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.1            
346900           ELSE                                                           
347000             MOVE 'P7'             TO R3-LINE-TAX-CODE                    
347100             MOVE ZERO             TO IN-EKH-SUVAT                        
347200           END-IF                                                         
347300           IF IN-EKH-SUVAT = ZERO                                         
347400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
347500                                      R3-LINE-TAX-AMOUNT-LC               
347600           ELSE                                                           
347700             IF IN-EKH-KDVALISO = 'KRW'                                   
347800               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
347900                                      R3-LINE-TAX-AMOUNT-LC               
348000             ELSE                                                         
348100               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
348200                                      R3-LINE-TAX-AMOUNT-LC               
348300               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
348400                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
348500             END-IF                                                       
348600           END-IF                                                         
348700**** CALCULATE NEW SUM WITH VAT                                           
348800           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
348900                   IN-EKH-SUVAT                                           
349000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
349100           IF IN-EKH-KDVALISO = 'KRW'                                     
349200             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
349300           ELSE                                                           
349400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
349500                     R3-LINE-AMOUNT * WS-PRKURS                           
349600           END-IF                                                         
349700           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
349800                                                                          
349900           PERFORM S04-WRITE-W57093A                                      
350000         END-IF                                                           
350100       END-IF                                                             
350200     END-EVALUATE                                                         
350300     .                                                                    
350400     EJECT                                                                
350500                                                                          
350600 CGA-MAIN-EVENT-303 SECTION.                                              
350700     EVALUATE IN-EKH-KDEKNIVA                                             
350800     WHEN 'SUM'                                                           
350900       IF SYST-IDSEKVNR = 1                                               
351000         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
351100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
351200         (IN-EKH-SUBEL / WS-PRKURS-KR)                                    
351300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
351400         MOVE '  '     TO R3-LINE-TAX-CODE                                
351500         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
351600         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
351700                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-KR                     
351800         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
351900                                                                          
352000         PERFORM S04-WRITE-W57093A                                        
352100       END-IF                                                             
352200     END-EVALUATE                                                         
352300     .                                                                    
352400     EJECT                                                                
352500                                                                          
352510 CGA-MAIN-EVENT-303-3XX SECTION.                                          
352520     EVALUATE IN-EKH-KDEKNIVA                                             
352530     WHEN 'SUM'                                                           
352540       IF SYST-IDSEKVNR = 1                                               
352550         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
352560         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
352570         (IN-EKH-SUBEL / WS-PRKURS-KR3)                                   
352580         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
352590         MOVE '  '     TO R3-LINE-TAX-CODE                                
352591         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
352592         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
352593                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-KR3                    
352594         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
352595                                                                          
352596         PERFORM S04-WRITE-W57093A                                        
352597       END-IF                                                             
352598     END-EVALUATE                                                         
352599     .                                                                    
352600     EJECT                                                                
352601                                                                          
352610 CGA-MAIN-EVENT-303-371 SECTION.                                          
352700     EVALUATE IN-EKH-KDEKNIVA                                             
352800     WHEN 'SUM'                                                           
352900       IF SYST-IDSEKVNR = 1                                               
353000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
353100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
353200         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
353300         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
353400               R3-LINE-AMOUNT-LC / WS-PRKURS-KR3                          
353500         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
353600         MOVE 'P7'                 TO R3-LINE-TAX-CODE                    
353700         MOVE ZERO                 TO IN-EKH-SUVAT                        
353800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
353900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
354000                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-KR3                    
354100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
354200                                                                          
354300         PERFORM S04-WRITE-W57093A                                        
354400       END-IF                                                             
354500     END-EVALUATE                                                         
354600     .                                                                    
354700     EJECT                                                                
354800 CH-BUILD-COMMON-310-PART SECTION.                                        
354900     MOVE SPACE              TO R3-LINE-R3                                
355000     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
355100                                R3-LINE-DUE-DATE                          
355200                                R3-LINE-AMOUNT                            
355300                                R3-LINE-AMOUNT-LC                         
355400                                R3-LINE-TAX-AMOUNT                        
355500                                R3-LINE-TAX-AMOUNT-LC                     
355600                                R3-LINE-NUMBER-OF-DAYS                    
355700                                R3-LINE-QUANTITY                          
355800                                R3-LINE-SAMNR                             
355900     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
356000     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
356100     MOVE 'KR02'             TO R3-LINE-COMPANY-CODE                      
356200     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
356300     IF SYST-KDPOST = '31'                                                
356400       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
356500     ELSE                                                                 
356600       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
356700     END-IF                                                               
356800     .                                                                    
356900     EJECT                                                                
357000                                                                          
357100 CI-SCHEDULE-LINE-AR SECTION.                                             
357200     MOVE NEJ                     TO WS-HEADER-SW                         
357300     MOVE JA                      TO WS-LINE-SW                           
357400     EVALUATE IN-EKH-KDEKHHT                                              
357500     WHEN '204'                                                           
357600         PERFORM CIA-MAIN-EVENT-204                                       
357700     END-EVALUATE                                                         
357800     .                                                                    
357900     EJECT                                                                
358000                                                                          
358100 CIA-MAIN-EVENT-204     SECTION.                                          
358200     EVALUATE IN-EKH-KDEKNIVA                                             
358300     WHEN 'SUM'                                                           
358400       IF IN-EKH-SUBEL > ZERO                                             
358500         IF SYST-IDSEKVNR = 1                                             
358600           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
358700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
358800           IF IN-EKH-KDVALISO = 'KRW'                                     
358900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
359000           END-IF                                                         
359100           IF IN-EKH-KDEKSHT = '301'                                      
359200             MOVE 'P7'             TO R3-LINE-TAX-CODE                    
359300           ELSE                                                           
359400             PERFORM S10-VATCODE                                          
359500           END-IF                                                         
359600           IF IN-EKH-SUVAT = ZERO                                         
359700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
359800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
359900           ELSE                                                           
360000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
360100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
360200           END-IF                                                         
360300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
360400                                                                          
360500           PERFORM S04-WRITE-W57093A                                      
360600         END-IF                                                           
360700       END-IF                                                             
360800                                                                          
360900     END-EVALUATE                                                         
361000     .                                                                    
361100     EJECT                                                                
361200                                                                          
361300 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
361400     MOVE ZERO             TO LOGG-W57073                                 
361500     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
361600     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
361700     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
361800     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
361900     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
362000     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
362100     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
362200     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
362300     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
362400     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
362500     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
362600     MOVE 'KR02'           TO LOGG-KDTRADP                                
362700                                                                          
362800****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
362900     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
363000     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
363100     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
363200     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
363300     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
363400     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
363500     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
363600     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
363700     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
363800     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
363900     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
364000     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
364100     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
364200     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
364300     .                                                                    
364400     EJECT                                                                
364500                                                                          
364600 Z-FINI SECTION.                                                          
364700     CLOSE W57066                                                         
364800           W57090                                                         
364900           W57091A                                                        
365000           W57092A                                                        
365100           W57093A                                                        
365200           W57095                                                         
365300           W5709N                                                         
365400           W51390                                                         
365500                                                                          
365600     MOVE 'S' TO POSTSUM-OPKOD                                            
365700     CALL POSTSUM USING POSTSUM-PARM                                      
365800     .                                                                    
365900     EJECT                                                                
366000                                                                          
366100 S01-READ-W57066  SECTION.                                                
366200     READ W57066 INTO IN-AREA                                             
366300     AT END                                                               
366400        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
366500        SET END-OF-W57066 TO TRUE                                         
366600                                                                          
366700     NOT AT END                                                           
366800        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
366900        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
367000        MOVE 'W57078D1'   TO POSTSUM-DDNAMN2                              
367100        CALL POSTSUM USING POSTSUM-PARM                                   
367200                                                                          
367300        IF IN-EKH-KDPRODSL NOT = 0                                        
367400          MOVE IN-EKH-KDPRODSL TO WS-KDPRODSL-SAVE                        
367500        END-IF                                                            
367600     END-READ                                                             
367700     .                                                                    
367800                                                                          
367900 S02-WRITE-W57091A SECTION.                                               
368000     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
368100     MOVE SPACE                 TO 71LINE-POST                            
368200     IF WS-LINE-SW = JA                                                   
368300       IF IN-EKH-KDSORT = 'SW'                                            
368400         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
368500         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
368600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
368700       ELSE                                                               
368800         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
368900         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
369000         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
369100       END-IF                                                             
369200       WRITE 71LINE-POST        FROM R3-LINE-R3                           
369300       PERFORM S20-CREATE-WRITE-LOG                                       
369400     ELSE                                                                 
369500       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
369600     END-IF                                                               
369700                                                                          
369800     IF WS-LINE-SW = JA                                                   
369900       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
370000     ELSE                                                                 
370100       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
370200     END-IF                                                               
370300     MOVE 'W57091A'             TO POSTSUM-FDNAMN                         
370400     MOVE 'W57078D2'            TO POSTSUM-DDNAMN2                        
370500     CALL POSTSUM USING POSTSUM-PARM                                      
370600     .                                                                    
370700                                                                          
370800 S002-WRITE-W57091A-HEAD SECTION.                                         
370900     MOVE SPACE                 TO 71LINE-POST                            
371000     IF IN-EKH-KDSORT = 'SW'                                              
371100       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
371200       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
371300       MOVE WS-TEXT           TO R3-LINE-TEXT                             
371400     ELSE                                                                 
371500       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
371600       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
371700       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
371800     END-IF                                                               
371900     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
372000                                                                          
372100     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
372200     MOVE 'W57091A'             TO POSTSUM-FDNAMN                         
372300     MOVE 'W57078D2'            TO POSTSUM-DDNAMN2                        
372400     CALL POSTSUM USING POSTSUM-PARM                                      
372500     .                                                                    
372600                                                                          
372700 S03-WRITE-W57072 SECTION.                                                
372800     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
372900     IF IN-EKH-KDSORT = 'SW'                                              
373000       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
373100       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
373200       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
373300     ELSE                                                                 
373400       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
373500       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
373600       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
373700     END-IF                                                               
373800     WRITE 72LINE-POST        FROM R3-LINE-R3                             
373900                                                                          
374000     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
374100     MOVE 'W57092A'           TO POSTSUM-FDNAMN                           
374200     MOVE 'W57078D3'          TO POSTSUM-DDNAMN2                          
374300     CALL POSTSUM USING POSTSUM-PARM                                      
374400                                                                          
374500     PERFORM S20-CREATE-WRITE-LOG                                         
374600     .                                                                    
374700                                                                          
374800 S04-WRITE-W57093A SECTION.                                               
374900     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
375000     MOVE SPACE                 TO 73LINE-POST                            
375100     IF WS-LINE-SW = JA                                                   
375200       IF IN-EKH-KDSORT = 'SW'                                            
375300         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
375400         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
375500         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
375600       ELSE                                                               
375700         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
375800         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
375900         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
376000       END-IF                                                             
376100       WRITE 73LINE-POST        FROM R3-LINE-R3                           
376200       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
376300     ELSE                                                                 
376400       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
376500       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
376600     END-IF                                                               
376700                                                                          
376800     MOVE 'W57093A'             TO POSTSUM-FDNAMN                         
376900     MOVE 'W57078D4'            TO POSTSUM-DDNAMN2                        
377000     CALL POSTSUM USING POSTSUM-PARM                                      
377100                                                                          
377200     IF WS-LINE-SW = JA                                                   
377300       PERFORM S20-CREATE-WRITE-LOG                                       
377400     END-IF                                                               
377500     .                                                                    
377600                                                                          
377700 S004-WRITE-W57093A-HEAD SECTION.                                         
377800     MOVE SPACE                 TO 73LINE-POST                            
377900     IF IN-EKH-KDSORT = 'SW'                                              
378000       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
378100       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
378200       MOVE WS-TEXT           TO R3-LINE-TEXT                             
378300     ELSE                                                                 
378400       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
378500       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
378600       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
378700     END-IF                                                               
378800     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
378900                                                                          
379000     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
379100     MOVE 'W57093A'             TO POSTSUM-FDNAMN                         
379200     MOVE 'W57078D4'            TO POSTSUM-DDNAMN2                        
379300     CALL POSTSUM USING POSTSUM-PARM                                      
379400                                                                          
379500     .                                                                    
379600                                                                          
379700 S10-VATCODE SECTION.                                                     
379800     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
379900     PERFORM IMS-GU-WDB601                                                
380000     IF DCS-KDDC = SPACE                                                  
380100       MOVE NEJ              TO WDB6-A-SW                                 
380200     ELSE                                                                 
380300       MOVE JA               TO WDB6-A-SW                                 
380400     END-IF                                                               
380500                                                                          
380600     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
380700     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
380800     IF IN-EKH-SUVAT = ZERO                                               
380900       MOVE 'P1'     TO R3-LINE-TAX-CODE                                  
381000**** HERE WE ADD VAT FOR 10% TO BOOKING                                   
381100       COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.1                  
381200     ELSE                                                                 
381300       MOVE 'P7'     TO R3-LINE-TAX-CODE                                  
381400     END-IF                                                               
381500     IF IN-EKH-BEVAT = 'XX'                                               
381600       MOVE 'P7'     TO R3-LINE-TAX-CODE                                  
381700     END-IF                                                               
381800     .                                                                    
381900     EJECT                                                                
382000                                                                          
382100 S13-GET-LANDING-COST SECTION.                                            
382200     MOVE '65'                   TO W-IDDC-B6                             
382300     PERFORM IMS-GU-WDB601                                                
382400     IF SEGMENT-FINNS                                                     
382500       PERFORM IMS-GNP-WDB617                                             
382600       IF SEGMENT-FINNS                                                   
382700         IF PROC-TILANDCO >  IN-EKH-DAVERDAT                              
382800           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
382900         ELSE                                                             
383000           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
383100         END-IF                                                           
383200       END-IF                                                             
383300     END-IF                                                               
383400     .                                                                    
383500     EJECT                                                                
383600 S20-CREATE-WRITE-LOG SECTION.                                            
383700     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
383800     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
383900     IF SYST-IDPTYP = '610'                                               
384000       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
384100     ELSE                                                                 
384200       MOVE ZERO                      TO WS-IDLEVNR                       
384300       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
384400                          FOR CHARACTERS BEFORE INITIAL SPACE             
384500       IF WS-IDLEVNR   > ZERO                                             
384600          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
384700                                      TO LOGG-IDKONTO                     
384800       END-IF                                                             
384900     END-IF                                                               
385000     IF R3-LINE-COST-CENTER NOT = SPACE                                   
385100       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
385200     END-IF                                                               
385300     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
385400     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
385500     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
385600     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
385700     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
385800                                                                          
385900     PERFORM S21-WRITE-W57095                                             
386000     PERFORM S22-WRITE-W57090                                             
386100                                                                          
386200     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
386300       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
386400       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
386500       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
386600                                                                          
386700       PERFORM S21-WRITE-W57095                                           
386800     END-IF                                                               
386900     .                                                                    
387000     EJECT                                                                
387100                                                                          
387200 S21-WRITE-W57095 SECTION.                                                
387300     IF DCS-IDDC NOT = LOGG-IDDC                                          
387400        MOVE LOGG-IDDC TO W-IDDC-B6                                       
387500        PERFORM IMS-GU-WDB601                                             
387600     END-IF                                                               
387700     IF DCS-KDDC = SPACE                                                  
387800       MOVE NEJ              TO WDB6-A-SW                                 
387900     ELSE                                                                 
388000       MOVE JA               TO WDB6-A-SW                                 
388100     END-IF                                                               
388200                                                                          
388300     IF  WDB6-A-FINNS                                                     
388400     AND DCS-DDC                                                          
388500       MOVE 'N'       TO LOGG-FLLSBOK                                     
388600     END-IF                                                               
388700     WRITE LOGG-POST FROM LOGG-W57073                                     
388800                                                                          
388900     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
389000     MOVE 'W57095'    TO POSTSUM-FDNAMN                                   
389100     MOVE 'W57078D5'  TO POSTSUM-DDNAMN2                                  
389200     CALL POSTSUM USING POSTSUM-PARM                                      
389300     .                                                                    
389400                                                                          
389500 S22-WRITE-W57090 SECTION.                                                
389600     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
389700     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
389800     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
389900                                                                          
390000     IF R3-LINE-AMOUNT-SIGN = '+'                                         
390100       IF AVST-SUBEL < +0                                                 
390200         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
390300       END-IF                                                             
390400       IF AVST-KVANTAL < +0                                               
390500         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
390600       END-IF                                                             
390700     ELSE                                                                 
390800       IF AVST-SUBEL > +0                                                 
390900         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
391000       END-IF                                                             
391100       IF AVST-KVANTAL > +0                                               
391200         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
391300       END-IF                                                             
391400     END-IF                                                               
391500                                                                          
391600     IF DCS-IDDC NOT = AVST-IDDC                                          
391700        MOVE AVST-IDDC  TO W-IDDC-B6                                      
391800        PERFORM IMS-GU-WDB601                                             
391900     END-IF                                                               
392000     IF DCS-KDDC = SPACE                                                  
392100       MOVE NEJ              TO WDB6-A-SW                                 
392200     ELSE                                                                 
392300       MOVE JA               TO WDB6-A-SW                                 
392400     END-IF                                                               
392500                                                                          
392600     IF  WDB6-A-FINNS                                                     
392700     AND DCS-DDC                                                          
392800       MOVE 'N'                 TO AVST-FLLSBOK                           
392900     END-IF                                                               
393000                                                                          
393100     IF AVST-IDKONTO(1:4) = '1454'                                        
393200       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
393300       WRITE AVST-POST FROM AVST-W57070                                   
393400                                                                          
393500       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
393600       MOVE 'W57090'            TO POSTSUM-FDNAMN                         
393700       MOVE 'W57078D6'          TO POSTSUM-DDNAMN2                        
393800       CALL POSTSUM USING POSTSUM-PARM                                    
393900     END-IF                                                               
394000     .                                                                    
394100     EJECT                                                                
394200                                                                          
394300 S30-READ-DATABASE-B2-B1 SECTION.                                         
394400                                                                          
394500     IF IN-EKH-IDLEVNR = '1441'                                           
394600       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
394700     ELSE                                                                 
394800       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
394900       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
395000       PERFORM IMS-GU-WDB201                                              
395100       IF SEGMENT-SAKNAS                                                  
395200         MOVE 'KR99999'       TO W-WDB1-IDPARTNR                          
395300       ELSE                                                               
395400         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
395500       END-IF                                                             
395600     END-IF                                                               
395700     MOVE WC-IDFTG-KR         TO W-WDB1-IDFTG                             
395800     PERFORM IMS-GU-WDB101                                                
395900     IF SEGMENT-SAKNAS                                                    
396000       DISPLAY 'BETALARUPPG. SAKNAS '                                     
396100       DISPLAY IN-EKH-IDVERGL                                             
396200       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
396300       DISPLAY GMT-IDPARTNR                                               
396400                                                                          
396500       MOVE SPACE         TO BET-KDTRADP                                  
396600       MOVE ZERO          TO BET-IDPARTNR                                 
396700       MOVE '????'        TO WS-KDBETVIL                                  
396800       MOVE '???'         TO WS-KDVALISO-WDB1                             
396900     ELSE                                                                 
397000       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
397100     END-IF                                                               
397200     MOVE 'KRW'           TO WS-KDVALISO-WDB1                             
397300                                                                          
397400     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
397500     MOVE ZERO TO TALLY                                                   
397600     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
397700                 FOR CHARACTERS BEFORE INITIAL SPACE                      
397800     IF TALLY = ZERO                                                      
397900       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
398000     ELSE                                                                 
398100       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
398200                                TO W-BET-IDPARTNR-NUM                     
398300     END-IF                                                               
398400     .                                                                    
398500     EJECT                                                                
398600                                                                          
398700 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
398800     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
398900     PERFORM IMS-GU-WDB601                                                
399000     IF DCS-KDDC = SPACE                                                  
399100       MOVE NEJ              TO WDB6-A-SW                                 
399200     ELSE                                                                 
399300       MOVE JA               TO WDB6-A-SW                                 
399400     END-IF                                                               
399500                                                                          
399600     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
399700       IF IN-EKH-KDEKSHT NOT = '406'                                      
399800         IF IN-EKH-FLDCET = NEJ                                           
399900           PERFORM S42-SKAPA-RW2-INV-POSTER                               
400000         END-IF                                                           
400100       END-IF                                                             
400200     END-IF                                                               
400300                                                                          
400400     IF IN-EKH-KDEKNIVA = 'DET'                                           
400500       IF  IN-EKH-KDEKHHT = '204'                                         
400600       AND (IN-EKH-KDEKSHT = '201')                                       
400700         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
400800       END-IF                                                             
400900                                                                          
401000       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
401100       AND (WDB6-A-FINNS                                                  
401200       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
401300       OR   DCS-DDC OR DCS-NDC-PF))                                       
401400         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
401500       END-IF                                                             
401600                                                                          
401700       IF IN-FIL-IDPGM = 'W4183000'                                       
401800       AND (WDB6-A-FINNS                                                  
401900       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
402000       OR   DCS-DDC OR DCS-NDC-PF))                                       
402100         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
402200       END-IF                                                             
402300     END-IF                                                               
402400     .                                                                    
402500     EJECT                                                                
402600                                                                          
402700 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
402800     MOVE 'RW2'              TO RW2-IDPTYP                                
402900     MOVE 'RW2'              TO WS-IDPTYP                                 
403000     MOVE ZERO               TO RW2-IDDISTR                               
403100     IF DCS-KDDC = SPACE OR DCS-DDC                                       
403200       MOVE WC-CDC-SE        TO RW2-IDDC                                  
403300     ELSE                                                                 
403400       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
403500     END-IF                                                               
403600     IF IN-EKH-KVANTAL < +0                                               
403700       MOVE '0422'           TO RW2-KDWRTYP                               
403800     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
403900     ELSE                                                                 
404000       MOVE '0421'           TO RW2-KDWRTYP                               
404100       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
404200     END-IF                                                               
404300                                                                          
404400     IF RW2-SUARTSTD NOT = +0                                             
404500       PERFORM S70-WRITE-W51390                                           
404600     END-IF                                                               
404700     .                                                                    
404800     EJECT                                                                
404900                                                                          
405000 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
405100     MOVE '0110'             TO RW1-KDWRTYP                               
405200     IF DCS-KDDC = SPACE OR DCS-DDC                                       
405300       MOVE WC-CDC-SE        TO RW1-IDDC                                  
405400     ELSE                                                                 
405500       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
405600     END-IF                                                               
405700     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
405800     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
405900     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
406000                                                                          
406100     IF RW1-SUARTSTD NOT = +0                                             
406200       MOVE 'RW1' TO WS-IDPTYP                                            
406300       PERFORM S70-WRITE-W51390                                           
406400     END-IF                                                               
406500     .                                                                    
406600     EJECT                                                                
406700                                                                          
406800 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
406900     MOVE '0110'             TO RW1-KDWRTYP                               
407000     IF DCS-KDDC = SPACE OR DCS-DDC                                       
407100       MOVE WC-CDC-SE        TO RW1-IDDC                                  
407200     ELSE                                                                 
407300       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
407400     END-IF                                                               
407500     IF IN-EKH-KDANMORS = '30'                                            
407600       MOVE ZERO             TO RW1-SUARTSTD                              
407700     ELSE                                                                 
407800      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
407900     END-IF                                                               
408000     IF IN-EKH-KDANMORS = '30' OR '80'                                    
408100       MOVE ZERO             TO RW1-SUARTSJK                              
408200     ELSE                                                                 
408300      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
408400     END-IF                                                               
408500     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
408600                                                                          
408700     IF RW1-SUARTSTD NOT = +0                                             
408800       MOVE 'RW1' TO WS-IDPTYP                                            
408900       PERFORM S70-WRITE-W51390                                           
409000     END-IF                                                               
409100     .                                                                    
409200     EJECT                                                                
409300                                                                          
409400 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
409500     MOVE '0110'             TO RW1-KDWRTYP                               
409600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
409700       MOVE WC-CDC-SE        TO RW1-IDDC                                  
409800     ELSE                                                                 
409900       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
410000     END-IF                                                               
410100     IF IN-EKH-KDEKSHT = '310'                                            
410200*** SKROTNING KDANMORS  13 O 23                                           
410300       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
410400     ELSE                                                                 
410500      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
410600     END-IF                                                               
410700                                                                          
410800     MOVE ZERO               TO RW1-SUARTSJK                              
410900                                RW1-SUARTFSG                              
411000     IF RW1-SUARTSTD NOT = +0                                             
411100       MOVE 'RW1' TO WS-IDPTYP                                            
411200       PERFORM S70-WRITE-W51390                                           
411300     END-IF                                                               
411400     .                                                                    
411500     EJECT                                                                
411600                                                                          
411700 S60-WRITE-W5709N SECTION.                                                
411800     WRITE SAPUT-POST  FROM IN-AREA                                       
411900                                                                          
412000     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
412100     MOVE 'W5709N'            TO POSTSUM-FDNAMN                           
412200     MOVE 'W57078D7'          TO POSTSUM-DDNAMN2                          
412300     CALL POSTSUM USING POSTSUM-PARM                                      
412400     .                                                                    
412500     EJECT                                                                
412600                                                                          
412700 S70-WRITE-W51390 SECTION.                                                
412800     IF WS-IDPTYP  = 'RW2'                                                
412900       IF DCS-KDDC = SPACE OR DCS-DDC                                     
413000         MOVE WC-CDC-SE        TO INV-IDDC                                
413100       ELSE                                                               
413200         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
413300       END-IF                                                             
413400       IF IN-EKH-KVANTAL < +0                                             
413500         MOVE '003'            TO INV-IDPTYP                              
413600       COMPUTE INV-SUARTSTD =                                             
413700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
413800       ELSE                                                               
413900         MOVE '002'            TO INV-IDPTYP                              
414000         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
414100       END-IF                                                             
414200       MOVE SPACE TO WS-IDPTYP                                            
414300       MOVE 0                  TO INV-ADLAGOMR                            
414400       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
414500       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
414600     END-IF                                                               
414700     IF WS-IDPTYP  = 'RW1'                                                
414800       IF DCS-KDDC = SPACE OR DCS-DDC                                     
414900         MOVE WC-CDC-SE        TO INV-IDDC                                
415000       ELSE                                                               
415100         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
415200       END-IF                                                             
415300       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
415400       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
415500       MOVE 0                  TO INV-ADLAGOMR                            
415600       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
415700       MOVE '001'              TO INV-IDPTYP                              
415800       MOVE SPACE              TO WS-IDPTYP                               
415900     END-IF                                                               
416000     WRITE INV-POST  FROM INV-W51310                                      
416100                                                                          
416200     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
416300     MOVE 'W51390'            TO POSTSUM-FDNAMN                           
416400     MOVE 'W57078D8'          TO POSTSUM-DDNAMN2                          
416500     CALL POSTSUM USING POSTSUM-PARM                                      
416600     .                                                                    
416700     EJECT                                                                
416800                                                                          
416900 S80-GET-CURRENCY-RATE SECTION.                                           
417000     MOVE +0                  TO W-ANT                                    
417100     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
417200             BEFORE INITIAL ' '                                           
417300     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
417400     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
417500     PERFORM IMS-GU-WDL601                                                
417600     IF SEGMENT-SAKNAS                                                    
417700       CONTINUE                                                           
417800     ELSE                                                                 
417900       PERFORM IMS-GNP-WDL611                                             
418000       IF SEGMENT-SAKNAS                                                  
418100         CONTINUE                                                         
418200       ELSE                                                               
418300         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
418400                                   - INL-DAINLEV                          
418500         MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                   
418600         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
418700         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
418800         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
418900         MOVE WS-KDVALISO-KR        TO CURR-KDVALISO-ROW                  
419000         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
419100         IF CURR-KDSVAR = ' '                                             
419200           MOVE CURR-PRKURS-NEW     TO WS-PRKURS-KR3                      
419300         ELSE                                                             
419400           MOVE +1                  TO WS-PRKURS-KR3                      
419500         END-IF                                                           
419600       END-IF                                                             
419700     END-IF                                                               
419800     .                                                                    
419900     EJECT                                                                
420000                                                                          
420100 S81-GET-CURRENCY-RATE SECTION.                                           
420200     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
420300     MOVE 'KRW'               TO CURR-KDVALISO-ROW                        
420400     IF IN-FIL-IDPGM = 'W4183300'                                         
420500       IF IN-EKH-DAAVIDAT > ZERO                                          
420600         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
420700         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
420800       ELSE                                                               
420900         MOVE WS-TIAA            TO WS-TIAA-CR                            
421000         MOVE WS-TIMM            TO WS-TIMM-CR                            
421100       END-IF                                                             
421200     ELSE                                                                 
421300       MOVE WS-TIAA              TO WS-TIAA-CR                            
421400       MOVE WS-TIMM              TO WS-TIMM-CR                            
421500     END-IF                                                               
421600     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
421700     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
421800     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
421900     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
422000     IF CURR-KDSVAR = ' '                                                 
422100       IF IN-EKH-IDDISTR > ZERO                                           
422200         MOVE CURR-PRKURS-NEW TO WS-PRKURS-KR3                            
422300       ELSE                                                               
422400         IF WS-PRKURS = ZERO                                              
422500           MOVE 1           TO WS-PRKURS-KR3                              
422600         END-IF                                                           
422700       END-IF                                                             
422800     ELSE                                                                 
422900       MOVE 1               TO WS-PRKURS-KR3                              
423000     END-IF                                                               
423100     .                                                                    
423200     EJECT                                                                
423300                                                                          
423400* --- IMS SECTIONS ---                                                    
423500                                                                          
423600 IMS-GU-WDH521 SECTION.                                                   
423700     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
423800          DELIMITED BY SIZE INTO SSA1                                     
423900     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
424000          DELIMITED BY SIZE INTO SSA2                                     
424100     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
424200          DELIMITED BY SIZE INTO SSA3                                     
424300     MOVE '  '              TO GODK-STATUSKODER                           
424400     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
424500                                                   SSA2                   
424600                                                   SSA3                   
424700     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
424800                                                                          
424900     PERFORM IMS-STATUS-CONTROL                                           
425000     .                                                                    
425100                                                                          
425200 IMS-GNP-WDH531 SECTION.                                                  
425300     MOVE 'WDH531  '        TO SSA1                                       
425400     MOVE '  GE'            TO GODK-STATUSKODER                           
425500     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
425600     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
425700                               WS-STATUS                                  
425800     PERFORM IMS-STATUS-CONTROL                                           
425900     .                                                                    
426000     EJECT                                                                
426100                                                                          
426200 IMS-GU-WDB201 SECTION.                                                   
426300     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-KEY ')'                         
426400          DELIMITED BY SIZE INTO SSA1                                     
426500     MOVE '  GE'                 TO GODK-STATUSKODER                      
426600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
426700      MOVE GMTA-STATUS-CODE      TO STATUS-WS                             
426800     PERFORM IMS-STATUS-CONTROL                                           
426900     .                                                                    
427000     EJECT                                                                
427100                                                                          
427200 IMS-GU-WDB101 SECTION.                                                   
427300     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
427400          DELIMITED BY SIZE INTO SSA1                                     
427500     MOVE '  GE'               TO GODK-STATUSKODER                        
427600     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
427700     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
427800     PERFORM IMS-STATUS-CONTROL                                           
427900     .                                                                    
428000     EJECT                                                                
428100                                                                          
428200 IMS-GU-5122 SECTION.                                                     
428300     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
428400            DELIMITED BY SIZE INTO SSA1                                   
428500     STRING 'WDGX5122(KEY5122  =' W-WDGXKEY-5122-X ')'                    
428600            DELIMITED BY SIZE INTO SSA2                                   
428700     MOVE '  GE'           TO GODK-STATUSKODER                            
428800     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5122 SSA1 SSA2            
428900     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
429000     PERFORM IMS-STATUS-CONTROL                                           
429100     .                                                                    
429200                                                                          
429300 IMS-GU-5121 SECTION.                                                     
429400     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
429500            DELIMITED BY SIZE INTO SSA1                                   
429600     MOVE '    '           TO GODK-STATUSKODER                            
429700     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5121 SSA1                 
429800     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
429900     PERFORM IMS-STATUS-CONTROL                                           
430000     .                                                                    
430100                                                                          
430200 IMS-GNP-5122 SECTION.                                                    
430300     STRING 'WDGX5122(KEY5122 >=' W-WDGXKEY-5122-MIN-X                    
430400                    '&KEY5122 <=' W-WDGXKEY-5122-MAX-X ')'                
430500            DELIMITED BY SIZE INTO SSA1                                   
430600     MOVE '  GE'           TO GODK-STATUSKODER                            
430700     CALL CBLTDLI USING GNP 5121-PCB DLI-IO-WDGX5122 SSA1                 
430800     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
430900     PERFORM IMS-STATUS-CONTROL                                           
431000     .                                                                    
431100     EJECT                                                                
431200                                                                          
431300 IMS-GU-WDB601    SECTION.                                                
431400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
431500          DELIMITED BY SIZE INTO SSA1                                     
431600     MOVE '  GE' TO GODK-STATUSKODER                                      
431700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
431800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
431900     PERFORM IMS-STATUS-CONTROL                                           
432000     IF SEGMENT-SAKNAS                                                    
432100        MOVE SPACE TO DCS-KDDC                                            
432200     END-IF                                                               
432300     .                                                                    
432400     EJECT                                                                
432500                                                                          
432600 IMS-GNP-WDB617 SECTION.                                                  
432700     MOVE 'WDB617   ' TO SSA1                                             
432800     MOVE '  GE'        TO GODK-STATUSKODER                               
432900     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
433000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
433100     PERFORM IMS-STATUS-CONTROL                                           
433200     .                                                                    
433300     SKIP3                                                                
433400                                                                          
433500 IMS-GU-WDL601   SECTION.                                                 
433600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
433700          DELIMITED BY SIZE INTO SSA1                                     
433800     MOVE '  GE' TO GODK-STATUSKODER                                      
433900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
434000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
434100     PERFORM IMS-STATUS-CONTROL                                           
434200     .                                                                    
434300     SKIP3                                                                
434400                                                                          
434500 IMS-GNP-WDL611   SECTION.                                                
434600     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
434700          DELIMITED BY SIZE INTO SSA1                                     
434800     MOVE '  GE' TO GODK-STATUSKODER                                      
434900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
435000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
435100     PERFORM IMS-STATUS-CONTROL                                           
435200     .                                                                    
435300     SKIP3                                                                
435400                                                                          
435500 IMS-GET-WDF101 SECTION.                                                  
435600     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
435700             DELIMITED BY SIZE INTO SSA1                                  
435800     MOVE '  GE'                 TO GODK-STATUSKODER                      
435900     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
436000     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
436100     PERFORM IMS-STATUS-CONTROL                                           
436200     .                                                                    
436300                                                                          
436400 IMS-GNP-WDF106 SECTION.                                                  
436500     MOVE 'WDF106   '            TO SSA1                                  
436600     MOVE '  GE'                 TO GODK-STATUSKODER                      
436700     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF106 SSA1                   
436800     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
436900     PERFORM IMS-STATUS-CONTROL                                           
437000     .                                                                    
437100                                                                          
437200 IMS-STATUS-CONTROL SECTION.                                              
437300     SET STATUS-IX TO 1                                                   
437400     SEARCH GODK-STATUS                                                   
437500       AT END                                                             
437600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
437700           DELIMITED BY SIZE INTO FELTEXT                                 
437800         DISPLAY FELTEXT                                                  
437900         CALL FELLOG                                                      
438000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
438100         CONTINUE                                                         
438200     END-SEARCH                                                           
438300     .                                                                    
