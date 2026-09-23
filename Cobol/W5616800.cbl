000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5616800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   20171102.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*       -PGM LÄSER KONTROLLERADE/KOMPLETTERADE EKONOMISKA                 
001000*        HÄNDELSETRANSAKTIONER OCH MATCHAR DESSA MOT                      
001100*        EKONOMISKA STYRPARAMETRAR FÖR ATT I SLUTÄNDEN                    
001200*        PRODUCERA POSTER TILL R3 I FORM AV                               
001300*        1 "LINE RECORD" HUVUDBOK               (PTYP 610)                
001400*        2 "LINE RECORD" KUNDRESKONTRA          (PTYP 310)                
001500*        3 "LINE RECORD" LEVERANTÖRSRESKONTRA   (PTYP 210)                
001600*                                                                         
001700*       -PGM SKAPAR/SKRIVER ÄVEN FÖLJANDE POSTER TILL R3                  
001800*        1 "HEADER RECORD" HUVUDBOK             (PTYP 600)                
001900*        2 "HEADER RECORD" KUNDRESKONTRA        (PTYP 300)                
002000*        3 "HEADER RECORD" LEVERANTÖRSRESKONTRA (PTYP 200)                
002100*                                                                         
002200*       -PGM PLOCKAR UNDAN NY MÅNADS POSTER VID MÅNADSSKIFTE              
002300*        FÖR ATT TA IN DESSA VID NÄSTA KÖRNING.                           
002400*        (NY MÅNADS POSTER = DATUMKORTS MÅNAD + 1, OM DENNA ÄR            
002500*         LIKA MED IN-POSTENS DAVERDAT'S MÅNAD,                           
002600*         SKRIVS POSTEN PÅ UTFIL FÖR AT TAS IN NÄSTA KÖRNING).            
002700*                                                                         
002800*       -PROGRAMMET LÄSER      WDH5                                       
002900*                              WLBETC (WDB1)                              
003000*                              WLGMTA (WDB2)                              
003100*                              WL5121 (WDR1)                              
003200*                         MÅNADSKURSER WDG2                               
003300*                           DCREGISTER WDB6                               
003400*                                                                         
003500*    ABENDKODER:                                                          
003600*        U0016 -  . . . .                                                 
003700*        U1000 -  . . . .                                                 
003800*                                                                         
003900                                                                          
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 INPUT-OUTPUT SECTION.                                                    
004300                                                                          
004400 FILE-CONTROL.                                                            
004500*          --- KONTR./KOMPL. HÄNDELSETRANSAKTIONER                        
004600     SELECT W56166                     ASSIGN TO W56168D1.                
004700                                                                          
004800*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
004900     SELECT W56171A                    ASSIGN TO W56168D2.                
005000                                                                          
005100*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005200     SELECT W56172A                    ASSIGN TO W56168D3.                
005300                                                                          
005400*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005500     SELECT W56173A                    ASSIGN TO W56168D4.                
005600                                                                          
005700*          --- LOGG TILL ON-DEMAND                                        
005800     SELECT W56175                     ASSIGN TO W56168D5.                
005900                                                                          
006000*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006100     SELECT W56170                     ASSIGN TO W56168D6.                
006200                                                                          
006300*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006400     SELECT W5616N                     ASSIGN TO W56168D7.                
006500                                                                          
006600*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006700     SELECT W51310                     ASSIGN TO W56168D8.                
006800     EJECT                                                                
006900                                                                          
007000 DATA DIVISION.                                                           
007100                                                                          
007200 FILE SECTION.                                                            
007300 FD  W56166                                                               
007400     RECORDING       F                                                    
007500     BLOCK CONTAINS  0.                                                   
007600 01  SAP-POST.                                                            
007700*    03  -COPY WDR801        -L.                                          
007800     03 FILLER                   PIC X(6).                                
007900                                                                          
008000 FD  W56171A                                                              
008100     RECORDING       V                                                    
008200     BLOCK CONTAINS  0.                                                   
008300*01  71INIT-POST -COPY R3INIT20               -L.                         
008400*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008500*01  71LINE-POST -COPY R3LINE20               -L.                         
008600                                                                          
008700 FD  W56172A                                                              
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000*01  72LINE-POST -COPY R3LINE20               -L.                         
009100                                                                          
009200 FD  W56173A                                                              
009300     RECORDING       V                                                    
009400     BLOCK CONTAINS  0.                                                   
009500*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009600*01  73LINE-POST -COPY R3LINE20               -L.                         
009700                                                                          
009800 FD  W56175                                                               
009900     RECORDING       F                                                    
010000     BLOCK CONTAINS  0.                                                   
010100*01  LOGG-POST   -COPY W56173                 -L.                         
010200                                                                          
010300 FD  W56170                                                               
010400     RECORDING       F                                                    
010500     BLOCK CONTAINS  0.                                                   
010600*01  AVST-POST   -COPY W56170                 -L.                         
010700                                                                          
010800 FD  W5616N                                                               
010900     RECORDING       F                                                    
011000     BLOCK CONTAINS  0.                                                   
011100                                                                          
011200 01  SAPUT-POST.                                                          
011300*    03  -COPY WDR801        -L.                                          
011400     03 FILLER                   PIC X(6).                                
011500                                                                          
011600 FD  W51310                                                               
011700     RECORDING       F                                                    
011800     BLOCK CONTAINS  0.                                                   
011900*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012000                                                                          
012100     EJECT                                                                
012200 WORKING-STORAGE SECTION.                                                 
012300*    -- CHECKED BY WY2000                                                 
012400 77  IDPGM                        PIC X(8)    VALUE 'W5616800'.           
012500 77  JA                           PIC X       VALUE 'J'.                  
012600 77  NEJ                          PIC X       VALUE 'N'.                  
012700 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
012800 77  W56166-EOF-SW                PIC X       VALUE 'N'.                  
012900     88  END-OF-W56166                        VALUE 'J'.                  
013000 77  WS-HEADER-SW                 PIC X       VALUE 'N'.                  
013100 77  WS-LINE-SW                   PIC X       VALUE 'N'.                  
013200 77  WS-STATUS                    PIC XX      VALUE '  '.                 
013300 77  WS-LINE-AMOUNT               PIC S9(13)V99 COMP-3.                   
013400 77  WS-LINE-AMOUNT-131-1         PIC S9(13)V99 COMP-3.                   
013500 77  WS-LINE-AMOUNT-131-2         PIC S9(13)V99 COMP-3.                   
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
015200 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
015300 77  WS-IND                       PIC 9(2)    VALUE ZERO.                 
015400 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
015500 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
015600 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
015700 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
015800 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
015900 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
016000                                                                          
016100 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
016200       88  WDB6-A-FINNS                       VALUE 'J'.                  
016300       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
016400                                                                          
016500 77    PRODKOD-TEST              PIC X        VALUE 'N'.                  
016600       88 PRODKOD-SAKNAS                      VALUE 'N'.                  
016700       88 PRODKOD-FINNS                       VALUE 'J'.                  
016800                                                                          
016900*01  -COPY WWDCKONS                                                       
017000     EJECT                                                                
017100                                                                          
017200 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
017300*01  -COPY WWIDFTG                                                        
017400     EJECT                                                                
017500                                                                          
017600 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
017700 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
017800*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
017900     EJECT                                                                
018000                                                                          
018100*****   REMARKUP-FAKTOR-TABELL                                            
018200*01  -COPY WWMARKUP                                                       
018300     EJECT                                                                
018400                                                                          
018500 01  FELTEXT                      PIC X(80).                              
018600 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
018700                                                                          
018800 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
018900 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
019000     EJECT                                                                
019100 01  WS-IDDISTR-IDKUNDNR.                                                 
019200     03  FILLER                   PIC X(2)    VALUE SPACE.                
019300     03  WS-IDDISTR               PIC 9(4).                               
019400     03  WS-IDKUNDNR              PIC 9(6).                               
019500                                                                          
019600 01  WS-KDBETVIL                  PIC X(4).                               
019700 01  WS-KDVALISO-WDB1             PIC X(3).                               
019800 01  WS-KDVALISO                  PIC X(3).                               
019900 01  WS-KDVALISO-US               PIC X(3) VALUE 'USD'.                   
020000 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
020100 01  WS-PRKURS-US                 PIC S9(6)V9(5) COMP-3.                  
020200 01  WS-PRKURS-US2                PIC S9(6)V9(5) COMP-3.                  
020300 01  WS-PRKURS-US3                PIC S9(6)V9(5) COMP-3.                  
020400 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
020500 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
020600                                                                          
020700 01  WS-DATE-YYMMDD               PIC 9(06).                              
020800 01  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                              
020900     03  WS-DATE-YYMM             PIC 9(04).                              
021000     03  WS-DATE-DD               PIC 9(02).                              
021100                                                                          
021200 01  W-PRKURS                     PIC S9(5)V9(5) VALUE +0 COMP-3.         
021300 01  W-REVALUTA                   PIC S9(5)      VALUE +0 COMP-3.         
021400                                                                          
021500 01  WS-ALLOCATE.                                                         
021600     03  WS-ALLOCATE-DC           PIC X(3).                               
021700     03  WS-ALLOCATE-DISTR        PIC X(5).                               
021800     03  WS-ALLOCATE-IDVERGL      PIC X(10).                              
021900                                                                          
022000 01  WS-TEXT.                                                             
022100     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
022200     03  WS-TEXT-KDEKHHT          PIC X(3).                               
022300     03  WS-TEXT-KDEKSHT          PIC X(3).                               
022400     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
022500     03  FILLER                   PIC X(7)    VALUE SPACE.                
022600                                                                          
022700 01  WS-LINE-TEXT.                                                        
022800     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
022900     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
023000     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
023100     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
023200     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
023300     03  FILLER                   PIC X(22)   VALUE SPACE.                
023400                                                                          
023500 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
023600 01  WS-PRCTR.                                                            
023700     03  WS-PRCTR-PRODSL          PIC X(2).                               
023800     03  FILLER                   PIC X(1).                               
023900     03  FILLER                   PIC X(7).                               
024000                                                                          
024100 01  WS-R3-ACCOUNT.                                                       
024200     03  WS-R3-ACCOUNT-ALFA.                                              
024300         05 FILLER                PIC X(3).                               
024400         05 WS-R3-ACCOUNT-7       PIC X(7).                               
024500     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
024600         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
024700                                                                          
024800 01  WS-ACCOUNT.                                                          
024900     03  FILLER                   PIC X(5).                               
025000     03  WS-ACCOUNT-3             PIC X(2).                               
025100     03  FILLER                   PIC X(3).                               
025200                                                                          
025300 01  SPAR-AREA.                                                           
025400     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
025500     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
025600     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
025700     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
025800                                                                          
025900 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
026000 01  FILLER REDEFINES DAGENS-DATUM.                                       
026100     03  DAGENS-DATUM-AAR         PIC 9(2).                               
026200     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
026300     03  DAGENS-DATUM-DAG         PIC 9(2).                               
026400                                                                          
026500 01  WS-NEW-MONTH                 PIC 9(2).                               
026600                                                                          
026700 01  WS-DAREGDAT.                                                         
026800     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
026900     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
027000                                                                          
027100 01  WS-TIREGDAT-TOT.                                                     
027200     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
027300     03  WS-TIREGDAT              PIC 9(6).                               
027400                                                                          
027500 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
027600 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
027700     EJECT                                                                
027800                                                                          
027900 01  DYNAMISKA-SUBPROGRAM.                                                
028000     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
028100     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
028200     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
028300     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
028400     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
028500     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
028600                                                                          
028700*    --- PARAMETRAR TILL ABEND                                            
028800 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
028900 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
029000 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
029100     EJECT                                                                
029200                                                                          
029300*    --- PARAMETRAR TILL DATKORT                                          
029400 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W56168'.             
029500                                                                          
029600 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
029700*01  -COPY WDATKORT                                                       
029800     EJECT                                                                
029900                                                                          
030000*    --- PARAMETRAR TILL POSTSUM                                          
030100*01  -COPY W0005   -PRE  POSTSUM-                                         
030200     EJECT                                                                
030300                                                                          
030400 01  FILLER                       PIC X(16) VALUE 'W510CURR-AREA'.        
030500*01  -COPY W510CURR                                                       
030600     EJECT                                                                
030700                                                                          
030800 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
030900*01  AREA -COPY WDR801           -PRE IN-                                 
031000*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
031100         05   IN-EKH-IDSYSMOT     PIC X(6).                               
031200                                                                          
031300     EJECT                                                                
031400 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
031500                                                                          
031600*01  -COPY R3LINE20              -PRE R3-                                 
031700*01  -COPY R3HEAD20              -PRE R3-                                 
031800*01  -COPY R3INIT20              -PRE R3-                                 
031900*01  -COPY W56173                -PRE LOGG-                               
032000*01  -COPY W56170                -PRE AVST-                               
032100*01  -COPY W517RW1               -PRE RW1-                                
032200*01  -COPY W517RW2               -PRE RW2-                                
032300*01  -COPY W51310                -PRE INV-                                
032400     EJECT                                                                
032500                                                                          
032600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032700 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
032800                                                                          
032900 01  NYCKLAR-TILL-DLI.                                                    
033000     03  W-WDH501KY-X.                                                    
033100         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
033200         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
033300     03  W-KDEKSHT-X.                                                     
033400         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
033500     03  W-KDEKNIVA-X.                                                    
033600         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
033700     03  W-WDH531KY-X.                                                    
033800         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
033900         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
034000     03  W-IDRADNR-X.                                                     
034100         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
034200                                                                          
034300     03  W-IDGMT-KEY.                                                     
034400         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
034500         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
034600                                                                          
034700     03  W-WDB101KY-X.                                                    
034800         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
034900         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
035000                                                                          
035100     03  W-WDGXKEY-5121-X.                                                
035200         05  FILLER               PIC X(4)    VALUE '5121'.               
035300         05  FILLER               PIC X(2)    VALUE '53'.                 
035400         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
035500     03  W-WDGXKEY-5122-X.                                                
035600         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
035700         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
035800     03  W-WDGXKEY-5122-MIN-X.                                            
035900         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
036000         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
036100     03  W-WDGXKEY-5122-MAX-X.                                            
036200         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
036300         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
036400                                                                          
036500     03  W-IDDC-B6-X.                                                     
036600         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
036700                                                                          
036800     03  W-IDARTNR-X.                                                     
036900         05 W-IDARTNR             PIC S9(9) COMP-3.                       
037000                                                                          
037100     03  W-IDFAKT-X.                                                      
037200         05 W-IDFAKT              PIC S9(7) COMP-3.                       
037300     03  W-WDGX9305-X.                                                    
037400         05  W-IDHTYP             PIC X(4)    VALUE '9305'.               
037500         05  W-KDVALISO-HUV       PIC X(3)    VALUE SPACE.                
037600         05  W-KDVALTYP           PIC X(1)    VALUE 'M'.                  
037700         05  FILLER               PIC X(22)   VALUE LOW-VALUE.            
037800     03  W-KDVALISO-X.                                                    
037900         05  W-KDVALISO-ROW       PIC X(3)    VALUE SPACE.                
038000     03  W-TISTADA9-X.                                                    
038100         05  W-TISTADAT-9KOMPL    PIC S9(7)   VALUE ZERO COMP-3.          
038200     EJECT                                                                
038300                                                                          
038400*    --- STATUS-KOD FRÅN IMS                                              
038500 01  STATUS-WS                    PIC XX.                                 
038600     88  SEGMENT-FINNS                        VALUE '  '.                 
038700     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
038800                                                                          
038900 01  GODK-STATUSKODER.                                                    
039000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039100                                                                          
039200 01  SSA1                         PIC X(128).                             
039300 01  SSA2                         PIC X(64).                              
039400 01  SSA3                         PIC X(64).                              
039500     EJECT                                                                
039600                                                                          
039700*    --- IMS FUNKTIONSKODER                                               
039800*01  -COPY W0003                                                          
039900     EJECT                                                                
040000                                                                          
040100*    ---  DLI INPUT-OUTPUT AREA                                           
040200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
040300 01  DLI-IO-WDH501.                                                       
040400*    03  -COPY WDH501                                                     
040500     EJECT                                                                
040600                                                                          
040700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
040800 01  DLI-IO-WDH511.                                                       
040900*    03  -COPY WDH511                                                     
041000     EJECT                                                                
041100                                                                          
041200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
041300 01  DLI-IO-WDH521.                                                       
041400*    03  -COPY WDH521                                                     
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
041800 01  DLI-IO-WDH531.                                                       
041900*    03  -COPY WDH531                                                     
042000     EJECT                                                                
042100                                                                          
042200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
042300 01  DLI-IO-WLBETC01.                                                     
042400*    03  -COPY WDB101                                                     
042500     EJECT                                                                
042600                                                                          
042700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
042800 01  DLI-IO-WLGMTA01.                                                     
042900*    03  -COPY WDB201                                                     
043000     EJECT                                                                
043100                                                                          
043200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
043300 01  DLI-IO-WDGX5121.                                                     
043400*    03  -COPY WDGX5121                                                   
043500     EJECT                                                                
043600                                                                          
043700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
043800 01  DLI-IO-WDGX5122.                                                     
043900*    03  -COPY WDGX5122                                                   
044000     EJECT                                                                
044100                                                                          
044200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
044300 01   DLI-IO-AREA-B601.                                                   
044400*     03  -COPY WDB601                                                    
044500     EJECT                                                                
044600 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
044700 01   DLI-IO-AREA-L601.                                                   
044800*     03  -COPY WDL601                                                    
044900     EJECT                                                                
045000 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
045100 01   DLI-IO-AREA-L611.                                                   
045200*     03  -COPY WDL611                                                    
045300     EJECT                                                                
045400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
045500 01  DLI-IO-WDGX9306.                                                     
045600*    03  -COPY WDGX9306                                                   
045700                                                                          
045800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
045900 01  DLI-IO-WDGX9308.                                                     
046000*    03  -COPY WDGX9308                                                   
046100                                                                          
046200                                                                          
046300 LINKAGE SECTION.                                                         
046400*01  -COPY W0008  -PRE WDH5-                                              
046500     05  FILLER                  PIC X.                                   
046600                                                                          
046700*01  -COPY W0008  -PRE GMTA-                                              
046800     05  FILLER                  PIC X.                                   
046900                                                                          
047000*01  -COPY W0008  -PRE BETC-                                              
047100     05  FILLER                  PIC X.                                   
047200                                                                          
047300*01  -COPY W0008  -PRE 5121-                                              
047400     05  FILLER                  PIC X.                                   
047500                                                                          
047600*01  -COPY W0008  -PRE WDB6-                                              
047700     05  FILLER                  PIC X.                                   
047800                                                                          
047900*01  -COPY W0008  -PRE WDL6-                                              
048000     05  FILLER                  PIC X.                                   
048100*01  -COPY W0008  -PRE 9305-                                              
048200     05  FILLER                  PIC X.                                   
048300     EJECT                                                                
048400                                                                          
048500     EJECT                                                                
048600                                                                          
048700 PROCEDURE DIVISION  USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
048800                           WDB6-PCB WDL6-PCB 9305-PCB.                    
048900 MAIN SECTION.                                                            
049000     ENTRY 'DLITCBL' USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
049100                           WDB6-PCB WDL6-PCB 9305-PCB.                    
049200                                                                          
049300     PERFORM A-INIT                                                       
049400                                                                          
049500     PERFORM S01-READ-W56166                                              
049600     PERFORM UNTIL END-OF-W56166                                          
049700*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
049800       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
049900       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
050000       AND WS-NEW-MONTH > 01                                              
050100         PERFORM S60-WRITE-W5616N                                         
050200       ELSE                                                               
050300         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
050400         PERFORM S30-READ-DATABASE-B2-B1                                  
050500         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
050600           PERFORM C-EXECUTE                                              
050700         END-IF                                                           
050800       END-IF                                                             
050900       PERFORM S01-READ-W56166                                            
051000     END-PERFORM                                                          
051100                                                                          
051200     PERFORM Z-FINI                                                       
051300                                                                          
051400     MOVE ZERO TO RETURN-CODE                                             
051500     GOBACK                                                               
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900 A-INIT SECTION.                                                          
052000     OPEN INPUT  W56166                                                   
052100                                                                          
052200     OPEN OUTPUT W56170                                                   
052300                 W56171A                                                  
052400                 W56172A                                                  
052500                 W56173A                                                  
052600                 W56175                                                   
052700                 W5616N                                                   
052800                 W51310                                                   
052900                                                                          
053000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
053100     MOVE 20               TO RW1-DAVVREG(1:2)                            
053200     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
053300                              RW1-DAVVREG(3:2)                            
053400                              W-DATE-AAMM(1:2)                            
053500                              WS-TIAA                                     
053600     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
053700                              W-DATE-AAMM(3:2)                            
053800                              WS-TIMM                                     
053900                              WS-NEW-MONTH                                
054000     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
054100     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
054200     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
054300                                                                          
054400*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
054500*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
054600     IF WS-NEW-MONTH = 12                                                 
054700       MOVE 1              TO WS-NEW-MONTH                                
054800     ELSE                                                                 
054900       ADD 1               TO WS-NEW-MONTH                                
055000*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
055100***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
055200***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
055300***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
055400***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
055500       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
055600       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
055700         ADD 1             TO WS-NEW-MONTH                                
055800         ADD 1             TO WS-TIMM                                     
055900*        ADD 1             TO W-TIMM                                      
056000         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
056100       END-IF                                                             
056200     END-IF                                                               
056300                                                                          
056400     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
056500                                                                          
056600     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
056700                                                                          
056800     ACCEPT DAGENS-KLOCKA FROM TIME                                       
056900     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
057000                                                                          
057100**** TA FRAM MÅNADENS FÖRSTA DAG                                          
057200     MOVE WS-DAREGDAT-AAMMDD(1:4)     TO WS-DATE-YYMM                     
057300     MOVE 01                          TO WS-DATE-DD                       
057400                                                                          
057500     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
057600     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
057700     MOVE 'M'                   TO CURR-KDVALTYP                          
057800                                                                          
057900**** DETTA ÄR KURSERNA FÖR ATT TA EMOT SAKER FRÅN VCCS                    
058000     MOVE WS-KDVALISO-US        TO CURR-KDVALISO-ROW                      
058100     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
058200     IF CURR-KDSVAR = ' '                                                 
058300       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-US                           
058400     ELSE                                                                 
058500       MOVE 1                   TO WS-PRKURS-US                           
058600     END-IF                                                               
058700     COMPUTE WS-PRKURS-US2 ROUNDED = 1 / WS-PRKURS-US                     
058800     MOVE WS-PRKURS-US          TO WS-PRKURS-US3                          
058900     .                                                                    
059000     EJECT                                                                
059100                                                                          
059200 C-EXECUTE SECTION.                                                       
059300     MOVE WC-IDFTG-US           TO W-IDFTG                                
059400     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
059500     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
059600     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
059700     PERFORM IMS-GU-WDH521                                                
059800     PERFORM IMS-GNP-WDH531                                               
059900                                                                          
060000     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
060100     IF IN-EKH-IDDISTR > ZERO                                             
060200       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
060300     ELSE                                                                 
060400       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
060500     END-IF                                                               
060600                                                                          
060700     IF WS-KDVALISO = 'USD'                                               
060800       MOVE IN-EKH-PRKURS       TO WS-PRKURS                              
060900       MOVE IN-EKH-PRKURS       TO W-PRKURS                               
061000     ELSE                                                                 
061100**** HÄMTA DC HUVUDVALUTA                                                 
061200       MOVE 'USD'               TO W-KDVALISO-HUV                         
061300       MOVE WS-KDVALISO         TO W-KDVALISO-ROW                         
061400       MOVE W-KDVALISO-HUV      TO CURR-KDVALISO-HUV                      
061500       MOVE 'M'                 TO CURR-KDVALTYP                          
061600                                                                          
061700****   DETTA ÄR KURSERNA FÖR ATT TA EMOT SAKER FRÅN VCCS                  
061800       MOVE W-KDVALISO-ROW        TO CURR-KDVALISO-ROW                    
061900       IF (IN-EKH-KDEKHHT = '102'                                         
062000       AND IN-EKH-KDEKSHT = '107')                                        
062100         IF IN-EKH-DAAVIDAT > ZERO                                        
062200           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
062300           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
062400         ELSE                                                             
062500           MOVE WS-TIAA              TO WS-TIAA-CR                        
062600           MOVE WS-TIMM              TO WS-TIMM-CR                        
062700         END-IF                                                           
062800       ELSE                                                               
062900         MOVE WS-TIAA              TO WS-TIAA-CR                          
063000         MOVE WS-TIMM              TO WS-TIMM-CR                          
063100       END-IF                                                             
063200       MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                         
063300       MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                         
063400       MOVE W-DATE-AAMM       TO CURR-TIAAMM                              
063500       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
063600       IF CURR-KDSVAR = ' '                                               
063700         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
063800         MOVE 1                   TO W-REVALUTA                           
063900       ELSE                                                               
064000         MOVE 1                   TO W-PRKURS                             
064100         MOVE 1                   TO W-REVALUTA                           
064200       END-IF                                                             
064300     END-IF                                                               
064400                                                                          
064500     IF  (IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                                
064600     AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                                
064700       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
064800     ELSE                                                                 
064900       MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                             
065000       MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                             
065100       IF WS-LOP = 9                                                      
065200         MOVE ZERO  TO WS-LOP                                             
065300       ELSE                                                               
065400         ADD +1     TO WS-LOP                                             
065500       END-IF                                                             
065600       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
065700     END-IF                                                               
065800* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
065900     IF SYST-IDPTYP = '210'                                               
066000       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
066100     ELSE                                                                 
066200       IF SYST-IDPTYP = '310'                                             
066300         PERFORM CC-CREATE-WRITE-HEADER-AR                                
066400       ELSE                                                               
066500* TEST OM BRYTNING PÅ VERIFIKATION                                        
066600         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
066700         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
066800         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
066900         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
067000           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
067100           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
067200           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
067300           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
067400           IF (IN-EKH-KDEKHHT = '102'                                     
067500           AND IN-EKH-KDEKSHT = '121')                                    
067600           OR (IN-EKH-KDEKHHT = '102'                                     
067700           AND IN-EKH-KDEKSHT = '122')                                    
067800           OR (IN-EKH-KDEKHHT = '102'                                     
067900           AND IN-EKH-KDEKSHT = '131')                                    
068000           OR (IN-EKH-KDEKHHT = '102'                                     
068100           AND IN-EKH-KDEKSHT = '132')                                    
068200             PERFORM S80-GET-CURRENCY-RATE                                
068300           END-IF                                                         
068400           IF (IN-EKH-KDEKHHT = '303'                                     
068500           AND IN-EKH-KDEKSHT = '371')                                    
068600             PERFORM S81-GET-CURRENCY-RATE                                
068700           END-IF                                                         
068800*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
068900*   HEADER-POST TILL HUVUDBOKEN                                           
069000           IF (IN-EKH-KDEKHHT = '102'                                     
069100           AND IN-EKH-KDEKSHT = '107')                                    
069200           OR (IN-EKH-KDEKHHT = '102'                                     
069300           AND IN-EKH-KDEKSHT = '120')                                    
069400           OR (IN-EKH-KDEKHHT = '102'                                     
069500           AND IN-EKH-KDEKSHT = '124')                                    
069600           OR (IN-EKH-KDEKHHT = '102'                                     
069700           AND IN-EKH-KDEKSHT = '130')                                    
069800           OR (IN-EKH-KDEKHHT = '102'                                     
069900           AND IN-EKH-KDEKSHT = '134')                                    
070000           OR (IN-EKH-KDEKHHT = '204'                                     
070100           AND IN-EKH-KDEKSHT = '301')                                    
070200           OR (IN-EKH-KDEKHHT = '303'                                     
070300           AND IN-EKH-KDEKSHT = '301')                                    
070400           OR (IN-EKH-KDEKHHT = '303'                                     
070500           AND IN-EKH-KDEKSHT = '307')                                    
070600           OR (IN-EKH-KDEKHHT = '303'                                     
070700           AND IN-EKH-KDEKSHT = '371')                                    
070800           OR (IN-EKH-KDEKHHT = '303'                                     
070900           AND IN-EKH-KDEKSHT = '377')                                    
071000           OR (IN-EKH-KDEKHHT = '303'                                     
071100           AND IN-EKH-KDEKSHT = '3XX')                                    
071200             CONTINUE                                                     
071300           ELSE                                                           
071400             PERFORM CA-CREATE-WRITE-HEADER-GL                            
071500           END-IF                                                         
071600         END-IF                                                           
071700       END-IF                                                             
071800     END-IF                                                               
071900                                                                          
072000**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
072100     MOVE WS-STATUS TO STATUS-WS                                          
072200     PERFORM UNTIL SEGMENT-SAKNAS                                         
072300       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
072400                                                                          
072500* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
072600       IF SYST-IDPTYP = '610'                                             
072700         PERFORM CD-BUILD-COMMON-610-PART                                 
072800         PERFORM CE-SCHEDULE-LINE-GL                                      
072900       ELSE                                                               
073000         IF SYST-IDPTYP = '210'                                           
073100           PERFORM CF-BUILD-COMMON-210-PART                               
073200           PERFORM CG-SCHEDULE-LINE-AP                                    
073300         ELSE                                                             
073400           IF SYST-IDPTYP = '310'                                         
073500             PERFORM CH-BUILD-COMMON-310-PART                             
073600             PERFORM CI-SCHEDULE-LINE-AR                                  
073700           END-IF                                                         
073800         END-IF                                                           
073900       END-IF                                                             
074000       PERFORM IMS-GNP-WDH531                                             
074100     END-PERFORM                                                          
074200     .                                                                    
074300     EJECT                                                                
074400                                                                          
074500 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
074600     MOVE SPACE                   TO R3-HEAD-R3                           
074700     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
074800     MOVE 'US01'                  TO R3-HEAD-COMPANY-CODE                 
074900     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
075000     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
075100     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
075200     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
075300     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
075400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
075500     ELSE                                                                 
075600       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
075700     END-IF                                                               
075800     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
075900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
076000     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
076100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
076200     IF (IN-EKH-KDEKHHT = '103'                                           
076300     AND IN-EKH-KDEKSHT = '102')                                          
076400       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
076500       IF IN-EKH-KDVALISO = 'USD'                                         
076600         MOVE WS-PRKURS           TO R3-HEAD-EXCHANGE-RATE                
076700       ELSE                                                               
076800**** HÄMTA DC HUVUDVALUTA                                                 
076900         MOVE 'USD'               TO W-KDVALISO-HUV                       
077000         MOVE WS-KDVALISO         TO W-KDVALISO-ROW                       
077100         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
077200         MOVE W-KDVALISO-HUV        TO CURR-KDVALISO-HUV                  
077300         MOVE 'M'                   TO CURR-KDVALTYP                      
077400                                                                          
077500****     DETTA ÄR KURSERNA FÖR ATT TA EMOT SAKER FRÅN VCCS                
077600         MOVE W-KDVALISO-ROW        TO CURR-KDVALISO-ROW                  
077700         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
077800         IF CURR-KDSVAR = ' '                                             
077900           MOVE CURR-PRKURS-NEW     TO W-PRKURS                           
078000           MOVE 1                   TO W-REVALUTA                         
078100         ELSE                                                             
078200           MOVE 1                   TO W-PRKURS                           
078300           MOVE 1                   TO W-REVALUTA                         
078400         END-IF                                                           
078500         COMPUTE R3-HEAD-EXCHANGE-RATE ROUNDED = W-PRKURS *               
078600                                                 W-REVALUTA               
078700         IF W-REVALUTA = +1                                               
078800           MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT              
078900         END-IF                                                           
079000         IF W-REVALUTA = +10                                              
079100           MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT              
079200         END-IF                                                           
079300         IF W-REVALUTA = +100                                             
079400           MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT              
079500         END-IF                                                           
079600       END-IF                                                             
079700     ELSE                                                                 
079800       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
079900       MOVE WS-PRKURS-US2         TO R3-HEAD-EXCHANGE-RATE                
080000     END-IF                                                               
080100**** WE SHALL ONLY SHOW USD IN THE EVENT                                  
080200     IF (IN-EKH-KDEKHHT = '102'                                           
080300     AND IN-EKH-KDEKSHT = '131')                                          
080400     OR (IN-EKH-KDEKHHT = '102'                                           
080500     AND IN-EKH-KDEKSHT = '132')                                          
080600       MOVE 'USD'                 TO R3-HEAD-CURRENCY                     
080700       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
080800     END-IF                                                               
080900     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
081000     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
081100     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
081200     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
081300     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
081400     MOVE JA                      TO WS-HEADER-SW                         
081500     MOVE NEJ                     TO WS-LINE-SW                           
081600                                                                          
081700* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
081800     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
081900     OR (IN-EKH-KDEKHHT = '303'                                           
082000     AND IN-EKH-KDEKSHT = '391')                                          
082100     OR (IN-EKH-KDEKHHT = '102'                                           
082200     AND IN-EKH-KDEKSHT = '121')                                          
082300     OR (IN-EKH-KDEKHHT = '102'                                           
082400     AND IN-EKH-KDEKSHT = '131')                                          
082500     OR (IN-EKH-KDEKHHT = '103'                                           
082600     AND IN-EKH-KDEKSHT = '102')                                          
082700       PERFORM S004-WRITE-W56173A-HEAD                                    
082800     ELSE                                                                 
082900       PERFORM S002-WRITE-W56171A-HEAD                                    
083000     END-IF                                                               
083100     .                                                                    
083200     EJECT                                                                
083300                                                                          
083400 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
083500     MOVE SPACE                   TO R3-HEAD-R3                           
083600     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
083700     MOVE 'US01'                  TO R3-HEAD-COMPANY-CODE                 
083800                                     R3-HEAD-CONTROL-AREA                 
083900     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
084000     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
084100     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
084200     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
084300     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
084400     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
084500       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
084600     ELSE                                                                 
084700       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
084800     END-IF                                                               
084900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
085000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
085100     IF (IN-EKH-KDEKHHT = '102'                                           
085200     AND IN-EKH-KDEKSHT = '107')                                          
085300       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
085400       IF IN-EKH-KDVALISO = 'USD'                                         
085500         MOVE WS-PRKURS           TO R3-HEAD-EXCHANGE-RATE                
085600       ELSE                                                               
085700**** HÄMTA DC HUVUDVALUTA                                                 
085800         MOVE 'USD'               TO W-KDVALISO-HUV                       
085900         MOVE WS-KDVALISO         TO W-KDVALISO-ROW                       
086000         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
086100         MOVE W-KDVALISO-HUV        TO CURR-KDVALISO-HUV                  
086200         MOVE 'M'                   TO CURR-KDVALTYP                      
086300                                                                          
086400****     DETTA ÄR KURSERNA FÖR ATT TA EMOT SAKER FRÅN VCCS                
086500         MOVE W-KDVALISO-ROW        TO CURR-KDVALISO-ROW                  
086600         IF IN-FIL-IDPGM = 'W4183300'                                     
086700           IF IN-EKH-DAAVIDAT > ZERO                                      
086800             MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                      
086900             MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                      
087000           ELSE                                                           
087100             MOVE WS-TIAA            TO WS-TIAA-CR                        
087200             MOVE WS-TIMM            TO WS-TIMM-CR                        
087300           END-IF                                                         
087400         ELSE                                                             
087500           MOVE WS-TIAA              TO WS-TIAA-CR                        
087600           MOVE WS-TIMM              TO WS-TIMM-CR                        
087700         END-IF                                                           
087800         MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                       
087900         MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                       
088000         MOVE W-DATE-AAMM       TO CURR-TIAAMM                            
088100         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
088200         IF CURR-KDSVAR = ' '                                             
088300           MOVE CURR-PRKURS-NEW     TO W-PRKURS                           
088400           MOVE 1                   TO W-REVALUTA                         
088500         ELSE                                                             
088600           MOVE 1                   TO W-PRKURS                           
088700           MOVE 1                   TO W-REVALUTA                         
088800         END-IF                                                           
088900         COMPUTE R3-HEAD-EXCHANGE-RATE ROUNDED = W-PRKURS *               
089000                                                 W-REVALUTA               
089100         IF W-REVALUTA = +1                                               
089200           MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT              
089300         END-IF                                                           
089400         IF W-REVALUTA = +10                                              
089500           MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT              
089600         END-IF                                                           
089700         IF W-REVALUTA = +100                                             
089800           MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT              
089900         END-IF                                                           
090000       END-IF                                                             
090100     ELSE                                                                 
090200       IF (IN-EKH-KDEKHHT = '102'                                         
090300       AND IN-EKH-KDEKSHT = '130')                                        
090400       OR (IN-EKH-KDEKHHT = '102'                                         
090500       AND IN-EKH-KDEKSHT = '134')                                        
090600         MOVE 'SEK'               TO R3-HEAD-CURRENCY                     
090700         MOVE WS-PRKURS-US2       TO R3-HEAD-EXCHANGE-RATE                
090800       ELSE                                                               
090900         MOVE 'SEK'               TO R3-HEAD-CURRENCY                     
091000         MOVE WS-PRKURS-US2       TO R3-HEAD-EXCHANGE-RATE                
091100         MOVE 'USD'               TO CURR-KDVALISO-ROW                    
091200         IF IN-FIL-IDPGM = 'W4183300'                                     
091300           IF IN-EKH-DAAVIDAT > ZERO                                      
091400             MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                      
091500             MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                      
091600           ELSE                                                           
091700             MOVE WS-TIAA            TO WS-TIAA-CR                        
091800             MOVE WS-TIMM            TO WS-TIMM-CR                        
091900           END-IF                                                         
092000         ELSE                                                             
092100           MOVE WS-TIAA              TO WS-TIAA-CR                        
092200           MOVE WS-TIMM              TO WS-TIMM-CR                        
092300         END-IF                                                           
092400         MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                       
092500         MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                       
092600         MOVE W-DATE-AAMM       TO CURR-TIAAMM                            
092700         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
092800         IF CURR-KDSVAR = ' '                                             
092900           IF IN-EKH-IDDISTR > ZERO                                       
093000             MOVE CURR-PRKURS-NEW TO WS-PRKURS-US                         
093100           ELSE                                                           
093200             IF WS-PRKURS = ZERO                                          
093300               MOVE 1           TO WS-PRKURS-US                           
093400             END-IF                                                       
093500           END-IF                                                         
093600         ELSE                                                             
093700           MOVE 1               TO WS-PRKURS-US                           
093800         END-IF                                                           
093900         COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-US *                   
094000                                         CURR-REVALUTA-TO                 
094100         END-COMPUTE                                                      
094200         IF CURR-REVALUTA-TO = +1                                         
094300           MOVE '1    '         TO R3-HEAD-EXCHANGE-FRFACT                
094400         END-IF                                                           
094500         IF CURR-REVALUTA-TO = +10                                        
094600           MOVE '10   '         TO R3-HEAD-EXCHANGE-FRFACT                
094700         END-IF                                                           
094800         IF CURR-REVALUTA-TO = +100                                       
094900           MOVE '100  '         TO R3-HEAD-EXCHANGE-FRFACT                
095000         END-IF                                                           
095100       END-IF                                                             
095200     END-IF                                                               
095300**** WE SHALL ONLY SHOW USD IN THE EVENT                                  
095400     IF (IN-EKH-KDEKHHT = '102'                                           
095500     AND IN-EKH-KDEKSHT = '130')                                          
095600     OR (IN-EKH-KDEKHHT = '102'                                           
095700     AND IN-EKH-KDEKSHT = '134')                                          
095800     OR (IN-EKH-KDEKHHT = '303'                                           
095900     AND IN-EKH-KDEKSHT = '371')                                          
096000       MOVE 'USD'                 TO R3-HEAD-CURRENCY                     
096100       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
096200     END-IF                                                               
096300     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
096400     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
096500     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
096600     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
096700     MOVE JA                      TO WS-HEADER-SW                         
096800     MOVE NEJ                     TO WS-LINE-SW                           
096900                                                                          
097000* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W56173A                       
097100     PERFORM S004-WRITE-W56173A-HEAD                                      
097200     .                                                                    
097300     EJECT                                                                
097400                                                                          
097500 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
097600     MOVE SPACE                   TO R3-HEAD-R3                           
097700     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
097800     MOVE 'US01'                  TO R3-HEAD-COMPANY-CODE                 
097900                                     R3-HEAD-CONTROL-AREA                 
098000     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
098100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
098200     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
098300     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
098400     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
098500     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
098600       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
098700     ELSE                                                                 
098800       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
098900     END-IF                                                               
099000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
099100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
099200     IF (IN-EKH-KDEKHHT = '204'                                           
099300     AND IN-EKH-KDEKSHT = '301')                                          
099400       MOVE 'USD'                 TO R3-HEAD-CURRENCY                     
099500       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
099600     ELSE                                                                 
099700       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
099800       MOVE WS-PRKURS-US2         TO R3-HEAD-EXCHANGE-RATE                
099900       MOVE 'USD'                 TO CURR-KDVALISO-ROW                    
100000       IF IN-FIL-IDPGM = 'W4183300'                                       
100100         IF IN-EKH-DAAVIDAT > ZERO                                        
100200           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
100300           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
100400         ELSE                                                             
100500           MOVE WS-TIAA              TO WS-TIAA-CR                        
100600           MOVE WS-TIMM              TO WS-TIMM-CR                        
100700         END-IF                                                           
100800       ELSE                                                               
100900         MOVE WS-TIAA                TO WS-TIAA-CR                        
101000         MOVE WS-TIMM                TO WS-TIMM-CR                        
101100       END-IF                                                             
101200       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
101300       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
101400       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
101500       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
101600       IF CURR-KDSVAR = ' '                                               
101700         IF IN-EKH-IDDISTR > ZERO                                         
101800           MOVE CURR-PRKURS-NEW TO WS-PRKURS-US                           
101900         ELSE                                                             
102000           IF WS-PRKURS = ZERO                                            
102100             MOVE 1             TO WS-PRKURS-US                           
102200           END-IF                                                         
102300         END-IF                                                           
102400       ELSE                                                               
102500         MOVE 1                 TO WS-PRKURS-US                           
102600       END-IF                                                             
102700       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-US *                     
102800                                       CURR-REVALUTA-TO                   
102900       END-COMPUTE                                                        
103000       IF CURR-REVALUTA-TO = +1                                           
103100         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
103200       END-IF                                                             
103300       IF CURR-REVALUTA-TO = +10                                          
103400         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
103500       END-IF                                                             
103600       IF CURR-REVALUTA-TO = +100                                         
103700         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
103800       END-IF                                                             
103900     END-IF                                                               
104000**** WE SHALL ONLY SHOW USD IN THE EVENT                                  
104100     IF (IN-EKH-KDEKHHT = '303'                                           
104200     AND IN-EKH-KDEKSHT = '371')                                          
104300       MOVE 'USD'                 TO R3-HEAD-CURRENCY                     
104400       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
104500     END-IF                                                               
104600     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
104700     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
104800     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
104900     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
105000     MOVE JA                      TO WS-HEADER-SW                         
105100     MOVE NEJ                     TO WS-LINE-SW                           
105200                                                                          
105300* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W56173A                       
105400       PERFORM S004-WRITE-W56173A-HEAD                                    
105500     .                                                                    
105600     EJECT                                                                
105700                                                                          
105800 CD-BUILD-COMMON-610-PART SECTION.                                        
105900     MOVE SPACE               TO R3-LINE-R3                               
106000     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
106100                                 R3-LINE-DUE-DATE                         
106200                                 R3-LINE-AMOUNT                           
106300                                 R3-LINE-AMOUNT-LC                        
106400                                 R3-LINE-TAX-AMOUNT                       
106500                                 R3-LINE-TAX-AMOUNT-LC                    
106600                                 R3-LINE-NUMBER-OF-DAYS                   
106700                                 R3-LINE-QUANTITY                         
106800                                 R3-LINE-SAMNR                            
106900     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
107000     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
107100     MOVE 'US01'              TO R3-LINE-COMPANY-CODE                     
107200     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
107300     IF SYST-KDPOST = '50'                                                
107400       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
107500     ELSE                                                                 
107600       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
107700     END-IF                                                               
107800     IF SYST-IDPRCTR NOT = SPACE                                          
107900       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
108000       IF WS-PRCTR-PRODSL = '??'                                          
108100***** HÄR FLYTTAR VI LOKALT PRODUKTSLAG, USA VILL ANVÄNDA DET             
108200         MOVE IN-EKH-KDPSLLOC      TO WS-PRCTR-PRODSL-DISP                
108300         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
108400       END-IF                                                             
108500       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900                                                                          
109000 CE-SCHEDULE-LINE-GL SECTION.                                             
109100     MOVE NEJ                     TO WS-HEADER-SW                         
109200     MOVE JA                      TO WS-LINE-SW                           
109300     EVALUATE IN-EKH-KDEKHHT                                              
109400     WHEN '102'                                                           
109500          PERFORM CEB-MAIN-EVENT-102                                      
109600     WHEN '103'                                                           
109700          PERFORM CEC-MAIN-EVENT-103                                      
109800     WHEN '204'                                                           
109900          PERFORM CED-MAIN-EVENT-204                                      
110000     WHEN '303'                                                           
110100          PERFORM CEE-MAIN-EVENT-303                                      
110200     END-EVALUATE                                                         
110300     .                                                                    
110400     EJECT                                                                
110500                                                                          
110600 CEB-MAIN-EVENT-102 SECTION.                                              
110700     EVALUATE IN-EKH-KDEKSHT                                              
110800     WHEN '106'                                                           
110900          PERFORM CEBA-SUB-EVENT-102-106                                  
111000     WHEN '107'                                                           
111100          PERFORM CEBB-SUB-EVENT-102-107                                  
111200     WHEN '130'                                                           
111300          PERFORM CEBC-SUB-EVENT-102-130                                  
111400     WHEN '131'                                                           
111500          PERFORM CEBD-SUB-EVENT-102-131                                  
111600     WHEN '132'                                                           
111700          PERFORM CEBE-SUB-EVENT-102-132                                  
111800     WHEN '134'                                                           
111900          PERFORM CEBF-SUB-EVENT-102-134                                  
112000     END-EVALUATE                                                         
112100     .                                                                    
112200     EJECT                                                                
112300                                                                          
112400                                                                          
112500 CEBA-SUB-EVENT-102-106 SECTION.                                          
112600     EVALUATE IN-EKH-KDEKNIVA                                             
112700     WHEN 'DET'                                                           
112800       IF SYST-IDSEKVNR = 1                                               
112900         IF IN-EKH-KVANTAL < 0                                            
113000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
113100           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
113200           COMPUTE R3-LINE-AMOUNT-LC =                                    
113300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
113400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
113500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
113600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
113700           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
113800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
113900           PERFORM S02-WRITE-W56171A                                      
114000         END-IF                                                           
114100       END-IF                                                             
114200                                                                          
114300       IF SYST-IDSEKVNR = 2                                               
114400         IF IN-EKH-KVANTAL < 0                                            
114500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
114600           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
114700           COMPUTE R3-LINE-AMOUNT-LC =                                    
114800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
114900           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
115000           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
115100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
115200           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
115300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
115400           PERFORM S02-WRITE-W56171A                                      
115500         END-IF                                                           
115600       END-IF                                                             
115700                                                                          
115800       IF SYST-IDSEKVNR = 3                                               
115900         IF IN-EKH-KVANTAL > 0                                            
116000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
116100           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
116200           COMPUTE R3-LINE-AMOUNT-LC =                                    
116300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
116400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
116500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
116600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
116700           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
116800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
116900           PERFORM S02-WRITE-W56171A                                      
117000         END-IF                                                           
117100       END-IF                                                             
117200                                                                          
117300       IF SYST-IDSEKVNR = 4                                               
117400         IF IN-EKH-KVANTAL > 0                                            
117500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
117600           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
117700           COMPUTE R3-LINE-AMOUNT-LC =                                    
117800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
117900           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
118000           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
118100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
118200           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
118300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
118400           PERFORM S02-WRITE-W56171A                                      
118500         END-IF                                                           
118600       END-IF                                                             
118700                                                                          
118800     END-EVALUATE                                                         
118900     .                                                                    
119000     EJECT                                                                
119100                                                                          
119200 CEBB-SUB-EVENT-102-107 SECTION.                                          
119300     EVALUATE IN-EKH-KDEKNIVA                                             
119400     WHEN 'DET'                                                           
119500       IF SYST-IDSEKVNR = 1                                               
119600         IF IN-EKH-KVANTAL < 0                                            
119700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
119800           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
119900**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
120000           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
120100           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
120200           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
120300           IF IN-EKH-KDVALISO = 'USD'                                     
120400             COMPUTE R3-LINE-AMOUNT-LC =                                  
120500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
120600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
120700           ELSE                                                           
120800             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
120900                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS          
121000           END-IF                                                         
121100*          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
121200           IF IN-EKH-IDDC-SEND = '41'                                     
121300             MOVE '266'             TO WS-ALLOCATE-DC                     
121400           END-IF                                                         
121500           IF IN-EKH-IDDC-SEND = '43'                                     
121600             MOVE '264'             TO WS-ALLOCATE-DC                     
121700           END-IF                                                         
121800           IF IN-EKH-IDDC-SEND = '44'                                     
121900             MOVE '255'             TO WS-ALLOCATE-DC                     
122000           END-IF                                                         
122100           IF IN-EKH-IDDC-SEND = '45'                                     
122200             MOVE '267'             TO WS-ALLOCATE-DC                     
122300           END-IF                                                         
122400           IF IN-EKH-IDDC-SEND = '46'                                     
122500             MOVE '268'             TO WS-ALLOCATE-DC                     
122600           END-IF                                                         
122700           IF IN-EKH-IDDC-SEND = '47'                                     
122800             MOVE '265'            TO WS-ALLOCATE-DC                      
122900           END-IF                                                         
123000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
123100           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
123200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
123300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
123400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
123500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
123600           PERFORM S04-WRITE-W56173A                                      
123700         END-IF                                                           
123800       END-IF                                                             
123900                                                                          
124000       IF SYST-IDSEKVNR = 2                                               
124100         IF IN-EKH-KVANTAL > 0                                            
124200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
124300           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
124400**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
124500           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
124600           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
124700           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
124800           IF IN-EKH-KDVALISO = 'USD'                                     
124900             COMPUTE R3-LINE-AMOUNT-LC =                                  
125000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
125100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
125200           ELSE                                                           
125300             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
125400                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS          
125500           END-IF                                                         
125600           IF IN-EKH-IDDC-SEND = '41'                                     
125700             MOVE '266'             TO WS-ALLOCATE-DC                     
125800           END-IF                                                         
125900           IF IN-EKH-IDDC-SEND = '43'                                     
126000             MOVE '264'             TO WS-ALLOCATE-DC                     
126100           END-IF                                                         
126200           IF IN-EKH-IDDC-SEND = '44'                                     
126300             MOVE '255'             TO WS-ALLOCATE-DC                     
126400           END-IF                                                         
126500           IF IN-EKH-IDDC-SEND = '45'                                     
126600             MOVE '267'             TO WS-ALLOCATE-DC                     
126700           END-IF                                                         
126800           IF IN-EKH-IDDC-SEND = '46'                                     
126900             MOVE '268'             TO WS-ALLOCATE-DC                     
127000           END-IF                                                         
127100           IF IN-EKH-IDDC-SEND = '47'                                     
127200             MOVE '265'            TO WS-ALLOCATE-DC                      
127300           END-IF                                                         
127400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
127500           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
127600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
127700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
127800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
127900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
128000           PERFORM S04-WRITE-W56173A                                      
128100         END-IF                                                           
128200       END-IF                                                             
128300                                                                          
128400     WHEN 'HEMT'                                                          
128500       IF SYST-IDSEKVNR = 1                                               
128600         IF IN-EKH-SUBEL > 0                                              
128700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
128800           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
128900**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
129000           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
129100           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
129200           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
129300           IF IN-EKH-KDVALISO = 'USD'                                     
129400             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
129500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
129600           ELSE                                                           
129700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
129800                                                 W-PRKURS                 
129900           END-IF                                                         
130000           IF IN-EKH-IDDC-SEND = '41'                                     
130100             MOVE '266'             TO WS-ALLOCATE-DC                     
130200           END-IF                                                         
130300           IF IN-EKH-IDDC-SEND = '43'                                     
130400             MOVE '264'             TO WS-ALLOCATE-DC                     
130500           END-IF                                                         
130600           IF IN-EKH-IDDC-SEND = '44'                                     
130700             MOVE '255'             TO WS-ALLOCATE-DC                     
130800           END-IF                                                         
130900           IF IN-EKH-IDDC-SEND = '45'                                     
131000             MOVE '267'             TO WS-ALLOCATE-DC                     
131100           END-IF                                                         
131200           IF IN-EKH-IDDC-SEND = '46'                                     
131300             MOVE '268'             TO WS-ALLOCATE-DC                     
131400           END-IF                                                         
131500           IF IN-EKH-IDDC-SEND = '47'                                     
131600             MOVE '265'            TO WS-ALLOCATE-DC                      
131700           END-IF                                                         
131800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
131900           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
132000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
132100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
132200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
132300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
132400           PERFORM S04-WRITE-W56173A                                      
132500         END-IF                                                           
132600       END-IF                                                             
132700                                                                          
132800       IF SYST-IDSEKVNR = 2                                               
132900         IF IN-EKH-SUBEL < 0                                              
133000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
133100           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
133200**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
133300           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
133400           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
133500           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
133600           IF IN-EKH-KDVALISO = 'USD'                                     
133700             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
133800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
133900           ELSE                                                           
134000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
134100                                         W-PRKURS                         
134200           END-IF                                                         
134300           IF IN-EKH-IDDC-SEND = '41'                                     
134400             MOVE '266'             TO WS-ALLOCATE-DC                     
134500           END-IF                                                         
134600           IF IN-EKH-IDDC-SEND = '43'                                     
134700             MOVE '264'             TO WS-ALLOCATE-DC                     
134800           END-IF                                                         
134900           IF IN-EKH-IDDC-SEND = '44'                                     
135000             MOVE '255'             TO WS-ALLOCATE-DC                     
135100           END-IF                                                         
135200           IF IN-EKH-IDDC-SEND = '45'                                     
135300             MOVE '267'             TO WS-ALLOCATE-DC                     
135400           END-IF                                                         
135500           IF IN-EKH-IDDC-SEND = '46'                                     
135600             MOVE '268'             TO WS-ALLOCATE-DC                     
135700           END-IF                                                         
135800           IF IN-EKH-IDDC-SEND = '47'                                     
135900             MOVE '265'            TO WS-ALLOCATE-DC                      
136000           END-IF                                                         
136100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
136200           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
136300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
136400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
136500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
136600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
136700           PERFORM S04-WRITE-W56173A                                      
136800         END-IF                                                           
136900       END-IF                                                             
137000                                                                          
137100       IF SYST-IDSEKVNR = 3                                               
137200         IF IN-EKH-SUBEL > 0                                              
137300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
137400           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
137500           IF IN-EKH-KDVALISO = 'USD'                                     
137600             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
137700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
137800           ELSE                                                           
137900             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
138000                                         W-PRKURS                         
138100           END-IF                                                         
138200           MOVE SYST-IDKST          TO WS-RED-IDKST                       
138300           IF WS-RED-IDKST > SPACE                                        
138400             IF IN-EKH-IDDC-SEND = '41'                                   
138500               MOVE '266'           TO WS-RED-IDKST(1:3)                  
138600             END-IF                                                       
138700             IF IN-EKH-IDDC-SEND = '43'                                   
138800               MOVE '264'           TO WS-RED-IDKST(1:3)                  
138900             END-IF                                                       
139000             IF IN-EKH-IDDC-SEND = '44'                                   
139100               MOVE '255'           TO WS-RED-IDKST(1:3)                  
139200             END-IF                                                       
139300             IF IN-EKH-IDDC-SEND = '45'                                   
139400               MOVE '267'           TO WS-RED-IDKST(1:3)                  
139500             END-IF                                                       
139600             IF IN-EKH-IDDC-SEND = '46'                                   
139700               MOVE '268'           TO WS-RED-IDKST(1:3)                  
139800             END-IF                                                       
139900             IF IN-EKH-IDDC-SEND = '47'                                   
140000               MOVE '265'          TO WS-RED-IDKST(1:3)                   
140100             END-IF                                                       
140200             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
140300           ELSE                                                           
140400             MOVE SPACE             TO R3-LINE-COST-CENTER                
140500           END-IF                                                         
140600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
140700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
140800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
140900           PERFORM S04-WRITE-W56173A                                      
141000         END-IF                                                           
141100       END-IF                                                             
141200                                                                          
141300       IF SYST-IDSEKVNR = 4                                               
141400         IF IN-EKH-SUBEL < 0                                              
141500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
141600           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
141700           IF IN-EKH-KDVALISO = 'USD'                                     
141800             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
141900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
142000           ELSE                                                           
142100             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
142200                                         W-PRKURS                         
142300           END-IF                                                         
142400           MOVE SYST-IDKST          TO WS-RED-IDKST                       
142500           IF WS-RED-IDKST > SPACE                                        
142600             IF IN-EKH-IDDC-SEND = '41'                                   
142700               MOVE '266'           TO WS-RED-IDKST(1:3)                  
142800             END-IF                                                       
142900             IF IN-EKH-IDDC-SEND = '43'                                   
143000               MOVE '264'           TO WS-RED-IDKST(1:3)                  
143100             END-IF                                                       
143200             IF IN-EKH-IDDC-SEND = '44'                                   
143300               MOVE '255'           TO WS-RED-IDKST(1:3)                  
143400             END-IF                                                       
143500             IF IN-EKH-IDDC-SEND = '45'                                   
143600               MOVE '267'           TO WS-RED-IDKST(1:3)                  
143700             END-IF                                                       
143800             IF IN-EKH-IDDC-SEND = '46'                                   
143900               MOVE '268'           TO WS-RED-IDKST(1:3)                  
144000             END-IF                                                       
144100             IF IN-EKH-IDDC-SEND = '47'                                   
144200               MOVE '265'          TO WS-RED-IDKST(1:3)                   
144300             END-IF                                                       
144400             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
144500           ELSE                                                           
144600             MOVE SPACE             TO R3-LINE-COST-CENTER                
144700           END-IF                                                         
144800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
144900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
145000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
145100           PERFORM S04-WRITE-W56173A                                      
145200         END-IF                                                           
145300       END-IF                                                             
145400                                                                          
145500     WHEN 'ARB'                                                           
145600       IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                                 
145700         MOVE IN-EKH-IDDC-SEND      TO W-IDDC-B6                          
145800         PERFORM IMS-GU-WDB601                                            
145900       END-IF                                                             
146000                                                                          
146100       IF SYST-IDSEKVNR = 1                                               
146200         IF IN-EKH-SUBEL < 0                                              
146300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
146400           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
146500           IF IN-EKH-KDVALISO = 'USD'                                     
146600             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
146700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
146800           ELSE                                                           
146900             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
147000                                         W-PRKURS                         
147100           END-IF                                                         
147200           MOVE SYST-IDKST          TO WS-RED-IDKST                       
147300           IF WS-RED-IDKST > SPACE                                        
147400             IF IN-EKH-IDDC-SEND = '41'                                   
147500               MOVE '266'           TO WS-RED-IDKST(1:3)                  
147600             END-IF                                                       
147700             IF IN-EKH-IDDC-SEND = '43'                                   
147800               MOVE '264'           TO WS-RED-IDKST(1:3)                  
147900             END-IF                                                       
148000             IF IN-EKH-IDDC-SEND = '44'                                   
148100               MOVE '255'           TO WS-RED-IDKST(1:3)                  
148200             END-IF                                                       
148300             IF IN-EKH-IDDC-SEND = '45'                                   
148400               MOVE '267'           TO WS-RED-IDKST(1:3)                  
148500             END-IF                                                       
148600             IF IN-EKH-IDDC-SEND = '46'                                   
148700               MOVE '268'           TO WS-RED-IDKST(1:3)                  
148800             END-IF                                                       
148900             IF IN-EKH-IDDC-SEND = '47'                                   
149000               MOVE '265'          TO WS-RED-IDKST(1:3)                   
149100             END-IF                                                       
149200             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
149300           ELSE                                                           
149400             MOVE SPACE             TO R3-LINE-COST-CENTER                
149500           END-IF                                                         
149600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
149700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
149800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
149900           PERFORM S04-WRITE-W56173A                                      
150000         END-IF                                                           
150100       END-IF                                                             
150200                                                                          
150300       IF SYST-IDSEKVNR = 2                                               
150400         IF IN-EKH-SUBEL > 0                                              
150500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
150600           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
150700           IF IN-EKH-KDVALISO = 'USD'                                     
150800             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
150900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
151000           ELSE                                                           
151100             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
151200                                         W-PRKURS                         
151300           END-IF                                                         
151400           MOVE SYST-IDKST          TO WS-RED-IDKST                       
151500           IF WS-RED-IDKST > SPACE                                        
151600             IF IN-EKH-IDDC-SEND = '41'                                   
151700               MOVE '266'           TO WS-RED-IDKST(1:3)                  
151800             END-IF                                                       
151900             IF IN-EKH-IDDC-SEND = '43'                                   
152000               MOVE '264'           TO WS-RED-IDKST(1:3)                  
152100             END-IF                                                       
152200             IF IN-EKH-IDDC-SEND = '44'                                   
152300               MOVE '255'           TO WS-RED-IDKST(1:3)                  
152400             END-IF                                                       
152500             IF IN-EKH-IDDC-SEND = '45'                                   
152600               MOVE '267'           TO WS-RED-IDKST(1:3)                  
152700             END-IF                                                       
152800             IF IN-EKH-IDDC-SEND = '46'                                   
152900               MOVE '268'           TO WS-RED-IDKST(1:3)                  
153000             END-IF                                                       
153100             IF IN-EKH-IDDC-SEND = '47'                                   
153200               MOVE '265'          TO WS-RED-IDKST(1:3)                   
153300             END-IF                                                       
153400             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
153500           ELSE                                                           
153600             MOVE SPACE             TO R3-LINE-COST-CENTER                
153700           END-IF                                                         
153800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
153900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
154000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
154100           PERFORM S04-WRITE-W56173A                                      
154200         END-IF                                                           
154300       END-IF                                                             
154400                                                                          
154500     WHEN 'TRP'                                                           
154600       IF SYST-IDSEKVNR = 1                                               
154700         IF IN-EKH-SUBEL < 0                                              
154800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
154900           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
155000           IF IN-EKH-KDVALISO = 'USD'                                     
155100             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
155200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
155300           ELSE                                                           
155400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
155500                                         W-PRKURS                         
155600           END-IF                                                         
155700           MOVE SYST-IDKST          TO WS-RED-IDKST                       
155800           IF WS-RED-IDKST > SPACE                                        
155900             IF IN-EKH-IDDC-SEND = '41'                                   
156000               MOVE '266'           TO WS-RED-IDKST(1:3)                  
156100             END-IF                                                       
156200             IF IN-EKH-IDDC-SEND = '43'                                   
156300               MOVE '264'           TO WS-RED-IDKST(1:3)                  
156400             END-IF                                                       
156500             IF IN-EKH-IDDC-SEND = '44'                                   
156600               MOVE '255'           TO WS-RED-IDKST(1:3)                  
156700             END-IF                                                       
156800             IF IN-EKH-IDDC-SEND = '45'                                   
156900               MOVE '267'           TO WS-RED-IDKST(1:3)                  
157000             END-IF                                                       
157100             IF IN-EKH-IDDC-SEND = '46'                                   
157200               MOVE '268'           TO WS-RED-IDKST(1:3)                  
157300             END-IF                                                       
157400             IF IN-EKH-IDDC-SEND = '47'                                   
157500               MOVE '265'          TO WS-RED-IDKST(1:3)                   
157600             END-IF                                                       
157700             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
157800           ELSE                                                           
157900             MOVE SPACE             TO R3-LINE-COST-CENTER                
158000           END-IF                                                         
158100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
158200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
158300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
158400           PERFORM S04-WRITE-W56173A                                      
158500         END-IF                                                           
158600       END-IF                                                             
158700                                                                          
158800       IF SYST-IDSEKVNR = 2                                               
158900         IF IN-EKH-SUBEL > 0                                              
159000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
159100           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
159200           IF IN-EKH-KDVALISO = 'USD'                                     
159300             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
159400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
159500           ELSE                                                           
159600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
159700                                         W-PRKURS                         
159800           END-IF                                                         
159900           MOVE SYST-IDKST          TO WS-RED-IDKST                       
160000           IF WS-RED-IDKST > SPACE                                        
160100             IF IN-EKH-IDDC-SEND = '41'                                   
160200               MOVE '266'           TO WS-RED-IDKST(1:3)                  
160300             END-IF                                                       
160400             IF IN-EKH-IDDC-SEND = '43'                                   
160500               MOVE '264'           TO WS-RED-IDKST(1:3)                  
160600             END-IF                                                       
160700             IF IN-EKH-IDDC-SEND = '44'                                   
160800               MOVE '255'           TO WS-RED-IDKST(1:3)                  
160900             END-IF                                                       
161000             IF IN-EKH-IDDC-SEND = '45'                                   
161100               MOVE '267'           TO WS-RED-IDKST(1:3)                  
161200             END-IF                                                       
161300             IF IN-EKH-IDDC-SEND = '46'                                   
161400               MOVE '268'           TO WS-RED-IDKST(1:3)                  
161500             END-IF                                                       
161600             IF IN-EKH-IDDC-SEND = '47'                                   
161700               MOVE '265'          TO WS-RED-IDKST(1:3)                   
161800             END-IF                                                       
161900             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
162000           ELSE                                                           
162100             MOVE SPACE             TO R3-LINE-COST-CENTER                
162200           END-IF                                                         
162300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
162400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
162500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
162600           PERFORM S04-WRITE-W56173A                                      
162700         END-IF                                                           
162800       END-IF                                                             
162900                                                                          
163000     WHEN 'MATR'                                                          
163100       IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                                 
163200         MOVE IN-EKH-IDDC-SEND      TO W-IDDC-B6                          
163300         PERFORM IMS-GU-WDB601                                            
163400       END-IF                                                             
163500                                                                          
163600       IF SYST-IDSEKVNR = 1                                               
163700         IF IN-EKH-SUBEL < 0                                              
163800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
163900           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
164000           IF IN-EKH-KDVALISO = 'USD'                                     
164100             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
164200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
164300           ELSE                                                           
164400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
164500                                         W-PRKURS                         
164600           END-IF                                                         
164700           MOVE SYST-IDKST          TO WS-RED-IDKST                       
164800           IF WS-RED-IDKST > SPACE                                        
164900             IF IN-EKH-IDDC-SEND = '41'                                   
165000               MOVE '266'           TO WS-RED-IDKST(1:3)                  
165100             END-IF                                                       
165200             IF IN-EKH-IDDC-SEND = '43'                                   
165300               MOVE '264'           TO WS-RED-IDKST(1:3)                  
165400             END-IF                                                       
165500             IF IN-EKH-IDDC-SEND = '44'                                   
165600               MOVE '255'           TO WS-RED-IDKST(1:3)                  
165700             END-IF                                                       
165800             IF IN-EKH-IDDC-SEND = '45'                                   
165900               MOVE '267'           TO WS-RED-IDKST(1:3)                  
166000             END-IF                                                       
166100             IF IN-EKH-IDDC-SEND = '46'                                   
166200               MOVE '268'           TO WS-RED-IDKST(1:3)                  
166300             END-IF                                                       
166400             IF IN-EKH-IDDC-SEND = '47'                                   
166500               MOVE '265'          TO WS-RED-IDKST(1:3)                   
166600             END-IF                                                       
166700             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
166800           ELSE                                                           
166900             MOVE SPACE             TO R3-LINE-COST-CENTER                
167000           END-IF                                                         
167100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
167200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
167300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
167400           PERFORM S04-WRITE-W56173A                                      
167500         END-IF                                                           
167600       END-IF                                                             
167700                                                                          
167800       IF SYST-IDSEKVNR = 2                                               
167900         IF IN-EKH-SUBEL > 0                                              
168000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
168100           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
168200           IF IN-EKH-KDVALISO = 'USD'                                     
168300             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
168400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
168500           ELSE                                                           
168600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
168700                                         W-PRKURS                         
168800           END-IF                                                         
168900           MOVE SYST-IDKST          TO WS-RED-IDKST                       
169000           IF WS-RED-IDKST > SPACE                                        
169100             IF IN-EKH-IDDC-SEND = '41'                                   
169200               MOVE '266'           TO WS-RED-IDKST(1:3)                  
169300             END-IF                                                       
169400             IF IN-EKH-IDDC-SEND = '43'                                   
169500               MOVE '264'           TO WS-RED-IDKST(1:3)                  
169600             END-IF                                                       
169700             IF IN-EKH-IDDC-SEND = '44'                                   
169800               MOVE '255'           TO WS-RED-IDKST(1:3)                  
169900             END-IF                                                       
170000             IF IN-EKH-IDDC-SEND = '45'                                   
170100               MOVE '267'           TO WS-RED-IDKST(1:3)                  
170200             END-IF                                                       
170300             IF IN-EKH-IDDC-SEND = '46'                                   
170400               MOVE '268'           TO WS-RED-IDKST(1:3)                  
170500             END-IF                                                       
170600             IF IN-EKH-IDDC-SEND = '47'                                   
170700               MOVE '265'          TO WS-RED-IDKST(1:3)                   
170800             END-IF                                                       
170900             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
171000           ELSE                                                           
171100             MOVE SPACE             TO R3-LINE-COST-CENTER                
171200           END-IF                                                         
171300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
171400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
171500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
171600           PERFORM S04-WRITE-W56173A                                      
171700         END-IF                                                           
171800       END-IF                                                             
171900                                                                          
172000     WHEN 'DDI'                                                           
172100       IF IN-EKH-SUBEL > ZERO                                             
172200         IF SYST-IDSEKVNR = 1                                             
172300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
172400           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
172500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
172600                   IN-EKH-SUBEL                                           
172700           IF IN-EKH-KDVALISO = 'USD'                                     
172800             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
172900           ELSE                                                           
173000             MOVE ZEROES              TO R3-LINE-AMOUNT                   
173100           END-IF                                                         
173200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
173300           MOVE SPACE               TO R3-LINE-ALLOCATE                   
173400           PERFORM S04-WRITE-W56173A                                      
173500         END-IF                                                           
173600       ELSE                                                               
173700         IF SYST-IDSEKVNR = 2                                             
173800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
173900           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
174000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
174100                   IN-EKH-SUBEL                                           
174200           IF IN-EKH-KDVALISO = 'USD'                                     
174300             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
174400           ELSE                                                           
174500             MOVE ZEROES              TO R3-LINE-AMOUNT                   
174600           END-IF                                                         
174700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
174800           MOVE SPACE               TO R3-LINE-ALLOCATE                   
174900           PERFORM S04-WRITE-W56173A                                      
175000         END-IF                                                           
175100       END-IF                                                             
175200     END-EVALUATE                                                         
175300     .                                                                    
175400     EJECT                                                                
175500                                                                          
175600 CEBC-SUB-EVENT-102-130 SECTION.                                          
175700     EVALUATE IN-EKH-KDEKNIVA                                             
175800     WHEN 'DET'                                                           
175900       IF SYST-IDSEKVNR = 1                                               
176000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
176100         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
176200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
176300            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-US * -1          
176400         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
176500         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
176600         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
176700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
176800         MOVE SPACE               TO WS-ALLOCATE-DC                       
176900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
177000         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                  
177100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
177200         PERFORM S03-WRITE-W56172                                         
177300       END-IF                                                             
177400                                                                          
177500     WHEN 'FÖRS'                                                          
177600     WHEN 'FRAKT'                                                         
177700     WHEN 'EMB'                                                           
177800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
177900       MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                        
178000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
178100               IN-EKH-SUBEL / WS-PRKURS-US * -1                           
178200       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
178300       MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                           
178400       MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                   
178500       MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                           
178600       MOVE SPACE               TO WS-ALLOCATE-DC                         
178700       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
178800       MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                    
178900       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
179000       PERFORM S03-WRITE-W56172                                           
179100                                                                          
179200     WHEN 'DDI'                                                           
179300       IF IN-EKH-SUBEL < ZERO                                             
179400         IF SYST-IDSEKVNR = 1                                             
179500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
179600           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
179700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
179800                   IN-EKH-SUBEL                                           
179900           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
180000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
180100           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
180200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
180300           MOVE SPACE               TO WS-ALLOCATE-DC                     
180400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
180500           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                
180600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
180700           PERFORM S04-WRITE-W56173A                                      
180800         END-IF                                                           
180900       ELSE                                                               
181000         IF SYST-IDSEKVNR = 2                                             
181100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
181200           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
181300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
181400                   IN-EKH-SUBEL                                           
181500           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
181600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
181700           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
181800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
181900           MOVE SPACE               TO WS-ALLOCATE-DC                     
182000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
182100           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                
182200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
182300           PERFORM S04-WRITE-W56173A                                      
182400         END-IF                                                           
182500       END-IF                                                             
182600                                                                          
182700     END-EVALUATE                                                         
182800     .                                                                    
182900     EJECT                                                                
183000                                                                          
183100 CEBD-SUB-EVENT-102-131 SECTION.                                          
183200     EVALUATE IN-EKH-KDEKNIVA                                             
183300     WHEN 'DET'                                                           
183400       IF SYST-IDSEKVNR = 1                                               
183500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
183600         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
183700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
183800            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-US3              
183900         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-1                   
184000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
184100         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
184200         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
184300         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
184400         MOVE SPACE               TO WS-ALLOCATE-DC                       
184500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
184600         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                  
184700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
184800         PERFORM S03-WRITE-W56172                                         
184900       END-IF                                                             
185000                                                                          
185100       IF SYST-IDSEKVNR = 2                                               
185200**** GET RIGHT MARKUP FOR LOCAL PRODUCT GROUP                             
185300         PERFORM I-SOEK-MARKUP                                            
185400                                                                          
185500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
185600         MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                           
185700**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
185800         MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                         
185900         MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                      
186000         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
186100                                                                          
186200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
186300            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-US3 +            
186400            IN-EKH-KVANTAL *                                              
186500            IN-EKH-PRARTNTO / WS-PRKURS-US3 * (WS-MARKUP - 1)             
186600         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-2                   
186700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
186800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
186900         IF IN-EKH-IDDC-REC = '41'                                        
187000           MOVE '266'             TO WS-ALLOCATE-DC                       
187100         END-IF                                                           
187200         IF IN-EKH-IDDC-REC = '43'                                        
187300           MOVE '264'             TO WS-ALLOCATE-DC                       
187400         END-IF                                                           
187500         IF IN-EKH-IDDC-REC = '44'                                        
187600           MOVE '255'             TO WS-ALLOCATE-DC                       
187700         END-IF                                                           
187800         IF IN-EKH-IDDC-REC = '45'                                        
187900           MOVE '267'             TO WS-ALLOCATE-DC                       
188000         END-IF                                                           
188100         IF IN-EKH-IDDC-REC = '46'                                        
188200           MOVE '268'             TO WS-ALLOCATE-DC                       
188300         END-IF                                                           
188400         IF IN-EKH-IDDC-REC = '47'                                        
188500           MOVE '265'            TO WS-ALLOCATE-DC                        
188600         END-IF                                                           
188700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
188800         MOVE SPACE               TO WS-ALLOCATE-IDVERGL                  
188900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
189000         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
189100         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
189200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
189300         PERFORM S03-WRITE-W56172                                         
189400       END-IF                                                             
189500                                                                          
189600       IF SYST-IDSEKVNR = 3                                               
189700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
189800         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
189900         COMPUTE R3-LINE-AMOUNT-LC =                                      
190000                 WS-LINE-AMOUNT-131-2 - WS-LINE-AMOUNT-131-1              
190100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
190200         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
190300         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
190400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
190500         MOVE SPACE               TO WS-ALLOCATE-DC                       
190600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
190700         MOVE SPACE               TO WS-ALLOCATE-IDVERGL                  
190800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
190900         PERFORM S03-WRITE-W56172                                         
191000       END-IF                                                             
191100                                                                          
191200     WHEN 'FRAKT'                                                         
191300       IF SYST-IDSEKVNR = 1                                               
191400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
191500         MOVE WS-R3-ACCOUNT-7   TO R3-LINE-ACCOUNT                        
191600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
191700                 IN-EKH-SUBEL / WS-PRKURS-US3                             
191800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
191900         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
192000         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
192100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
192200         MOVE SPACE               TO WS-ALLOCATE-DC                       
192300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
192400         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                  
192500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
192600         PERFORM S03-WRITE-W56172                                         
192700       END-IF                                                             
192800                                                                          
192900       IF IN-EKH-KDFRAKT = 17                                             
193000         IF SYST-IDSEKVNR = 2                                             
193100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
193200           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
193300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
193400                   IN-EKH-SUBEL / WS-PRKURS-US3                           
193500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
193600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
193700           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
193800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
193900           MOVE SPACE             TO WS-ALLOCATE-DC                       
194000           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
194100           MOVE SPACE             TO WS-ALLOCATE-IDVERGL                  
194200           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
194300           PERFORM S04-WRITE-W56173A                                      
194400         END-IF                                                           
194500       END-IF                                                             
194600                                                                          
194700       IF IN-EKH-KDFRAKT = 43                                             
194800         IF SYST-IDSEKVNR = 3                                             
194900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
195000           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
195100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
195200                   IN-EKH-SUBEL / WS-PRKURS-US3                           
195300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
195400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
195500           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
195600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
195700           MOVE SPACE             TO WS-ALLOCATE-DC                       
195800           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
195900           MOVE SPACE             TO WS-ALLOCATE-IDVERGL                  
196000           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
196100           PERFORM S04-WRITE-W56173A                                      
196200         END-IF                                                           
196300       END-IF                                                             
196400                                                                          
196500       IF IN-EKH-KDFRAKT = 17                                             
196600         IF SYST-IDSEKVNR = 4                                             
196700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
196800           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
196900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
197000                   IN-EKH-SUBEL / WS-PRKURS-US3                           
197100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
197200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
197300           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
197400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
197500           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
197600           IF IN-EKH-IDDC-REC = '41'                                      
197700             MOVE '266'           TO WS-ALLOCATE-DC                       
197800           END-IF                                                         
197900           IF IN-EKH-IDDC-REC = '43'                                      
198000             MOVE '264'           TO WS-ALLOCATE-DC                       
198100           END-IF                                                         
198200           IF IN-EKH-IDDC-REC = '44'                                      
198300             MOVE '255'           TO WS-ALLOCATE-DC                       
198400           END-IF                                                         
198500           IF IN-EKH-IDDC-REC = '45'                                      
198600             MOVE '267'           TO WS-ALLOCATE-DC                       
198700           END-IF                                                         
198800           IF IN-EKH-IDDC-REC = '46'                                      
198900             MOVE '268'           TO WS-ALLOCATE-DC                       
199000           END-IF                                                         
199100           IF IN-EKH-IDDC-REC = '47'                                      
199200             MOVE '265'          TO WS-ALLOCATE-DC                        
199300           END-IF                                                         
199400           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
199500           MOVE IN-EKH-IDVERGL    TO WS-ALLOCATE-IDVERGL                  
199600           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
199700           MOVE SYST-IDKST          TO WS-RED-IDKST                       
199800           IF WS-RED-IDKST > SPACE                                        
199900             IF IN-EKH-IDDC-REC  = '41'                                   
200000               MOVE '266'           TO WS-RED-IDKST(1:3)                  
200100             END-IF                                                       
200200             IF IN-EKH-IDDC-REC  = '43'                                   
200300               MOVE '264'           TO WS-RED-IDKST(1:3)                  
200400             END-IF                                                       
200500             IF IN-EKH-IDDC-REC  = '44'                                   
200600               MOVE '255'           TO WS-RED-IDKST(1:3)                  
200700             END-IF                                                       
200800             IF IN-EKH-IDDC-REC  = '45'                                   
200900               MOVE '267'           TO WS-RED-IDKST(1:3)                  
201000             END-IF                                                       
201100             IF IN-EKH-IDDC-REC  = '46'                                   
201200               MOVE '268'           TO WS-RED-IDKST(1:3)                  
201300             END-IF                                                       
201400             IF IN-EKH-IDDC-REC  = '47'                                   
201500               MOVE '265'          TO WS-RED-IDKST(1:3)                   
201600             END-IF                                                       
201700             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
201800           ELSE                                                           
201900             MOVE SPACE             TO R3-LINE-COST-CENTER                
202000           END-IF                                                         
202100           PERFORM S04-WRITE-W56173A                                      
202200         END-IF                                                           
202300       END-IF                                                             
202400                                                                          
202500       IF IN-EKH-KDFRAKT = 17                                             
202600         IF SYST-IDSEKVNR = 5                                             
202700           MOVE SYST-IDKONTO  TO WS-R3-ACCOUNT-10                         
202800           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
202900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
203000                   IN-EKH-SUBEL / WS-PRKURS-US3                           
203100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
203200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
203300           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
203400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
203500           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
203600           IF IN-EKH-IDDC-REC = '41'                                      
203700             MOVE '266'           TO WS-ALLOCATE-DC                       
203800           END-IF                                                         
203900           IF IN-EKH-IDDC-REC = '43'                                      
204000             MOVE '264'           TO WS-ALLOCATE-DC                       
204100           END-IF                                                         
204200           IF IN-EKH-IDDC-REC = '44'                                      
204300             MOVE '255'           TO WS-ALLOCATE-DC                       
204400           END-IF                                                         
204500           IF IN-EKH-IDDC-REC = '45'                                      
204600             MOVE '267'           TO WS-ALLOCATE-DC                       
204700           END-IF                                                         
204800           IF IN-EKH-IDDC-REC = '46'                                      
204900             MOVE '268'           TO WS-ALLOCATE-DC                       
205000           END-IF                                                         
205100           IF IN-EKH-IDDC-REC = '47'                                      
205200             MOVE '265'          TO WS-ALLOCATE-DC                        
205300           END-IF                                                         
205400           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
205500           MOVE IN-EKH-IDVERGL    TO WS-ALLOCATE-IDVERGL                  
205600           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
205700           MOVE SYST-IDKST          TO WS-RED-IDKST                       
205800           IF WS-RED-IDKST > SPACE                                        
205900             IF IN-EKH-IDDC-REC  = '41'                                   
206000               MOVE '266'           TO WS-RED-IDKST(1:3)                  
206100             END-IF                                                       
206200             IF IN-EKH-IDDC-REC  = '43'                                   
206300               MOVE '264'           TO WS-RED-IDKST(1:3)                  
206400             END-IF                                                       
206500             IF IN-EKH-IDDC-REC  = '44'                                   
206600               MOVE '255'           TO WS-RED-IDKST(1:3)                  
206700             END-IF                                                       
206800             IF IN-EKH-IDDC-REC  = '45'                                   
206900               MOVE '267'           TO WS-RED-IDKST(1:3)                  
207000             END-IF                                                       
207100             IF IN-EKH-IDDC-REC  = '46'                                   
207200               MOVE '268'           TO WS-RED-IDKST(1:3)                  
207300             END-IF                                                       
207400             IF IN-EKH-IDDC-REC  = '47'                                   
207500               MOVE '265'          TO WS-ALLOCATE-DC                      
207600             END-IF                                                       
207700             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
207800           ELSE                                                           
207900             MOVE SPACE             TO R3-LINE-COST-CENTER                
208000           END-IF                                                         
208100           PERFORM S04-WRITE-W56173A                                      
208200         END-IF                                                           
208300       END-IF                                                             
208400                                                                          
208500     WHEN 'FÖRS'                                                          
208600     WHEN 'EMB'                                                           
208700       IF SYST-IDSEKVNR = 1                                               
208800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
208900         MOVE WS-R3-ACCOUNT-7   TO R3-LINE-ACCOUNT                        
209000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
209100                 IN-EKH-SUBEL / WS-PRKURS-US3                             
209200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
209300         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
209400         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
209500         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
209600         MOVE SPACE               TO WS-ALLOCATE-DC                       
209700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
209800         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                  
209900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
210000         PERFORM S03-WRITE-W56172                                         
210100       END-IF                                                             
210200                                                                          
210300       IF SYST-IDSEKVNR = 2                                               
210400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
210500         MOVE WS-R3-ACCOUNT-7   TO R3-LINE-ACCOUNT                        
210600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
210700                 IN-EKH-SUBEL / WS-PRKURS-US3                             
210800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
210900         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
211000         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
211100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
211200         MOVE SPACE               TO WS-ALLOCATE-DC                       
211300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
211400         MOVE SPACE               TO WS-ALLOCATE-IDVERGL                  
211500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
211600         PERFORM S04-WRITE-W56173A                                        
211700       END-IF                                                             
211800     END-EVALUATE                                                         
211900     .                                                                    
212000     EJECT                                                                
212100                                                                          
212200 CEBE-SUB-EVENT-102-132 SECTION.                                          
212300     EVALUATE IN-EKH-KDEKNIVA                                             
212400     WHEN 'DET'                                                           
212500       PERFORM I-SOEK-MARKUP                                              
212600       IF SYST-IDSEKVNR = 1                                               
212700         IF IN-EKH-KVANTAL > 0                                            
212800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
212900           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
213000**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
213100           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
213200           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
213300           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
213400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
213500            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-US3)) +        
213600            (IN-EKH-KVANTAL *                                             
213700            (IN-EKH-PRARTNTO / WS-PRKURS-US3) * (WS-MARKUP - 1))          
213800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
213900           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
214000           IF IN-EKH-IDDC-REC = '41'                                      
214100             MOVE '266'           TO WS-ALLOCATE-DC                       
214200           END-IF                                                         
214300           IF IN-EKH-IDDC-REC = '43'                                      
214400             MOVE '264'           TO WS-ALLOCATE-DC                       
214500           END-IF                                                         
214600           IF IN-EKH-IDDC-REC = '44'                                      
214700             MOVE '255'           TO WS-ALLOCATE-DC                       
214800           END-IF                                                         
214900           IF IN-EKH-IDDC-REC = '45'                                      
215000             MOVE '267'           TO WS-ALLOCATE-DC                       
215100           END-IF                                                         
215200           IF IN-EKH-IDDC-REC = '46'                                      
215300             MOVE '268'           TO WS-ALLOCATE-DC                       
215400           END-IF                                                         
215500           IF IN-EKH-IDDC-REC = '47'                                      
215600             MOVE '265'        TO   WS-ALLOCATE-DC                        
215700           END-IF                                                         
215800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
215900           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
216000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
216100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
216200           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
216300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
216400           PERFORM S02-WRITE-W56171A                                      
216500         END-IF                                                           
216600       END-IF                                                             
216700                                                                          
216800       IF SYST-IDSEKVNR = 2                                               
216900         IF IN-EKH-KVANTAL < 0                                            
217000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
217100           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
217200**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
217300           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
217400           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
217500           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
217600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
217700            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-US3)) +        
217800            (IN-EKH-KVANTAL *                                             
217900            (IN-EKH-PRARTNTO / WS-PRKURS-US3) * (WS-MARKUP - 1))          
218000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
218100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
218200           IF IN-EKH-IDDC-REC = '41'                                      
218300             MOVE '266'           TO WS-ALLOCATE-DC                       
218400           END-IF                                                         
218500           IF IN-EKH-IDDC-REC = '43'                                      
218600             MOVE '264'           TO WS-ALLOCATE-DC                       
218700           END-IF                                                         
218800           IF IN-EKH-IDDC-REC = '44'                                      
218900             MOVE '255'           TO WS-ALLOCATE-DC                       
219000           END-IF                                                         
219100           IF IN-EKH-IDDC-REC = '45'                                      
219200             MOVE '267'           TO WS-ALLOCATE-DC                       
219300           END-IF                                                         
219400           IF IN-EKH-IDDC-REC = '46'                                      
219500             MOVE '268'           TO WS-ALLOCATE-DC                       
219600           END-IF                                                         
219700           IF IN-EKH-IDDC-REC = '47'                                      
219800             MOVE '265'          TO WS-ALLOCATE-DC                        
219900           END-IF                                                         
220000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
220100           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
220200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
220300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
220400           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
220500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
220600           PERFORM S02-WRITE-W56171A                                      
220700         END-IF                                                           
220800       END-IF                                                             
220900                                                                          
221000       IF SYST-IDSEKVNR = 3                                               
221100         IF IN-EKH-KVANTAL < 0                                            
221200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
221300           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
221400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
221500            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-US3)) +        
221600            (IN-EKH-KVANTAL *                                             
221700            (IN-EKH-PRARTNTO / WS-PRKURS-US3) * (WS-MARKUP - 1))          
221800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
221900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
222000           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
222100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
222200           PERFORM S02-WRITE-W56171A                                      
222300         END-IF                                                           
222400       END-IF                                                             
222500                                                                          
222600       IF SYST-IDSEKVNR = 4                                               
222700         IF IN-EKH-KVANTAL > 0                                            
222800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
222900           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
223000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
223100            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-US3)) +        
223200            (IN-EKH-KVANTAL *                                             
223300            (IN-EKH-PRARTNTO / WS-PRKURS-US3) * (WS-MARKUP - 1))          
223400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
223500           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
223600           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
223700           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
223800           PERFORM S02-WRITE-W56171A                                      
223900         END-IF                                                           
224000       END-IF                                                             
224100     END-EVALUATE                                                         
224200     .                                                                    
224300     EJECT                                                                
224400                                                                          
224500 CEBF-SUB-EVENT-102-134 SECTION.                                          
224600     EVALUATE IN-EKH-KDEKNIVA                                             
224700     WHEN 'DET'                                                           
224800       IF SYST-IDSEKVNR = 1                                               
224900         IF IN-EKH-KVANTAL > 0                                            
225000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
225100           MOVE WS-R3-ACCOUNT-7   TO R3-LINE-ACCOUNT                      
225200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
225300            IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-US              
225400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
225500           IF IN-EKH-IDDC-REC = '41'                                      
225600             MOVE '266'             TO WS-ALLOCATE-DC                     
225700           END-IF                                                         
225800           IF IN-EKH-IDDC-REC = '43'                                      
225900             MOVE '264'             TO WS-ALLOCATE-DC                     
226000           END-IF                                                         
226100           IF IN-EKH-IDDC-REC = '44'                                      
226200             MOVE '255'             TO WS-ALLOCATE-DC                     
226300           END-IF                                                         
226400           IF IN-EKH-IDDC-REC = '45'                                      
226500             MOVE '267'             TO WS-ALLOCATE-DC                     
226600           END-IF                                                         
226700           IF IN-EKH-IDDC-REC = '46'                                      
226800             MOVE '268'             TO WS-ALLOCATE-DC                     
226900           END-IF                                                         
227000           IF IN-EKH-IDDC-REC = '47'                                      
227100             MOVE '265'            TO WS-ALLOCATE-DC                      
227200           END-IF                                                         
227300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
227400           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                
227500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
227600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
227700           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
227800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
227900           PERFORM S04-WRITE-W56173A                                      
228000         END-IF                                                           
228100       END-IF                                                             
228200                                                                          
228300       IF SYST-IDSEKVNR = 2                                               
228400         IF IN-EKH-KVANTAL < 0                                            
228500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
228600           MOVE WS-R3-ACCOUNT-7   TO R3-LINE-ACCOUNT                      
228700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
228800           IN-EKH-KVANTAL *                                               
228900            IN-EKH-PRARTNTO / WS-PRKURS-US   * -1                         
229000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
229100           IF IN-EKH-IDDC-REC = '41'                                      
229200             MOVE '266'             TO WS-ALLOCATE-DC                     
229300           END-IF                                                         
229400           IF IN-EKH-IDDC-REC = '43'                                      
229500             MOVE '264'             TO WS-ALLOCATE-DC                     
229600           END-IF                                                         
229700           IF IN-EKH-IDDC-REC = '44'                                      
229800             MOVE '255'             TO WS-ALLOCATE-DC                     
229900           END-IF                                                         
230000           IF IN-EKH-IDDC-REC = '45'                                      
230100             MOVE '267'             TO WS-ALLOCATE-DC                     
230200           END-IF                                                         
230300           IF IN-EKH-IDDC-REC = '46'                                      
230400             MOVE '268'             TO WS-ALLOCATE-DC                     
230500           END-IF                                                         
230600           IF IN-EKH-IDDC-REC = '47'                                      
230700             MOVE '265'            TO WS-ALLOCATE-DC                      
230800           END-IF                                                         
230900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
231000           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-IDVERGL                
231100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
231200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
231300           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
231400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
231500           PERFORM S04-WRITE-W56173A                                      
231600         END-IF                                                           
231700       END-IF                                                             
231800                                                                          
231900     WHEN 'EMB'                                                           
232000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
232100       MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                        
232200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
232300               IN-EKH-SUBEL / WS-PRKURS-US                                
232400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
232500         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
232600         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
232700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
232800       PERFORM S04-WRITE-W56173A                                          
232900                                                                          
233000     WHEN 'DDI'                                                           
233100       IF IN-EKH-SUBEL < ZERO                                             
233200         IF SYST-IDSEKVNR = 1                                             
233300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
233400           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
233500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
233600                   IN-EKH-SUBEL                                           
233700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
233800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
233900           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
234000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
234100           PERFORM S04-WRITE-W56173A                                      
234200         END-IF                                                           
234300       ELSE                                                               
234400         IF SYST-IDSEKVNR = 2                                             
234500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
234600           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
234700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
234800                   IN-EKH-SUBEL                                           
234900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
235000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
235100           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
235200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
235300           PERFORM S04-WRITE-W56173A                                      
235400         END-IF                                                           
235500       END-IF                                                             
235600                                                                          
235700     WHEN 'LAND'                                                          
235800         IF SYST-IDSEKVNR = 1                                             
235900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
236000           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
236100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
236200                   IN-EKH-SUBEL                                           
236300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
236400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
236500           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
236600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
236700           PERFORM S04-WRITE-W56173A                                      
236800         END-IF                                                           
236900                                                                          
237000         IF SYST-IDSEKVNR = 2                                             
237100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
237200           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
237300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
237400                   IN-EKH-SUBEL                                           
237500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
237600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
237700           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
237800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
237900           PERFORM S04-WRITE-W56173A                                      
238000         END-IF                                                           
238100                                                                          
238200     END-EVALUATE                                                         
238300     .                                                                    
238400     EJECT                                                                
238500                                                                          
238600                                                                          
238700 CEC-MAIN-EVENT-103 SECTION.                                              
238800     EVALUATE IN-EKH-KDEKSHT                                              
238900     WHEN '102'                                                           
239000          PERFORM CECA-SUB-EVENT-103-102                                  
239100     END-EVALUATE                                                         
239200     .                                                                    
239300     EJECT                                                                
239400                                                                          
239500 CECA-SUB-EVENT-103-102 SECTION.                                          
239600     MOVE IN-EKH-IDARTNR              TO TEST-IDARTNR                     
239700     EVALUATE IN-EKH-KDEKNIVA                                             
239800     WHEN 'DET'                                                           
239900       IF SYST-IDSEKVNR = 1                                               
240000         IF IN-EKH-KVANTAL < 0                                            
240100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
240200           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
240300**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
240400           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
240500           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
240600           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
240700           IF IN-EKH-KDVALISO = 'USD'                                     
240800             COMPUTE R3-LINE-AMOUNT-LC =                                  
240900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
241000                                                                          
241100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
241200           ELSE                                                           
241300             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
241400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS            
241500           END-IF                                                         
241600           IF IN-EKH-IDDC-SEND = '41'                                     
241700             MOVE '266'             TO WS-ALLOCATE-DC                     
241800           END-IF                                                         
241900           IF IN-EKH-IDDC-SEND = '43'                                     
242000             MOVE '264'             TO WS-ALLOCATE-DC                     
242100           END-IF                                                         
242200           IF IN-EKH-IDDC-SEND = '44'                                     
242300             MOVE '255'             TO WS-ALLOCATE-DC                     
242400           END-IF                                                         
242500           IF IN-EKH-IDDC-SEND = '45'                                     
242600             MOVE '267'             TO WS-ALLOCATE-DC                     
242700           END-IF                                                         
242800           IF IN-EKH-IDDC-SEND = '46'                                     
242900             MOVE '268'             TO WS-ALLOCATE-DC                     
243000           END-IF                                                         
243100           IF IN-EKH-IDDC-SEND = '47'                                     
243200             MOVE '265'            TO WS-ALLOCATE-DC                      
243300           END-IF                                                         
243400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
243500           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
243600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
243700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
243800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
243900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
244000           PERFORM S04-WRITE-W56173A                                      
244100         END-IF                                                           
244200       END-IF                                                             
244300                                                                          
244400       IF SYST-IDSEKVNR = 2                                               
244500         IF IN-EKH-KVANTAL > 0                                            
244600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
244700           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
244800**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
244900           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
245000           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
245100           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
245200           IF IN-EKH-KDVALISO = 'USD'                                     
245300             COMPUTE R3-LINE-AMOUNT-LC =                                  
245400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
245500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
245600           ELSE                                                           
245700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
245800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS            
245900           END-IF                                                         
246000           IF IN-EKH-IDDC-SEND = '41'                                     
246100             MOVE '266'             TO WS-ALLOCATE-DC                     
246200           END-IF                                                         
246300           IF IN-EKH-IDDC-SEND = '43'                                     
246400             MOVE '264'             TO WS-ALLOCATE-DC                     
246500           END-IF                                                         
246600           IF IN-EKH-IDDC-SEND = '44'                                     
246700             MOVE '255'             TO WS-ALLOCATE-DC                     
246800           END-IF                                                         
246900           IF IN-EKH-IDDC-SEND = '45'                                     
247000             MOVE '267'             TO WS-ALLOCATE-DC                     
247100           END-IF                                                         
247200           IF IN-EKH-IDDC-SEND = '46'                                     
247300             MOVE '268'             TO WS-ALLOCATE-DC                     
247400           END-IF                                                         
247500           IF IN-EKH-IDDC-SEND = '47'                                     
247600             MOVE '265'            TO WS-ALLOCATE-DC                      
247700           END-IF                                                         
247800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
247900           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
248000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
248100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
248200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
248300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
248400           PERFORM S04-WRITE-W56173A                                      
248500         END-IF                                                           
248600       END-IF                                                             
248700                                                                          
248800       IF SYST-IDSEKVNR = 3                                               
248900         IF IN-EKH-KVANTAL > 0                                            
249000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
249100           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
249200           IF IN-EKH-KDVALISO = 'USD'                                     
249300             COMPUTE R3-LINE-AMOUNT    =                                  
249400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
249500             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
249600           ELSE                                                           
249700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
249800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS            
249900           END-IF                                                         
250000           MOVE SPACE               TO WS-ALLOCATE-DC                     
250100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
250200           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
250300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
250400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
250500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
250600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
250700           PERFORM S04-WRITE-W56173A                                      
250800         END-IF                                                           
250900       END-IF                                                             
251000                                                                          
251100       IF SYST-IDSEKVNR = 4                                               
251200         IF IN-EKH-KVANTAL < 0                                            
251300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
251400           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
251500           IF IN-EKH-KDVALISO = 'USD'                                     
251600             COMPUTE R3-LINE-AMOUNT    =                                  
251700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
251800             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
251900           ELSE                                                           
252000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
252100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS            
252200           END-IF                                                         
252300           MOVE SPACE               TO WS-ALLOCATE-DC                     
252400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
252500           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
252600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
252700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
252800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
252900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
253000           PERFORM S04-WRITE-W56173A                                      
253100         END-IF                                                           
253200       END-IF                                                             
253300                                                                          
253400       IF SYST-IDSEKVNR = 5                                               
253500         IF BYT02-RENOV                                                   
253600           IF IN-EKH-KVANTAL > 0                                          
253700             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
253800             MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                  
253900             COMPUTE R3-LINE-AMOUNT    =                                  
254000                     IN-EKH-KVANTAL * 0.01                                
254100             MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                
254200             IF IN-EKH-IDDC-SEND = '41'                                   
254300               MOVE '266'             TO WS-ALLOCATE-DC                   
254400             END-IF                                                       
254500             IF IN-EKH-IDDC-SEND = '43'                                   
254600               MOVE '264'             TO WS-ALLOCATE-DC                   
254700             END-IF                                                       
254800             IF IN-EKH-IDDC-SEND = '44'                                   
254900               MOVE '255'             TO WS-ALLOCATE-DC                   
255000             END-IF                                                       
255100             IF IN-EKH-IDDC-SEND = '45'                                   
255200               MOVE '267'             TO WS-ALLOCATE-DC                   
255300             END-IF                                                       
255400             IF IN-EKH-IDDC-SEND = '46'                                   
255500               MOVE '268'             TO WS-ALLOCATE-DC                   
255600             END-IF                                                       
255700             IF IN-EKH-IDDC-SEND = '47'                                   
255800               MOVE '265'            TO WS-ALLOCATE-DC                    
255900             END-IF                                                       
256000             MOVE SPACE               TO WS-ALLOCATE-DISTR                
256100             MOVE SPACE               TO WS-ALLOCATE-IDVERGL              
256200             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
256300             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
256400             MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                     
256500             MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF            
256600             MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                     
256700             PERFORM S04-WRITE-W56173A                                    
256800           END-IF                                                         
256900         END-IF                                                           
257000       END-IF                                                             
257100                                                                          
257200       IF SYST-IDSEKVNR = 6                                               
257300         IF BYT02-RENOV                                                   
257400           IF IN-EKH-KVANTAL > 0                                          
257500             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
257600             MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                  
257700             COMPUTE R3-LINE-AMOUNT    =                                  
257800                     IN-EKH-KVANTAL * 0.01                                
257900             MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                
258000             IF IN-EKH-IDDC-SEND = '41'                                   
258100               MOVE '266'             TO WS-ALLOCATE-DC                   
258200             END-IF                                                       
258300             IF IN-EKH-IDDC-SEND = '43'                                   
258400               MOVE '264'             TO WS-ALLOCATE-DC                   
258500             END-IF                                                       
258600             IF IN-EKH-IDDC-SEND = '44'                                   
258700               MOVE '255'             TO WS-ALLOCATE-DC                   
258800             END-IF                                                       
258900             IF IN-EKH-IDDC-SEND = '45'                                   
259000               MOVE '267'             TO WS-ALLOCATE-DC                   
259100             END-IF                                                       
259200             IF IN-EKH-IDDC-SEND = '46'                                   
259300               MOVE '268'             TO WS-ALLOCATE-DC                   
259400             END-IF                                                       
259500             IF IN-EKH-IDDC-SEND = '47'                                   
259600               MOVE '265'            TO WS-ALLOCATE-DC                    
259700             END-IF                                                       
259800             MOVE SPACE               TO WS-ALLOCATE-DISTR                
259900             MOVE SPACE               TO WS-ALLOCATE-IDVERGL              
260000             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
260100             MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                     
260200             MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF            
260300             MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                     
260400             PERFORM S04-WRITE-W56173A                                    
260500           END-IF                                                         
260600         END-IF                                                           
260700       END-IF                                                             
260800                                                                          
260900       IF SYST-IDSEKVNR = 7                                               
261000         IF BYT02-RENOV                                                   
261100           IF IN-EKH-KVANTAL < 0                                          
261200             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
261300             MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                  
261400             COMPUTE R3-LINE-AMOUNT    =                                  
261500                     IN-EKH-KVANTAL * 0.01                                
261600             MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                
261700             IF IN-EKH-IDDC-SEND = '41'                                   
261800               MOVE '266'             TO WS-ALLOCATE-DC                   
261900             END-IF                                                       
262000             IF IN-EKH-IDDC-SEND = '43'                                   
262100               MOVE '264'             TO WS-ALLOCATE-DC                   
262200             END-IF                                                       
262300             IF IN-EKH-IDDC-SEND = '44'                                   
262400               MOVE '255'             TO WS-ALLOCATE-DC                   
262500             END-IF                                                       
262600             IF IN-EKH-IDDC-SEND = '45'                                   
262700               MOVE '267'             TO WS-ALLOCATE-DC                   
262800             END-IF                                                       
262900             IF IN-EKH-IDDC-SEND = '46'                                   
263000               MOVE '268'             TO WS-ALLOCATE-DC                   
263100             END-IF                                                       
263200             IF IN-EKH-IDDC-SEND = '47'                                   
263300               MOVE '265'            TO WS-ALLOCATE-DC                    
263400             END-IF                                                       
263500             MOVE SPACE               TO WS-ALLOCATE-DISTR                
263600             MOVE SPACE               TO WS-ALLOCATE-IDVERGL              
263700             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
263800             MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                     
263900             MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF            
264000             MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                     
264100             PERFORM S04-WRITE-W56173A                                    
264200           END-IF                                                         
264300         END-IF                                                           
264400       END-IF                                                             
264500                                                                          
264600       IF SYST-IDSEKVNR = 8                                               
264700         IF BYT02-RENOV                                                   
264800           IF IN-EKH-KVANTAL < 0                                          
264900             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
265000             MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                  
265100             COMPUTE R3-LINE-AMOUNT    =                                  
265200                     IN-EKH-KVANTAL * 0.01                                
265300             MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                
265400             IF IN-EKH-IDDC-SEND = '41'                                   
265500               MOVE '266'             TO WS-ALLOCATE-DC                   
265600             END-IF                                                       
265700             IF IN-EKH-IDDC-SEND = '43'                                   
265800               MOVE '264'             TO WS-ALLOCATE-DC                   
265900             END-IF                                                       
266000             IF IN-EKH-IDDC-SEND = '44'                                   
266100               MOVE '255'             TO WS-ALLOCATE-DC                   
266200             END-IF                                                       
266300             IF IN-EKH-IDDC-SEND = '45'                                   
266400               MOVE '267'             TO WS-ALLOCATE-DC                   
266500             END-IF                                                       
266600             IF IN-EKH-IDDC-SEND = '46'                                   
266700               MOVE '268'             TO WS-ALLOCATE-DC                   
266800             END-IF                                                       
266900             IF IN-EKH-IDDC-SEND = '47'                                   
267000               MOVE '265'            TO WS-ALLOCATE-DC                    
267100             END-IF                                                       
267200             MOVE SPACE               TO WS-ALLOCATE-DISTR                
267300             MOVE SPACE               TO WS-ALLOCATE-IDVERGL              
267400             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
267500             MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                     
267600             MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF            
267700             MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                     
267800             PERFORM S04-WRITE-W56173A                                    
267900           END-IF                                                         
268000         END-IF                                                           
268100       END-IF                                                             
268200                                                                          
268300     WHEN 'HEMT'                                                          
268400       IF SYST-IDSEKVNR = 1                                               
268500         IF IN-EKH-SUBEL > 0                                              
268600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
268700           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
268800           IF IN-EKH-KDVALISO = 'USD'                                     
268900             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
269000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
269100           ELSE                                                           
269200             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
269300                                         W-PRKURS                         
269400           END-IF                                                         
269500           MOVE SYST-IDKST          TO WS-RED-IDKST                       
269600           IF WS-RED-IDKST > SPACE                                        
269700             IF IN-EKH-IDDC-SEND = '41'                                   
269800               MOVE '266'           TO WS-RED-IDKST(1:3)                  
269900             END-IF                                                       
270000             IF IN-EKH-IDDC-SEND = '43'                                   
270100               MOVE '264'           TO WS-RED-IDKST(1:3)                  
270200             END-IF                                                       
270300             IF IN-EKH-IDDC-SEND = '44'                                   
270400               MOVE '255'           TO WS-RED-IDKST(1:3)                  
270500             END-IF                                                       
270600             IF IN-EKH-IDDC-SEND = '45'                                   
270700               MOVE '267'           TO WS-RED-IDKST(1:3)                  
270800             END-IF                                                       
270900             IF IN-EKH-IDDC-SEND = '46'                                   
271000               MOVE '268'           TO WS-RED-IDKST(1:3)                  
271100             END-IF                                                       
271200             IF IN-EKH-IDDC-SEND = '47'                                   
271300               MOVE '265'          TO WS-RED-IDKST(1:3)                   
271400             END-IF                                                       
271500             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
271600           ELSE                                                           
271700             MOVE SPACE             TO R3-LINE-COST-CENTER                
271800           END-IF                                                         
271900           MOVE SPACE               TO WS-ALLOCATE-DC                     
272000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
272100           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
272200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
272300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
272400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
272500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
272600           PERFORM S04-WRITE-W56173A                                      
272700         END-IF                                                           
272800       END-IF                                                             
272900                                                                          
273000       IF SYST-IDSEKVNR = 2                                               
273100         IF IN-EKH-SUBEL > 0                                              
273200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
273300           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
273400**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
273500           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
273600           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
273700           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
273800           IF IN-EKH-KDVALISO = 'USD'                                     
273900             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
274000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
274100           ELSE                                                           
274200             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
274300                                         W-PRKURS                         
274400           END-IF                                                         
274500           IF IN-EKH-IDDC-SEND = '41'                                     
274600             MOVE '266'             TO WS-ALLOCATE-DC                     
274700           END-IF                                                         
274800           IF IN-EKH-IDDC-SEND = '43'                                     
274900             MOVE '264'             TO WS-ALLOCATE-DC                     
275000           END-IF                                                         
275100           IF IN-EKH-IDDC-SEND = '44'                                     
275200             MOVE '255'             TO WS-ALLOCATE-DC                     
275300           END-IF                                                         
275400           IF IN-EKH-IDDC-SEND = '45'                                     
275500             MOVE '267'             TO WS-ALLOCATE-DC                     
275600           END-IF                                                         
275700           IF IN-EKH-IDDC-SEND = '46'                                     
275800             MOVE '268'             TO WS-ALLOCATE-DC                     
275900           END-IF                                                         
276000           IF IN-EKH-IDDC-SEND = '47'                                     
276100             MOVE '265'            TO WS-ALLOCATE-DC                      
276200           END-IF                                                         
276300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
276400           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
276500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
276600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
276700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
276800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
276900           PERFORM S04-WRITE-W56173A                                      
277000         END-IF                                                           
277100       END-IF                                                             
277200                                                                          
277300       IF SYST-IDSEKVNR = 3                                               
277400         IF IN-EKH-SUBEL < 0                                              
277500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
277600           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
277700           IF IN-EKH-KDVALISO = 'USD'                                     
277800             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
277900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
278000           ELSE                                                           
278100             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
278200                                         W-PRKURS                         
278300           END-IF                                                         
278400           MOVE SYST-IDKST          TO WS-RED-IDKST                       
278500           IF WS-RED-IDKST > SPACE                                        
278600             IF IN-EKH-IDDC-SEND = '41'                                   
278700               MOVE '266'           TO WS-RED-IDKST(1:3)                  
278800             END-IF                                                       
278900             IF IN-EKH-IDDC-SEND = '43'                                   
279000               MOVE '264'           TO WS-RED-IDKST(1:3)                  
279100             END-IF                                                       
279200             IF IN-EKH-IDDC-SEND = '44'                                   
279300               MOVE '255'           TO WS-RED-IDKST(1:3)                  
279400             END-IF                                                       
279500             IF IN-EKH-IDDC-SEND = '45'                                   
279600               MOVE '267'           TO WS-RED-IDKST(1:3)                  
279700             END-IF                                                       
279800             IF IN-EKH-IDDC-SEND = '46'                                   
279900               MOVE '268'           TO WS-RED-IDKST(1:3)                  
280000             END-IF                                                       
280100             IF IN-EKH-IDDC-SEND = '47'                                   
280200               MOVE '265'          TO WS-RED-IDKST(1:3)                   
280300             END-IF                                                       
280400             MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                
280500           ELSE                                                           
280600             MOVE SPACE             TO R3-LINE-COST-CENTER                
280700           END-IF                                                         
280800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
280900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
281000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
281100           PERFORM S04-WRITE-W56173A                                      
281200         END-IF                                                           
281300       END-IF                                                             
281400                                                                          
281500       IF SYST-IDSEKVNR = 4                                               
281600         IF IN-EKH-SUBEL < 0                                              
281700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
281800           MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                         
281900**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
282000           MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                       
282100           MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                    
282200           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
282300           IF IN-EKH-KDVALISO = 'USD'                                     
282400             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
282500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
282600           ELSE                                                           
282700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
282800                                                 W-PRKURS                 
282900           END-IF                                                         
283000           IF IN-EKH-IDDC-SEND = '41'                                     
283100             MOVE '266'             TO WS-ALLOCATE-DC                     
283200           END-IF                                                         
283300           IF IN-EKH-IDDC-SEND = '43'                                     
283400             MOVE '264'             TO WS-ALLOCATE-DC                     
283500           END-IF                                                         
283600           IF IN-EKH-IDDC-SEND = '44'                                     
283700             MOVE '255'             TO WS-ALLOCATE-DC                     
283800           END-IF                                                         
283900           IF IN-EKH-IDDC-SEND = '45'                                     
284000             MOVE '267'             TO WS-ALLOCATE-DC                     
284100           END-IF                                                         
284200           IF IN-EKH-IDDC-SEND = '46'                                     
284300             MOVE '268'             TO WS-ALLOCATE-DC                     
284400           END-IF                                                         
284500           IF IN-EKH-IDDC-SEND = '47'                                     
284600             MOVE '265'            TO WS-ALLOCATE-DC                      
284700           END-IF                                                         
284800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
284900           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
285000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
285100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
285200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
285300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
285400           PERFORM S04-WRITE-W56173A                                      
285500         END-IF                                                           
285600       END-IF                                                             
285700                                                                          
285800     WHEN 'DDI'                                                           
285900       IF IN-EKH-SUBEL < ZERO                                             
286000         IF SYST-IDSEKVNR = 1                                             
286100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
286200           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
286300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
286400                   IN-EKH-SUBEL                                           
286500           IF IN-EKH-KDVALISO = 'USD'                                     
286600             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
286700           ELSE                                                           
286800             MOVE ZEROES              TO R3-LINE-AMOUNT                   
286900           END-IF                                                         
287000           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
287100           MOVE SPACE               TO R3-LINE-ALLOCATE                   
287200           PERFORM S04-WRITE-W56173A                                      
287300         END-IF                                                           
287400       ELSE                                                               
287500         IF SYST-IDSEKVNR = 2                                             
287600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
287700           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
287800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
287900                   IN-EKH-SUBEL                                           
288000           IF IN-EKH-KDVALISO = 'USD'                                     
288100             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
288200           ELSE                                                           
288300             MOVE ZEROES              TO R3-LINE-AMOUNT                   
288400           END-IF                                                         
288500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
288600           MOVE SPACE               TO R3-LINE-ALLOCATE                   
288700           PERFORM S04-WRITE-W56173A                                      
288800         END-IF                                                           
288900       END-IF                                                             
289000     END-EVALUATE                                                         
289100     .                                                                    
289200     EJECT                                                                
289300                                                                          
289400 CED-MAIN-EVENT-204 SECTION.                                              
289500     EVALUATE IN-EKH-KDEKSHT                                              
289600     WHEN '301'                                                           
289700          PERFORM CEDA-SUB-EVENT-204-301                                  
289800     END-EVALUATE                                                         
289900     .                                                                    
290000     EJECT                                                                
290100                                                                          
290200 CEDA-SUB-EVENT-204-301 SECTION.                                          
290300     EVALUATE IN-EKH-KDEKNIVA                                             
290400     WHEN 'DET'                                                           
290500         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
290600*        MOVE IN-EKH-IDARTNR      TO TEST-IDARTNR                         
290700* R-FAKTURA                                                               
290800       IF SYST-IDSEKVNR = 1                                               
290900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
291000         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
291100         COMPUTE R3-LINE-AMOUNT-LC =                                      
291200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
291300         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
291400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
291500         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
291600         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
291700         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
291800         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
291900         PERFORM S03-WRITE-W56172                                         
292000       END-IF                                                             
292100                                                                          
292200       IF SYST-IDSEKVNR = 2                                               
292300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
292400         MOVE WS-R3-ACCOUNT-7     TO WS-ACCOUNT                           
292500**** FLYTTA LOKALT PRODUKTSLAG TILL DE 2 SISTA I KONTO                    
292600         MOVE IN-EKH-KDPSLLOC     TO WS-ACCOUNT-3                         
292700         MOVE WS-ACCOUNT(1:7)     TO WS-R3-ACCOUNT-7                      
292800         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
292900         COMPUTE R3-LINE-AMOUNT-LC =                                      
293000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
293100         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
293200         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
293300         IF IN-EKH-IDDC-SEND = '41'                                       
293400           MOVE '266'             TO WS-ALLOCATE-DC                       
293500         END-IF                                                           
293600         IF IN-EKH-IDDC-SEND = '43'                                       
293700           MOVE '264'             TO WS-ALLOCATE-DC                       
293800         END-IF                                                           
293900         IF IN-EKH-IDDC-SEND = '44'                                       
294000           MOVE '255'             TO WS-ALLOCATE-DC                       
294100         END-IF                                                           
294200         IF IN-EKH-IDDC-SEND = '45'                                       
294300           MOVE '267'             TO WS-ALLOCATE-DC                       
294400         END-IF                                                           
294500         IF IN-EKH-IDDC-SEND = '46'                                       
294600           MOVE '268'             TO WS-ALLOCATE-DC                       
294700         END-IF                                                           
294800         IF IN-EKH-IDDC-SEND = '47'                                       
294900           MOVE '265'            TO WS-ALLOCATE-DC                        
295000         END-IF                                                           
295100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
295200         MOVE SPACE               TO WS-ALLOCATE-IDVERGL                  
295300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
295400         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
295500         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
295600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
295700         PERFORM S03-WRITE-W56172                                         
295800       END-IF                                                             
295900                                                                          
296000       IF SYST-IDSEKVNR = 3                                               
296100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
296200         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
296300         COMPUTE R3-LINE-AMOUNT-LC =                                      
296400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
296500         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
296600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
296700         MOVE SPACE               TO WS-ALLOCATE-DC                       
296800         IF IN-EKH-IDDC-SEND = '41'                                       
296900           MOVE '266'             TO WS-ALLOCATE-DC                       
297000         END-IF                                                           
297100         IF IN-EKH-IDDC-SEND = '43'                                       
297200           MOVE '264'             TO WS-ALLOCATE-DC                       
297300         END-IF                                                           
297400         IF IN-EKH-IDDC-SEND = '44'                                       
297500           MOVE '255'             TO WS-ALLOCATE-DC                       
297600         END-IF                                                           
297700         IF IN-EKH-IDDC-SEND = '45'                                       
297800           MOVE '267'             TO WS-ALLOCATE-DC                       
297900         END-IF                                                           
298000         IF IN-EKH-IDDC-SEND = '46'                                       
298100           MOVE '268'             TO WS-ALLOCATE-DC                       
298200         END-IF                                                           
298300         IF IN-EKH-IDDC-SEND = '47'                                       
298400           MOVE '265'            TO WS-ALLOCATE-DC                        
298500         END-IF                                                           
298600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
298700         MOVE SPACE               TO WS-ALLOCATE-IDVERGL                  
298800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
298900         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
299000         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
299100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
299200         PERFORM S03-WRITE-W56172                                         
299300       END-IF                                                             
299400                                                                          
299500     WHEN 'FÖRS'                                                          
299600     WHEN 'FRAKT'                                                         
299700     WHEN 'EMB'                                                           
299800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
299900       MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                        
300000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
300100               IN-EKH-SUBEL * -1                                          
300200       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
300300       MOVE SYST-IDKST              TO WS-RED-IDKST                       
300400       IF WS-RED-IDKST > SPACE                                            
300500         IF IN-EKH-IDDC-SEND = '41'                                       
300600           MOVE '266'           TO WS-RED-IDKST(1:3)                      
300700         END-IF                                                           
300800         IF IN-EKH-IDDC-SEND = '43'                                       
300900           MOVE '264'           TO WS-RED-IDKST(1:3)                      
301000         END-IF                                                           
301100         IF IN-EKH-IDDC-SEND = '44'                                       
301200           MOVE '255'           TO WS-RED-IDKST(1:3)                      
301300         END-IF                                                           
301400         IF IN-EKH-IDDC-SEND = '45'                                       
301500           MOVE '267'           TO WS-RED-IDKST(1:3)                      
301600         END-IF                                                           
301700         IF IN-EKH-IDDC-SEND = '46'                                       
301800           MOVE '268'           TO WS-RED-IDKST(1:3)                      
301900         END-IF                                                           
302000         IF IN-EKH-IDDC-SEND = '47'                                       
302100           MOVE '265'          TO WS-RED-IDKST(1:3)                       
302200         END-IF                                                           
302300         MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                    
302400       ELSE                                                               
302500         MOVE SPACE             TO R3-LINE-COST-CENTER                    
302600       END-IF                                                             
302700       MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                           
302800       MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                   
302900       MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                           
303000                                                                          
303100       PERFORM S04-WRITE-W56173A                                          
303200                                                                          
303300     END-EVALUATE                                                         
303400     .                                                                    
303500     EJECT                                                                
303600 CEE-MAIN-EVENT-303 SECTION.                                              
303700                                                                          
303800     EVALUATE IN-EKH-KDEKSHT                                              
303900      WHEN '371'                                                          
304000        PERFORM CEEA-SUB-EVENT-303-371                                    
304100      WHEN '377'                                                          
304200        PERFORM CEEB-SUB-EVENT-303-377                                    
304300     END-EVALUATE                                                         
304400     .                                                                    
304500     EJECT                                                                
304600                                                                          
304700 CEEA-SUB-EVENT-303-371 SECTION.                                          
304800     EVALUATE IN-EKH-KDEKNIVA                                             
304900     WHEN 'DET'                                                           
305000       IF SYST-IDSEKVNR = 1                                               
305100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
305200         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
305300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
305400           IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-US3              
305500         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
305600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
305700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
305800         MOVE SPACE               TO WS-ALLOCATE-IDVERGL                  
305900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
306000         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
306100         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
306200         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
306300         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
306400         PERFORM S03-WRITE-W56172                                         
306500       END-IF                                                             
306600                                                                          
306700     WHEN 'LAND'                                                          
306800       IF SYST-IDSEKVNR = 1                                               
306900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
307000         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
307100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
307200          IN-EKH-SUBEL / WS-PRKURS-US3                                    
307300         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
307400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
307500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
307600         MOVE SPACE               TO WS-ALLOCATE-IDVERGL                  
307700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
307800         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
307900         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
308000         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
308100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
308200         PERFORM S03-WRITE-W56172                                         
308300       END-IF                                                             
308400                                                                          
308500     WHEN 'DDI'                                                           
308600       IF IN-EKH-SUBEL < ZERO                                             
308700         IF SYST-IDSEKVNR = 1                                             
308800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
308900           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
309000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
309100                   IN-EKH-SUBEL                                           
309200           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
309300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
309400           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
309500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
309600           PERFORM S04-WRITE-W56173A                                      
309700         END-IF                                                           
309800       ELSE                                                               
309900         IF SYST-IDSEKVNR = 2                                             
310000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
310100           MOVE WS-R3-ACCOUNT-7 TO R3-LINE-ACCOUNT                        
310200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
310300                   IN-EKH-SUBEL                                           
310400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
310500           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
310600           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
310700           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
310800           PERFORM S04-WRITE-W56173A                                      
310900         END-IF                                                           
311000       END-IF                                                             
311100                                                                          
311200     END-EVALUATE                                                         
311300     .                                                                    
311400     EJECT                                                                
311500 CEEB-SUB-EVENT-303-377 SECTION.                                          
311600     EVALUATE IN-EKH-KDEKNIVA                                             
311700     WHEN 'DET'                                                           
311800       IF SYST-IDSEKVNR = 1                                               
311900         IF IN-EKH-KVANTAL < 0                                            
312000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
312100           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
312200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
312300             IN-EKH-KVANTAL * IN-EKH-PRARTNTO                             
312400           COMPUTE R3-LINE-AMOUNT ROUNDED =                               
312500               R3-LINE-AMOUNT-LC / WS-PRKURS-US2                          
312600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
312700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
312800           MOVE SPACE               TO WS-ALLOCATE-IDVERGL                
312900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
313000           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
313100         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
313200         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
313300         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
313400           PERFORM S04-WRITE-W56173A                                      
313500         END-IF                                                           
313600       END-IF                                                             
313700                                                                          
313800     END-EVALUATE                                                         
313900     .                                                                    
314000     EJECT                                                                
314100 CF-BUILD-COMMON-210-PART SECTION.                                        
314200     MOVE SPACE              TO R3-LINE-R3                                
314300     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
314400                                R3-LINE-DUE-DATE                          
314500                                R3-LINE-AMOUNT                            
314600                                R3-LINE-AMOUNT-LC                         
314700                                R3-LINE-TAX-AMOUNT                        
314800                                R3-LINE-TAX-AMOUNT-LC                     
314900                                R3-LINE-NUMBER-OF-DAYS                    
315000                                R3-LINE-QUANTITY                          
315100                                R3-LINE-SAMNR                             
315200     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
315300     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
315400     MOVE 'US01'             TO R3-LINE-COMPANY-CODE                      
315500     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
315600     IF SYST-KDPOST = '31'                                                
315700       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
315800     ELSE                                                                 
315900       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
316000     END-IF                                                               
316100     IF SYST-IDPRCTR NOT = SPACE                                          
316200       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
316300       IF WS-PRCTR-PRODSL = '??'                                          
316400***** HÄR FLYTTAR VI LOKALT PRODUKTSLAG, OM USA VILL ANVÄNDA DET          
316500         MOVE IN-EKH-KDPSLLOC      TO WS-PRCTR-PRODSL-DISP                
316600         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
316700       END-IF                                                             
316800       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
316900     END-IF                                                               
317000     .                                                                    
317100     EJECT                                                                
317200                                                                          
317300 CG-SCHEDULE-LINE-AP SECTION.                                             
317400     MOVE NEJ                     TO WS-HEADER-SW                         
317500     MOVE JA                      TO WS-LINE-SW                           
317600     EVALUATE IN-EKH-KDEKHHT                                              
317700     WHEN '102'                                                           
317800       IF IN-EKH-KDEKSHT = '107'                                          
317900         PERFORM CGA-MAIN-EVENT-102-107                                   
318000       ELSE                                                               
318100         IF IN-EKH-KDEKSHT = '130'                                        
318200         OR IN-EKH-KDEKSHT = '134'                                        
318300           IF IN-EKH-KDEKSHT = '130'                                      
318400             PERFORM CGA-MAIN-EVENT-102-130                               
318500           ELSE                                                           
318600             PERFORM CGA-MAIN-EVENT-102-134                               
318700           END-IF                                                         
318800         ELSE                                                             
318900           PERFORM CGA-MAIN-EVENT-102                                     
319000         END-IF                                                           
319100       END-IF                                                             
319200     WHEN '303'                                                           
319300       IF IN-EKH-KDEKSHT = '371'                                          
319400         PERFORM S81-GET-CURRENCY-RATE                                    
319500         PERFORM CGB-MAIN-EVENT-303-371                                   
319600       ELSE                                                               
319700         PERFORM CGB-MAIN-EVENT-303                                       
319800       END-IF                                                             
319900                                                                          
320000     END-EVALUATE                                                         
320100     .                                                                    
320200     EJECT                                                                
320300                                                                          
320400 CGA-MAIN-EVENT-102-107 SECTION.                                          
320500     EVALUATE IN-EKH-KDEKNIVA                                             
320600     WHEN 'SUM'                                                           
320700       IF IN-EKH-SUBEL > ZERO                                             
320800         IF SYST-IDSEKVNR = 1                                             
320900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
321000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
321100           IF IN-EKH-KDVALISO = 'USD'                                     
321200             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
321300                     R3-LINE-AMOUNT    / W-PRKURS                         
321400           ELSE                                                           
321500             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
321600                     R3-LINE-AMOUNT    * W-PRKURS                         
321700           END-IF                                                         
321800           PERFORM S10-VATCODE                                            
321900           IF IN-EKH-SUVAT = ZERO                                         
322000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
322100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
322200           ELSE                                                           
322300             IF IN-EKH-KDVALISO = 'USD'                                   
322400               MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                
322500               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
322600                       R3-LINE-TAX-AMOUNT    / W-PRKURS                   
322700             ELSE                                                         
322800               MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                
322900               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
323000                       R3-LINE-TAX-AMOUNT    * W-PRKURS                   
323100             END-IF                                                       
323200           END-IF                                                         
323300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
323400                                                                          
323500           PERFORM S04-WRITE-W56173A                                      
323600         END-IF                                                           
323700       END-IF                                                             
323800                                                                          
323900       IF IN-EKH-SUBEL < ZERO                                             
324000         IF SYST-IDSEKVNR = 2                                             
324100           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
324200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
324300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
324400                   R3-LINE-AMOUNT    / W-PRKURS                           
324500           PERFORM S10-VATCODE                                            
324600           IF IN-EKH-SUVAT = ZERO                                         
324700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
324800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
324900           ELSE                                                           
325000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
325100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
325200                     R3-LINE-TAX-AMOUNT    / W-PRKURS                     
325300           END-IF                                                         
325400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
325500                                                                          
325600           PERFORM S04-WRITE-W56173A                                      
325700         END-IF                                                           
325800       END-IF                                                             
325900     END-EVALUATE                                                         
326000     .                                                                    
326100     EJECT                                                                
326200 CGA-MAIN-EVENT-102-130 SECTION.                                          
326300     EVALUATE IN-EKH-KDEKNIVA                                             
326400     WHEN 'SUM'                                                           
326500       IF IN-EKH-SUBEL > ZERO                                             
326600         IF SYST-IDSEKVNR = 1                                             
326700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
326800           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
326900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
327000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
327100                   R3-LINE-AMOUNT    / WS-PRKURS-US * -1                  
327200           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
327300           PERFORM S10-VATCODE                                            
327400           IF IN-EKH-SUVAT = ZERO                                         
327500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
327600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
327700           ELSE                                                           
327800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
327900             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
328000                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-US * -1            
328100             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
328200           END-IF                                                         
328300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
328400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
328500           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
328600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
328700                                                                          
328800           PERFORM S04-WRITE-W56173A                                      
328900         END-IF                                                           
329000       END-IF                                                             
329100                                                                          
329200       IF IN-EKH-SUBEL < ZERO                                             
329300         IF SYST-IDSEKVNR = 2                                             
329400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
329500           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
329600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
329700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
329800                   R3-LINE-AMOUNT    / WS-PRKURS-US                       
329900           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
330000           PERFORM S10-VATCODE                                            
330100           IF IN-EKH-SUVAT = ZERO                                         
330200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
330300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
330400           ELSE                                                           
330500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
330600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
330700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-US                 
330800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
330900           END-IF                                                         
331000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
331100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
331200           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
331300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
331400                                                                          
331500           PERFORM S04-WRITE-W56173A                                      
331600         END-IF                                                           
331700       END-IF                                                             
331800     END-EVALUATE                                                         
331900     .                                                                    
332000     EJECT                                                                
332100 CGA-MAIN-EVENT-102-134 SECTION.                                          
332200     EVALUATE IN-EKH-KDEKNIVA                                             
332300     WHEN 'SUM'                                                           
332400       IF IN-EKH-SUBEL > ZERO                                             
332500         IF SYST-IDSEKVNR = 1                                             
332600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
332700           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
332800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
332900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
333000                   R3-LINE-AMOUNT    / WS-PRKURS-US  * -1                 
333100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
333200           PERFORM S10-VATCODE                                            
333300           IF IN-EKH-SUVAT = ZERO                                         
333400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
333500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
333600           ELSE                                                           
333700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
333800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
333900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-US  * -1           
334000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
334100           END-IF                                                         
334200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
334300                                                                          
334400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
334500           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
334600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
334700           PERFORM S04-WRITE-W56173A                                      
334800         END-IF                                                           
334900       END-IF                                                             
335000                                                                          
335100       IF IN-EKH-SUBEL < ZERO                                             
335200         IF SYST-IDSEKVNR = 2                                             
335300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
335400           MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                    
335500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
335600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
335700                   R3-LINE-AMOUNT    / WS-PRKURS-US                       
335800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
335900           PERFORM S10-VATCODE                                            
336000           IF IN-EKH-SUVAT = ZERO                                         
336100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
336200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
336300           ELSE                                                           
336400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
336500             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
336600                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-US                 
336700             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
336800           END-IF                                                         
336900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
337000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
337100           MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL               
337200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
337300                                                                          
337400           PERFORM S04-WRITE-W56173A                                      
337500         END-IF                                                           
337600       END-IF                                                             
337700     END-EVALUATE                                                         
337800     .                                                                    
337900     EJECT                                                                
338000 CGA-MAIN-EVENT-102     SECTION.                                          
338100     EVALUATE IN-EKH-KDEKNIVA                                             
338200     WHEN 'SUM'                                                           
338300       IF IN-EKH-SUBEL > ZERO                                             
338400         IF SYST-IDSEKVNR = 1                                             
338500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
338600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
338700           PERFORM S10-VATCODE                                            
338800           IF IN-EKH-SUVAT = ZERO                                         
338900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
339000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
339100           ELSE                                                           
339200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
339300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
339400           END-IF                                                         
339500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
339600                                                                          
339700           PERFORM S04-WRITE-W56173A                                      
339800         END-IF                                                           
339900       END-IF                                                             
340000                                                                          
340100       IF IN-EKH-SUBEL < ZERO                                             
340200         IF SYST-IDSEKVNR = 2                                             
340300           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
340400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
340500           PERFORM S10-VATCODE                                            
340600           IF IN-EKH-SUVAT = ZERO                                         
340700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
340800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
340900           ELSE                                                           
341000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
341100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
341200           END-IF                                                         
341300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
341400                                                                          
341500           PERFORM S04-WRITE-W56173A                                      
341600         END-IF                                                           
341700       END-IF                                                             
341800     END-EVALUATE                                                         
341900     .                                                                    
342000     EJECT                                                                
342100                                                                          
342200 CGB-MAIN-EVENT-303 SECTION.                                              
342300                                                                          
342400     EVALUATE IN-EKH-KDEKNIVA                                             
342500     WHEN 'SUM'                                                           
342600       IF SYST-IDSEKVNR = 1                                               
342700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
342800         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
342900         MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                        
343000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
343100               R3-LINE-AMOUNT / WS-PRKURS-US3                             
343200         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
343300         PERFORM S10-VATCODE                                              
343400         IF IN-EKH-SUVAT = ZERO                                           
343500           MOVE ZERO             TO R3-LINE-TAX-AMOUNT                    
343600           MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC                 
343700         ELSE                                                             
343800           MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                    
343900           COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                        
344000                 R3-LINE-TAX-AMOUNT / WS-PRKURS-US3                       
344100           MOVE R3-LINE-TAX-AMOUNT-LC   TO R3-LINE-TAX-AMOUNT             
344200         END-IF                                                           
344300         MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                      
344400         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
344500         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
344600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
344700                                                                          
344800         PERFORM S04-WRITE-W56173A                                        
344900       END-IF                                                             
345000                                                                          
345100     END-EVALUATE                                                         
345200     .                                                                    
345300     EJECT                                                                
345400                                                                          
345500 CGB-MAIN-EVENT-303-371 SECTION.                                          
345600                                                                          
345700     EVALUATE IN-EKH-KDEKNIVA                                             
345800     WHEN 'SUM'                                                           
345900       IF SYST-IDSEKVNR = 1                                               
346000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
346100         MOVE WS-R3-ACCOUNT-7     TO R3-LINE-ACCOUNT                      
346200         MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                        
346300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
346400               R3-LINE-AMOUNT / WS-PRKURS-US3                             
346500         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
346600         PERFORM S10-VATCODE                                              
346700         IF IN-EKH-SUVAT = ZERO                                           
346800           MOVE ZERO             TO R3-LINE-TAX-AMOUNT                    
346900           MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC                 
347000         ELSE                                                             
347100           MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                    
347200           COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                        
347300                 R3-LINE-TAX-AMOUNT / WS-PRKURS-US3                       
347400           MOVE R3-LINE-TAX-AMOUNT-LC   TO R3-LINE-TAX-AMOUNT             
347500         END-IF                                                           
347600         MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                      
347700         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
347800         MOVE IN-EKH-IDVERGL      TO WS-LINE-TEXT-IDVERGL                 
347900         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
348000                                                                          
348100         PERFORM S04-WRITE-W56173A                                        
348200       END-IF                                                             
348300                                                                          
348400     END-EVALUATE                                                         
348500     .                                                                    
348600     EJECT                                                                
348700                                                                          
348800 CH-BUILD-COMMON-310-PART SECTION.                                        
348900     MOVE SPACE              TO R3-LINE-R3                                
349000     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
349100                                R3-LINE-DUE-DATE                          
349200                                R3-LINE-AMOUNT                            
349300                                R3-LINE-AMOUNT-LC                         
349400                                R3-LINE-TAX-AMOUNT                        
349500                                R3-LINE-TAX-AMOUNT-LC                     
349600                                R3-LINE-NUMBER-OF-DAYS                    
349700                                R3-LINE-QUANTITY                          
349800                                R3-LINE-SAMNR                             
349900     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
350000     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
350100     MOVE 'US01'             TO R3-LINE-COMPANY-CODE                      
350200     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
350300     IF SYST-KDPOST = '31'                                                
350400       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
350500     ELSE                                                                 
350600       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
350700     END-IF                                                               
350800     .                                                                    
350900     EJECT                                                                
351000                                                                          
351100 CI-SCHEDULE-LINE-AR SECTION.                                             
351200     MOVE NEJ                     TO WS-HEADER-SW                         
351300     MOVE JA                      TO WS-LINE-SW                           
351400     EVALUATE IN-EKH-KDEKHHT                                              
351500     WHEN '204'                                                           
351600         PERFORM CIA-MAIN-EVENT-204                                       
351700     END-EVALUATE                                                         
351800     .                                                                    
351900     EJECT                                                                
352000                                                                          
352100 CIA-MAIN-EVENT-204     SECTION.                                          
352200     EVALUATE IN-EKH-KDEKNIVA                                             
352300     WHEN 'SUM'                                                           
352400       IF IN-EKH-SUBEL > ZERO                                             
352500         IF SYST-IDSEKVNR = 1                                             
352600           MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                    
352700           MOVE WS-R3-ACCOUNT-7    TO R3-LINE-ACCOUNT                     
352800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
352900           COMPUTE R3-LINE-AMOUNT ROUNDED =                               
353000                   R3-LINE-AMOUNT-LC / W-PRKURS                           
353100           PERFORM S10-VATCODE                                            
353200           IF IN-EKH-SUVAT = ZERO                                         
353300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
353400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
353500           ELSE                                                           
353600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
353700             COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                         
353800                     R3-LINE-TAX-AMOUNT-LC / W-PRKURS                     
353900           END-IF                                                         
354000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
354100                                                                          
354200           PERFORM S04-WRITE-W56173A                                      
354300         END-IF                                                           
354400       END-IF                                                             
354500                                                                          
354600     END-EVALUATE                                                         
354700     .                                                                    
354800     EJECT                                                                
354900                                                                          
355000 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
355100     MOVE ZERO             TO LOGG-W56173                                 
355200     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
355300     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
355400     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
355500     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
355600     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
355700     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
355800     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
355900     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
356000     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
356100     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
356200     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
356300                                                                          
356400****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
356500     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
356600     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
356700     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
356800     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
356900     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
357000     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
357100     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
357200     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
357300     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
357400     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
357500     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
357600     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
357700     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
357800     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
357900     .                                                                    
358000     EJECT                                                                
358100                                                                          
358200 I-SOEK-MARKUP  SECTION.                                                  
358300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
358400* SÖK UPP RÄTT MARKUP-FAKTOR I TABELLEN                         *         
358500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
358600                                                                          
358700     MOVE NEJ                  TO PRODKOD-TEST                            
358800     MOVE 1                    TO WS-IND                                  
358900                                                                          
359000     PERFORM UNTIL PRODKOD-FINNS OR  WS-IND > MARKUP-TAB-MAX              
359100       IF MARKUP-LPC (WS-IND) = IN-EKH-KDPSLLOC                           
359200         MOVE JA               TO PRODKOD-TEST                            
359300       ELSE                                                               
359400         ADD 1                 TO WS-IND                                  
359500       END-IF                                                             
359600     END-PERFORM                                                          
359700                                                                          
359800     IF PRODKOD-FINNS                                                     
359900         MOVE MARKUP-FAKTOR-USA (WS-IND)                                  
360000                               TO WS-MARKUP                               
360100     ELSE                                                                 
360200       MOVE 1 TO WS-MARKUP                                                
360300     END-IF                                                               
360400                                                                          
360500     .                                                                    
360600     EJECT                                                                
360700                                                                          
360800 Z-FINI SECTION.                                                          
360900     CLOSE W56166                                                         
361000           W56170                                                         
361100           W56171A                                                        
361200           W56172A                                                        
361300           W56173A                                                        
361400           W56175                                                         
361500           W5616N                                                         
361600           W51310                                                         
361700                                                                          
361800     MOVE 'S' TO POSTSUM-OPKOD                                            
361900     CALL POSTSUM USING POSTSUM-PARM                                      
362000     .                                                                    
362100     EJECT                                                                
362200                                                                          
362300 S01-READ-W56166  SECTION.                                                
362400     READ W56166 INTO IN-AREA                                             
362500     AT END                                                               
362600        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
362700        SET END-OF-W56166 TO TRUE                                         
362800                                                                          
362900     NOT AT END                                                           
363000        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
363100        MOVE 'W56166'     TO POSTSUM-FDNAMN                               
363200        MOVE 'W56168D1'   TO POSTSUM-DDNAMN2                              
363300        CALL POSTSUM USING POSTSUM-PARM                                   
363400     END-READ                                                             
363500     .                                                                    
363600                                                                          
363700 S02-WRITE-W56171A SECTION.                                               
363800     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
363900     MOVE SPACE                 TO 71LINE-POST                            
364000     IF WS-LINE-SW = JA                                                   
364100       IF IN-EKH-KDSORT = 'SW'                                            
364200         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
364300         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
364400         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
364500       ELSE                                                               
364600         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
364700         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
364800         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
364900       END-IF                                                             
365000       WRITE 71LINE-POST        FROM R3-LINE-R3                           
365100       PERFORM S20-CREATE-WRITE-LOG                                       
365200     ELSE                                                                 
365300       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
365400     END-IF                                                               
365500                                                                          
365600     IF WS-LINE-SW = JA                                                   
365700       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
365800     ELSE                                                                 
365900       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
366000     END-IF                                                               
366100     MOVE 'W56171A'             TO POSTSUM-FDNAMN                         
366200     MOVE 'W56168D2'            TO POSTSUM-DDNAMN2                        
366300     CALL POSTSUM USING POSTSUM-PARM                                      
366400     .                                                                    
366500                                                                          
366600 S002-WRITE-W56171A-HEAD SECTION.                                         
366700     MOVE SPACE                 TO 71LINE-POST                            
366800     IF IN-EKH-KDSORT = 'SW'                                              
366900       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
367000       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
367100       MOVE WS-TEXT           TO R3-LINE-TEXT                             
367200     ELSE                                                                 
367300       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
367400       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
367500       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
367600     END-IF                                                               
367700     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
367800                                                                          
367900     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
368000     MOVE 'W56171A'             TO POSTSUM-FDNAMN                         
368100     MOVE 'W56168D2'            TO POSTSUM-DDNAMN2                        
368200     CALL POSTSUM USING POSTSUM-PARM                                      
368300     .                                                                    
368400                                                                          
368500 S03-WRITE-W56172 SECTION.                                                
368600     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
368700     IF IN-EKH-KDSORT = 'SW'                                              
368800       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
368900       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
369000       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
369100     ELSE                                                                 
369200       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
369300       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
369400       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
369500     END-IF                                                               
369600     WRITE 72LINE-POST        FROM R3-LINE-R3                             
369700                                                                          
369800     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
369900     MOVE 'W56172A'           TO POSTSUM-FDNAMN                           
370000     MOVE 'W56168D3'          TO POSTSUM-DDNAMN2                          
370100     CALL POSTSUM USING POSTSUM-PARM                                      
370200                                                                          
370300     PERFORM S20-CREATE-WRITE-LOG                                         
370400     .                                                                    
370500                                                                          
370600 S04-WRITE-W56173A SECTION.                                               
370700     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
370800     MOVE SPACE                 TO 73LINE-POST                            
370900     IF WS-LINE-SW = JA                                                   
371000       IF IN-EKH-KDSORT = 'SW'                                            
371100         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
371200         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
371300         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
371400       ELSE                                                               
371500         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
371600         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
371700         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
371800       END-IF                                                             
371900       WRITE 73LINE-POST        FROM R3-LINE-R3                           
372000       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
372100     ELSE                                                                 
372200       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
372300       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
372400     END-IF                                                               
372500                                                                          
372600     MOVE 'W56173A'             TO POSTSUM-FDNAMN                         
372700     MOVE 'W56168D4'            TO POSTSUM-DDNAMN2                        
372800     CALL POSTSUM USING POSTSUM-PARM                                      
372900                                                                          
373000     IF WS-LINE-SW = JA                                                   
373100       PERFORM S20-CREATE-WRITE-LOG                                       
373200     END-IF                                                               
373300     .                                                                    
373400                                                                          
373500 S004-WRITE-W56173A-HEAD SECTION.                                         
373600     MOVE SPACE                 TO 73LINE-POST                            
373700     IF IN-EKH-KDSORT = 'SW'                                              
373800       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
373900       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
374000       MOVE WS-TEXT           TO R3-LINE-TEXT                             
374100     ELSE                                                                 
374200       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
374300       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
374400       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
374500     END-IF                                                               
374600     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
374700                                                                          
374800     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
374900     MOVE 'W56173A'             TO POSTSUM-FDNAMN                         
375000     MOVE 'W56168D4'            TO POSTSUM-DDNAMN2                        
375100     CALL POSTSUM USING POSTSUM-PARM                                      
375200     .                                                                    
375300                                                                          
375400 S10-VATCODE SECTION.                                                     
375500     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
375600     PERFORM IMS-GU-WDB601                                                
375700     IF DCS-KDDC = SPACE                                                  
375800       MOVE NEJ              TO WDB6-A-SW                                 
375900     ELSE                                                                 
376000       MOVE JA               TO WDB6-A-SW                                 
376100     END-IF                                                               
376200                                                                          
376300     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
376400     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
376500     IF IN-EKH-SUVAT = ZERO                                               
376600       MOVE '  '     TO R3-LINE-TAX-CODE                                  
376700     ELSE                                                                 
376800       MOVE '  '     TO R3-LINE-TAX-CODE                                  
376900     END-IF                                                               
377000     IF IN-EKH-BEVAT = 'XX'                                               
377100       MOVE '  '     TO R3-LINE-TAX-CODE                                  
377200     END-IF                                                               
377300     .                                                                    
377400     EJECT                                                                
377500                                                                          
377600 S20-CREATE-WRITE-LOG SECTION.                                            
377700     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
377800     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
377900     IF SYST-IDPTYP = '610'                                               
378000       MOVE R3-LINE-ACCOUNT(1:7)      TO LOGG-IDKONTO                     
378100     ELSE                                                                 
378200       MOVE ZERO                      TO WS-IDLEVNR                       
378300       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
378400                          FOR CHARACTERS BEFORE INITIAL SPACE             
378500       IF WS-IDLEVNR   > ZERO                                             
378600          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
378700                                      TO LOGG-IDKONTO                     
378800       END-IF                                                             
378900     END-IF                                                               
379000     IF R3-LINE-COST-CENTER NOT = SPACE                                   
379100       MOVE R3-LINE-COST-CENTER(1:6)  TO LOGG-IDKST                       
379200     END-IF                                                               
379300     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
379400     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
379500     MOVE R3-LINE-AMOUNT-LC           TO LOGG-SUBEL                       
379600     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
379700     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
379800                                                                          
379900     PERFORM S21-WRITE-W56175                                             
380000     PERFORM S22-WRITE-W56170                                             
380100                                                                          
380200     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
380300       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
380400       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
380500       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
380600                                                                          
380700       PERFORM S21-WRITE-W56175                                           
380800     END-IF                                                               
380900     .                                                                    
381000     EJECT                                                                
381100                                                                          
381200 S21-WRITE-W56175 SECTION.                                                
381300     IF DCS-IDDC NOT = LOGG-IDDC                                          
381400        MOVE LOGG-IDDC TO W-IDDC-B6                                       
381500        PERFORM IMS-GU-WDB601                                             
381600     END-IF                                                               
381700     IF DCS-KDDC = SPACE                                                  
381800       MOVE NEJ              TO WDB6-A-SW                                 
381900     ELSE                                                                 
382000       MOVE JA               TO WDB6-A-SW                                 
382100     END-IF                                                               
382200                                                                          
382300     IF LOGG-KDEKHHT = '501' AND LOGG-KDEKSHT = '501'                     
382400       IF  WDB6-A-FINNS                                                   
382500       AND (DCS-NDC-PF                                                    
382600       OR   DCS-SDC)                                                      
382700         MOVE 'Y'     TO LOGG-FLLSBOK                                     
382800       END-IF                                                             
382900     END-IF                                                               
383000     IF  WDB6-A-FINNS                                                     
383100     AND DCS-DDC                                                          
383200       MOVE 'N'       TO LOGG-FLLSBOK                                     
383300     END-IF                                                               
383400     IF LOGG-IDDC = '25' OR '26'                                          
383500       IF R3-LINE-ALLOCATE(1:3) = '266'                                   
383600           MOVE '41' TO LOGG-IDDC                                         
383700       END-IF                                                             
383800       IF R3-LINE-ALLOCATE(1:3) = '264'                                   
383900           MOVE '43' TO LOGG-IDDC                                         
384000       END-IF                                                             
384100       IF R3-LINE-ALLOCATE(1:3) = '267'                                   
384200           MOVE '45' TO LOGG-IDDC                                         
384300       END-IF                                                             
384400       IF R3-LINE-ALLOCATE(1:3) = '268'                                   
384500           MOVE '46' TO LOGG-IDDC                                         
384600       END-IF                                                             
384700       IF R3-LINE-ALLOCATE(1:3) = '265'                                   
384800           MOVE '47' TO LOGG-IDDC                                         
384900       END-IF                                                             
385000       IF R3-LINE-ALLOCATE(1:3) = '255'                                   
385100           MOVE '44' TO LOGG-IDDC                                         
385200       END-IF                                                             
385300     END-IF                                                               
385400     WRITE LOGG-POST FROM LOGG-W56173                                     
385500                                                                          
385600     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
385700     MOVE 'W56175'    TO POSTSUM-FDNAMN                                   
385800     MOVE 'W56168D5'  TO POSTSUM-DDNAMN2                                  
385900     CALL POSTSUM USING POSTSUM-PARM                                      
386000     .                                                                    
386100                                                                          
386200 S22-WRITE-W56170 SECTION.                                                
386300     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
386400     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
386500     MOVE R3-LINE-AMOUNT-LC     TO AVST-SUBEL                             
386600                                                                          
386700     IF R3-LINE-AMOUNT-SIGN = '+'                                         
386800       IF AVST-SUBEL < +0                                                 
386900         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
387000       END-IF                                                             
387100       IF AVST-KVANTAL < +0                                               
387200         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
387300       END-IF                                                             
387400     ELSE                                                                 
387500       IF AVST-SUBEL > +0                                                 
387600         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
387700       END-IF                                                             
387800       IF AVST-KVANTAL > +0                                               
387900         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
388000       END-IF                                                             
388100     END-IF                                                               
388200                                                                          
388300     IF DCS-IDDC NOT = AVST-IDDC                                          
388400        MOVE AVST-IDDC  TO W-IDDC-B6                                      
388500        PERFORM IMS-GU-WDB601                                             
388600     END-IF                                                               
388700     IF DCS-KDDC = SPACE                                                  
388800       MOVE NEJ              TO WDB6-A-SW                                 
388900     ELSE                                                                 
389000       MOVE JA               TO WDB6-A-SW                                 
389100     END-IF                                                               
389200                                                                          
389300     IF AVST-KDEKHHT = '501' AND AVST-KDEKSHT = '501'                     
389400       IF  WDB6-A-FINNS                                                   
389500       AND (DCS-NDC-PF OR DCS-SDC)                                        
389600         MOVE 'Y'               TO AVST-FLLSBOK                           
389700       END-IF                                                             
389800     END-IF                                                               
389900     IF  WDB6-A-FINNS                                                     
390000     AND DCS-DDC                                                          
390100       MOVE 'N'                 TO AVST-FLLSBOK                           
390200     END-IF                                                               
390300                                                                          
390400     IF AVST-IDKONTO(1:4) = '1454'                                        
390500*      MOVE '0000'              TO AVST-IDKONTO(7:4)                      
390600       WRITE AVST-POST FROM AVST-W56170                                   
390700                                                                          
390800       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
390900       MOVE 'W56170'            TO POSTSUM-FDNAMN                         
391000       MOVE 'W56168D6'          TO POSTSUM-DDNAMN2                        
391100       CALL POSTSUM USING POSTSUM-PARM                                    
391200     END-IF                                                               
391300     .                                                                    
391400     EJECT                                                                
391500                                                                          
391600 S30-READ-DATABASE-B2-B1 SECTION.                                         
391700     MOVE IN-EKH-IDDISTR      TO W-IDDISTR-WDB2                           
391800     MOVE IN-EKH-IDKUNDNR     TO W-IDKUNDNR-WDB2                          
391900     PERFORM IMS-GU-WDB201                                                
392000     IF SEGMENT-SAKNAS                                                    
392100**** OM MAN SKICKAR PÅ EXPORT SÅ KAN TILLÄGGSKOSTNADERNA HAMNA PÅ         
392200**** KUND 0 OCH OM DEN SAKNAS SÅ SÄTTER VI EN ANNAN DEFAULT               
392300*      IF IN-EKH-IDDISTR = 9211                                           
392400*      OR IN-EKH-IDDISTR = 9271                                           
392500*      OR IN-EKH-IDDISTR = 9272                                           
392600*      OR IN-EKH-IDDISTR = 9273                                           
392700*      OR IN-EKH-IDDISTR = 9274                                           
392800       IF IN-EKH-IDLEVNR = '1441'                                         
392900         MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                        
393000       ELSE                                                               
393100         MOVE 'US99999'         TO W-WDB1-IDPARTNR                        
393200       END-IF                                                             
393300     ELSE                                                                 
393400       MOVE GMT-IDPARTNR        TO W-WDB1-IDPARTNR                        
393500**** IF BOUNCE DISTRICT REPLACE PARMA WITH 1441                           
393600*      IF W-WDB1-IDPARTNR = '4113'                                        
393700       IF IN-EKH-IDLEVNR = '1441'                                         
393800         MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                        
393900       END-IF                                                             
394000     END-IF                                                               
394100     MOVE WC-IDFTG-US         TO W-WDB1-IDFTG                             
394200     PERFORM IMS-GU-WDB101                                                
394300     IF SEGMENT-SAKNAS                                                    
394400       DISPLAY 'BETALARUPPG. SAKNAS '                                     
394500       DISPLAY IN-EKH-IDVERGL                                             
394600       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
394700       DISPLAY GMT-IDPARTNR                                               
394800                                                                          
394900       MOVE SPACE           TO BET-KDTRADP                                
395000       MOVE ZERO            TO BET-IDPARTNR                               
395100       MOVE '????'          TO WS-KDBETVIL                                
395200       MOVE '???'           TO WS-KDVALISO-WDB1                           
395300     ELSE                                                                 
395400       MOVE BET-KDBETVIL    TO WS-KDBETVIL                                
395500     END-IF                                                               
395600     MOVE 'USD'             TO WS-KDVALISO-WDB1                           
395700                                                                          
395800     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
395900     MOVE ZERO TO TALLY                                                   
396000     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
396100                 FOR CHARACTERS BEFORE INITIAL SPACE                      
396200     IF TALLY = ZERO                                                      
396300       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
396400     ELSE                                                                 
396500       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
396600                                TO W-BET-IDPARTNR-NUM                     
396700     END-IF                                                               
396800     .                                                                    
396900     EJECT                                                                
397000                                                                          
397100 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
397200     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
397300     PERFORM IMS-GU-WDB601                                                
397400     IF DCS-KDDC = SPACE                                                  
397500       MOVE NEJ              TO WDB6-A-SW                                 
397600     ELSE                                                                 
397700       MOVE JA               TO WDB6-A-SW                                 
397800     END-IF                                                               
397900                                                                          
398000     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
398100       IF IN-EKH-KDEKSHT NOT = '406'                                      
398200         IF IN-EKH-FLDCET = NEJ                                           
398300           PERFORM S42-SKAPA-RW2-INV-POSTER                               
398400         END-IF                                                           
398500       END-IF                                                             
398600     END-IF                                                               
398700                                                                          
398800     IF IN-EKH-KDEKNIVA = 'DET'                                           
398900       IF  IN-EKH-KDEKHHT = '204'                                         
399000       AND (IN-EKH-KDEKSHT = '201' OR '202' OR '251')                     
399100         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
399200       END-IF                                                             
399300                                                                          
399400       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
399500       AND (WDB6-A-FINNS                                                  
399600       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
399700       OR   DCS-DDC OR DCS-NDC-PF))                                       
399800         IF IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '304'             
399900           CONTINUE                                                       
400000         ELSE                                                             
400100           PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                            
400200         END-IF                                                           
400300       END-IF                                                             
400400                                                                          
400500       IF IN-FIL-IDPGM = 'W4183000'                                       
400600       AND (WDB6-A-FINNS                                                  
400700       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
400800       OR   DCS-DDC OR DCS-NDC-PF))                                       
400900         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
401000       END-IF                                                             
401100     END-IF                                                               
401200     .                                                                    
401300     EJECT                                                                
401400                                                                          
401500 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
401600     MOVE 'RW2'              TO RW2-IDPTYP                                
401700     MOVE 'RW2'              TO WS-IDPTYP                                 
401800     MOVE ZERO               TO RW2-IDDISTR                               
401900     IF DCS-KDDC = SPACE OR DCS-DDC                                       
402000       MOVE WC-CDC-SE        TO RW2-IDDC                                  
402100     ELSE                                                                 
402200       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
402300     END-IF                                                               
402400     IF IN-EKH-KVANTAL < +0                                               
402500       MOVE '0422'           TO RW2-KDWRTYP                               
402600     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
402700     ELSE                                                                 
402800       MOVE '0421'           TO RW2-KDWRTYP                               
402900       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
403000     END-IF                                                               
403100                                                                          
403200     IF RW2-SUARTSTD NOT = +0                                             
403300       PERFORM S70-WRITE-W51310                                           
403400     END-IF                                                               
403500     .                                                                    
403600     EJECT                                                                
403700                                                                          
403800 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
403900     MOVE '0110'             TO RW1-KDWRTYP                               
404000     IF DCS-KDDC = SPACE OR DCS-DDC                                       
404100       MOVE WC-CDC-SE        TO RW1-IDDC                                  
404200     ELSE                                                                 
404300       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
404400     END-IF                                                               
404500     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
404600     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
404700     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
404800                                                                          
404900     IF RW1-SUARTSTD NOT = +0                                             
405000       MOVE 'RW1' TO WS-IDPTYP                                            
405100       PERFORM S70-WRITE-W51310                                           
405200     END-IF                                                               
405300     .                                                                    
405400     EJECT                                                                
405500                                                                          
405600 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
405700     MOVE '0110'             TO RW1-KDWRTYP                               
405800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
405900       MOVE WC-CDC-SE        TO RW1-IDDC                                  
406000     ELSE                                                                 
406100       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
406200     END-IF                                                               
406300     IF IN-EKH-KDANMORS = '30'                                            
406400       MOVE ZERO             TO RW1-SUARTSTD                              
406500     ELSE                                                                 
406600      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
406700     END-IF                                                               
406800     IF IN-EKH-KDANMORS = '30' OR '80'                                    
406900       MOVE ZERO             TO RW1-SUARTSJK                              
407000     ELSE                                                                 
407100      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
407200     END-IF                                                               
407300     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
407400                                                                          
407500     IF RW1-SUARTSTD NOT = +0                                             
407600       MOVE 'RW1' TO WS-IDPTYP                                            
407700       PERFORM S70-WRITE-W51310                                           
407800     END-IF                                                               
407900     .                                                                    
408000     EJECT                                                                
408100                                                                          
408200 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
408300     MOVE '0110'             TO RW1-KDWRTYP                               
408400     IF DCS-KDDC = SPACE OR DCS-DDC                                       
408500       MOVE WC-CDC-SE        TO RW1-IDDC                                  
408600     ELSE                                                                 
408700       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
408800     END-IF                                                               
408900     IF IN-EKH-KDEKSHT = '310'                                            
409000*** SKROTNING KDANMORS  13 O 23                                           
409100       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
409200     ELSE                                                                 
409300      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
409400     END-IF                                                               
409500                                                                          
409600     MOVE ZERO               TO RW1-SUARTSJK                              
409700                                RW1-SUARTFSG                              
409800     IF RW1-SUARTSTD NOT = +0                                             
409900       MOVE 'RW1' TO WS-IDPTYP                                            
410000       PERFORM S70-WRITE-W51310                                           
410100     END-IF                                                               
410200     .                                                                    
410300     EJECT                                                                
410400                                                                          
410500 S60-WRITE-W5616N SECTION.                                                
410600     WRITE SAPUT-POST  FROM IN-AREA                                       
410700                                                                          
410800     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
410900     MOVE 'W5616N'            TO POSTSUM-FDNAMN                           
411000     MOVE 'W56168D7'          TO POSTSUM-DDNAMN2                          
411100     CALL POSTSUM USING POSTSUM-PARM                                      
411200     .                                                                    
411300     EJECT                                                                
411400                                                                          
411500 S70-WRITE-W51310 SECTION.                                                
411600     IF WS-IDPTYP  = 'RW2'                                                
411700       IF DCS-KDDC = SPACE OR DCS-DDC                                     
411800         MOVE WC-CDC-SE        TO INV-IDDC                                
411900       ELSE                                                               
412000         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
412100       END-IF                                                             
412200       IF IN-EKH-KVANTAL < +0                                             
412300         MOVE '003'            TO INV-IDPTYP                              
412400       COMPUTE INV-SUARTSTD =                                             
412500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
412600       ELSE                                                               
412700         MOVE '002'            TO INV-IDPTYP                              
412800         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
412900       END-IF                                                             
413000       MOVE SPACE TO WS-IDPTYP                                            
413100       MOVE 0                  TO INV-ADLAGOMR                            
413200       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
413300       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
413400     END-IF                                                               
413500     IF WS-IDPTYP  = 'RW1'                                                
413600       IF DCS-KDDC = SPACE OR DCS-DDC                                     
413700         MOVE WC-CDC-SE        TO INV-IDDC                                
413800       ELSE                                                               
413900         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
414000       END-IF                                                             
414100       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
414200       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
414300       MOVE 0                  TO INV-ADLAGOMR                            
414400       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
414500       MOVE '001'              TO INV-IDPTYP                              
414600       MOVE SPACE              TO WS-IDPTYP                               
414700     END-IF                                                               
414800     WRITE INV-POST  FROM INV-W51310                                      
414900                                                                          
415000     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
415100     MOVE 'W51310'            TO POSTSUM-FDNAMN                           
415200     MOVE 'W56168D8'          TO POSTSUM-DDNAMN2                          
415300     CALL POSTSUM USING POSTSUM-PARM                                      
415400     .                                                                    
415500     EJECT                                                                
415600                                                                          
415700 S80-GET-CURRENCY-RATE SECTION.                                           
415800     MOVE +0                  TO W-ANT                                    
415900     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
416000             BEFORE INITIAL ' '                                           
416100     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
416200     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
416300     PERFORM IMS-GU-WDL601                                                
416400     IF SEGMENT-SAKNAS                                                    
416500       CONTINUE                                                           
416600     ELSE                                                                 
416700       PERFORM IMS-GNP-WDL611                                             
416800       IF SEGMENT-SAKNAS                                                  
416900         CONTINUE                                                         
417000       ELSE                                                               
417100         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
417200                                   - INL-DAINLEV                          
417300         MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                   
417400         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
417500         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
417600         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
417700         MOVE WS-KDVALISO-US        TO CURR-KDVALISO-ROW                  
417800         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
417900         IF CURR-KDSVAR = ' '                                             
418000           MOVE CURR-PRKURS-NEW     TO WS-PRKURS-US3                      
418100         ELSE                                                             
418200           MOVE +1                  TO WS-PRKURS-US3                      
418300         END-IF                                                           
418400       END-IF                                                             
418500     END-IF                                                               
418600     .                                                                    
418700     EJECT                                                                
418800                                                                          
418900 S81-GET-CURRENCY-RATE SECTION.                                           
419000     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
419100     MOVE 'USD'               TO CURR-KDVALISO-ROW                        
419200     IF IN-FIL-IDPGM = 'W4183300'                                         
419300       IF IN-EKH-DAAVIDAT > ZERO                                          
419400         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
419500         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
419600       ELSE                                                               
419700         MOVE WS-TIAA            TO WS-TIAA-CR                            
419800         MOVE WS-TIMM            TO WS-TIMM-CR                            
419900       END-IF                                                             
420000     ELSE                                                                 
420100       MOVE WS-TIAA              TO WS-TIAA-CR                            
420200       MOVE WS-TIMM              TO WS-TIMM-CR                            
420300     END-IF                                                               
420400     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
420500     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
420600     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
420700     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
420800     IF CURR-KDSVAR = ' '                                                 
420900       IF IN-EKH-IDDISTR > ZERO                                           
421000         MOVE CURR-PRKURS-NEW TO WS-PRKURS-US3                            
421100       ELSE                                                               
421200         IF WS-PRKURS = ZERO                                              
421300           MOVE 1           TO WS-PRKURS-US3                              
421400         END-IF                                                           
421500       END-IF                                                             
421600     ELSE                                                                 
421700       MOVE 1               TO WS-PRKURS-US3                              
421800     END-IF                                                               
421900     .                                                                    
422000     EJECT                                                                
422100                                                                          
422200* --- IMS SECTIONS ---                                                    
422300                                                                          
422400 IMS-GU-WDH521 SECTION.                                                   
422500     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
422600          DELIMITED BY SIZE INTO SSA1                                     
422700     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
422800          DELIMITED BY SIZE INTO SSA2                                     
422900     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
423000          DELIMITED BY SIZE INTO SSA3                                     
423100     MOVE '  '              TO GODK-STATUSKODER                           
423200     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
423300                                                   SSA2                   
423400                                                   SSA3                   
423500     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
423600                                                                          
423700     PERFORM IMS-STATUS-CONTROL                                           
423800     .                                                                    
423900                                                                          
424000 IMS-GNP-WDH531 SECTION.                                                  
424100     MOVE 'WDH531  '        TO SSA1                                       
424200     MOVE '  GE'            TO GODK-STATUSKODER                           
424300     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
424400     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
424500                               WS-STATUS                                  
424600     PERFORM IMS-STATUS-CONTROL                                           
424700     .                                                                    
424800     EJECT                                                                
424900                                                                          
425000 IMS-GU-WDB201 SECTION.                                                   
425100     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-KEY ')'                         
425200          DELIMITED BY SIZE INTO SSA1                                     
425300     MOVE '  GE'                 TO GODK-STATUSKODER                      
425400     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
425500      MOVE GMTA-STATUS-CODE      TO STATUS-WS                             
425600     PERFORM IMS-STATUS-CONTROL                                           
425700     .                                                                    
425800     EJECT                                                                
425900                                                                          
426000 IMS-GU-WDB101 SECTION.                                                   
426100     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
426200          DELIMITED BY SIZE INTO SSA1                                     
426300     MOVE '  GE'               TO GODK-STATUSKODER                        
426400     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
426500     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
426600     PERFORM IMS-STATUS-CONTROL                                           
426700     .                                                                    
426800     EJECT                                                                
426900                                                                          
427000 IMS-GU-WDB601    SECTION.                                                
427100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
427200          DELIMITED BY SIZE INTO SSA1                                     
427300     MOVE '  GE' TO GODK-STATUSKODER                                      
427400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
427500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
427600     PERFORM IMS-STATUS-CONTROL                                           
427700     IF SEGMENT-SAKNAS                                                    
427800        MOVE SPACE TO DCS-KDDC                                            
427900     END-IF                                                               
428000     .                                                                    
428100     EJECT                                                                
428200                                                                          
428300 IMS-GU-WDL601   SECTION.                                                 
428400     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
428500          DELIMITED BY SIZE INTO SSA1                                     
428600     MOVE '  GE' TO GODK-STATUSKODER                                      
428700     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
428800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
428900     PERFORM IMS-STATUS-CONTROL                                           
429000     .                                                                    
429100     SKIP3                                                                
429200                                                                          
429300 IMS-GNP-WDL611   SECTION.                                                
429400     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
429500          DELIMITED BY SIZE INTO SSA1                                     
429600     MOVE '  GE' TO GODK-STATUSKODER                                      
429700     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
429800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
429900     PERFORM IMS-STATUS-CONTROL                                           
430000     .                                                                    
430100     SKIP3                                                                
430200                                                                          
430300 IMS-GU-WDGX9306 SECTION.                                                 
430400     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
430500             DELIMITED BY SIZE INTO SSA1                                  
430600     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
430700             DELIMITED BY SIZE INTO SSA2                                  
430800     MOVE '  GE'   TO GODK-STATUSKODER                                    
430900     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
431000     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
431100     PERFORM IMS-STATUS-CONTROL                                           
431200     .                                                                    
431300     SKIP3                                                                
431400                                                                          
431500                                                                          
431600 IMS-GNP-WDGX9308 SECTION.                                                
431700     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
431800             DELIMITED BY SIZE INTO SSA1                                  
431900     MOVE '  GE'   TO GODK-STATUSKODER                                    
432000     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
432100     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
432200     PERFORM IMS-STATUS-CONTROL                                           
432300     .                                                                    
432400     SKIP3                                                                
432500                                                                          
432600 IMS-GNP-WDGX9308-FIRST SECTION.                                          
432700     MOVE 'WDGX9308*F' TO SSA1                                            
432800     MOVE '  GE'   TO GODK-STATUSKODER                                    
432900     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
433000     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
433100     PERFORM IMS-STATUS-CONTROL                                           
433200     .                                                                    
433300     SKIP3                                                                
433400                                                                          
433500 IMS-STATUS-CONTROL SECTION.                                              
433600     SET STATUS-IX TO 1                                                   
433700     SEARCH GODK-STATUS                                                   
433800       AT END                                                             
433900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
434000           DELIMITED BY SIZE INTO FELTEXT                                 
434100         DISPLAY FELTEXT                                                  
434200         CALL FELLOG                                                      
434300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
434400         CONTINUE                                                         
434500     END-SEARCH                                                           
434600     .                                                                    
