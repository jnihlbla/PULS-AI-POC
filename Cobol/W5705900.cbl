000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5705900.                                                
000300 AUTHOR.         JOHN SAMUEL.                                             
000400 DATE-WRITTEN.   20240313.                                                
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
004700     SELECT W57066                     ASSIGN TO W57059D1.                
004800                                                                          
004900*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
005000     SELECT W57051B                    ASSIGN TO W57059D2.                
005100                                                                          
005200*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005300     SELECT W57052B                    ASSIGN TO W57059D3.                
005400                                                                          
005500*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005600     SELECT W57053B                    ASSIGN TO W57059D4.                
005700                                                                          
005800*          --- LOGG TILL ON-DEMAND                                        
005900     SELECT W57055B                    ASSIGN TO W57059D5.                
006000                                                                          
006100*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006200     SELECT W57058B                    ASSIGN TO W57059D6.                
006300                                                                          
006400*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006500     SELECT W5705NB                    ASSIGN TO W57059D7.                
006600                                                                          
006700*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006800     SELECT W51350B                    ASSIGN TO W57059D8.                
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
008100 FD  W57051B                                                              
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400*01  71INIT-POST -COPY R3INIT20               -L.                         
008500*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008600*01  71LINE-POST -COPY R3LINE20               -L.                         
008700                                                                          
008800 FD  W57052B                                                              
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100*01  72LINE-POST -COPY R3LINE20               -L.                         
009200                                                                          
009300 FD  W57053B                                                              
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009700*01  73LINE-POST -COPY R3LINE20               -L.                         
009800                                                                          
009900 FD  W57055B                                                              
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200*01  LOGG-POST   -COPY W57073                 -L.                         
010300                                                                          
010400 FD  W57058B                                                              
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700*01  AVST-POST   -COPY W57070                 -L.                         
010800                                                                          
010900 FD  W5705NB                                                              
011000     RECORDING       F                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300 01  SAPUT-POST.                                                          
011400*    03  -COPY WDR801        -L.                                          
011500     03 FILLER                   PIC X(6).                                
011600                                                                          
011700 FD  W51350B                                                              
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012100                                                                          
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400*    -- CHECKED BY WY2000                                                 
012500 77  IDPGM                        PIC X(8)    VALUE 'W5705900'.           
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
013700 77  WS-LINE-AMOUNT-126-1         PIC S9(13)V99 COMP-3.                   
013800 77  WS-LINE-AMOUNT-126-2         PIC S9(13)V99 COMP-3.                   
013900 77  WS-LINE-AMOUNT-131-1         PIC S9(13)V99 COMP-3.                   
014000 77  WS-LINE-AMOUNT-131-2         PIC S9(13)V99 COMP-3.                   
014100 77  SPAR-SUMMA-102-125         PIC S9(13)V99  COMP-3 VALUE ZERO.         
014200 77  WS-BELOPP                  PIC S9(13)V99  COMP-3.                    
014300 77  WS-LOP                       PIC 9       VALUE ZERO.                 
014400 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
014500 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
014600 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
014700 77  SPAR-LINE-ORDER              PIC X(12).                              
014800 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
014900 77  WS-RED-IDKST                 PIC X(10).                              
015000 77  WS-IDPTYP                    PIC X(3).                               
015100 77  WS-FAKTURA-DATUM             PIC X(16).                              
015200 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
015300 77  SPAR-SUMMA                 PIC S9(13)V99  COMP-3 VALUE ZERO.         
015400 77  SPAR-PRDMTRL               PIC S9(13)V99  COMP-3 VALUE ZERO.         
015500 77  SPAR-PROVRPAL              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015600 77  SPAR-PRDIRLON              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015700 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
015800 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
015900 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
016000 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
016100 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
016200 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
016300 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
016400 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
016500 77  WS-KDPRODSL-SAVE             PIC X(2)    VALUE SPACE.                
016600                                                                          
016700 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
016800       88  WDB6-A-FINNS                       VALUE 'J'.                  
016900       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
017000                                                                          
017100*01  -COPY WWPRODSL                                                       
017200                                                                          
017300*01  -COPY WWDCKONS                                                       
017400     EJECT                                                                
017500                                                                          
017600 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
017700*01  -COPY WWIDFTG                                                        
017800     EJECT                                                                
017900                                                                          
018000 01  FELTEXT                      PIC X(80).                              
018100 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
018200*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
018300     EJECT                                                                
018400                                                                          
018500 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
018600 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
018700     EJECT                                                                
018800 01  WS-IDDISTR-IDKUNDNR.                                                 
018900     03  FILLER                   PIC X(2)    VALUE SPACE.                
019000     03  WS-IDDISTR               PIC 9(4).                               
019100     03  WS-IDKUNDNR              PIC 9(6).                               
019200                                                                          
019300 01  WS-KDBETVIL                  PIC X(4).                               
019400 01  WS-KDVALISO-WDB1             PIC X(3).                               
019500 01  WS-KDVALISO                  PIC X(3).                               
019600 01  WS-KDVALISO-BR               PIC X(3) VALUE 'BRL'.                   
019700 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
019800 01  WS-PRKURS-BR                 PIC S9(6)V9(5) COMP-3.                  
019900 01  WS-PRKURS-BR2                PIC S9(6)V9(5) COMP-3.                  
020000 01  WS-PRKURS-BR3                PIC S9(6)V9(5) COMP-3.                  
020100 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
020200 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
020300                                                                          
020400 01  WS-ALLOCATE.                                                         
020500     03  WS-ALLOCATE-DC           PIC X(2).                               
020600     03  WS-ALLOCATE-DISTR        PIC X(5).                               
020700     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
020800     03  FILLER                   PIC X(4)    VALUE SPACE.                
020900                                                                          
021000 01  WS-TEXT.                                                             
021100     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
021200     03  WS-TEXT-KDEKHHT          PIC X(3).                               
021300     03  WS-TEXT-KDEKSHT          PIC X(3).                               
021400     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
021500     03  FILLER                   PIC X(7)    VALUE SPACE.                
021600                                                                          
021700 01  WS-LINE-TEXT.                                                        
021800     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
021900     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
022000     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
022100     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
022200     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
022300     03  FILLER                   PIC X(22)   VALUE SPACE.                
022400                                                                          
022500 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
022600 01  WS-PRCTR.                                                            
022700     03  WS-PRCTR-PRODSL          PIC X(2).                               
022800     03  FILLER                   PIC X(1).                               
022900     03  FILLER                   PIC X(7).                               
023000                                                                          
023100 01  WS-R3-ACCOUNT.                                                       
023200     03  WS-R3-ACCOUNT-ALFA.                                              
023300         05 FILLER                PIC X(4).                               
023400         05 WS-R3-ACCOUNT-6       PIC X(6).                               
023500     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
023600         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
023700                                                                          
023800 01  WS-ACCOUNT.                                                          
023900     03  FILLER                   PIC X(7).                               
024000     03  WS-ACCOUNT-4             PIC X(1).                               
024100     03  FILLER                   PIC X(2).                               
024200                                                                          
024300 01  SPAR-AREA.                                                           
024400     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
024500     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
024600     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
024700     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
024800                                                                          
024900 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
025000 01  FILLER REDEFINES DAGENS-DATUM.                                       
025100     03  DAGENS-DATUM-AAR         PIC 9(2).                               
025200     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
025300     03  DAGENS-DATUM-DAG         PIC 9(2).                               
025400                                                                          
025500 01  WS-NEW-MONTH                 PIC 9(2).                               
025600                                                                          
025700 01  WS-DAREGDAT.                                                         
025800     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
025900     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
026000                                                                          
026100 01  WS-TIREGDAT-TOT.                                                     
026200     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
026300     03  WS-TIREGDAT              PIC 9(6).                               
026400                                                                          
026500 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
026600 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
026700     EJECT                                                                
026800                                                                          
026900 01  DYNAMISKA-SUBPROGRAM.                                                
027000     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
027100     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
027200     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
027300     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
027400     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
027500     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
027600                                                                          
027700*    --- PARAMETRAR TILL ABEND                                            
027800 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
027900 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
028000 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
028100     EJECT                                                                
028200                                                                          
028300*    --- PARAMETRAR TILL DATKORT                                          
028400 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57059'.             
028500                                                                          
028600 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
028700*01  -COPY WDATKORT                                                       
028800     EJECT                                                                
028900                                                                          
029000*    --- PARAMETRAR TILL POSTSUM                                          
029100*01  -COPY W0005   -PRE  POSTSUM-                                         
029200     EJECT                                                                
029300                                                                          
029400 01  FILLER                          PIC X(16) VALUE 'W510CURR '.         
029500*01  -COPY W510CURR                                                       
029600     EJECT                                                                
029700                                                                          
029800 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
029900*01  AREA -COPY WDR801           -PRE IN-                                 
030000*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
030100         05   IN-EKH-IDSYSMOT     PIC X(6).                               
030200                                                                          
030300     EJECT                                                                
030400 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
030500                                                                          
030600*01  -COPY R3LINE20              -PRE R3-                                 
030700*01  -COPY R3HEAD20              -PRE R3-                                 
030800*01  -COPY R3INIT20              -PRE R3-                                 
030900*01  -COPY W57073                -PRE LOGG-                               
031000*01  -COPY W57070                -PRE AVST-                               
031100*01  -COPY W517RW1               -PRE RW1-                                
031200*01  -COPY W517RW2               -PRE RW2-                                
031300*01  -COPY W51310                -PRE INV-                                
031400     EJECT                                                                
031500                                                                          
031600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031700 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
031800                                                                          
031900 01  NYCKLAR-TILL-DLI.                                                    
032000     03  W-WDH501KY-X.                                                    
032100         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
032200         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
032300     03  W-KDEKSHT-X.                                                     
032400         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
032500     03  W-KDEKNIVA-X.                                                    
032600         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
032700     03  W-WDH531KY-X.                                                    
032800         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
032900         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
033000     03  W-IDRADNR-X.                                                     
033100         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
033200                                                                          
033300     03  W-IDGMT-KEY.                                                     
033400         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
033500         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
033600                                                                          
033700     03  W-WDB101KY-X.                                                    
033800         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
033900         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
034000                                                                          
034100     03  W-WDGXKEY-5121-X.                                                
034200         05  FILLER               PIC X(4)    VALUE '5121'.               
034300         05  FILLER               PIC X(2)    VALUE '61'.                 
034400         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
034500     03  W-WDGXKEY-5122-X.                                                
034600         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
034700         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
034800     03  W-WDGXKEY-5122-MIN-X.                                            
034900         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
035000         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
035100     03  W-WDGXKEY-5122-MAX-X.                                            
035200         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
035300         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
035400                                                                          
035500     03  W-IDDC-B6-X.                                                     
035600         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
035700                                                                          
035800     03  W-IDARTNR-X.                                                     
035900         05 W-IDARTNR             PIC S9(9) COMP-3.                       
036000                                                                          
036100     03  W-IDFAKT-X.                                                      
036200         05 W-IDFAKT              PIC S9(7) COMP-3.                       
036300                                                                          
036400     03  W-IDLEVNR-X.                                                     
036500         05  W-IDLEVNR            PIC X(5)    VALUE SPACE.                
036600                                                                          
036700     EJECT                                                                
036800                                                                          
036900*    --- STATUS-KOD FRÅN IMS                                              
037000 01  STATUS-WS                    PIC XX.                                 
037100     88  SEGMENT-FINNS                        VALUE '  '.                 
037200     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
037300                                                                          
037400 01  GODK-STATUSKODER.                                                    
037500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037600                                                                          
037700 01  SSA1                         PIC X(128).                             
037800 01  SSA2                         PIC X(64).                              
037900 01  SSA3                         PIC X(64).                              
038000     EJECT                                                                
038100                                                                          
038200*    --- IMS FUNKTIONSKODER                                               
038300*01  -COPY W0003                                                          
038400     EJECT                                                                
038500                                                                          
038600*    ---  DLI INPUT-OUTPUT AREA                                           
038700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
038800 01  DLI-IO-WDH501.                                                       
038900*    03  -COPY WDH501                                                     
039000     EJECT                                                                
039100                                                                          
039200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
039300 01  DLI-IO-WDH511.                                                       
039400*    03  -COPY WDH511                                                     
039500     EJECT                                                                
039600                                                                          
039700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
039800 01  DLI-IO-WDH521.                                                       
039900*    03  -COPY WDH521                                                     
040000     EJECT                                                                
040100                                                                          
040200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
040300 01  DLI-IO-WDH531.                                                       
040400*    03  -COPY WDH531                                                     
040500     EJECT                                                                
040600                                                                          
040700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
040800 01  DLI-IO-WLBETC01.                                                     
040900*    03  -COPY WDB101                                                     
041000     EJECT                                                                
041100                                                                          
041200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
041300 01  DLI-IO-WLGMTA01.                                                     
041400*    03  -COPY WDB201                                                     
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
041800 01  DLI-IO-WDGX5121.                                                     
041900*    03  -COPY WDGX5121                                                   
042000     EJECT                                                                
042100                                                                          
042200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
042300 01  DLI-IO-WDGX5122.                                                     
042400*    03  -COPY WDGX5122                                                   
042500     EJECT                                                                
042600                                                                          
042700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042800 01   DLI-IO-AREA-B601.                                                   
042900*     03  -COPY WDB601                                                    
043000     EJECT                                                                
043100 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
043200 01   DLI-IO-WDB617.                                                      
043300*     03  -COPY WDB617                                                    
043400     EJECT                                                                
043500 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
043600 01   DLI-IO-AREA-L601.                                                   
043700*     03  -COPY WDL601                                                    
043800     EJECT                                                                
043900 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
044000 01   DLI-IO-AREA-L611.                                                   
044100*     03  -COPY WDL611                                                    
044200     EJECT                                                                
044300 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6C1'.                
044400     SKIP3                                                                
044500 01  FILLER               PIC X(16)   VALUE 'WDF101 AREA'.                
044600 01  DLI-IO-WDF101.                                                       
044700*    03  -COPY WDF101                                                     
044800     EJECT                                                                
044900 01  FILLER               PIC X(16)   VALUE 'WDF106 AREA'.                
045000 01  DLI-IO-WDF106.                                                       
045100*    03  -COPY WDF106                                                     
045200     EJECT                                                                
045300 LINKAGE SECTION.                                                         
045400*01  -COPY W0008  -PRE WDH5-                                              
045500     05  FILLER                  PIC X.                                   
045600                                                                          
045700*01  -COPY W0008  -PRE GMTA-                                              
045800     05  FILLER                  PIC X.                                   
045900                                                                          
046000*01  -COPY W0008  -PRE BETC-                                              
046100     05  FILLER                  PIC X.                                   
046200                                                                          
046300*01  -COPY W0008  -PRE 5121-                                              
046400     05  FILLER                  PIC X.                                   
046500                                                                          
046600*01  -COPY W0008  -PRE WDG2-                                              
046700     05  FILLER                  PIC X.                                   
046800                                                                          
046900*01  -COPY W0008  -PRE WDB6-                                              
047000     05  FILLER                  PIC X.                                   
047100                                                                          
047200*01  -COPY W0008  -PRE WDL6-                                              
047300     05  FILLER                  PIC X.                                   
047400                                                                          
047500*01  -COPY W0008  -PRE WDF1-                                              
047600     05  FILLER                  PIC X.                                   
047700                                                                          
047800     EJECT                                                                
047900                                                                          
048000 PROCEDURE DIVISION  USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
048100                           WDG2-PCB WDB6-PCB WDL6-PCB WDF1-PCB.           
048200 MAIN SECTION.                                                            
048300     ENTRY 'DLITCBL' USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
048400                           WDG2-PCB WDB6-PCB WDL6-PCB WDF1-PCB.           
048500                                                                          
048600     PERFORM A-INIT                                                       
048700                                                                          
048800     PERFORM S01-READ-W57066                                              
048900     PERFORM UNTIL END-OF-W57066                                          
049000*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
049100       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
049200       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
049300       AND WS-NEW-MONTH > 01                                              
049400         PERFORM S60-WRITE-W5705NB                                        
049500       ELSE                                                               
049600         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
049700         PERFORM S30-READ-DATABASE-B2-B1                                  
049800         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
049900           PERFORM C-EXECUTE                                              
050000         END-IF                                                           
050100       END-IF                                                             
050200       PERFORM S01-READ-W57066                                            
050300     END-PERFORM                                                          
050400                                                                          
050500     PERFORM Z-FINI                                                       
050600                                                                          
050700     MOVE ZERO TO RETURN-CODE                                             
050800     GOBACK                                                               
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200 A-INIT SECTION.                                                          
051300     OPEN INPUT  W57066                                                   
051400                                                                          
051500     OPEN OUTPUT W57058B                                                  
051600                 W57051B                                                  
051700                 W57052B                                                  
051800                 W57053B                                                  
051900                 W57055B                                                  
052000                 W5705NB                                                  
052100                 W51350B                                                  
052200                                                                          
052300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
052400     MOVE 20               TO RW1-DAVVREG(1:2)                            
052500     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
052600                              RW1-DAVVREG(3:2)                            
052700                              W-DATE-AAMM(1:2)                            
052800                              WS-TIAA                                     
052900     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
053000                              W-DATE-AAMM(3:2)                            
053100                              WS-TIMM                                     
053200                              WS-NEW-MONTH                                
053300     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
053400     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
053500     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
053600                                                                          
053700*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
053800*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
053900     IF WS-NEW-MONTH = 12                                                 
054000       MOVE 1              TO WS-NEW-MONTH                                
054100     ELSE                                                                 
054200       ADD 1               TO WS-NEW-MONTH                                
054300*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
054400***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
054500***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
054600***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
054700***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
054800       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
054900       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
055000         ADD 1             TO WS-NEW-MONTH                                
055100         ADD 1             TO WS-TIMM                                     
055200         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
055300       END-IF                                                             
055400     END-IF                                                               
055500                                                                          
055600     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
055700                                                                          
055800     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
055900                                                                          
056000     ACCEPT DAGENS-KLOCKA FROM TIME                                       
056100     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
056200                                                                          
056300     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
056400     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
056500     MOVE 'M'                   TO CURR-KDVALTYP                          
056600                                                                          
056700     MOVE WS-KDVALISO-BR        TO CURR-KDVALISO-ROW                      
056800     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
056900     IF CURR-KDSVAR = ' '                                                 
057000       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-BR                           
057100     ELSE                                                                 
057200       MOVE 1                   TO WS-PRKURS-BR                           
057300     END-IF                                                               
057400     COMPUTE WS-PRKURS-BR2 ROUNDED = 1 / WS-PRKURS-BR                     
057500     MOVE WS-PRKURS-BR          TO WS-PRKURS-BR3                          
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 C-EXECUTE SECTION.                                                       
058000     MOVE WC-IDFTG-BR           TO W-IDFTG                                
058100     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
058200     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
058300     IF IN-EKH-KDEKNIVA = 'TDET'                                          
058400       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
058500     END-IF                                                               
058600     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
058700     PERFORM IMS-GU-WDH521                                                
058800     PERFORM IMS-GNP-WDH531                                               
058900                                                                          
059000     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
059100     IF IN-EKH-IDDISTR > ZERO                                             
059200       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
059300     ELSE                                                                 
059400       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
059500     END-IF                                                               
059600     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
059700                                                                          
059800* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
059900* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
060000* BOKFÖRINGEN I SAP SKER DOCK ALLTID I BRL, DÄRFÖR BYTET HÄR:             
060100*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
060200*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '106')               
060300*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '107')               
060400*      MOVE 'BRL'               TO WS-KDVALISO                            
060500*    END-IF                                                               
060600                                                                          
060700     IF  ((IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                               
060800     AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                                
060900     OR (IN-EKH-KDEKHHT = '303'                                           
061000     AND IN-EKH-KDEKSHT = '301')                                          
061100     OR (IN-EKH-KDEKHHT = '303'                                           
061200     AND IN-EKH-KDEKSHT = '307'))                                         
061300       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
061400     ELSE                                                                 
061500       MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                             
061600       MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                             
061700       IF WS-LOP = 9                                                      
061800         MOVE ZERO  TO WS-LOP                                             
061900       ELSE                                                               
062000         ADD +1     TO WS-LOP                                             
062100       END-IF                                                             
062200       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
062300     END-IF                                                               
062400* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
062500     IF SYST-IDPTYP = '210'                                               
062600       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
062700     ELSE                                                                 
062800       IF SYST-IDPTYP = '310'                                             
062900         PERFORM CC-CREATE-WRITE-HEADER-AR                                
063000       ELSE                                                               
063100* TEST OM BRYTNING PÅ VERIFIKATION                                        
063200         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
063300         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
063400         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
063500         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
063600           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
063700           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
063800           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
063900           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
064000           IF (IN-EKH-KDEKHHT = '102'                                     
064100           AND IN-EKH-KDEKSHT = '121')                                    
064200           OR (IN-EKH-KDEKHHT = '102'                                     
064300           AND IN-EKH-KDEKSHT = '122')                                    
064400           OR (IN-EKH-KDEKHHT = '102'                                     
064500           AND IN-EKH-KDEKSHT = '131')                                    
064600           OR (IN-EKH-KDEKHHT = '102'                                     
064700           AND IN-EKH-KDEKSHT = '132')                                    
064800           OR (IN-EKH-KDEKHHT = '102'                                     
064900           AND IN-EKH-KDEKSHT = '126')                                    
065000             PERFORM S80-GET-CURRENCY-RATE                                
065100           END-IF                                                         
065200           IF (IN-EKH-KDEKHHT = '303'                                     
065300           AND IN-EKH-KDEKSHT = '301')                                    
065310           OR (IN-EKH-KDEKHHT = '303'                                     
065320           AND IN-EKH-KDEKSHT = '307')                                    
065330           OR (IN-EKH-KDEKHHT = '303'                                     
065340           AND IN-EKH-KDEKSHT = '371')                                    
065350           OR (IN-EKH-KDEKHHT = '303'                                     
065360           AND IN-EKH-KDEKSHT = '3XX')                                    
065400             PERFORM S81-GET-CURRENCY-RATE                                
065500           END-IF                                                         
065600*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
065700*   HEADER-POST TILL HUVUDBOKEN                                           
065800           IF (IN-EKH-KDEKHHT = '102'                                     
065900           AND IN-EKH-KDEKSHT = '120')                                    
066000           OR (IN-EKH-KDEKHHT = '102'                                     
066100           AND IN-EKH-KDEKSHT = '124')                                    
066200           OR (IN-EKH-KDEKHHT = '102'                                     
066300           AND IN-EKH-KDEKSHT = '125')                                    
066400           OR (IN-EKH-KDEKHHT = '102'                                     
066500           AND IN-EKH-KDEKSHT = '130')                                    
066600           OR (IN-EKH-KDEKHHT = '102'                                     
066700           AND IN-EKH-KDEKSHT = '134')                                    
066800           OR (IN-EKH-KDEKHHT = '103'                                     
066900           AND IN-EKH-KDEKSHT = '102')                                    
067000           OR (IN-EKH-KDEKHHT = '103'                                     
067100           AND IN-EKH-KDEKSHT = '106')                                    
067200           OR (IN-EKH-KDEKHHT = '103'                                     
067300           AND IN-EKH-KDEKSHT = '107')                                    
067400           OR (IN-EKH-KDEKHHT = '204'                                     
067500           AND IN-EKH-KDEKSHT = '301')                                    
067600           OR (IN-EKH-KDEKHHT = '303'                                     
067700           AND IN-EKH-KDEKSHT = '301')                                    
067800           OR (IN-EKH-KDEKHHT = '303'                                     
067900           AND IN-EKH-KDEKSHT = '307')                                    
068000           OR (IN-EKH-KDEKHHT = '303'                                     
068100           AND IN-EKH-KDEKSHT = '3XX')                                    
068200           OR (IN-EKH-KDEKHHT = '303'                                     
068300           AND IN-EKH-KDEKSHT = '371')                                    
068400             CONTINUE                                                     
068500           ELSE                                                           
068600             PERFORM CA-CREATE-WRITE-HEADER-GL                            
068700           END-IF                                                         
068800         END-IF                                                           
068900       END-IF                                                             
069000     END-IF                                                               
069100                                                                          
069200**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
069300     MOVE WS-STATUS TO STATUS-WS                                          
069400     PERFORM UNTIL SEGMENT-SAKNAS                                         
069500       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
069600                                                                          
069700* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
069800       IF SYST-IDPTYP = '610'                                             
069900         PERFORM CD-BUILD-COMMON-610-PART                                 
070000         PERFORM CE-SCHEDULE-LINE-GL                                      
070100       ELSE                                                               
070200         IF SYST-IDPTYP = '210'                                           
070300           PERFORM CF-BUILD-COMMON-210-PART                               
070400           PERFORM CG-SCHEDULE-LINE-AP                                    
070500         ELSE                                                             
070600           IF SYST-IDPTYP = '310'                                         
070700             PERFORM CH-BUILD-COMMON-310-PART                             
070800             PERFORM CI-SCHEDULE-LINE-AR                                  
070900           END-IF                                                         
071000         END-IF                                                           
071100       END-IF                                                             
071200       PERFORM IMS-GNP-WDH531                                             
071300     END-PERFORM                                                          
071400     .                                                                    
071500     EJECT                                                                
071600                                                                          
071700 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
071800     MOVE SPACE                   TO R3-HEAD-R3                           
071900     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
072000     MOVE 'BR12'                  TO R3-HEAD-COMPANY-CODE                 
072100     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
072200     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
072300     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
072400     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
072500     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
072600       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
072700     ELSE                                                                 
072800       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
072900     END-IF                                                               
073000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
073100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
073200     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
073300     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
073400     IF WS-KDVALISO = 'BRL'                                               
073500       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
073600     ELSE                                                                 
073700       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
073800       MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                     
073900       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
074000       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
074100       IF CURR-KDSVAR = ' '                                               
074200         IF IN-EKH-IDDISTR > ZERO                                         
074300           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
074400         ELSE                                                             
074500           MOVE 1               TO WS-PRKURS                              
074600         END-IF                                                           
074700       ELSE                                                               
074800         MOVE 1                 TO WS-PRKURS                              
074900       END-IF                                                             
075000       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
075100                                       CURR-REVALUTA-TO                   
075200       IF CURR-REVALUTA-TO = +1                                           
075300         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
075400       END-IF                                                             
075500       IF CURR-REVALUTA-TO = +10                                          
075600         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
075700       END-IF                                                             
075800       IF CURR-REVALUTA-TO = +100                                         
075900         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
076000       END-IF                                                             
076100     END-IF                                                               
076200     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
076300     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
076400     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
076500     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
076600     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
076700     MOVE JA                      TO WS-HEADER-SW                         
076800     MOVE NEJ                     TO WS-LINE-SW                           
076900                                                                          
077000* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
077100     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
077200     OR (IN-EKH-KDEKHHT = '303'                                           
077300     AND IN-EKH-KDEKSHT = '391')                                          
077400     OR (IN-EKH-KDEKHHT = '102'                                           
077500     AND IN-EKH-KDEKSHT = '121')                                          
077600     OR (IN-EKH-KDEKHHT = '102'                                           
077700     AND IN-EKH-KDEKSHT = '131')                                          
077800       PERFORM S004-WRITE-W57053B-HEAD                                    
077900     ELSE                                                                 
078000       PERFORM S002-WRITE-W57051B-HEAD                                    
078100     END-IF                                                               
078200     .                                                                    
078300     EJECT                                                                
078400                                                                          
078500 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
078600     MOVE SPACE                   TO R3-HEAD-R3                           
078700     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
078800     MOVE 'BR12'                  TO R3-HEAD-COMPANY-CODE                 
078900                                     R3-HEAD-CONTROL-AREA                 
079000     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
079100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
079200     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
079300     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
079400     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
079500     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
079600       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
079700     ELSE                                                                 
079800       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
079900     END-IF                                                               
080000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
080100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
080200     IF (IN-EKH-KDEKHHT = '103'                                           
080300     AND IN-EKH-KDEKSHT = '102')                                          
080400     OR (IN-EKH-KDEKHHT = '103'                                           
080500     AND IN-EKH-KDEKSHT = '106')                                          
080600     OR (IN-EKH-KDEKHHT = '103'                                           
080700     AND IN-EKH-KDEKSHT = '107')                                          
080800       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
080900       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
081000     ELSE                                                                 
081100       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
081200       MOVE WS-PRKURS-BR2         TO R3-HEAD-EXCHANGE-RATE                
081300       MOVE 'BRL'                 TO CURR-KDVALISO-ROW                    
081400       IF IN-FIL-IDPGM = 'W4183300'                                       
081500         IF IN-EKH-DAAVIDAT > ZERO                                        
081600           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
081700           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
081800         ELSE                                                             
081900           MOVE WS-TIAA              TO WS-TIAA-CR                        
082000           MOVE WS-TIMM              TO WS-TIMM-CR                        
082100         END-IF                                                           
082200       ELSE                                                               
082300         MOVE WS-TIAA                TO WS-TIAA-CR                        
082400         MOVE WS-TIMM                TO WS-TIMM-CR                        
082500       END-IF                                                             
082600       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
082700       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
082800       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
082900       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
083000       IF CURR-KDSVAR = ' '                                               
083100         IF IN-EKH-IDDISTR > ZERO                                         
083200           MOVE CURR-PRKURS-NEW TO WS-PRKURS-BR                           
083300         ELSE                                                             
083400           IF WS-PRKURS = ZERO                                            
083500             MOVE 1             TO WS-PRKURS-BR                           
083600           END-IF                                                         
083700         END-IF                                                           
083800       ELSE                                                               
083900         MOVE 1                 TO WS-PRKURS-BR                           
084000       END-IF                                                             
084100       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-BR *                     
084200                                       CURR-REVALUTA-TO                   
084300       END-COMPUTE                                                        
084400       IF CURR-REVALUTA-TO = +1                                           
084500         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
084600       END-IF                                                             
084700       IF CURR-REVALUTA-TO = +10                                          
084800         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
084900       END-IF                                                             
085000       IF CURR-REVALUTA-TO = +100                                         
085100         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
085200       END-IF                                                             
085300     END-IF                                                               
085400     IF (IN-EKH-KDEKHHT = '102'                                           
085500     AND IN-EKH-KDEKSHT = '120')                                          
085600     OR (IN-EKH-KDEKHHT = '102'                                           
085700     AND IN-EKH-KDEKSHT = '124')                                          
085800     OR (IN-EKH-KDEKHHT = '102'                                           
085900     AND IN-EKH-KDEKSHT = '125')                                          
086000     OR (IN-EKH-KDEKHHT = '102'                                           
086100     AND IN-EKH-KDEKSHT = '130')                                          
086200     OR (IN-EKH-KDEKHHT = '102'                                           
086300     AND IN-EKH-KDEKSHT = '134')                                          
086400     OR (IN-EKH-KDEKHHT = '303'                                           
086500     AND IN-EKH-KDEKSHT = '301')                                          
086600     OR (IN-EKH-KDEKHHT = '303'                                           
086700     AND IN-EKH-KDEKSHT = '307')                                          
086800     OR (IN-EKH-KDEKHHT = '303'                                           
086900     AND IN-EKH-KDEKSHT = '371')                                          
087000     OR (IN-EKH-KDEKHHT = '303'                                           
087100     AND IN-EKH-KDEKSHT = '3XX')                                          
087200       MOVE 'BRL'                 TO R3-HEAD-CURRENCY                     
087300       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
087400     END-IF                                                               
087500     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
087600     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
087700     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
087800     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
087900     MOVE JA                      TO WS-HEADER-SW                         
088000     MOVE NEJ                     TO WS-LINE-SW                           
088100                                                                          
088200* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57053B                       
088300       PERFORM S004-WRITE-W57053B-HEAD                                    
088400     .                                                                    
088500     EJECT                                                                
088600                                                                          
088700 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
088800     MOVE SPACE                   TO R3-HEAD-R3                           
088900     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
089000     MOVE 'BR12'                  TO R3-HEAD-COMPANY-CODE                 
089100                                     R3-HEAD-CONTROL-AREA                 
089200     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
089300     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
089400     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
089500     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
089600     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
089700     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
089800       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
089900     ELSE                                                                 
090000       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
090100     END-IF                                                               
090200     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
090300     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
090400     IF (IN-EKH-KDEKHHT = '204'                                           
090500     AND IN-EKH-KDEKSHT = '301')                                          
090600       MOVE 'BRL'                 TO R3-HEAD-CURRENCY                     
090700       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
090800     ELSE                                                                 
090900       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
091000       MOVE WS-PRKURS-BR2         TO R3-HEAD-EXCHANGE-RATE                
091100       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
091200       IF IN-FIL-IDPGM = 'W4183300'                                       
091300         IF IN-EKH-DAAVIDAT > ZERO                                        
091400           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
091500           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
091600         ELSE                                                             
091700           MOVE WS-TIAA              TO WS-TIAA-CR                        
091800           MOVE WS-TIMM              TO WS-TIMM-CR                        
091900         END-IF                                                           
092000       ELSE                                                               
092100         MOVE WS-TIAA                TO WS-TIAA-CR                        
092200         MOVE WS-TIMM                TO WS-TIMM-CR                        
092300       END-IF                                                             
092400       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
092500       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
092600       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
092700       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
092800       IF CURR-KDSVAR = ' '                                               
092900         IF IN-EKH-IDDISTR > ZERO                                         
093000           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
093100         ELSE                                                             
093200           IF WS-PRKURS = ZERO                                            
093300             MOVE 1             TO WS-PRKURS                              
093400           END-IF                                                         
093500         END-IF                                                           
093600       ELSE                                                               
093700         MOVE 1                 TO WS-PRKURS                              
093800       END-IF                                                             
093900       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
094000                                       CURR-REVALUTA-TO                   
094100       END-COMPUTE                                                        
094200       IF CURR-REVALUTA-TO = +1                                           
094300         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
094400       END-IF                                                             
094500       IF CURR-REVALUTA-TO = +10                                          
094600         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
094700       END-IF                                                             
094800       IF CURR-REVALUTA-TO = +100                                         
094900         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
095000       END-IF                                                             
095100     END-IF                                                               
095200     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
095300     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
095400     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
095500     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
095600     MOVE JA                      TO WS-HEADER-SW                         
095700     MOVE NEJ                     TO WS-LINE-SW                           
095800                                                                          
095900* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57053B                       
096000       PERFORM S004-WRITE-W57053B-HEAD                                    
096100     .                                                                    
096200     EJECT                                                                
096300                                                                          
096400 CD-BUILD-COMMON-610-PART SECTION.                                        
096500     MOVE SPACE               TO R3-LINE-R3                               
096600     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
096700                                 R3-LINE-DUE-DATE                         
096800                                 R3-LINE-AMOUNT                           
096900                                 R3-LINE-AMOUNT-LC                        
097000                                 R3-LINE-TAX-AMOUNT                       
097100                                 R3-LINE-TAX-AMOUNT-LC                    
097200                                 R3-LINE-NUMBER-OF-DAYS                   
097300                                 R3-LINE-QUANTITY                         
097400                                 R3-LINE-SAMNR                            
097500     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
097600     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
097700     MOVE 'BR12'              TO R3-LINE-COMPANY-CODE                     
097800     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
097900     IF SYST-KDPOST = '50'                                                
098000       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
098100     ELSE                                                                 
098200       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
098300     END-IF                                                               
098400     IF SYST-IDPRCTR NOT = SPACE                                          
098500       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
098600       IF WS-PRCTR-PRODSL = '??'                                          
098700         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
098800         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
098900       END-IF                                                             
099000       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400                                                                          
099500 CE-SCHEDULE-LINE-GL SECTION.                                             
099600     MOVE NEJ                     TO WS-HEADER-SW                         
099700     MOVE JA                      TO WS-LINE-SW                           
099800     EVALUATE IN-EKH-KDEKHHT                                              
099900     WHEN '102'                                                           
100000          PERFORM CEB-MAIN-EVENT-102                                      
100100     WHEN '103'                                                           
100200          PERFORM CEC-MAIN-EVENT-103                                      
100300     WHEN '201'                                                           
100400          PERFORM CED-MAIN-EVENT-201                                      
100500     WHEN '203'                                                           
100600          PERFORM CEF-MAIN-EVENT-203                                      
100700     WHEN '204'                                                           
100800          PERFORM CEG-MAIN-EVENT-204                                      
100900     WHEN '302'                                                           
101000          PERFORM CEI-MAIN-EVENT-302                                      
101100     WHEN '303'                                                           
101200          PERFORM CEJ-MAIN-EVENT-303                                      
101300     WHEN '401'                                                           
101400          PERFORM CEK-MAIN-EVENT-401                                      
101500     WHEN '402'                                                           
101600          PERFORM CEL-MAIN-EVENT-402                                      
101700     WHEN '403'                                                           
101800          PERFORM CEM-MAIN-EVENT-403                                      
101900     WHEN '404'                                                           
102000          PERFORM CEN-MAIN-EVENT-404                                      
102100     END-EVALUATE                                                         
102200     .                                                                    
102300     EJECT                                                                
102400                                                                          
102500 CEB-MAIN-EVENT-102 SECTION.                                              
102600     EVALUATE IN-EKH-KDEKSHT                                              
102700     WHEN '102'                                                           
102800          PERFORM CEBB-SUB-EVENT-102-102                                  
102900     WHEN '120'                                                           
103000          PERFORM CEBD-SUB-EVENT-102-120                                  
103100     WHEN '121'                                                           
103200          PERFORM CEBD-SUB-EVENT-102-121                                  
103300     WHEN '122'                                                           
103400          PERFORM CEBD-SUB-EVENT-102-122                                  
103500     WHEN '123'                                                           
103600          PERFORM CEBD-SUB-EVENT-102-123                                  
103700     WHEN '124'                                                           
103800          PERFORM CEBD-SUB-EVENT-102-124                                  
103900     WHEN '125'                                                           
104000          PERFORM CEBD-SUB-EVENT-102-125                                  
104100     WHEN '126'                                                           
104200          PERFORM CEBD-SUB-EVENT-102-126                                  
104300     WHEN '130'                                                           
104400          PERFORM CEBE-SUB-EVENT-102-130                                  
104500     WHEN '131'                                                           
104600          PERFORM CEBE-SUB-EVENT-102-131                                  
104700     WHEN '132'                                                           
104800          PERFORM CEBE-SUB-EVENT-102-132                                  
104900     WHEN '134'                                                           
105000          PERFORM CEBE-SUB-EVENT-102-134                                  
105100     END-EVALUATE                                                         
105200     .                                                                    
105300     EJECT                                                                
105400                                                                          
105500 CEBB-SUB-EVENT-102-102 SECTION.                                          
105600     EVALUATE IN-EKH-KDEKNIVA                                             
105700     WHEN 'DET'                                                           
105800       IF SYST-IDSEKVNR = 1                                               
105900         IF IN-EKH-KVANTAL > 0                                            
106000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
106100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
106200           COMPUTE R3-LINE-AMOUNT-LC =                                    
106300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
106400           IF IN-EKH-KDVALISO = 'BRL'                                     
106500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
106600           END-IF                                                         
106700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
106800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
106900           MOVE SPACE               TO WS-ALLOCATE-REF                    
107000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
107100           PERFORM S02-WRITE-W57051B                                      
107200         END-IF                                                           
107300       END-IF                                                             
107400                                                                          
107500       IF SYST-IDSEKVNR = 2                                               
107600         IF IN-EKH-KVANTAL < 0                                            
107700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
107800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
107900           COMPUTE R3-LINE-AMOUNT-LC =                                    
108000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
108100           IF IN-EKH-KDVALISO = 'BRL'                                     
108200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
108300           END-IF                                                         
108400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
108500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
108600           MOVE SPACE               TO WS-ALLOCATE-REF                    
108700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
108800           PERFORM S02-WRITE-W57051B                                      
108900         END-IF                                                           
109000       END-IF                                                             
109100                                                                          
109200       IF SYST-IDSEKVNR = 3                                               
109300         IF IN-EKH-KVANTAL < 0                                            
109400           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
109500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
109600           COMPUTE R3-LINE-AMOUNT-LC =                                    
109700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
109800           IF IN-EKH-KDVALISO = 'BRL'                                     
109900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
110000           END-IF                                                         
110100           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
110200           PERFORM S02-WRITE-W57051B                                      
110300         END-IF                                                           
110400       END-IF                                                             
110500                                                                          
110600       IF SYST-IDSEKVNR = 4                                               
110700         IF IN-EKH-KVANTAL > 0                                            
110800           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
110900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
111000           COMPUTE R3-LINE-AMOUNT-LC =                                    
111100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
111200           IF IN-EKH-KDVALISO = 'BRL'                                     
111300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
111400           END-IF                                                         
111500           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
111600           PERFORM S02-WRITE-W57051B                                      
111700         END-IF                                                           
111800       END-IF                                                             
111900                                                                          
112000     END-EVALUATE                                                         
112100     .                                                                    
112200     EJECT                                                                
112300                                                                          
112400 CEBD-SUB-EVENT-102-120 SECTION.                                          
112500     EVALUATE IN-EKH-KDEKNIVA                                             
112600     WHEN 'DET'                                                           
112700       IF SYST-IDSEKVNR = 1                                               
112800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
112900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
113000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
113100          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR * -1            
113200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
113300         PERFORM S03-WRITE-W57072                                         
113400       END-IF                                                             
113500                                                                          
113600     WHEN 'FÖRS'                                                          
113700     WHEN 'FRAKT'                                                         
113800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
113900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
114000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
114100               IN-EKH-SUBEL / WS-PRKURS-BR  * -1                          
114200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
114300       PERFORM S04-WRITE-W57053B                                          
114400                                                                          
114500     WHEN 'EMB'                                                           
114600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
114700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
114800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
114900               IN-EKH-SUBEL / WS-PRKURS-BR  * -1                          
115000       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
115100       PERFORM S04-WRITE-W57053B                                          
115200                                                                          
115300     WHEN 'DDI'                                                           
115400       IF IN-EKH-SUBEL > ZERO                                             
115500         IF SYST-IDSEKVNR = 1                                             
115600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
115700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
115800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
115900                   IN-EKH-SUBEL                                           
116000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
116100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
116200           PERFORM S04-WRITE-W57053B                                      
116300         END-IF                                                           
116400       ELSE                                                               
116500         IF SYST-IDSEKVNR = 2                                             
116600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
116700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
116800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
116900                   IN-EKH-SUBEL                                           
117000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
117100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
117200           PERFORM S04-WRITE-W57053B                                      
117300         END-IF                                                           
117400       END-IF                                                             
117500     END-EVALUATE                                                         
117600     .                                                                    
117700     EJECT                                                                
117800                                                                          
117900 CEBD-SUB-EVENT-102-121 SECTION.                                          
118000     EVALUATE IN-EKH-KDEKNIVA                                             
118100     WHEN 'DET'                                                           
118200       IF SYST-IDSEKVNR = 1                                               
118300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
118400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
118500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
118600             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-BR3            
118700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
118800         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-1                      
118900         PERFORM S03-WRITE-W57072                                         
119000       END-IF                                                             
119100                                                                          
119200       IF SYST-IDSEKVNR = 2                                               
119300         PERFORM S13-GET-LANDING-COST                                     
119400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
119500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
119600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
119700            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
119910*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
119920*           (IN-EKH-KVANTAL *                                             
119930*           IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                  
120000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
120100         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-2                      
120200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
120300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
120400         MOVE SPACE               TO WS-ALLOCATE-REF                      
120500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
120600         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
120700         PERFORM S03-WRITE-W57072                                         
120800       END-IF                                                             
120900                                                                          
121000*      IF SYST-IDSEKVNR = 3                                               
121100*        MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
121200*        MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
121300*        COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
121400*           WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
121500*        MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
121600*        MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
121700*        MOVE SPACE               TO WS-ALLOCATE-DISTR                    
121800*        MOVE SPACE               TO WS-ALLOCATE-REF                      
121900*        MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
122000*        PERFORM S03-WRITE-W57072                                         
122100*      END-IF                                                             
122200                                                                          
122300     WHEN 'FÖRS'                                                          
122400     WHEN 'FRAKT'                                                         
122500       IF SYST-IDSEKVNR = 1                                               
122600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
122700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
122800         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
122900                 IN-EKH-SUBEL / WS-PRKURS-BR3                             
123000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
123100         PERFORM S04-WRITE-W57053B                                        
123200       END-IF                                                             
123300                                                                          
123400       IF SYST-IDSEKVNR = 2                                               
123500         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
123600         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
123700         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
123800                 IN-EKH-SUBEL / WS-PRKURS-BR3                             
123900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
124000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
124100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
124200         MOVE SPACE               TO WS-ALLOCATE-REF                      
124300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
124400         PERFORM S04-WRITE-W57053B                                        
124500       END-IF                                                             
124600                                                                          
124700     WHEN 'EMB'                                                           
124800       IF SYST-IDSEKVNR = 1                                               
124900         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
125000         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
125100         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
125200                 IN-EKH-SUBEL / WS-PRKURS-BR3                             
125300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
125400         PERFORM S04-WRITE-W57053B                                        
125500       END-IF                                                             
125600                                                                          
125700       IF SYST-IDSEKVNR = 2                                               
125800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
125900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
126000         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
126100                 IN-EKH-SUBEL / WS-PRKURS-BR3                             
126200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
126300         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
126400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
126500         MOVE SPACE               TO WS-ALLOCATE-REF                      
126600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
126700         PERFORM S04-WRITE-W57053B                                        
126800       END-IF                                                             
126900     END-EVALUATE                                                         
127000                                                                          
127100     .                                                                    
127200     EJECT                                                                
127300                                                                          
127400 CEBD-SUB-EVENT-102-122 SECTION.                                          
127500     EVALUATE IN-EKH-KDEKNIVA                                             
127600     WHEN 'DET'                                                           
127700       IF IN-EKH-KVANTAL > 0                                              
127800         IF SYST-IDSEKVNR = 1                                             
127900           PERFORM S13-GET-LANDING-COST                                   
128000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
128100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
128200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
128300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
128510*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
128520*           (IN-EKH-KVANTAL *                                             
128530*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
128600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
128700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
128800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
128900           MOVE SPACE               TO WS-ALLOCATE-REF                    
129000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
129100           PERFORM S02-WRITE-W57051B                                      
129200         END-IF                                                           
129300                                                                          
129400         IF SYST-IDSEKVNR = 4                                             
129500           PERFORM S13-GET-LANDING-COST                                   
129600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
129700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
129800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
129900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
130110*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
130120*           (IN-EKH-KVANTAL *                                             
130130*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
130200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
130300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
130400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
130500           MOVE SPACE               TO WS-ALLOCATE-REF                    
130600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
130700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
130800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
130900           PERFORM S02-WRITE-W57051B                                      
131000         END-IF                                                           
131100       END-IF                                                             
131200                                                                          
131300                                                                          
131400       IF IN-EKH-KVANTAL < 0                                              
131500         PERFORM S13-GET-LANDING-COST                                     
131600         IF SYST-IDSEKVNR = 2                                             
131700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
131800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
131900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
132000            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
132210*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
132220*           (IN-EKH-KVANTAL *                                             
132230*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
132300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
132400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
132500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
132600           MOVE SPACE               TO WS-ALLOCATE-REF                    
132700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
132800           MOVE SPACE             TO R3-LINE-COST-CENTER                  
132900           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
133000           PERFORM S02-WRITE-W57051B                                      
133100         END-IF                                                           
133200                                                                          
133300         IF SYST-IDSEKVNR = 3                                             
133400           PERFORM S13-GET-LANDING-COST                                   
133500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
133600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
133700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
133800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
134010*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
134020*           (IN-EKH-KVANTAL *                                             
134030*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
134100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
134200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
134300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
134400           MOVE SPACE               TO WS-ALLOCATE-REF                    
134500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
134501           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
134600           PERFORM S02-WRITE-W57051B                                      
134700         END-IF                                                           
134800       END-IF                                                             
134900                                                                          
135000     END-EVALUATE                                                         
135100     .                                                                    
135200     EJECT                                                                
135300                                                                          
135400 CEBD-SUB-EVENT-102-123 SECTION.                                          
135500     EVALUATE IN-EKH-KDEKNIVA                                             
135600     WHEN 'DET'                                                           
135700       IF SYST-IDSEKVNR = 1                                               
135800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
135900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
136000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
136100              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
136200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
136300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
136400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
136500         MOVE SPACE               TO WS-ALLOCATE-REF                      
136600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
136700         PERFORM S02-WRITE-W57051B                                        
136800       END-IF                                                             
136900                                                                          
137000       IF SYST-IDSEKVNR = 2                                               
137100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
137200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
137300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
137400             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
137500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
137600         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
137700         PERFORM S02-WRITE-W57051B                                        
137800       END-IF                                                             
137900     END-EVALUATE                                                         
138000     .                                                                    
138100     EJECT                                                                
138200                                                                          
138300 CEBD-SUB-EVENT-102-124 SECTION.                                          
138400     EVALUATE IN-EKH-KDEKNIVA                                             
138500     WHEN 'DET'                                                           
138600       IF SYST-IDSEKVNR = 1                                               
138700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
138800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
138900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
139000         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR  * -1            
139100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
139200         MOVE SPACE               TO WS-ALLOCATE-DC                       
139300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
139400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
139500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
139600         PERFORM S03-WRITE-W57072                                         
139700       END-IF                                                             
139800                                                                          
139900     WHEN 'FÖRS'                                                          
140000     WHEN 'FRAKT'                                                         
140100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
140200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
140300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
140400               IN-EKH-SUBEL * -1                                          
140500       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
140600       MOVE SPACE               TO WS-ALLOCATE-DC                         
140700       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
140800       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
140900       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
141000       PERFORM S04-WRITE-W57053B                                          
141100                                                                          
141200     WHEN 'EMB'                                                           
141300       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
141400       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
141500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
141600               (IN-EKH-SUBEL / WS-PRKURS-BR) * -1                         
141700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
141800       MOVE SPACE               TO WS-ALLOCATE-DC                         
141900       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
142000       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
142100       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
142200       PERFORM S04-WRITE-W57053B                                          
142300                                                                          
142400     WHEN 'DDI'                                                           
142500       IF IN-EKH-SUBEL > ZERO                                             
142600         IF SYST-IDSEKVNR = 1                                             
142700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
142800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
142900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
143000                   IN-EKH-SUBEL                                           
143100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
143200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
143300           MOVE SPACE               TO WS-ALLOCATE-DC                     
143400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
143500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
143600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
143700           PERFORM S04-WRITE-W57053B                                      
143800         END-IF                                                           
143900       ELSE                                                               
144000         IF SYST-IDSEKVNR = 2                                             
144100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
144200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
144300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
144400                   IN-EKH-SUBEL                                           
144500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
144600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
144700           MOVE SPACE               TO WS-ALLOCATE-DC                     
144800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
144900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
145000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
145100           PERFORM S04-WRITE-W57053B                                      
145200         END-IF                                                           
145300       END-IF                                                             
145400     END-EVALUATE                                                         
145500     .                                                                    
145600     EJECT                                                                
145700                                                                          
145800 CEBD-SUB-EVENT-102-125 SECTION.                                          
145900     EVALUATE IN-EKH-KDEKNIVA                                             
146000     WHEN 'DET'                                                           
146100       IF SYST-IDSEKVNR = 1                                               
146200         PERFORM S13-GET-LANDING-COST                                     
146300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
146400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
146500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
146600         (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR)                
146810*        (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR) +              
146820*        (IN-EKH-KVANTAL *                                                
146830*         IN-EKH-PRARTNTO / WS-PRKURS-BR * WS-MARKUP)                     
146900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
147000         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
147100         PERFORM S03-WRITE-W57072                                         
147200       END-IF                                                             
147300                                                                          
147400*      IF SYST-IDSEKVNR = 2                                               
147500*        PERFORM S13-GET-LANDING-COST                                     
147600*        MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
147700*        MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
147800*        COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
147900*           (IN-EKH-KVANTAL *                                             
148000*            IN-EKH-PRARTNTO / WS-PRKURS-BR * WS-MARKUP)                  
148100*        MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
148200*        SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
148300*        PERFORM S03-WRITE-W57072                                         
148400*      END-IF                                                             
148500                                                                          
148600     WHEN 'FÖRS'                                                          
148700     WHEN 'FRAKT'                                                         
148800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
148900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
149000       MOVE SPACE             TO R3-LINE-COST-CENTER                      
149100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
149200          IN-EKH-SUBEL / WS-PRKURS-BR  * -1                               
149300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
149400       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
149500       PERFORM S04-WRITE-W57053B                                          
149600                                                                          
149700     WHEN 'EMB'                                                           
149800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
149900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
150000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
150100          IN-EKH-SUBEL / WS-PRKURS-BR  * -1                               
150200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
150300       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
150400       PERFORM S04-WRITE-W57053B                                          
150500                                                                          
150600     WHEN 'DDI'                                                           
150700       IF SPAR-SUMMA-102-125 < ZERO                                       
150800         IF SYST-IDSEKVNR = 1                                             
150900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
151000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
151100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
151200                   SPAR-SUMMA-102-125                                     
151300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
151400           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
151500           PERFORM S04-WRITE-W57053B                                      
151600         END-IF                                                           
151700       END-IF                                                             
151800       IF SPAR-SUMMA-102-125 > ZERO                                       
151900         IF SYST-IDSEKVNR = 2                                             
152000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
152100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
152200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
152300                   SPAR-SUMMA-102-125                                     
152400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
152500           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
152600           PERFORM S04-WRITE-W57053B                                      
152700         END-IF                                                           
152800       END-IF                                                             
152900                                                                          
153000     END-EVALUATE                                                         
153100     .                                                                    
153200     EJECT                                                                
153300                                                                          
153400 CEBD-SUB-EVENT-102-126 SECTION.                                          
153500     EVALUATE IN-EKH-KDEKNIVA                                             
153600     WHEN 'DET'                                                           
153700         IF SYST-IDSEKVNR = 1                                             
153800           PERFORM S13-GET-LANDING-COST                                   
153900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
154000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
154100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
154200            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-BR3))          
154410*           (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-BR3))          
154420*           + (IN-EKH-KVANTAL *                                           
154430*           (IN-EKH-PRARTNTO / WS-PRKURS-BR3) * WS-MARKUP)                
154500           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
154600           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-126-1               
154700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
154800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
154900           MOVE SPACE               TO WS-ALLOCATE-REF                    
155000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
155100           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
155200           PERFORM S02-WRITE-W57051B                                      
155300         END-IF                                                           
155400                                                                          
155500         IF SYST-IDSEKVNR = 2                                             
155600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
155700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
155800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
155900            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-BR3))          
156000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
156100           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-126-2               
156200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
156300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
156400           MOVE SPACE               TO WS-ALLOCATE-REF                    
156500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
156600           MOVE SPACE               TO R3-LINE-COST-CENTER                
156700           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
156800           PERFORM S02-WRITE-W57051B                                      
156900         END-IF                                                           
157000                                                                          
157100*        IF SYST-IDSEKVNR = 3                                             
157200*          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
157300*          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
157400*          COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
157500*             WS-LINE-AMOUNT-126-1 - WS-LINE-AMOUNT-126-2                 
157600*          MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
157700*          MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
157800*          MOVE SPACE               TO WS-ALLOCATE-DISTR                  
157900*          MOVE SPACE               TO WS-ALLOCATE-REF                    
158000*          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
158100*          MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
158200*          PERFORM S02-WRITE-W57051B                                      
158300*        END-IF                                                           
158400                                                                          
158500     END-EVALUATE                                                         
158600     .                                                                    
158700     EJECT                                                                
158800                                                                          
158900 CEBE-SUB-EVENT-102-130 SECTION.                                          
159000     EVALUATE IN-EKH-KDEKNIVA                                             
159100     WHEN 'DET'                                                           
159200       IF SYST-IDSEKVNR = 1                                               
159300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
159400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
159500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
159600          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR * -1            
159700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
159800         PERFORM S03-WRITE-W57072                                         
159900       END-IF                                                             
160000                                                                          
160100     WHEN 'EMB'                                                           
160200     WHEN 'FÖRS'                                                          
160300     WHEN 'FRAKT'                                                         
160400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
160500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
160600       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
160700               IN-EKH-SUBEL / WS-PRKURS-BR  * -1                          
160800       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
160900       PERFORM S04-WRITE-W57053B                                          
161000                                                                          
161100     WHEN 'DDI'                                                           
161200       IF IN-EKH-SUBEL > ZERO                                             
161300         IF SYST-IDSEKVNR = 1                                             
161400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
161500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
161600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
161700                   IN-EKH-SUBEL                                           
161800           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
161900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
162000           PERFORM S04-WRITE-W57053B                                      
162100         END-IF                                                           
162200       ELSE                                                               
162300         IF SYST-IDSEKVNR = 2                                             
162400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
162500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
162600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
162700                   IN-EKH-SUBEL                                           
162800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
162900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
163000           PERFORM S04-WRITE-W57053B                                      
163100         END-IF                                                           
163200       END-IF                                                             
163300     END-EVALUATE                                                         
163400     .                                                                    
163500     EJECT                                                                
163600                                                                          
163700 CEBE-SUB-EVENT-102-131 SECTION.                                          
163800     EVALUATE IN-EKH-KDEKNIVA                                             
163900     WHEN 'DET'                                                           
164000       IF SYST-IDSEKVNR = 1                                               
164100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
164200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
164300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
164400             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-BR3            
164500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
164600         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-131-1                      
164700         PERFORM S03-WRITE-W57072                                         
164800       END-IF                                                             
164900                                                                          
165000       IF SYST-IDSEKVNR = 2                                               
165100         PERFORM S13-GET-LANDING-COST                                     
165200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
165300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
165400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
165500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
165710*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
165720*           (IN-EKH-KVANTAL *                                             
165730*           IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                  
165800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
165900         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-131-2                      
166000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
166100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
166200         MOVE SPACE               TO WS-ALLOCATE-REF                      
166300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
166400         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
166500         PERFORM S03-WRITE-W57072                                         
166600       END-IF                                                             
166700                                                                          
166800*      IF SYST-IDSEKVNR = 3                                               
166900*        MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
167000*        MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
167100*        COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
167200*           WS-LINE-AMOUNT-131-2 - WS-LINE-AMOUNT-131-1                   
167300*        MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
167400*        MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
167500*        MOVE SPACE               TO WS-ALLOCATE-DISTR                    
167600*        MOVE SPACE               TO WS-ALLOCATE-REF                      
167700*        MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
167800*        PERFORM S03-WRITE-W57072                                         
167900*      END-IF                                                             
168000                                                                          
168100     WHEN 'EMB'                                                           
168200     WHEN 'FÖRS'                                                          
168300     WHEN 'FRAKT'                                                         
168400       IF SYST-IDSEKVNR = 1                                               
168500         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
168600         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
168700         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
168800                 IN-EKH-SUBEL / WS-PRKURS-BR3                             
168900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
169000         PERFORM S04-WRITE-W57053B                                        
169100       END-IF                                                             
169200                                                                          
169300       IF SYST-IDSEKVNR = 2                                               
169400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
169500         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
169600         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
169700                 IN-EKH-SUBEL / WS-PRKURS-BR3                             
169800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
169900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
170000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
170100         MOVE SPACE               TO WS-ALLOCATE-REF                      
170200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
170300         PERFORM S04-WRITE-W57053B                                        
170400       END-IF                                                             
170500                                                                          
170600     END-EVALUATE                                                         
170700     .                                                                    
170800     EJECT                                                                
170900                                                                          
171000 CEBE-SUB-EVENT-102-132 SECTION.                                          
171100     EVALUATE IN-EKH-KDEKNIVA                                             
171200     WHEN 'DET'                                                           
171300       IF IN-EKH-KVANTAL > 0                                              
171400         IF SYST-IDSEKVNR = 1                                             
171500           PERFORM S13-GET-LANDING-COST                                   
171600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
171700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
171800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
171900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
172110*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
172120*           (IN-EKH-KVANTAL *                                             
172130*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
172200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
172300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
172400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
172500           MOVE SPACE               TO WS-ALLOCATE-REF                    
172600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
172700           PERFORM S02-WRITE-W57051B                                      
172800         END-IF                                                           
172900                                                                          
173000         IF SYST-IDSEKVNR = 4                                             
173100           PERFORM S13-GET-LANDING-COST                                   
173200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
173300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
173400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
173500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
173710*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
173720*           (IN-EKH-KVANTAL *                                             
173730*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
173800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
173900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
174000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
174100           MOVE SPACE               TO WS-ALLOCATE-REF                    
174200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
174300           MOVE SPACE               TO R3-LINE-COST-CENTER                
174400           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
174500           PERFORM S02-WRITE-W57051B                                      
174600         END-IF                                                           
174700       END-IF                                                             
174800                                                                          
174900                                                                          
175000       IF IN-EKH-KVANTAL < 0                                              
175100         PERFORM S13-GET-LANDING-COST                                     
175200         IF SYST-IDSEKVNR = 2                                             
175300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
175400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
175500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
175600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
175810*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
175820*           (IN-EKH-KVANTAL *                                             
175830*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
175900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
176000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
176100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
176200           MOVE SPACE               TO WS-ALLOCATE-REF                    
176300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
176400           MOVE SPACE             TO R3-LINE-COST-CENTER                  
176500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
176600           PERFORM S02-WRITE-W57051B                                      
176700         END-IF                                                           
176800                                                                          
176900         IF SYST-IDSEKVNR = 3                                             
177000           PERFORM S13-GET-LANDING-COST                                   
177100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
177200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
177300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
177400            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3)            
177610*           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3) +          
177620*           (IN-EKH-KVANTAL *                                             
177630*            IN-EKH-PRARTNTO / WS-PRKURS-BR3 * WS-MARKUP)                 
177700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
177800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
177900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
178000           MOVE SPACE               TO WS-ALLOCATE-REF                    
178100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
178200           PERFORM S02-WRITE-W57051B                                      
178300         END-IF                                                           
178400       END-IF                                                             
178500                                                                          
178600     END-EVALUATE                                                         
178700     .                                                                    
178800     EJECT                                                                
178900                                                                          
179000 CEBE-SUB-EVENT-102-134 SECTION.                                          
179100     EVALUATE IN-EKH-KDEKNIVA                                             
179200     WHEN 'DET'                                                           
179300       IF SYST-IDSEKVNR = 1                                               
179400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
179500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
179600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
179700         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR  * -1            
179800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
179900         MOVE SPACE               TO WS-ALLOCATE-DC                       
180000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
180100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
180200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
180300         PERFORM S03-WRITE-W57072                                         
180400       END-IF                                                             
180500                                                                          
180600     WHEN 'FÖRS'                                                          
180700     WHEN 'FRAKT'                                                         
180800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
180900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
181000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
181100               IN-EKH-SUBEL * -1                                          
181200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
181300       MOVE SPACE               TO WS-ALLOCATE-DC                         
181400       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
181500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
181600       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
181700       PERFORM S04-WRITE-W57053B                                          
181800                                                                          
181900     WHEN 'EMB'                                                           
182000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
182100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
182200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
182300               (IN-EKH-SUBEL / WS-PRKURS-BR) * -1                         
182400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
182500       MOVE SPACE               TO WS-ALLOCATE-DC                         
182600       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
182700       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
182800       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
182900       PERFORM S04-WRITE-W57053B                                          
183000                                                                          
183100     WHEN 'DDI'                                                           
183200       IF IN-EKH-SUBEL > ZERO                                             
183300         IF SYST-IDSEKVNR = 1                                             
183400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
183500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
183600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
183700                   IN-EKH-SUBEL                                           
183800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
183900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
184000           MOVE SPACE               TO WS-ALLOCATE-DC                     
184100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
184200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
184300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
184400           PERFORM S04-WRITE-W57053B                                      
184500         END-IF                                                           
184600       ELSE                                                               
184700         IF SYST-IDSEKVNR = 2                                             
184800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
184900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
185000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
185100                   IN-EKH-SUBEL                                           
185200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
185300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
185400           MOVE SPACE               TO WS-ALLOCATE-DC                     
185500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
185600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
185700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
185800           PERFORM S04-WRITE-W57053B                                      
185900         END-IF                                                           
186000       END-IF                                                             
186100     END-EVALUATE                                                         
186200     .                                                                    
186300     EJECT                                                                
186400 CEC-MAIN-EVENT-103 SECTION.                                              
186500     EVALUATE IN-EKH-KDEKSHT                                              
186600     WHEN '102'                                                           
186700          PERFORM CECB-SUB-EVENT-103-102                                  
186800     WHEN '106'                                                           
186900          PERFORM CECB-SUB-EVENT-103-106                                  
187000     WHEN '107'                                                           
187100          PERFORM CECB-SUB-EVENT-103-107                                  
187200     END-EVALUATE                                                         
187300     .                                                                    
187400     EJECT                                                                
187500                                                                          
187600 CECB-SUB-EVENT-103-102 SECTION.                                          
187700     EVALUATE IN-EKH-KDEKNIVA                                             
187800     WHEN 'DET'                                                           
187900       IF SYST-IDSEKVNR = 1                                               
188000         IF IN-EKH-KVANTAL < 0                                            
188100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
188200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
188300           COMPUTE R3-LINE-AMOUNT-LC =                                    
188400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
188500           IF IN-EKH-KDVALISO = 'BRL'                                     
188600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
188700           END-IF                                                         
188800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
188900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
189000           MOVE SPACE               TO WS-ALLOCATE-REF                    
189100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
189200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
189300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
189400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
189500           PERFORM S04-WRITE-W57053B                                      
189600         END-IF                                                           
189700       END-IF                                                             
189800                                                                          
189900       IF SYST-IDSEKVNR = 2                                               
190000         IF IN-EKH-KVANTAL > 0                                            
190100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
190200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
190300           COMPUTE R3-LINE-AMOUNT-LC =                                    
190400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
190500           IF IN-EKH-KDVALISO = 'BRL'                                     
190600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
190700           END-IF                                                         
190800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
190900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
191000           MOVE SPACE               TO WS-ALLOCATE-REF                    
191100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
191200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
191300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
191400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
191500           PERFORM S04-WRITE-W57053B                                      
191600         END-IF                                                           
191700       END-IF                                                             
191800                                                                          
191900     WHEN 'KALK'                                                          
192000       IF SYST-IDSEKVNR = 1                                               
192100         IF IN-EKH-SUBEL > 0                                              
192200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
192300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
192400           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
192500           IF IN-EKH-KDVALISO = 'BRL'                                     
192600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
192700           END-IF                                                         
192800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
192900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
193000           MOVE SPACE               TO WS-ALLOCATE-REF                    
193100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
193200           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
193300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
193400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
193500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
193600           PERFORM S04-WRITE-W57053B                                      
193700         END-IF                                                           
193800       END-IF                                                             
193900                                                                          
194000       IF SYST-IDSEKVNR = 2                                               
194100         IF IN-EKH-SUBEL < 0                                              
194200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
194300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
194400           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
194500           IF IN-EKH-KDVALISO = 'BRL'                                     
194600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
194700           END-IF                                                         
194800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
194900           MOVE SPACE               TO WS-ALLOCATE-DC                     
195000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
195100           MOVE SPACE               TO WS-ALLOCATE-REF                    
195200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
195300           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
195400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
195500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
195600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
195700           PERFORM S04-WRITE-W57053B                                      
195800         END-IF                                                           
195900       END-IF                                                             
196000                                                                          
196100       IF SYST-IDSEKVNR = 3                                               
196200         IF IN-EKH-SUBEL > 0                                              
196300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
196400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
196500           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
196600           IF IN-EKH-KDVALISO = 'BRL'                                     
196700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
196800           END-IF                                                         
196900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
197000           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
197100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
197200           PERFORM S04-WRITE-W57053B                                      
197300         END-IF                                                           
197400       END-IF                                                             
197500                                                                          
197600       IF SYST-IDSEKVNR = 4                                               
197700         IF IN-EKH-SUBEL < 0                                              
197800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
197900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
198000           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
198100           IF IN-EKH-KDVALISO = 'BRL'                                     
198200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
198300           END-IF                                                         
198400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
198500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
198600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
198700           PERFORM S04-WRITE-W57053B                                      
198800         END-IF                                                           
198900       END-IF                                                             
199000                                                                          
199100     WHEN 'DDI'                                                           
199200       IF IN-EKH-SUBEL > ZERO                                             
199300         IF SYST-IDSEKVNR = 1                                             
199400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
199500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
199600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
199700                   IN-EKH-SUBEL                                           
199800           MOVE ZEROES              TO R3-LINE-AMOUNT                     
199900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
200000           MOVE SPACE               TO WS-ALLOCATE-DC                     
200100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
200200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
200300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
200400           PERFORM S04-WRITE-W57053B                                      
200500         END-IF                                                           
200600       ELSE                                                               
200700         IF SYST-IDSEKVNR = 2                                             
200800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
200900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
201000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
201100                   IN-EKH-SUBEL                                           
201200           MOVE ZEROES              TO R3-LINE-AMOUNT                     
201300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
201400           MOVE SPACE               TO WS-ALLOCATE-DC                     
201500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
201600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
201700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
201800           PERFORM S04-WRITE-W57053B                                      
201900         END-IF                                                           
202000       END-IF                                                             
202100     END-EVALUATE                                                         
202200     .                                                                    
202300     EJECT                                                                
202400                                                                          
202500 CECB-SUB-EVENT-103-106 SECTION.                                          
202600     EVALUATE IN-EKH-KDEKNIVA                                             
202700     WHEN 'DET'                                                           
202800       IF SYST-IDSEKVNR = 1                                               
202900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
203000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
203100         COMPUTE R3-LINE-AMOUNT-LC =                                      
203200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
203300         IF IN-EKH-KDVALISO = 'BRL'                                       
203400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
203500         END-IF                                                           
203600         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
203700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
203800         MOVE SPACE               TO WS-ALLOCATE-REF                      
203900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
204000         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
204100         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
204200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
204300         PERFORM S04-WRITE-W57053B                                        
204400       END-IF                                                             
204500                                                                          
204600                                                                          
204700     WHEN 'KALK'                                                          
204800       IF SYST-IDSEKVNR = 1                                               
204900         IF IN-EKH-SUBEL > 0                                              
205000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
205100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
205200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
205300           IF IN-EKH-KDVALISO = 'BRL'                                     
205400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
205500           END-IF                                                         
205600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
205700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
205800           MOVE SPACE               TO WS-ALLOCATE-REF                    
205900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
206000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
206100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
206200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
206300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
206400           PERFORM S04-WRITE-W57053B                                      
206500         END-IF                                                           
206600       END-IF                                                             
206700                                                                          
206800       IF SYST-IDSEKVNR = 2                                               
206900         IF IN-EKH-SUBEL < 0                                              
207000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
207100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
207200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
207300           IF IN-EKH-KDVALISO = 'BRL'                                     
207400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
207500           END-IF                                                         
207600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
207700           MOVE SPACE               TO WS-ALLOCATE-DC                     
207800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
207900           MOVE SPACE               TO WS-ALLOCATE-REF                    
208000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
208100           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
208200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
208300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
208400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
208500           PERFORM S04-WRITE-W57053B                                      
208600         END-IF                                                           
208700       END-IF                                                             
208800                                                                          
208900       IF SYST-IDSEKVNR = 3                                               
209000         IF IN-EKH-SUBEL > 0                                              
209100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
209200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
209300           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
209400           IF IN-EKH-KDVALISO = 'BRL'                                     
209500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
209600           END-IF                                                         
209700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
209800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
209900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
210000           PERFORM S04-WRITE-W57053B                                      
210100         END-IF                                                           
210200       END-IF                                                             
210300                                                                          
210400       IF SYST-IDSEKVNR = 4                                               
210500         IF IN-EKH-SUBEL < 0                                              
210600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
210700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
210800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
210900           IF IN-EKH-KDVALISO = 'BRL'                                     
211000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
211100           END-IF                                                         
211200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
211300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
211400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
211500           PERFORM S04-WRITE-W57053B                                      
211600         END-IF                                                           
211700       END-IF                                                             
211800                                                                          
211900     END-EVALUATE                                                         
212000     .                                                                    
212100     EJECT                                                                
212200                                                                          
212300 CECB-SUB-EVENT-103-107 SECTION.                                          
212400     EVALUATE IN-EKH-KDEKNIVA                                             
212500     WHEN 'DET'                                                           
212600       IF SYST-IDSEKVNR = 1                                               
212700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
212800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
212900         COMPUTE R3-LINE-AMOUNT-LC =                                      
213000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
213100         IF IN-EKH-KDVALISO = 'BRL'                                       
213200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
213300         END-IF                                                           
213400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
213500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
213600         MOVE SPACE               TO WS-ALLOCATE-REF                      
213700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
213800         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
213900         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
214000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
214100         PERFORM S04-WRITE-W57053B                                        
214200       END-IF                                                             
214300                                                                          
214400     WHEN 'KALK'                                                          
214500       IF SYST-IDSEKVNR = 1                                               
214600         IF IN-EKH-SUBEL > 0                                              
214700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
214800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
214900           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
215000           IF IN-EKH-KDVALISO = 'BRL'                                     
215100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
215200           END-IF                                                         
215300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
215400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
215500           MOVE SPACE               TO WS-ALLOCATE-REF                    
215600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
215700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
215800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
215900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
216000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
216100           PERFORM S04-WRITE-W57053B                                      
216200         END-IF                                                           
216300       END-IF                                                             
216400                                                                          
216500       IF SYST-IDSEKVNR = 2                                               
216600         IF IN-EKH-SUBEL < 0                                              
216700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
216800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
216900           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
217000           IF IN-EKH-KDVALISO = 'BRL'                                     
217100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
217200           END-IF                                                         
217300           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
217400           MOVE SPACE               TO WS-ALLOCATE-DC                     
217500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
217600           MOVE SPACE               TO WS-ALLOCATE-REF                    
217700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
217800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
217900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
218000           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
218100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
218200           PERFORM S04-WRITE-W57053B                                      
218300         END-IF                                                           
218400       END-IF                                                             
218500                                                                          
218600       IF SYST-IDSEKVNR = 3                                               
218700         IF IN-EKH-SUBEL > 0                                              
218800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
218900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
219000           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
219100           IF IN-EKH-KDVALISO = 'BRL'                                     
219200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
219300           END-IF                                                         
219400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
219500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
219600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
219700           PERFORM S04-WRITE-W57053B                                      
219800         END-IF                                                           
219900       END-IF                                                             
220000                                                                          
220100       IF SYST-IDSEKVNR = 4                                               
220200         IF IN-EKH-SUBEL < 0                                              
220300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
220400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
220500           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
220600           IF IN-EKH-KDVALISO = 'BRL'                                     
220700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
220800           END-IF                                                         
220900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
221000           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
221100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
221200           PERFORM S04-WRITE-W57053B                                      
221300         END-IF                                                           
221400       END-IF                                                             
221500                                                                          
221600     END-EVALUATE                                                         
221700     .                                                                    
221800     EJECT                                                                
221900                                                                          
222000 CED-MAIN-EVENT-201 SECTION.                                              
222100     EVALUATE IN-EKH-KDEKSHT                                              
222200     WHEN '201'                                                           
222300          PERFORM CEDA-SUB-EVENT-201-201                                  
222400     END-EVALUATE                                                         
222500     .                                                                    
222600     EJECT                                                                
222700                                                                          
222800 CEDA-SUB-EVENT-201-201 SECTION.                                          
222900     EVALUATE IN-EKH-KDEKNIVA                                             
223000     WHEN 'DET'                                                           
223100       IF SYST-IDSEKVNR = 1                                               
223200         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
223300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
223400         COMPUTE R3-LINE-AMOUNT-LC =                                      
223500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
223600         IF IN-EKH-KDVALISO = 'BRL'                                       
223700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
223800         END-IF                                                           
223900         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
224110         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
224200         PERFORM S03-WRITE-W57072                                         
224300       END-IF                                                             
224400                                                                          
224500       IF SYST-IDSEKVNR = 2                                               
224600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
224700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
224800         COMPUTE R3-LINE-AMOUNT-LC =                                      
224900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
225000         IF IN-EKH-KDVALISO = 'BRL'                                       
225100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
225200         END-IF                                                           
225300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
225400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
225500         MOVE SPACE               TO WS-ALLOCATE-REF                      
225600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
225810         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
225900         PERFORM S03-WRITE-W57072                                         
226000       END-IF                                                             
226100     END-EVALUATE                                                         
226200     .                                                                    
226300     EJECT                                                                
226400                                                                          
226500 CEF-MAIN-EVENT-203 SECTION.                                              
226600     EVALUATE IN-EKH-KDEKSHT                                              
226700     WHEN '201'                                                           
226800          PERFORM CEFA-SUB-EVENT-203-201                                  
226900     END-EVALUATE                                                         
227000     .                                                                    
227100     EJECT                                                                
227200                                                                          
227300 CEFA-SUB-EVENT-203-201 SECTION.                                          
227400     EVALUATE IN-EKH-KDEKNIVA                                             
227500     WHEN 'DET'                                                           
227600       IF SYST-IDSEKVNR = 1                                               
227700         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
227800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
227900         COMPUTE R3-LINE-AMOUNT-LC =                                      
228000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
228100         IF IN-EKH-KDVALISO = 'BRL'                                       
228200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
228300         END-IF                                                           
228400         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
228610         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
228700         PERFORM S03-WRITE-W57072                                         
228800       END-IF                                                             
228900                                                                          
229000       IF SYST-IDSEKVNR = 2                                               
229100         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
229200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
229300         COMPUTE R3-LINE-AMOUNT-LC =                                      
229400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
229500         IF IN-EKH-KDVALISO = 'BRL'                                       
229600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
229700         END-IF                                                           
229800         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
229900         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
230000         MOVE SPACE             TO WS-ALLOCATE-REF                        
230100         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
230310         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
230400         PERFORM S03-WRITE-W57072                                         
230500       END-IF                                                             
230600     END-EVALUATE                                                         
230700     .                                                                    
230800     EJECT                                                                
230900                                                                          
231000 CEG-MAIN-EVENT-204 SECTION.                                              
231100     EVALUATE IN-EKH-KDEKSHT                                              
231200     WHEN '201'                                                           
231300          PERFORM CEGA-SUB-EVENT-204-201                                  
231400     WHEN '301'                                                           
231500          PERFORM CEGB-SUB-EVENT-204-301                                  
231600     END-EVALUATE                                                         
231700     .                                                                    
231800     EJECT                                                                
231900                                                                          
232000 CEGA-SUB-EVENT-204-201 SECTION.                                          
232100     EVALUATE IN-EKH-KDEKNIVA                                             
232200     WHEN 'DET'                                                           
232300         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
232400* R-FAKTURA                                                               
232500       IF SYST-IDSEKVNR = 1                                               
232600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
232700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
232800         COMPUTE R3-LINE-AMOUNT-LC =                                      
232900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
233000         IF IN-EKH-KDVALISO = 'BRL'                                       
233100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
233200         END-IF                                                           
233300         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
233500         MOVE SPACE               TO WS-ALLOCATE-DC                       
233600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
233700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
233800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
233900         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
234000         PERFORM S03-WRITE-W57072                                         
234100       END-IF                                                             
234200                                                                          
234300       IF SYST-IDSEKVNR = 2                                               
234400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
234500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
234600         COMPUTE R3-LINE-AMOUNT-LC =                                      
234700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
234800         IF IN-EKH-KDVALISO = 'BRL'                                       
234900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
235000         END-IF                                                           
235100         MOVE SPACE               TO WS-ALLOCATE-DC                       
235200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
235300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
235400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
235610         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
235700         PERFORM S03-WRITE-W57072                                         
235800       END-IF                                                             
235900     END-EVALUATE                                                         
236000     .                                                                    
236100     EJECT                                                                
236200                                                                          
236300 CEGB-SUB-EVENT-204-301 SECTION.                                          
236400     EVALUATE IN-EKH-KDEKNIVA                                             
236500     WHEN 'DET'                                                           
236600* R-FAKTURA                                                               
236700       IF SYST-IDSEKVNR = 1                                               
236800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
236900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
237000         COMPUTE R3-LINE-AMOUNT-LC =                                      
237100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
237200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
237300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
237400         IF BET-KDTRADP(3:2) NOT = SPACE                                  
237500            MOVE '1'             TO WS-ACCOUNT-4                          
237600            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
237700         ELSE                                                             
237800            MOVE '3'             TO WS-ACCOUNT-4                          
237900            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
238000         END-IF                                                           
238100         PERFORM S03-WRITE-W57072                                         
238200       END-IF                                                             
238300                                                                          
238400       IF SYST-IDSEKVNR = 2                                               
238500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
238600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
238700         COMPUTE R3-LINE-AMOUNT-LC =                                      
238800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
238900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
239000         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
239100         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
239200         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
239300         MOVE SPACE             TO WS-ALLOCATE-REF                        
239400         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
239500         PERFORM S03-WRITE-W57072                                         
239600       END-IF                                                             
239700                                                                          
239800       IF SYST-IDSEKVNR = 3                                               
239900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
240000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
240100         COMPUTE R3-LINE-AMOUNT-LC =                                      
240200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
240300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
240400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
240500         IF BET-KDTRADP(3:2) NOT = SPACE                                  
240600            MOVE '1'             TO WS-ACCOUNT-4                          
240700            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
240800         ELSE                                                             
240900            MOVE '3'             TO WS-ACCOUNT-4                          
241000            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
241100         END-IF                                                           
241200         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
241300         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
241400         MOVE SPACE             TO WS-ALLOCATE-REF                        
241500         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
241600         PERFORM S03-WRITE-W57072                                         
241700       END-IF                                                             
241800                                                                          
241900     WHEN 'EMB'                                                           
242000     WHEN 'FÖRS'                                                          
242100     WHEN 'FRAKT'                                                         
242200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
242300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
242400       MOVE SYST-IDKST          TO WS-RED-IDKST                           
242500       IF WS-RED-IDKST > SPACE                                            
242600         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
242700       ELSE                                                               
242800         MOVE SPACE             TO R3-LINE-COST-CENTER                    
242900       END-IF                                                             
243000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
243100               IN-EKH-SUBEL * -1                                          
243200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
243300       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
243400       PERFORM S04-WRITE-W57053B                                          
243500                                                                          
243600     END-EVALUATE                                                         
243700     .                                                                    
243800     EJECT                                                                
243900                                                                          
244000 CEI-MAIN-EVENT-302 SECTION.                                              
244100     EVALUATE IN-EKH-KDEKSHT                                              
244200     WHEN '301'                                                           
244300          PERFORM CEIA-SUB-EVENT-302-301                                  
244400     WHEN '302'                                                           
244500          PERFORM CEIB-SUB-EVENT-302-302                                  
244600     END-EVALUATE                                                         
244700     .                                                                    
244800     EJECT                                                                
244900                                                                          
245000 CEIA-SUB-EVENT-302-301 SECTION.                                          
245100     EVALUATE IN-EKH-KDEKNIVA                                             
245200     WHEN 'DET'                                                           
245300       IF SYST-IDSEKVNR = 1                                               
245400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
245500         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
245600         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
245700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
245800         COMPUTE R3-LINE-AMOUNT-LC =                                      
245900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
246000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
246100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
246200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
246300         MOVE SPACE               TO WS-ALLOCATE-REF                      
246400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
246500         PERFORM S02-WRITE-W57051B                                        
246600       END-IF                                                             
246700                                                                          
246800       IF SYST-IDSEKVNR = 2                                               
246900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
247000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
247100         MOVE SPACE           TO R3-LINE-COST-CENTER                      
247200         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
247300         COMPUTE R3-LINE-AMOUNT-LC =                                      
247400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
247500         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
247600         MOVE SPACE               TO WS-LINE-TEXT                         
247700         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
247800         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
247900         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
248000         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
248100         PERFORM S02-WRITE-W57051B                                        
248200       END-IF                                                             
248300     END-EVALUATE                                                         
248400     .                                                                    
248500     EJECT                                                                
248600                                                                          
248700 CEIB-SUB-EVENT-302-302 SECTION.                                          
248800     EVALUATE IN-EKH-KDEKNIVA                                             
248900     WHEN 'DET'                                                           
249000       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
249100         IF SYST-IDSEKVNR = 1                                             
249200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
249300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
249400           IF BET-KDTRADP(3:2) NOT = SPACE                                
249500             MOVE '1'             TO WS-ACCOUNT-4                         
249600           ELSE                                                           
249700             MOVE '3'             TO WS-ACCOUNT-4                         
249800           END-IF                                                         
249900           COMPUTE R3-LINE-AMOUNT-LC  =                                   
250000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
250100           IF IN-EKH-KDVALISO = 'BRL'                                     
250200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
250300           END-IF                                                         
250310           MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                
250600           PERFORM S02-WRITE-W57051B                                      
250700         END-IF                                                           
250800                                                                          
250900         IF SYST-IDSEKVNR = 4                                             
251000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
251100           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
251200           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
251300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
251400           COMPUTE R3-LINE-AMOUNT-LC  =                                   
251500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
251600           IF IN-EKH-KDVALISO = 'BRL'                                     
251700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
251800           END-IF                                                         
251900           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
252000           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
252100           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
252200           MOVE SPACE             TO WS-ALLOCATE-REF                      
252300           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
252400           MOVE 0000470653        TO R3-LINE-PA-CUSTOMER                  
252600           PERFORM S02-WRITE-W57051B                                      
252700         END-IF                                                           
252800       ELSE                                                               
252900         IF SYST-IDSEKVNR = 2                                             
253000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
253100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
253200           IF BET-KDTRADP(3:2) NOT = SPACE                                
253300             MOVE '1'             TO WS-ACCOUNT-4                         
253400           ELSE                                                           
253500             MOVE '3'             TO WS-ACCOUNT-4                         
253600           END-IF                                                         
253700           COMPUTE R3-LINE-AMOUNT-LC  =                                   
253800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
253900           IF IN-EKH-KDVALISO = 'BRL'                                     
254000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
254100           END-IF                                                         
254200           MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                
254400           PERFORM S02-WRITE-W57051B                                      
254500         END-IF                                                           
254600                                                                          
254700         IF SYST-IDSEKVNR = 3                                             
254800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
254900           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
255000           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
255100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
255200           COMPUTE R3-LINE-AMOUNT-LC  =                                   
255300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
255400           IF IN-EKH-KDVALISO = 'BRL'                                     
255500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
255600           END-IF                                                         
255700           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
255800           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
255900           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
256000           MOVE SPACE             TO WS-ALLOCATE-REF                      
256100           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
256200           MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                
256400           PERFORM S02-WRITE-W57051B                                      
256500         END-IF                                                           
256600       END-IF                                                             
256700     END-EVALUATE                                                         
256800     .                                                                    
256900     EJECT                                                                
257000                                                                          
257100 CEJ-MAIN-EVENT-303 SECTION.                                              
257200     EVALUATE IN-EKH-KDEKSHT                                              
257300     WHEN '3XX'                                                           
257400          PERFORM CEJ301-SUB-EVENT-303-3XX                                
257500     WHEN '301'                                                           
257600          PERFORM CEJ301-SUB-EVENT-303-301                                
257700     WHEN '307'                                                           
257800          PERFORM CEJ307-SUB-EVENT-303-307                                
257900     WHEN '310'                                                           
258000          PERFORM CEJ310-SUB-EVENT-303-310                                
258100     WHEN '311'                                                           
258200          PERFORM CEJ311-SUB-EVENT-303-311                                
258300     WHEN '391'                                                           
258400          PERFORM CEJ301-SUB-EVENT-303-391                                
258500     WHEN '371'                                                           
258600          PERFORM CEJ303-SUB-EVENT-303-371                                
258700     END-EVALUATE                                                         
258800     .                                                                    
258900     EJECT                                                                
259000                                                                          
259100 CEJ303-SUB-EVENT-303-371 SECTION.                                        
259200     EVALUATE IN-EKH-KDEKNIVA                                             
259300     WHEN 'DET'                                                           
259400       IF SYST-IDSEKVNR = 1                                               
259500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
259600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
259700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
259800           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3               
259900         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
260000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
260100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
260200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
260300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
260400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
260500         PERFORM S04-WRITE-W57053B                                        
260600       END-IF                                                             
260700                                                                          
260800     WHEN 'LAND'                                                          
260900       IF SYST-IDSEKVNR = 1                                               
261000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
261100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
261200         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
261300         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
261400               R3-LINE-AMOUNT-LC / WS-PRKURS-BR3                          
261500         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
261600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
261700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
261800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
261900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
262000         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
262100         PERFORM S02-WRITE-W57051B                                        
262200       END-IF                                                             
262300                                                                          
262400     WHEN 'DDI'                                                           
262500       IF IN-EKH-SUBEL < ZERO                                             
262600         IF SYST-IDSEKVNR = 1                                             
262700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
262800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
262900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
263000                   IN-EKH-SUBEL                                           
263100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
263200           MOVE SPACE               TO WS-ALLOCATE-DC                     
263300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
263400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
263500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
263600           PERFORM S02-WRITE-W57051B                                      
263700         END-IF                                                           
263800       ELSE                                                               
263900         IF SYST-IDSEKVNR = 2                                             
264000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
264100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
264200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
264300                   IN-EKH-SUBEL                                           
264400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
264500           MOVE SPACE               TO WS-ALLOCATE-DC                     
264600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
264700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
264800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
264900           PERFORM S02-WRITE-W57051B                                      
265000         END-IF                                                           
265100       END-IF                                                             
265200     END-EVALUATE                                                         
265300     .                                                                    
265400     EJECT                                                                
265500 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
265600     EVALUATE IN-EKH-KDEKNIVA                                             
265700                                                                          
265800     WHEN 'FÖRS'                                                          
265900     WHEN 'FRAKT'                                                         
266000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
266100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
266200       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
266300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
266400              (IN-EKH-SUBEL * -1) / WS-PRKURS-BR3                         
266500       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
266600       MOVE SPACE               TO WS-ALLOCATE-DC                         
266700       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
266800       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
266900       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
267000       PERFORM S03-WRITE-W57072                                           
267100                                                                          
267200     WHEN 'LAND'                                                          
267300       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
267400       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
267500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
267600              (IN-EKH-SUBEL * -1) / WS-PRKURS-BR3                         
267700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
267800       MOVE SPACE               TO WS-ALLOCATE-DC                         
267900       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
268000       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
268100       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
268200       PERFORM S03-WRITE-W57072                                           
268300                                                                          
268400     WHEN 'DDI'                                                           
268500       IF IN-EKH-SUBEL < ZERO                                             
268600         IF SYST-IDSEKVNR = 1                                             
268700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
268800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
268900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
269000                   IN-EKH-SUBEL                                           
269100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
269200           MOVE SPACE               TO WS-ALLOCATE-DC                     
269300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
269400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
269500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
269600           PERFORM S04-WRITE-W57053B                                      
269700         END-IF                                                           
269800       ELSE                                                               
269900         IF SYST-IDSEKVNR = 2                                             
270000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
270100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
270200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
270300                   IN-EKH-SUBEL                                           
270400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
270500           MOVE SPACE               TO WS-ALLOCATE-DC                     
270600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
270700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
270800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
270900           PERFORM S04-WRITE-W57053B                                      
271000         END-IF                                                           
271100       END-IF                                                             
271200     END-EVALUATE                                                         
271300     .                                                                    
271400     EJECT                                                                
271500                                                                          
271600 CEJ301-SUB-EVENT-303-301 SECTION.                                        
271700     EVALUATE IN-EKH-KDEKNIVA                                             
271800     WHEN 'DET'                                                           
271900       IF SYST-IDSEKVNR = 1                                               
272000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
272100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
272200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
272300         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3 * -1            
272400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
272500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
272600         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
272700         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
272800         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
272900         MOVE SPACE               TO WS-ALLOCATE-DC                       
273000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
273100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
273200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
273201         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
273300         PERFORM S03-WRITE-W57072                                         
273400       END-IF                                                             
273500                                                                          
273600     END-EVALUATE                                                         
273700     .                                                                    
273800     EJECT                                                                
273900                                                                          
274000 CEJ307-SUB-EVENT-303-307 SECTION.                                        
274100     EVALUATE IN-EKH-KDEKNIVA                                             
274200     WHEN 'DET'                                                           
274300       IF SYST-IDSEKVNR = 1                                               
274400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
274500         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
274600         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
274700         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-BR3 * -1            
274800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
274900         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
275000         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
275100         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
275200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
275300         MOVE SPACE               TO WS-ALLOCATE-DC                       
275400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
275500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
275600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
275601         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
275700         PERFORM S03-WRITE-W57072                                         
275800       END-IF                                                             
275900                                                                          
276000     END-EVALUATE                                                         
276100     .                                                                    
276200     EJECT                                                                
276300                                                                          
276400 CEJ310-SUB-EVENT-303-310 SECTION.                                        
276500     EVALUATE IN-EKH-KDEKNIVA                                             
276600     WHEN 'DET'                                                           
276700       IF SYST-IDSEKVNR = 1                                               
276800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
276900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
277000         COMPUTE R3-LINE-AMOUNT-LC =                                      
277100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
277200         IF IN-EKH-KDVALISO = 'BRL'                                       
277300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
277400         END-IF                                                           
277500         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
277600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
277700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
277800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
277900         MOVE SPACE               TO WS-LINE-TEXT                         
278000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
278100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
278200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
278210         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
278300         PERFORM S02-WRITE-W57051B                                        
278400       END-IF                                                             
278500                                                                          
278600       IF SYST-IDSEKVNR = 2                                               
278700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
278800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
278900         COMPUTE R3-LINE-AMOUNT-LC =                                      
279000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
279100         IF IN-EKH-KDVALISO = 'BRL'                                       
279200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
279300         END-IF                                                           
279400         MOVE SPACE               TO WS-LINE-TEXT                         
279500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
279600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
279700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
279800         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
279900         MOVE SPACE               TO WS-ALLOCATE-DC                       
280000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
280100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
280200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
280210         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
280300         PERFORM S02-WRITE-W57051B                                        
280400       END-IF                                                             
280500     END-EVALUATE                                                         
280600     .                                                                    
280700     EJECT                                                                
280800                                                                          
280900 CEJ311-SUB-EVENT-303-311 SECTION.                                        
281000     EVALUATE IN-EKH-KDEKNIVA                                             
281100     WHEN 'DET'                                                           
281200        IF SYST-IDSEKVNR = 1                                              
281300          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
281400          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
281500          COMPUTE R3-LINE-AMOUNT-LC =                                     
281600                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
281700          IF IN-EKH-KDVALISO = 'BRL'                                      
281800            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
281900          END-IF                                                          
282000*         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER             
282100          MOVE SPACE               TO WS-LINE-TEXT                        
282200          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
282300          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
282400          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
282500          MOVE SPACE               TO WS-ALLOCATE-DC                      
282600          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
282700          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
282800          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
282810          MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                 
282900          PERFORM S02-WRITE-W57051B                                       
283000        END-IF                                                            
283100                                                                          
283200        IF SYST-IDSEKVNR = 2                                              
283300          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
283400          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
283500          COMPUTE R3-LINE-AMOUNT-LC =                                     
283600                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
283700          IF IN-EKH-KDVALISO = 'BRL'                                      
283800            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
283900          END-IF                                                          
284000          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
284100          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
284200          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
284300          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
284400          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
284500          MOVE SPACE               TO WS-LINE-TEXT                        
284600          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
284700          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
284800          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
284810          MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                 
284900          PERFORM S02-WRITE-W57051B                                       
285000        END-IF                                                            
285100     END-EVALUATE                                                         
285200     .                                                                    
285300     EJECT                                                                
285400                                                                          
285500 CEJ301-SUB-EVENT-303-391 SECTION.                                        
285600     EVALUATE IN-EKH-KDEKNIVA                                             
285700     WHEN 'DET'                                                           
285800       IF SYST-IDSEKVNR = 1                                               
285900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
286000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
286100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
286200              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
286300         IF IN-EKH-KDVALISO = 'BRL'                                       
286400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
286500         END-IF                                                           
286600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
286700         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
286800         MOVE SPACE               TO WS-LINE-TEXT                         
286900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
287000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
287100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
287400         MOVE SPACE               TO WS-ALLOCATE-DC                       
287500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
287510         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
287600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
287700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
287800         PERFORM S03-WRITE-W57072                                         
287900       END-IF                                                             
288000                                                                          
288100       IF SYST-IDSEKVNR = 2                                               
288200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
288300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
288400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
288500              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
288600         IF IN-EKH-KDVALISO = 'BRL'                                       
288700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
288800         END-IF                                                           
288900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
289000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
289100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
289200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
289300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
289400         MOVE SPACE               TO WS-LINE-TEXT                         
289500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
289600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
289700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
289800         MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                  
290000         PERFORM S03-WRITE-W57072                                         
290100       END-IF                                                             
290200                                                                          
290300     END-EVALUATE                                                         
290400     .                                                                    
290500     EJECT                                                                
290600                                                                          
290700 CEK-MAIN-EVENT-401 SECTION.                                              
290800     EVALUATE IN-EKH-KDEKNIVA                                             
290900                                                                          
291000* PRISÄNDRING LÖPANDE                                                     
291100     WHEN 'DET'                                                           
291200       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
291300                           IN-EKH-PRARTSTD                                
291400       IF SYST-IDSEKVNR = 1                                               
291500* PRISHÖJNING                                                             
291600         IF WS-BELOPP > 0                                                 
291700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
291800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
291900           COMPUTE R3-LINE-AMOUNT-LC =                                    
292000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
292100           IF IN-EKH-KDVALISO = 'BRL'                                     
292200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
292300           END-IF                                                         
292400           MOVE SPACES              TO R3-LINE-TRADING-PARTNER            
292610           MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                
292700           PERFORM S02-WRITE-W57051B                                      
292800         END-IF                                                           
292900       END-IF                                                             
293000                                                                          
293100       IF SYST-IDSEKVNR = 2                                               
293200* PRISSÄKNING                                                             
293300         IF WS-BELOPP < 0                                                 
293400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
293500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
293600           COMPUTE R3-LINE-AMOUNT-LC =                                    
293700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
293800           IF IN-EKH-KDVALISO = 'BRL'                                     
293900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
294000           END-IF                                                         
294100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
294200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
294300           MOVE SPACE               TO WS-ALLOCATE-REF                    
294400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
294610           MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                
294700           PERFORM S02-WRITE-W57051B                                      
294800         END-IF                                                           
294900       END-IF                                                             
295000                                                                          
295100       IF SYST-IDSEKVNR = 3                                               
295200* PRISSÄNKNING                                                            
295300         IF WS-BELOPP < 0                                                 
295400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
295500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
295600           COMPUTE R3-LINE-AMOUNT-LC =                                    
295700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
295800           IF IN-EKH-KDVALISO = 'BRL'                                     
295900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
296000           END-IF                                                         
296100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
296310           MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                
296400           PERFORM S02-WRITE-W57051B                                      
296500         END-IF                                                           
296600       END-IF                                                             
296700                                                                          
296800       IF SYST-IDSEKVNR = 4                                               
296900* PRISHÖJNING                                                             
297000         IF WS-BELOPP > 0                                                 
297100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
297200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
297300           COMPUTE R3-LINE-AMOUNT-LC =                                    
297400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
297500           IF IN-EKH-KDVALISO = 'BRL'                                     
297600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
297700           END-IF                                                         
297800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
297900           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
298000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
298100           MOVE SPACE               TO WS-ALLOCATE-REF                    
298200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
298410           MOVE 0000470653          TO R3-LINE-PA-CUSTOMER                
298500           PERFORM S02-WRITE-W57051B                                      
298600         END-IF                                                           
298700       END-IF                                                             
298800     END-EVALUATE                                                         
298900     .                                                                    
299000     EJECT                                                                
299100                                                                          
299200 CEL-MAIN-EVENT-402 SECTION.                                              
299300     EVALUATE IN-EKH-KDEKNIVA                                             
299400     WHEN 'DET'                                                           
299500       IF SYST-IDSEKVNR = 1                                               
299600         IF IN-EKH-KVANTAL > 0                                            
299700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
299800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
299900           COMPUTE R3-LINE-AMOUNT-LC =                                    
300000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
300100           IF IN-EKH-KDVALISO = 'BRL'                                     
300200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
300300           END-IF                                                         
300400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
300500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
300600           MOVE SPACE               TO WS-ALLOCATE-REF                    
300700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
300800           PERFORM S02-WRITE-W57051B                                      
300900         END-IF                                                           
301000       END-IF                                                             
301100                                                                          
301200       IF SYST-IDSEKVNR = 2                                               
301300         IF IN-EKH-KVANTAL < 0                                            
301400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
301500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
301600           COMPUTE R3-LINE-AMOUNT-LC =                                    
301700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
301800           IF IN-EKH-KDVALISO = 'BRL'                                     
301900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
302000           END-IF                                                         
302100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
302200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
302300           MOVE SPACE               TO WS-ALLOCATE-REF                    
302400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
302500           PERFORM S02-WRITE-W57051B                                      
302600         END-IF                                                           
302700       END-IF                                                             
302800     END-EVALUATE                                                         
302900     .                                                                    
303000     EJECT                                                                
303100                                                                          
303200 CEM-MAIN-EVENT-403 SECTION.                                              
303300     EVALUATE IN-EKH-KDEKSHT                                              
303400     WHEN '401'                                                           
303500     WHEN '402'                                                           
303600     WHEN '403'                                                           
303700     WHEN '404'                                                           
303800     WHEN '405'                                                           
303900     WHEN '407'                                                           
304000     WHEN '408'                                                           
304100     WHEN '409'                                                           
304200          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
304300     END-EVALUATE                                                         
304400     .                                                                    
304500     EJECT                                                                
304600                                                                          
304700 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
304800     EVALUATE IN-EKH-KDEKNIVA                                             
304900     WHEN 'DET'                                                           
305000       IF IN-EKH-KVANTAL > 0                                              
305100         IF SYST-IDSEKVNR = 1                                             
305200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
305300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
305400           COMPUTE R3-LINE-AMOUNT-LC =                                    
305500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
305600           IF IN-EKH-KDVALISO = 'BRL'                                     
305700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
305800           END-IF                                                         
305900           PERFORM S02-WRITE-W57051B                                      
306000         END-IF                                                           
306100                                                                          
306200         IF SYST-IDSEKVNR = 4                                             
306300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
306400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
306500           COMPUTE R3-LINE-AMOUNT-LC =                                    
306600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
306700           IF IN-EKH-KDVALISO = 'BRL'                                     
306800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
306900           END-IF                                                         
307000           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
307100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
307200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
307300           MOVE SPACE               TO WS-ALLOCATE-REF                    
307400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
307500           PERFORM S02-WRITE-W57051B                                      
307600         END-IF                                                           
307700       END-IF                                                             
307800                                                                          
307900       IF IN-EKH-KVANTAL < 0                                              
308000         IF SYST-IDSEKVNR = 2                                             
308100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
308200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
308300           COMPUTE R3-LINE-AMOUNT-LC =                                    
308400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
308500           IF IN-EKH-KDVALISO = 'BRL'                                     
308600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
308700           END-IF                                                         
308800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
308900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
309000           MOVE SPACE               TO WS-ALLOCATE-REF                    
309100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
309200           PERFORM S02-WRITE-W57051B                                      
309300         END-IF                                                           
309400                                                                          
309500         IF SYST-IDSEKVNR = 3                                             
309600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
309700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
309800           COMPUTE R3-LINE-AMOUNT-LC =                                    
309900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
310000           IF IN-EKH-KDVALISO = 'BRL'                                     
310100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
310200           END-IF                                                         
310300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
310400           PERFORM S02-WRITE-W57051B                                      
310500         END-IF                                                           
310600       END-IF                                                             
310700     END-EVALUATE                                                         
310800     .                                                                    
310900     EJECT                                                                
311000                                                                          
311100 CEN-MAIN-EVENT-404 SECTION.                                              
311200     EVALUATE IN-EKH-KDEKNIVA                                             
311300     WHEN 'DET'                                                           
311400       IF SYST-IDSEKVNR = 1                                               
311500* KONTO EJ MANUELLT REGISTRERAT                                           
311600         IF IN-EKH-IDKONTO = 0                                            
311700           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
311800           IF DIST18-SCRAP-NDC-SC                                         
311900           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
312100             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
312200             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
312300             COMPUTE R3-LINE-AMOUNT-LC =                                  
312400                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
312500             IF IN-EKH-KDVALISO = 'BRL'                                   
312600               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
312700             END-IF                                                       
312800             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
312900             PERFORM S02-WRITE-W57051B                                    
313000           END-IF                                                         
313100         END-IF                                                           
313200       END-IF                                                             
313300                                                                          
313400       IF SYST-IDSEKVNR = 2                                               
313500* KONTO MANUELLT REGISTRERAT                                              
313600         IF IN-EKH-IDKONTO > 0                                            
313700           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
313800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
313900           COMPUTE R3-LINE-AMOUNT-LC =                                    
314000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
314100           IF IN-EKH-KDVALISO = 'BRL'                                     
314200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
314300           END-IF                                                         
314400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
314500           PERFORM S02-WRITE-W57051B                                      
314600         END-IF                                                           
314700       END-IF                                                             
314800                                                                          
314900       IF SYST-IDSEKVNR = 3                                               
315000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
315100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
315200         COMPUTE R3-LINE-AMOUNT-LC =                                      
315300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
315400         IF IN-EKH-KDVALISO = 'BRL'                                       
315500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
315600         END-IF                                                           
315700         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
315800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
315900         MOVE SPACE               TO WS-ALLOCATE-REF                      
316000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
316100         PERFORM S02-WRITE-W57051B                                        
316200       END-IF                                                             
316201                                                                          
316210       IF SYST-IDSEKVNR = 4                                               
316220         IF IN-EKH-IDKONTO = 0                                            
316230           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
316240           IF DIST18-SCRAP-NDC-QUAL                                       
316250             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
316260             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
316270             COMPUTE R3-LINE-AMOUNT-LC =                                  
316280                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
316290             IF IN-EKH-KDVALISO = 'BRL'                                   
316291               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
316292             END-IF                                                       
316293             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
316294             PERFORM S02-WRITE-W57051B                                    
316295           END-IF                                                         
316296         END-IF                                                           
316297       END-IF                                                             
316300                                                                          
316400                                                                          
316500     END-EVALUATE                                                         
316600     .                                                                    
316700     EJECT                                                                
316800                                                                          
316900 CF-BUILD-COMMON-210-PART SECTION.                                        
317000     MOVE SPACE              TO R3-LINE-R3                                
317100     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
317200                                R3-LINE-DUE-DATE                          
317300                                R3-LINE-AMOUNT                            
317400                                R3-LINE-AMOUNT-LC                         
317500                                R3-LINE-TAX-AMOUNT                        
317600                                R3-LINE-TAX-AMOUNT-LC                     
317700                                R3-LINE-NUMBER-OF-DAYS                    
317800                                R3-LINE-QUANTITY                          
317900                                R3-LINE-SAMNR                             
318000     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
318100     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
318200     MOVE 'BR12'             TO R3-LINE-COMPANY-CODE                      
318300     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
318400     IF SYST-KDPOST = '31'                                                
318500       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
318600     ELSE                                                                 
318700       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
318800     END-IF                                                               
318900     .                                                                    
319000     EJECT                                                                
319100                                                                          
319200 CG-SCHEDULE-LINE-AP SECTION.                                             
319300     MOVE NEJ                     TO WS-HEADER-SW                         
319400     MOVE JA                      TO WS-LINE-SW                           
319500     EVALUATE IN-EKH-KDEKHHT                                              
319600     WHEN '102'                                                           
319700       IF IN-EKH-KDEKSHT = '130'                                          
319800       OR IN-EKH-KDEKSHT = '134'                                          
319900         IF IN-EKH-KDEKSHT = '130'                                        
320000           PERFORM CGA-MAIN-EVENT-102-130                                 
320100         ELSE                                                             
320200           PERFORM CGA-MAIN-EVENT-102-134                                 
320300         END-IF                                                           
320400       ELSE                                                               
320500         IF IN-EKH-KDEKSHT = '120'                                        
320600         OR IN-EKH-KDEKSHT = '124'                                        
320700         OR IN-EKH-KDEKSHT = '125'                                        
320800           IF IN-EKH-KDEKSHT = '125'                                      
320900             PERFORM CGA-MAIN-EVENT-102-125                               
321000           ELSE                                                           
321100             PERFORM CGA-MAIN-EVENT-102-12X                               
321200           END-IF                                                         
321300         ELSE                                                             
321400           PERFORM CGA-MAIN-EVENT-102                                     
321500         END-IF                                                           
321600       END-IF                                                             
321700     WHEN '103'                                                           
321800         PERFORM CGA-MAIN-EVENT-103                                       
321900     WHEN '303'                                                           
322000       IF IN-EKH-KDEKSHT = '371'                                          
322100         PERFORM S81-GET-CURRENCY-RATE                                    
322200         PERFORM CGA-MAIN-EVENT-303-371                                   
322300       ELSE                                                               
322301         IF IN-EKH-KDEKSHT = '3XX'                                        
322310           PERFORM S81-GET-CURRENCY-RATE                                  
322400           PERFORM CGA-MAIN-EVENT-303-3XX                                 
322401         ELSE                                                             
322410           PERFORM CGA-MAIN-EVENT-303                                     
322420         END-IF                                                           
322500       END-IF                                                             
322600     END-EVALUATE                                                         
322700     .                                                                    
322800     EJECT                                                                
322900                                                                          
323000                                                                          
323100 CGA-MAIN-EVENT-102     SECTION.                                          
323200     EVALUATE IN-EKH-KDEKNIVA                                             
323300     WHEN 'SUM'                                                           
323400       IF IN-EKH-SUBEL > ZERO                                             
323500         IF SYST-IDSEKVNR = 1                                             
323600           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
323700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
323800            IN-EKH-SUBEL                                                  
323900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
324000           PERFORM S10-VATCODE                                            
324100           IF IN-EKH-SUVAT = ZERO                                         
324200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
324300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
324400           ELSE                                                           
324500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
324600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
324700           END-IF                                                         
324800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
324900                                                                          
325000           PERFORM S04-WRITE-W57053B                                      
325100         END-IF                                                           
325200       END-IF                                                             
325300                                                                          
325400       IF IN-EKH-SUBEL < ZERO                                             
325500         IF SYST-IDSEKVNR = 2                                             
325600           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
325700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
325800           IN-EKH-SUBEL                                                   
325900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
326000           PERFORM S10-VATCODE                                            
326100           IF IN-EKH-SUVAT = ZERO                                         
326200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
326300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
326400           ELSE                                                           
326500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
326600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
326700           END-IF                                                         
326800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
326900                                                                          
327000           PERFORM S04-WRITE-W57053B                                      
327100         END-IF                                                           
327200       END-IF                                                             
327300     END-EVALUATE                                                         
327400     .                                                                    
327500     EJECT                                                                
327600                                                                          
327700 CGA-MAIN-EVENT-102-12X SECTION.                                          
327800     EVALUATE IN-EKH-KDEKNIVA                                             
327900     WHEN 'SUM'                                                           
328000       IF IN-EKH-SUBEL > ZERO                                             
328100         IF SYST-IDSEKVNR = 1                                             
328200           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
328300           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
328400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
328500                   R3-LINE-AMOUNT    / WS-PRKURS-BR  * -1                 
328600           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
328700*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
328710           MOVE '  '     TO R3-LINE-TAX-CODE                              
328800           IF IN-EKH-SUVAT = ZERO                                         
328900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
329000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
329100           ELSE                                                           
329200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
329300             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
329400                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-BR  * -1           
329500             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
329600           END-IF                                                         
329700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
329800                                                                          
329900           PERFORM S04-WRITE-W57053B                                      
330000         END-IF                                                           
330100       END-IF                                                             
330200                                                                          
330300       IF IN-EKH-SUBEL < ZERO                                             
330400         IF SYST-IDSEKVNR = 2                                             
330500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
330600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
330700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
330800                   R3-LINE-AMOUNT    / WS-PRKURS-BR  * -1                 
330900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
331000*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
331010           MOVE '  '     TO R3-LINE-TAX-CODE                              
331100           IF IN-EKH-SUVAT = ZERO                                         
331200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
331300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
331400           ELSE                                                           
331500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
331600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
331700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-BR  * -1           
331800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
331900           END-IF                                                         
332000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
332100           MOVE SPACE           TO R3-LINE-COST-CENTER                    
332200                                                                          
332300           PERFORM S04-WRITE-W57053B                                      
332400         END-IF                                                           
332500       END-IF                                                             
332600     END-EVALUATE                                                         
332700     .                                                                    
332800     EJECT                                                                
332900                                                                          
333000                                                                          
333100 CGA-MAIN-EVENT-102-125 SECTION.                                          
333200     EVALUATE IN-EKH-KDEKNIVA                                             
333300     WHEN 'SUM'                                                           
333400       IF IN-EKH-SUBEL > ZERO                                             
333500         IF SYST-IDSEKVNR = 1                                             
333600           MOVE ZERO TO SPAR-SUMMA-102-125                                
333700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
333800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
333900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
334000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
334100                   R3-LINE-AMOUNT    / WS-PRKURS-BR  * -1                 
334200           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
334300*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
334310           MOVE '  '     TO R3-LINE-TAX-CODE                              
334400           IF IN-EKH-SUVAT = ZERO                                         
334500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
334600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
334700           ELSE                                                           
334800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
334900             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
335000               R3-LINE-TAX-AMOUNT / WS-PRKURS-BR  * -1                    
335100             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
335200           END-IF                                                         
335300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
335400           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
335500                                                                          
335600           PERFORM S04-WRITE-W57053B                                      
335700         END-IF                                                           
335800       END-IF                                                             
335900                                                                          
336000       IF IN-EKH-SUBEL < ZERO                                             
336100         IF SYST-IDSEKVNR = 2                                             
336200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
336300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
336400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
336500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
336600                   R3-LINE-AMOUNT    / WS-PRKURS-BR  * -1                 
336700           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
336800*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
336810           MOVE '  '     TO R3-LINE-TAX-CODE                              
336900           IF IN-EKH-SUVAT = ZERO                                         
337000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
337100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
337200           ELSE                                                           
337300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
337400             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
337500               R3-LINE-TAX-AMOUNT / WS-PRKURS-BR * -1                     
337600             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
337700           END-IF                                                         
337800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
337900           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
338000                                                                          
338100           PERFORM S04-WRITE-W57053B                                      
338200         END-IF                                                           
338300       END-IF                                                             
338400     END-EVALUATE                                                         
338500     .                                                                    
338600     EJECT                                                                
338700 CGA-MAIN-EVENT-102-130 SECTION.                                          
338800     EVALUATE IN-EKH-KDEKNIVA                                             
338900     WHEN 'SUM'                                                           
339000       IF IN-EKH-SUBEL > ZERO                                             
339100         IF SYST-IDSEKVNR = 1                                             
339200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
339300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
339400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
339500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
339600                   R3-LINE-AMOUNT    / WS-PRKURS-BR  * -1                 
339700           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
339800*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
339810           MOVE '  '     TO R3-LINE-TAX-CODE                              
339900           IF IN-EKH-SUVAT = ZERO                                         
340000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
340100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
340200           ELSE                                                           
340300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
340400             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
340500                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-BR  * -1           
340600             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
340700           END-IF                                                         
340800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
340900                                                                          
341000           PERFORM S04-WRITE-W57053B                                      
341100         END-IF                                                           
341200       END-IF                                                             
341300                                                                          
341400       IF IN-EKH-SUBEL < ZERO                                             
341500         IF SYST-IDSEKVNR = 2                                             
341600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
341700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
341800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
341900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
342000                   R3-LINE-AMOUNT    / WS-PRKURS-BR                       
342100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
342200*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
342210           MOVE '  '     TO R3-LINE-TAX-CODE                              
342300           IF IN-EKH-SUVAT = ZERO                                         
342400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
342500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
342600           ELSE                                                           
342700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
342800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
342900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-BR                 
343000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
343100           END-IF                                                         
343200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
343300                                                                          
343400           PERFORM S04-WRITE-W57053B                                      
343500         END-IF                                                           
343600       END-IF                                                             
343700     END-EVALUATE                                                         
343800     .                                                                    
343900     EJECT                                                                
344000                                                                          
344100 CGA-MAIN-EVENT-102-134 SECTION.                                          
344200     EVALUATE IN-EKH-KDEKNIVA                                             
344300     WHEN 'SUM'                                                           
344400       IF IN-EKH-SUBEL > ZERO                                             
344500         IF SYST-IDSEKVNR = 1                                             
344600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
344700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
344800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
344900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
345000                   R3-LINE-AMOUNT    / WS-PRKURS-BR  * -1                 
345100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
345200*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
345210           MOVE '  '     TO R3-LINE-TAX-CODE                              
345300           IF IN-EKH-SUVAT = ZERO                                         
345400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
345500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
345600           ELSE                                                           
345700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
345800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
345900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-BR  * -1           
346000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
346100           END-IF                                                         
346200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
346300                                                                          
346400           PERFORM S04-WRITE-W57053B                                      
346500         END-IF                                                           
346600       END-IF                                                             
346700                                                                          
346800       IF IN-EKH-SUBEL < ZERO                                             
346900         IF SYST-IDSEKVNR = 2                                             
347000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
347100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
347200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
347300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
347400                   R3-LINE-AMOUNT    / WS-PRKURS-BR                       
347500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
347600*          MOVE 'P7'     TO R3-LINE-TAX-CODE                              
347610           MOVE '  '     TO R3-LINE-TAX-CODE                              
347700           IF IN-EKH-SUVAT = ZERO                                         
347800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
347900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
348000           ELSE                                                           
348100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
348200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
348300                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-BR                 
348400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
348500           END-IF                                                         
348600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
348700                                                                          
348800           PERFORM S04-WRITE-W57053B                                      
348900         END-IF                                                           
349000       END-IF                                                             
349100     END-EVALUATE                                                         
349200     .                                                                    
349300     EJECT                                                                
349400                                                                          
349500 CGA-MAIN-EVENT-103     SECTION.                                          
349600     EVALUATE IN-EKH-KDEKNIVA                                             
349700     WHEN 'SUM'                                                           
349800       MOVE IN-EKH-IDLEVNR         TO W-IDLEVNR                           
349900       PERFORM IMS-GET-WDF101                                             
350000       PERFORM IMS-GNP-WDF106                                             
350100       IF IN-EKH-SUBEL > ZERO                                             
350200         IF SYST-IDSEKVNR = 1                                             
350300           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
350400           IF ADR-IDLANDX2 = 'BR'                                         
350500             PERFORM S10-VATCODE                                          
350600           ELSE                                                           
350700*            MOVE 'P7'             TO R3-LINE-TAX-CODE                    
350710             MOVE '  '             TO R3-LINE-TAX-CODE                    
350800             MOVE ZERO             TO IN-EKH-SUVAT                        
350900           END-IF                                                         
351000           IF IN-EKH-SUVAT = ZERO                                         
351100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
351200                                      R3-LINE-TAX-AMOUNT-LC               
351300           ELSE                                                           
351400             IF IN-EKH-KDVALISO = 'BRL'                                   
351500               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
351600                                      R3-LINE-TAX-AMOUNT-LC               
351700             ELSE                                                         
351800               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
351900                                      R3-LINE-TAX-AMOUNT-LC               
352000               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
352100                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
352200             END-IF                                                       
352300           END-IF                                                         
352400**** CALCULATE NEW SUM WITH VAT                                           
352500           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
352600                   IN-EKH-SUVAT                                           
352700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
352800           IF IN-EKH-KDVALISO = 'BRL'                                     
352900             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
353000           ELSE                                                           
353100             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
353200                     R3-LINE-AMOUNT * WS-PRKURS                           
353300           END-IF                                                         
353400           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
353500                                                                          
353600           PERFORM S04-WRITE-W57053B                                      
353700         END-IF                                                           
353800       END-IF                                                             
353900                                                                          
354000       IF IN-EKH-SUBEL < ZERO                                             
354100         IF SYST-IDSEKVNR = 1                                             
354200           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
354300           IF ADR-IDLANDX2 = 'BR'                                         
354400*            MOVE 'P1'             TO R3-LINE-TAX-CODE                    
354410             MOVE '  '             TO R3-LINE-TAX-CODE                    
354500*            COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.1            
354510             COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL                  
354600           ELSE                                                           
354700*            MOVE 'P7'             TO R3-LINE-TAX-CODE                    
354710             MOVE '  '             TO R3-LINE-TAX-CODE                    
354800             MOVE ZERO             TO IN-EKH-SUVAT                        
354900           END-IF                                                         
355000           IF IN-EKH-SUVAT = ZERO                                         
355100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
355200                                      R3-LINE-TAX-AMOUNT-LC               
355300           ELSE                                                           
355400             IF IN-EKH-KDVALISO = 'BRL'                                   
355500               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
355600                                      R3-LINE-TAX-AMOUNT-LC               
355700             ELSE                                                         
355800               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
355900                                      R3-LINE-TAX-AMOUNT-LC               
356000               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
356100                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
356200             END-IF                                                       
356300           END-IF                                                         
356400**** CALCULATE NEW SUM WITH VAT                                           
356500           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
356600                   IN-EKH-SUVAT                                           
356700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
356800           IF IN-EKH-KDVALISO = 'BRL'                                     
356900             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
357000           ELSE                                                           
357100             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
357200                     R3-LINE-AMOUNT * WS-PRKURS                           
357300           END-IF                                                         
357400           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
357500                                                                          
357600           PERFORM S04-WRITE-W57053B                                      
357700         END-IF                                                           
357800       END-IF                                                             
357900     END-EVALUATE                                                         
358000     .                                                                    
358100     EJECT                                                                
358200                                                                          
358300 CGA-MAIN-EVENT-303 SECTION.                                              
358400     EVALUATE IN-EKH-KDEKNIVA                                             
358500     WHEN 'SUM'                                                           
358600       IF SYST-IDSEKVNR = 1                                               
358700         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
358800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
358900         (IN-EKH-SUBEL / WS-PRKURS-BR)                                    
359000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
359100         MOVE '  '     TO R3-LINE-TAX-CODE                                
359200         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
359300         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
359400                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-BR                     
359500         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
359600                                                                          
359700         PERFORM S04-WRITE-W57053B                                        
359800       END-IF                                                             
359900     END-EVALUATE                                                         
360000     .                                                                    
360100     EJECT                                                                
360200                                                                          
360210 CGA-MAIN-EVENT-303-3XX SECTION.                                          
360220     EVALUATE IN-EKH-KDEKNIVA                                             
360230     WHEN 'SUM'                                                           
360240       IF SYST-IDSEKVNR = 1                                               
360250         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
360260         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
360270         (IN-EKH-SUBEL / WS-PRKURS-BR3)                                   
360280         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
360290         MOVE '  '     TO R3-LINE-TAX-CODE                                
360291         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
360292         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
360293                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-BR3                    
360294         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
360295                                                                          
360296         PERFORM S04-WRITE-W57053B                                        
360297       END-IF                                                             
360298     END-EVALUATE                                                         
360299     .                                                                    
360300     EJECT                                                                
360301                                                                          
360310 CGA-MAIN-EVENT-303-371 SECTION.                                          
360400     EVALUATE IN-EKH-KDEKNIVA                                             
360500     WHEN 'SUM'                                                           
360600       IF SYST-IDSEKVNR = 1                                               
360700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
360800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
360900         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
361000         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
361100               R3-LINE-AMOUNT-LC / WS-PRKURS-BR3                          
361200         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
361300*        MOVE 'P7'                 TO R3-LINE-TAX-CODE                    
361310         MOVE '  '                 TO R3-LINE-TAX-CODE                    
361400         MOVE ZERO                 TO IN-EKH-SUVAT                        
361500         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
361600         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
361700                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-BR3                    
361800         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
361900                                                                          
362000         PERFORM S04-WRITE-W57053B                                        
362100       END-IF                                                             
362200     END-EVALUATE                                                         
362300     .                                                                    
362400     EJECT                                                                
362500 CH-BUILD-COMMON-310-PART SECTION.                                        
362600     MOVE SPACE              TO R3-LINE-R3                                
362700     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
362800                                R3-LINE-DUE-DATE                          
362900                                R3-LINE-AMOUNT                            
363000                                R3-LINE-AMOUNT-LC                         
363100                                R3-LINE-TAX-AMOUNT                        
363200                                R3-LINE-TAX-AMOUNT-LC                     
363300                                R3-LINE-NUMBER-OF-DAYS                    
363400                                R3-LINE-QUANTITY                          
363500                                R3-LINE-SAMNR                             
363600     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
363700     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
363800     MOVE 'BR12'             TO R3-LINE-COMPANY-CODE                      
363900     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
364000     IF SYST-KDPOST = '31'                                                
364100       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
364200     ELSE                                                                 
364300       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
364400     END-IF                                                               
364500     .                                                                    
364600     EJECT                                                                
364700                                                                          
364800 CI-SCHEDULE-LINE-AR SECTION.                                             
364900     MOVE NEJ                     TO WS-HEADER-SW                         
365000     MOVE JA                      TO WS-LINE-SW                           
365100     EVALUATE IN-EKH-KDEKHHT                                              
365200     WHEN '204'                                                           
365300         PERFORM CIA-MAIN-EVENT-204                                       
365400     END-EVALUATE                                                         
365500     .                                                                    
365600     EJECT                                                                
365700                                                                          
365800 CIA-MAIN-EVENT-204     SECTION.                                          
365900     EVALUATE IN-EKH-KDEKNIVA                                             
366000     WHEN 'SUM'                                                           
366100       IF IN-EKH-SUBEL > ZERO                                             
366200         IF SYST-IDSEKVNR = 1                                             
366300           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
366400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
366500           IF IN-EKH-KDVALISO = 'BRL'                                     
366600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
366700           END-IF                                                         
366800           IF IN-EKH-KDEKSHT = '301'                                      
366900*            MOVE 'P7'             TO R3-LINE-TAX-CODE                    
366910             MOVE '  '             TO R3-LINE-TAX-CODE                    
367000           ELSE                                                           
367100             PERFORM S10-VATCODE                                          
367200           END-IF                                                         
367300           IF IN-EKH-SUVAT = ZERO                                         
367400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
367500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
367600           ELSE                                                           
367700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
367800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
367900           END-IF                                                         
368000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
368100                                                                          
368200           PERFORM S04-WRITE-W57053B                                      
368300         END-IF                                                           
368400       END-IF                                                             
368500                                                                          
368600     END-EVALUATE                                                         
368700     .                                                                    
368800     EJECT                                                                
368900                                                                          
369000 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
369100     MOVE ZERO             TO LOGG-W57073                                 
369200     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
369300     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
369400     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
369500     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
369600     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
369700     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
369800     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
369900     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
370000     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
370100     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
370200     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
370300     MOVE 'BR12'           TO LOGG-KDTRADP                                
370400                                                                          
370500****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
370600     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
370700     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
370800     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
370900     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
371000     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
371100     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
371200     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
371300     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
371400     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
371500     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
371600     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
371700     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
371800     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
371900     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
372000     .                                                                    
372100     EJECT                                                                
372200                                                                          
372300 Z-FINI SECTION.                                                          
372400     CLOSE W57066                                                         
372500           W57058B                                                        
372600           W57051B                                                        
372700           W57052B                                                        
372800           W57053B                                                        
372900           W57055B                                                        
373000           W5705NB                                                        
373100           W51350B                                                        
373200                                                                          
373300     MOVE 'S' TO POSTSUM-OPKOD                                            
373400     CALL POSTSUM USING POSTSUM-PARM                                      
373500     .                                                                    
373600     EJECT                                                                
373700                                                                          
373800 S01-READ-W57066  SECTION.                                                
373900     READ W57066 INTO IN-AREA                                             
374000     AT END                                                               
374100        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
374200        SET END-OF-W57066 TO TRUE                                         
374300                                                                          
374400     NOT AT END                                                           
374500        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
374600        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
374700        MOVE 'W57059D1'   TO POSTSUM-DDNAMN2                              
374800        CALL POSTSUM USING POSTSUM-PARM                                   
374900                                                                          
375000        IF IN-EKH-KDPRODSL NOT = 0                                        
375100          MOVE IN-EKH-KDPRODSL TO WS-KDPRODSL-SAVE                        
375200        END-IF                                                            
375300     END-READ                                                             
375400     .                                                                    
375500                                                                          
375600 S02-WRITE-W57051B SECTION.                                               
375700     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
375800     MOVE SPACE                 TO 71LINE-POST                            
375900     IF WS-LINE-SW = JA                                                   
376000       IF IN-EKH-KDSORT = 'SW'                                            
376100         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
376200         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
376300         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
376400       ELSE                                                               
376500         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
376600         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
376700         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
376800       END-IF                                                             
376900       WRITE 71LINE-POST        FROM R3-LINE-R3                           
377000       PERFORM S20-CREATE-WRITE-LOG                                       
377100     ELSE                                                                 
377200       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
377300     END-IF                                                               
377400                                                                          
377500     IF WS-LINE-SW = JA                                                   
377600       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
377700     ELSE                                                                 
377800       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
377900     END-IF                                                               
378000     MOVE 'W57051B'             TO POSTSUM-FDNAMN                         
378100     MOVE 'W57059D2'            TO POSTSUM-DDNAMN2                        
378200     CALL POSTSUM USING POSTSUM-PARM                                      
378300     .                                                                    
378400                                                                          
378500 S002-WRITE-W57051B-HEAD SECTION.                                         
378600     MOVE SPACE                 TO 71LINE-POST                            
378700     IF IN-EKH-KDSORT = 'SW'                                              
378800       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
378900       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
379000       MOVE WS-TEXT           TO R3-LINE-TEXT                             
379100     ELSE                                                                 
379200       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
379300       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
379400       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
379500     END-IF                                                               
379600     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
379700                                                                          
379800     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
379900     MOVE 'W57051B'             TO POSTSUM-FDNAMN                         
380000     MOVE 'W57059D2'            TO POSTSUM-DDNAMN2                        
380100     CALL POSTSUM USING POSTSUM-PARM                                      
380200     .                                                                    
380300                                                                          
380400 S03-WRITE-W57072 SECTION.                                                
380500     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
380600     IF IN-EKH-KDSORT = 'SW'                                              
380700       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
380800       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
380900       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
381000     ELSE                                                                 
381100       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
381200       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
381300       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
381400     END-IF                                                               
381500     WRITE 72LINE-POST        FROM R3-LINE-R3                             
381600                                                                          
381700     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
381800     MOVE 'W57052B'           TO POSTSUM-FDNAMN                           
381900     MOVE 'W57059D3'          TO POSTSUM-DDNAMN2                          
382000     CALL POSTSUM USING POSTSUM-PARM                                      
382100                                                                          
382200     PERFORM S20-CREATE-WRITE-LOG                                         
382300     .                                                                    
382400                                                                          
382500 S04-WRITE-W57053B SECTION.                                               
382600     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
382700     MOVE SPACE                 TO 73LINE-POST                            
382800     IF WS-LINE-SW = JA                                                   
382900       IF IN-EKH-KDSORT = 'SW'                                            
383000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
383100         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
383200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
383300       ELSE                                                               
383400         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
383500         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
383600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
383700       END-IF                                                             
383800       WRITE 73LINE-POST        FROM R3-LINE-R3                           
383900       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
384000     ELSE                                                                 
384100       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
384200       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
384300     END-IF                                                               
384400                                                                          
384500     MOVE 'W57053B'             TO POSTSUM-FDNAMN                         
384600     MOVE 'W57059D4'            TO POSTSUM-DDNAMN2                        
384700     CALL POSTSUM USING POSTSUM-PARM                                      
384800                                                                          
384900     IF WS-LINE-SW = JA                                                   
385000       PERFORM S20-CREATE-WRITE-LOG                                       
385100     END-IF                                                               
385200     .                                                                    
385300                                                                          
385400 S004-WRITE-W57053B-HEAD SECTION.                                         
385500     MOVE SPACE                 TO 73LINE-POST                            
385600     IF IN-EKH-KDSORT = 'SW'                                              
385700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
385800       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
385900       MOVE WS-TEXT           TO R3-LINE-TEXT                             
386000     ELSE                                                                 
386100       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
386200       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
386300       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
386400     END-IF                                                               
386500     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
386600                                                                          
386700     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
386800     MOVE 'W57053B'             TO POSTSUM-FDNAMN                         
386900     MOVE 'W57059D4'            TO POSTSUM-DDNAMN2                        
387000     CALL POSTSUM USING POSTSUM-PARM                                      
387100                                                                          
387200     .                                                                    
387300                                                                          
387400 S10-VATCODE SECTION.                                                     
387500     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
387600     PERFORM IMS-GU-WDB601                                                
387700     IF DCS-KDDC = SPACE                                                  
387800       MOVE NEJ              TO WDB6-A-SW                                 
387900     ELSE                                                                 
388000       MOVE JA               TO WDB6-A-SW                                 
388100     END-IF                                                               
388200                                                                          
388300     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
388400     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
388500     IF IN-EKH-SUVAT = ZERO                                               
388600*      MOVE 'P1'     TO R3-LINE-TAX-CODE                                  
388610       MOVE '  '     TO R3-LINE-TAX-CODE                                  
388700**** HERE WE ADD VAT FOR 10% TO BOOKING                                   
388800*      COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.1                  
388810       COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL                        
388900     ELSE                                                                 
389000*      MOVE 'P7'     TO R3-LINE-TAX-CODE                                  
389010       MOVE '  '     TO R3-LINE-TAX-CODE                                  
389100     END-IF                                                               
389200     IF IN-EKH-BEVAT = 'XX'                                               
389300*      MOVE 'P7'     TO R3-LINE-TAX-CODE                                  
389310       MOVE '  '     TO R3-LINE-TAX-CODE                                  
389400     END-IF                                                               
389500     .                                                                    
389600     EJECT                                                                
389700                                                                          
389800 S13-GET-LANDING-COST SECTION.                                            
389900     MOVE WC-NDC-BR              TO W-IDDC-B6                             
390000     PERFORM IMS-GU-WDB601                                                
390100     IF SEGMENT-FINNS                                                     
390200       PERFORM IMS-GNP-WDB617                                             
390300       IF SEGMENT-FINNS                                                   
390400         IF PROC-TILANDCO >  IN-EKH-DAVERDAT                              
390500           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
390600         ELSE                                                             
390700           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
390800         END-IF                                                           
390900       END-IF                                                             
391000     END-IF                                                               
391100     .                                                                    
391200     EJECT                                                                
391300 S20-CREATE-WRITE-LOG SECTION.                                            
391400     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
391500     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
391600     IF SYST-IDPTYP = '610'                                               
391700       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
391800     ELSE                                                                 
391900       MOVE ZERO                      TO WS-IDLEVNR                       
392000       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
392100                          FOR CHARACTERS BEFORE INITIAL SPACE             
392200       IF WS-IDLEVNR   > ZERO                                             
392300          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
392400                                      TO LOGG-IDKONTO                     
392500       END-IF                                                             
392600     END-IF                                                               
392700     IF R3-LINE-COST-CENTER NOT = SPACE                                   
392800       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
392900     END-IF                                                               
393000     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
393100     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
393200     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
393300     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
393400     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
393500                                                                          
393600     PERFORM S21-WRITE-W57055B                                            
393700     PERFORM S22-WRITE-W57058B                                            
393800                                                                          
393900     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
394000       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
394100       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
394200       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
394300                                                                          
394400       PERFORM S21-WRITE-W57055B                                          
394500     END-IF                                                               
394600     .                                                                    
394700     EJECT                                                                
394800                                                                          
394900 S21-WRITE-W57055B SECTION.                                               
395000     IF DCS-IDDC NOT = LOGG-IDDC                                          
395100        MOVE LOGG-IDDC TO W-IDDC-B6                                       
395200        PERFORM IMS-GU-WDB601                                             
395300     END-IF                                                               
395400     IF DCS-KDDC = SPACE                                                  
395500       MOVE NEJ              TO WDB6-A-SW                                 
395600     ELSE                                                                 
395700       MOVE JA               TO WDB6-A-SW                                 
395800     END-IF                                                               
395900                                                                          
396000     IF  WDB6-A-FINNS                                                     
396100     AND DCS-DDC                                                          
396200       MOVE 'N'       TO LOGG-FLLSBOK                                     
396300     END-IF                                                               
396400     WRITE LOGG-POST FROM LOGG-W57073                                     
396500                                                                          
396600     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
396700     MOVE 'W57055B'   TO POSTSUM-FDNAMN                                   
396800     MOVE 'W57059D5'  TO POSTSUM-DDNAMN2                                  
396900     CALL POSTSUM USING POSTSUM-PARM                                      
397000     .                                                                    
397100                                                                          
397200 S22-WRITE-W57058B SECTION.                                               
397300     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
397400     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
397500     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
397600                                                                          
397700     IF R3-LINE-AMOUNT-SIGN = '+'                                         
397800       IF AVST-SUBEL < +0                                                 
397900         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
398000       END-IF                                                             
398100       IF AVST-KVANTAL < +0                                               
398200         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
398300       END-IF                                                             
398400     ELSE                                                                 
398500       IF AVST-SUBEL > +0                                                 
398600         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
398700       END-IF                                                             
398800       IF AVST-KVANTAL > +0                                               
398900         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
399000       END-IF                                                             
399100     END-IF                                                               
399200                                                                          
399300     IF DCS-IDDC NOT = AVST-IDDC                                          
399400        MOVE AVST-IDDC  TO W-IDDC-B6                                      
399500        PERFORM IMS-GU-WDB601                                             
399600     END-IF                                                               
399700     IF DCS-KDDC = SPACE                                                  
399800       MOVE NEJ              TO WDB6-A-SW                                 
399900     ELSE                                                                 
400000       MOVE JA               TO WDB6-A-SW                                 
400100     END-IF                                                               
400200                                                                          
400300     IF  WDB6-A-FINNS                                                     
400400     AND DCS-DDC                                                          
400500       MOVE 'N'                 TO AVST-FLLSBOK                           
400600     END-IF                                                               
400700                                                                          
400800     IF AVST-IDKONTO(1:4) = '1454'                                        
400900       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
401000       WRITE AVST-POST FROM AVST-W57070                                   
401100                                                                          
401200       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
401300       MOVE 'W57058B'           TO POSTSUM-FDNAMN                         
401400       MOVE 'W57059D6'          TO POSTSUM-DDNAMN2                        
401500       CALL POSTSUM USING POSTSUM-PARM                                    
401600     END-IF                                                               
401700     .                                                                    
401800     EJECT                                                                
401900                                                                          
402000 S30-READ-DATABASE-B2-B1 SECTION.                                         
402100                                                                          
402200     IF IN-EKH-IDLEVNR = '1441'                                           
402300       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
402400     ELSE                                                                 
402500       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
402600       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
402700       PERFORM IMS-GU-WDB201                                              
402800       IF SEGMENT-SAKNAS                                                  
402900         MOVE 'BR99999'       TO W-WDB1-IDPARTNR                          
403000       ELSE                                                               
403100         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
403200       END-IF                                                             
403300     END-IF                                                               
403400     MOVE WC-IDFTG-BR         TO W-WDB1-IDFTG                             
403500     PERFORM IMS-GU-WDB101                                                
403600     IF SEGMENT-SAKNAS                                                    
403700       DISPLAY 'BETALARUPPG. SAKNAS '                                     
403800       DISPLAY IN-EKH-IDVERGL                                             
403900       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
404000       DISPLAY GMT-IDPARTNR                                               
404100                                                                          
404200       MOVE SPACE         TO BET-KDTRADP                                  
404300       MOVE ZERO          TO BET-IDPARTNR                                 
404400       MOVE '????'        TO WS-KDBETVIL                                  
404500       MOVE '???'         TO WS-KDVALISO-WDB1                             
404600     ELSE                                                                 
404700       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
404800     END-IF                                                               
404900     MOVE 'BRL'           TO WS-KDVALISO-WDB1                             
405000                                                                          
405100     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
405200     MOVE ZERO TO TALLY                                                   
405300     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
405400                 FOR CHARACTERS BEFORE INITIAL SPACE                      
405500     IF TALLY = ZERO                                                      
405600       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
405700     ELSE                                                                 
405800       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
405900                                TO W-BET-IDPARTNR-NUM                     
406000     END-IF                                                               
406100     .                                                                    
406200     EJECT                                                                
406300                                                                          
406400 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
406500     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
406600     PERFORM IMS-GU-WDB601                                                
406700     IF DCS-KDDC = SPACE                                                  
406800       MOVE NEJ              TO WDB6-A-SW                                 
406900     ELSE                                                                 
407000       MOVE JA               TO WDB6-A-SW                                 
407100     END-IF                                                               
407200                                                                          
407300     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
407400       IF IN-EKH-KDEKSHT NOT = '406'                                      
407500         IF IN-EKH-FLDCET = NEJ                                           
407600           PERFORM S42-SKAPA-RW2-INV-POSTER                               
407700         END-IF                                                           
407800       END-IF                                                             
407900     END-IF                                                               
408000                                                                          
408100     IF IN-EKH-KDEKNIVA = 'DET'                                           
408200       IF  IN-EKH-KDEKHHT = '204'                                         
408300       AND (IN-EKH-KDEKSHT = '201')                                       
408400         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
408500       END-IF                                                             
408600                                                                          
408700       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
408800       AND (WDB6-A-FINNS                                                  
408900       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
409000       OR   DCS-DDC OR DCS-NDC-PF))                                       
409100         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
409200       END-IF                                                             
409300                                                                          
409400       IF IN-FIL-IDPGM = 'W4183000'                                       
409500       AND (WDB6-A-FINNS                                                  
409600       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
409700       OR   DCS-DDC OR DCS-NDC-PF))                                       
409800         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
409900       END-IF                                                             
410000     END-IF                                                               
410100     .                                                                    
410200     EJECT                                                                
410300                                                                          
410400 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
410500     MOVE 'RW2'              TO RW2-IDPTYP                                
410600     MOVE 'RW2'              TO WS-IDPTYP                                 
410700     MOVE ZERO               TO RW2-IDDISTR                               
410800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
410900       MOVE WC-CDC-SE        TO RW2-IDDC                                  
411000     ELSE                                                                 
411100       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
411200     END-IF                                                               
411300     IF IN-EKH-KVANTAL < +0                                               
411400       MOVE '0422'           TO RW2-KDWRTYP                               
411500     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
411600     ELSE                                                                 
411700       MOVE '0421'           TO RW2-KDWRTYP                               
411800       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
411900     END-IF                                                               
412000                                                                          
412100     IF RW2-SUARTSTD NOT = +0                                             
412200       PERFORM S70-WRITE-W51350B                                          
412300     END-IF                                                               
412400     .                                                                    
412500     EJECT                                                                
412600                                                                          
412700 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
412800     MOVE '0110'             TO RW1-KDWRTYP                               
412900     IF DCS-KDDC = SPACE OR DCS-DDC                                       
413000       MOVE WC-CDC-SE        TO RW1-IDDC                                  
413100     ELSE                                                                 
413200       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
413300     END-IF                                                               
413400     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
413500     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
413600     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
413700                                                                          
413800     IF RW1-SUARTSTD NOT = +0                                             
413900       MOVE 'RW1' TO WS-IDPTYP                                            
414000       PERFORM S70-WRITE-W51350B                                          
414100     END-IF                                                               
414200     .                                                                    
414300     EJECT                                                                
414400                                                                          
414500 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
414600     MOVE '0110'             TO RW1-KDWRTYP                               
414700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
414800       MOVE WC-CDC-SE        TO RW1-IDDC                                  
414900     ELSE                                                                 
415000       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
415100     END-IF                                                               
415200     IF IN-EKH-KDANMORS = '30'                                            
415300       MOVE ZERO             TO RW1-SUARTSTD                              
415400     ELSE                                                                 
415500      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
415600     END-IF                                                               
415700     IF IN-EKH-KDANMORS = '30' OR '80'                                    
415800       MOVE ZERO             TO RW1-SUARTSJK                              
415900     ELSE                                                                 
416000      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
416100     END-IF                                                               
416200     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
416300                                                                          
416400     IF RW1-SUARTSTD NOT = +0                                             
416500       MOVE 'RW1' TO WS-IDPTYP                                            
416600       PERFORM S70-WRITE-W51350B                                          
416700     END-IF                                                               
416800     .                                                                    
416900     EJECT                                                                
417000                                                                          
417100 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
417200     MOVE '0110'             TO RW1-KDWRTYP                               
417300     IF DCS-KDDC = SPACE OR DCS-DDC                                       
417400       MOVE WC-CDC-SE        TO RW1-IDDC                                  
417500     ELSE                                                                 
417600       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
417700     END-IF                                                               
417800     IF IN-EKH-KDEKSHT = '310'                                            
417900*** SKROTNING KDANMORS  13 O 23                                           
418000       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
418100     ELSE                                                                 
418200      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
418300     END-IF                                                               
418400                                                                          
418500     MOVE ZERO               TO RW1-SUARTSJK                              
418600                                RW1-SUARTFSG                              
418700     IF RW1-SUARTSTD NOT = +0                                             
418800       MOVE 'RW1' TO WS-IDPTYP                                            
418900       PERFORM S70-WRITE-W51350B                                          
419000     END-IF                                                               
419100     .                                                                    
419200     EJECT                                                                
419300                                                                          
419400 S60-WRITE-W5705NB SECTION.                                               
419500     WRITE SAPUT-POST  FROM IN-AREA                                       
419600                                                                          
419700     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
419800     MOVE 'W5705NB'           TO POSTSUM-FDNAMN                           
419900     MOVE 'W57059D7'          TO POSTSUM-DDNAMN2                          
420000     CALL POSTSUM USING POSTSUM-PARM                                      
420100     .                                                                    
420200     EJECT                                                                
420300                                                                          
420400 S70-WRITE-W51350B SECTION.                                               
420500     IF WS-IDPTYP  = 'RW2'                                                
420600       IF DCS-KDDC = SPACE OR DCS-DDC                                     
420700         MOVE WC-CDC-SE        TO INV-IDDC                                
420800       ELSE                                                               
420900         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
421000       END-IF                                                             
421100       IF IN-EKH-KVANTAL < +0                                             
421200         MOVE '003'            TO INV-IDPTYP                              
421300       COMPUTE INV-SUARTSTD =                                             
421400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
421500       ELSE                                                               
421600         MOVE '002'            TO INV-IDPTYP                              
421700         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
421800       END-IF                                                             
421900       MOVE SPACE TO WS-IDPTYP                                            
422000       MOVE 0                  TO INV-ADLAGOMR                            
422100       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
422200       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
422300     END-IF                                                               
422400     IF WS-IDPTYP  = 'RW1'                                                
422500       IF DCS-KDDC = SPACE OR DCS-DDC                                     
422600         MOVE WC-CDC-SE        TO INV-IDDC                                
422700       ELSE                                                               
422800         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
422900       END-IF                                                             
423000       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
423100       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
423200       MOVE 0                  TO INV-ADLAGOMR                            
423300       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
423400       MOVE '001'              TO INV-IDPTYP                              
423500       MOVE SPACE              TO WS-IDPTYP                               
423600     END-IF                                                               
423700     WRITE INV-POST  FROM INV-W51310                                      
423800                                                                          
423900     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
424000     MOVE 'W51350B'           TO POSTSUM-FDNAMN                           
424100     MOVE 'W57059D8'          TO POSTSUM-DDNAMN2                          
424200     CALL POSTSUM USING POSTSUM-PARM                                      
424300     .                                                                    
424400     EJECT                                                                
424500                                                                          
424600 S80-GET-CURRENCY-RATE SECTION.                                           
424700     MOVE +0                  TO W-ANT                                    
424800     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
424900             BEFORE INITIAL ' '                                           
425000     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
425100     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
425200     PERFORM IMS-GU-WDL601                                                
425300     IF SEGMENT-SAKNAS                                                    
425400       CONTINUE                                                           
425500     ELSE                                                                 
425600       PERFORM IMS-GNP-WDL611                                             
425700       IF SEGMENT-SAKNAS                                                  
425800         CONTINUE                                                         
425900       ELSE                                                               
426000         IF (IN-EKH-KDEKHHT = '102'                                       
426100         AND IN-EKH-KDEKSHT = '126')                                      
426200**** EVENT 102-126 CREATES A NEW POST ON WDL611                           
426300**** WITH A DIFFERENT DAINLEV, SO WE NEED TO GET THE CURRENCY             
426400**** RATE FROM THE ORIGINAL POST AND THAT IS SAVED                        
426500**** IF THEY DO BINNING 102-121 OR DEVIATION 102-122 AT THE SAME          
426600**** TIME AS THEY REPORT HAC 102-126 AND THEN SAY THAT THEY               
426700**** RECIEVED THE GOODS BACK FROM CUSTOME 102-127 OR 102-128              
426800**** 102-121 AND 102-122 CAN ALSO GET WRONG CURRENCY RATE                 
426900           MOVE INL-PRKURS          TO WS-PRKURS-BR3                      
427000         ELSE                                                             
427100           COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                   
427200                                     - INL-DAINLEV                        
427300           MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                 
427400           MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                 
427500           MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                 
427600           MOVE W-DATE-AAMM           TO CURR-TIAAMM                      
427700           MOVE WS-KDVALISO-BR        TO CURR-KDVALISO-ROW                
427800           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
427900           IF CURR-KDSVAR = ' '                                           
428000             MOVE CURR-PRKURS-NEW     TO WS-PRKURS-BR3                    
428100           ELSE                                                           
428200             MOVE +1                  TO WS-PRKURS-BR3                    
428300           END-IF                                                         
428400         END-IF                                                           
428500       END-IF                                                             
428600     END-IF                                                               
428700     .                                                                    
428800     EJECT                                                                
428900                                                                          
429000 S81-GET-CURRENCY-RATE SECTION.                                           
429100     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
429200     MOVE 'BRL'               TO CURR-KDVALISO-ROW                        
429300     IF IN-FIL-IDPGM = 'W4183300'                                         
429400       IF IN-EKH-DAAVIDAT > ZERO                                          
429500         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
429600         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
429700       ELSE                                                               
429800         MOVE WS-TIAA            TO WS-TIAA-CR                            
429900         MOVE WS-TIMM            TO WS-TIMM-CR                            
430000       END-IF                                                             
430100     ELSE                                                                 
430200       MOVE WS-TIAA              TO WS-TIAA-CR                            
430300       MOVE WS-TIMM              TO WS-TIMM-CR                            
430400     END-IF                                                               
430500     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
430600     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
430700     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
430800     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
430900     IF CURR-KDSVAR = ' '                                                 
431000       IF IN-EKH-IDDISTR > ZERO                                           
431100         MOVE CURR-PRKURS-NEW TO WS-PRKURS-BR3                            
431200       ELSE                                                               
431300         IF WS-PRKURS = ZERO                                              
431400           MOVE 1           TO WS-PRKURS-BR3                              
431500         END-IF                                                           
431600       END-IF                                                             
431700     ELSE                                                                 
431800       MOVE 1               TO WS-PRKURS-BR3                              
431900     END-IF                                                               
432000     .                                                                    
432100     EJECT                                                                
432200                                                                          
432300* --- IMS SECTIONS ---                                                    
432400                                                                          
432500 IMS-GU-WDH521 SECTION.                                                   
432600     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
432700          DELIMITED BY SIZE INTO SSA1                                     
432800     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
432900          DELIMITED BY SIZE INTO SSA2                                     
433000     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
433100          DELIMITED BY SIZE INTO SSA3                                     
433200     MOVE '  '              TO GODK-STATUSKODER                           
433300     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
433400                                                   SSA2                   
433500                                                   SSA3                   
433600     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
433700                                                                          
433800     PERFORM IMS-STATUS-CONTROL                                           
433900     .                                                                    
434000                                                                          
434100 IMS-GNP-WDH531 SECTION.                                                  
434200     MOVE 'WDH531  '        TO SSA1                                       
434300     MOVE '  GE'            TO GODK-STATUSKODER                           
434400     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
434500     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
434600                               WS-STATUS                                  
434700     PERFORM IMS-STATUS-CONTROL                                           
434800     .                                                                    
434900     EJECT                                                                
435000                                                                          
435100 IMS-GU-WDB201 SECTION.                                                   
435200     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-KEY ')'                         
435300          DELIMITED BY SIZE INTO SSA1                                     
435400     MOVE '  GE'                 TO GODK-STATUSKODER                      
435500     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
435600      MOVE GMTA-STATUS-CODE      TO STATUS-WS                             
435700     PERFORM IMS-STATUS-CONTROL                                           
435800     .                                                                    
435900     EJECT                                                                
436000                                                                          
436100 IMS-GU-WDB101 SECTION.                                                   
436200     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
436300          DELIMITED BY SIZE INTO SSA1                                     
436400     MOVE '  GE'               TO GODK-STATUSKODER                        
436500     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
436600     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
436700     PERFORM IMS-STATUS-CONTROL                                           
436800     .                                                                    
436900     EJECT                                                                
437000                                                                          
437100 IMS-GU-5122 SECTION.                                                     
437200     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
437300            DELIMITED BY SIZE INTO SSA1                                   
437400     STRING 'WDGX5122(KEY5122  =' W-WDGXKEY-5122-X ')'                    
437500            DELIMITED BY SIZE INTO SSA2                                   
437600     MOVE '  GE'           TO GODK-STATUSKODER                            
437700     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5122 SSA1 SSA2            
437800     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
437900     PERFORM IMS-STATUS-CONTROL                                           
438000     .                                                                    
438100                                                                          
438200 IMS-GU-5121 SECTION.                                                     
438300     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
438400            DELIMITED BY SIZE INTO SSA1                                   
438500     MOVE '    '           TO GODK-STATUSKODER                            
438600     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5121 SSA1                 
438700     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
438800     PERFORM IMS-STATUS-CONTROL                                           
438900     .                                                                    
439000                                                                          
439100 IMS-GNP-5122 SECTION.                                                    
439200     STRING 'WDGX5122(KEY5122 >=' W-WDGXKEY-5122-MIN-X                    
439300                    '&KEY5122 <=' W-WDGXKEY-5122-MAX-X ')'                
439400            DELIMITED BY SIZE INTO SSA1                                   
439500     MOVE '  GE'           TO GODK-STATUSKODER                            
439600     CALL CBLTDLI USING GNP 5121-PCB DLI-IO-WDGX5122 SSA1                 
439700     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
439800     PERFORM IMS-STATUS-CONTROL                                           
439900     .                                                                    
440000     EJECT                                                                
440100                                                                          
440200 IMS-GU-WDB601    SECTION.                                                
440300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
440400          DELIMITED BY SIZE INTO SSA1                                     
440500     MOVE '  GE' TO GODK-STATUSKODER                                      
440600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
440700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
440800     PERFORM IMS-STATUS-CONTROL                                           
440900     IF SEGMENT-SAKNAS                                                    
441000        MOVE SPACE TO DCS-KDDC                                            
441100     END-IF                                                               
441200     .                                                                    
441300     EJECT                                                                
441400                                                                          
441500 IMS-GNP-WDB617 SECTION.                                                  
441600     MOVE 'WDB617   ' TO SSA1                                             
441700     MOVE '  GE'        TO GODK-STATUSKODER                               
441800     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
441900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
442000     PERFORM IMS-STATUS-CONTROL                                           
442100     .                                                                    
442200     SKIP3                                                                
442300                                                                          
442400 IMS-GU-WDL601   SECTION.                                                 
442500     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
442600          DELIMITED BY SIZE INTO SSA1                                     
442700     MOVE '  GE' TO GODK-STATUSKODER                                      
442800     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
442900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
443000     PERFORM IMS-STATUS-CONTROL                                           
443100     .                                                                    
443200     SKIP3                                                                
443300                                                                          
443400 IMS-GNP-WDL611   SECTION.                                                
443500     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
443600          DELIMITED BY SIZE INTO SSA1                                     
443700     MOVE '  GE' TO GODK-STATUSKODER                                      
443800     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
443900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
444000     PERFORM IMS-STATUS-CONTROL                                           
444100     .                                                                    
444200     SKIP3                                                                
444300                                                                          
444400 IMS-GET-WDF101 SECTION.                                                  
444500     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
444600             DELIMITED BY SIZE INTO SSA1                                  
444700     MOVE '  GE'                 TO GODK-STATUSKODER                      
444800     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
444900     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
445000     PERFORM IMS-STATUS-CONTROL                                           
445100     .                                                                    
445200                                                                          
445300 IMS-GNP-WDF106 SECTION.                                                  
445400     MOVE 'WDF106   '            TO SSA1                                  
445500     MOVE '  GE'                 TO GODK-STATUSKODER                      
445600     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF106 SSA1                   
445700     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
445800     PERFORM IMS-STATUS-CONTROL                                           
445900     .                                                                    
446000                                                                          
446100 IMS-STATUS-CONTROL SECTION.                                              
446200     SET STATUS-IX TO 1                                                   
446300     SEARCH GODK-STATUS                                                   
446400       AT END                                                             
446500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
446600           DELIMITED BY SIZE INTO FELTEXT                                 
446700         DISPLAY FELTEXT                                                  
446800         CALL FELLOG                                                      
446900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
447000         CONTINUE                                                         
447100     END-SEARCH                                                           
447200     .                                                                    
