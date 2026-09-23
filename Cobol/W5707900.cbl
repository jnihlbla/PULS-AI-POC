000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5707900.                                                
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
004700     SELECT W57066                     ASSIGN TO W57079D1.                
004800                                                                          
004900*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
005000     SELECT W57051M                    ASSIGN TO W57079D2.                
005100                                                                          
005200*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005300     SELECT W57052M                    ASSIGN TO W57079D3.                
005400                                                                          
005500*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005600     SELECT W57053M                    ASSIGN TO W57079D4.                
005700                                                                          
005800*          --- LOGG TILL ON-DEMAND                                        
005900     SELECT W57055M                    ASSIGN TO W57079D5.                
006000                                                                          
006100*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006200     SELECT W57058M                    ASSIGN TO W57079D6.                
006300                                                                          
006400*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006500     SELECT W5705NM                    ASSIGN TO W57079D7.                
006600                                                                          
006700*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006800     SELECT W51350M                    ASSIGN TO W57079D8.                
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
008100 FD  W57051M                                                              
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400*01  71INIT-POST -COPY R3INIT20               -L.                         
008500*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008600*01  71LINE-POST -COPY R3LINE20               -L.                         
008700                                                                          
008800 FD  W57052M                                                              
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100*01  72LINE-POST -COPY R3LINE20               -L.                         
009200                                                                          
009300 FD  W57053M                                                              
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009700*01  73LINE-POST -COPY R3LINE20               -L.                         
009800                                                                          
009900 FD  W57055M                                                              
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200*01  LOGG-POST   -COPY W57073                 -L.                         
010300                                                                          
010400 FD  W57058M                                                              
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700*01  AVST-POST   -COPY W57070                 -L.                         
010800                                                                          
010900 FD  W5705NM                                                              
011000     RECORDING       F                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300 01  SAPUT-POST.                                                          
011400*    03  -COPY WDR801        -L.                                          
011500     03 FILLER                   PIC X(6).                                
011600                                                                          
011700 FD  W51350M                                                              
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012100                                                                          
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400*    -- CHECKED BY WY2000                                                 
012500 77  IDPGM                        PIC X(8)    VALUE 'W5707900'.           
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
013900 77  WS-LINE-AMOUNT-127-1         PIC S9(13)V99 COMP-3.                   
014000 77  WS-LINE-AMOUNT-127-2         PIC S9(13)V99 COMP-3.                   
014100 77  WS-LINE-AMOUNT-128-1         PIC S9(13)V99 COMP-3.                   
014200 77  WS-LINE-AMOUNT-128-2         PIC S9(13)V99 COMP-3.                   
014300 77  WS-LINE-AMOUNT-131-1         PIC S9(13)V99 COMP-3.                   
014400 77  WS-LINE-AMOUNT-131-2         PIC S9(13)V99 COMP-3.                   
014500 77  WS-102-124-DET-1             PIC S9(13)V99 COMP-3.                   
014600 77  WS-102-124-DET-2             PIC S9(13)V99 COMP-3.                   
014700 77  WS-102-134-DET-1             PIC S9(13)V99 COMP-3.                   
014800 77  WS-102-134-DET-2             PIC S9(13)V99 COMP-3.                   
014900 77  SPAR-SUMMA-102-125         PIC S9(13)V99  COMP-3 VALUE ZERO.         
015000 77  WS-BELOPP                  PIC S9(13)V99  COMP-3.                    
015100 77  WS-LOP                       PIC 9       VALUE ZERO.                 
015200 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
015300 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
015400 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
015500 77  SPAR-LINE-ORDER              PIC X(12).                              
015600 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
015700 77  WS-RED-IDKST                 PIC X(10).                              
015800 77  WS-IDPTYP                    PIC X(3).                               
015900 77  WS-FAKTURA-DATUM             PIC X(16).                              
016000 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
016100 77  SPAR-SUMMA                 PIC S9(13)V99  COMP-3 VALUE ZERO.         
016200 77  SPAR-PRDMTRL               PIC S9(13)V99  COMP-3 VALUE ZERO.         
016300 77  SPAR-PROVRPAL              PIC S9(13)V99  COMP-3 VALUE ZERO.         
016400 77  SPAR-PRDIRLON              PIC S9(13)V99  COMP-3 VALUE ZERO.         
016500 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
016600 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
016700 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
016800 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
016900 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
017000 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
017100 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
017200 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
017300 77  WS-KDPRODSL-SAVE             PIC X(2)    VALUE SPACE.                
017400                                                                          
017500 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
017600       88  WDB6-A-FINNS                       VALUE 'J'.                  
017700       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
017800                                                                          
017900*01  -COPY WWPRODSL                                                       
018000                                                                          
018100*01  -COPY WWDCKONS                                                       
018200     EJECT                                                                
018300                                                                          
018400 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
018500*01  -COPY WWIDFTG                                                        
018600     EJECT                                                                
018700                                                                          
018800 01  FELTEXT                      PIC X(80).                              
018900 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
019000*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
019100     EJECT                                                                
019200                                                                          
019300 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
019400 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
019500     EJECT                                                                
019600 01  WS-IDDISTR-IDKUNDNR.                                                 
019700     03  FILLER                   PIC X(2)    VALUE SPACE.                
019800     03  WS-IDDISTR               PIC 9(4).                               
019900     03  WS-IDKUNDNR              PIC 9(6).                               
020000                                                                          
020100 01  WS-KDBETVIL                  PIC X(4).                               
020200 01  WS-KDVALISO-WDB1             PIC X(3).                               
020300 01  WS-KDVALISO                  PIC X(3).                               
020400 01  WS-KDVALISO-MX               PIC X(3) VALUE 'MXN'.                   
020500 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
020600 01  WS-PRKURS-MX                 PIC S9(6)V9(5) COMP-3.                  
020700 01  WS-PRKURS-MX2                PIC S9(6)V9(5) COMP-3.                  
020800 01  WS-PRKURS-MX3                PIC S9(6)V9(5) COMP-3.                  
020900 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
021000 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
021100                                                                          
021200 01  WS-ALLOCATE.                                                         
021300     03  WS-ALLOCATE-DC           PIC X(2).                               
021400     03  WS-ALLOCATE-DISTR        PIC X(5).                               
021500     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
021600     03  FILLER                   PIC X(4)    VALUE SPACE.                
021700                                                                          
021800 01  WS-TEXT.                                                             
021900     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
022000     03  WS-TEXT-KDEKHHT          PIC X(3).                               
022100     03  WS-TEXT-KDEKSHT          PIC X(3).                               
022200     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
022300     03  FILLER                   PIC X(7)    VALUE SPACE.                
022400                                                                          
022500 01  WS-LINE-TEXT.                                                        
022600     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
022700     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
022800     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
022900     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
023000     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
023100     03  FILLER                   PIC X(22)   VALUE SPACE.                
023200                                                                          
023300 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
023400 01  WS-PRCTR.                                                            
023500     03  WS-PRCTR-PRODSL          PIC X(2).                               
023600     03  FILLER                   PIC X(1).                               
023700     03  FILLER                   PIC X(7).                               
023800                                                                          
023900 01  WS-R3-ACCOUNT.                                                       
024000     03  WS-R3-ACCOUNT-ALFA.                                              
024100         05 FILLER                PIC X(4).                               
024200         05 WS-R3-ACCOUNT-6       PIC X(6).                               
024300     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
024400         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
024500                                                                          
024600 01  WS-ACCOUNT.                                                          
024700     03  FILLER                   PIC X(7).                               
024800     03  WS-ACCOUNT-4             PIC X(1).                               
024900     03  FILLER                   PIC X(2).                               
025000                                                                          
025100 01  SPAR-AREA.                                                           
025200     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
025300     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
025400     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
025500     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
025600                                                                          
025700 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
025800 01  FILLER REDEFINES DAGENS-DATUM.                                       
025900     03  DAGENS-DATUM-AAR         PIC 9(2).                               
026000     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
026100     03  DAGENS-DATUM-DAG         PIC 9(2).                               
026200                                                                          
026300 01  WS-NEW-MONTH                 PIC 9(2).                               
026400                                                                          
026500 01  WS-DAREGDAT.                                                         
026600     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
026700     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
026800                                                                          
026900 01  WS-TIREGDAT-TOT.                                                     
027000     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
027100     03  WS-TIREGDAT              PIC 9(6).                               
027200                                                                          
027300 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
027400 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
027500     EJECT                                                                
027600                                                                          
027700 01  DYNAMISKA-SUBPROGRAM.                                                
027800     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
027900     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
028000     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
028100     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
028200     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
028300     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
028400                                                                          
028500*    --- PARAMETRAR TILL ABEND                                            
028600 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
028700 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
028800 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
028900     EJECT                                                                
029000                                                                          
029100*    --- PARAMETRAR TILL DATKORT                                          
029200 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57079'.             
029300                                                                          
029400 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
029500*01  -COPY WDATKORT                                                       
029600     EJECT                                                                
029700                                                                          
029800*    --- PARAMETRAR TILL POSTSUM                                          
029900*01  -COPY W0005   -PRE  POSTSUM-                                         
030000     EJECT                                                                
030100                                                                          
030200 01  FILLER                          PIC X(16) VALUE 'W510CURR '.         
030300*01  -COPY W510CURR                                                       
030400     EJECT                                                                
030500                                                                          
030600 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
030700*01  AREA -COPY WDR801           -PRE IN-                                 
030800*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
030900         05   IN-EKH-IDSYSMOT     PIC X(6).                               
031000                                                                          
031100     EJECT                                                                
031200 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
031300                                                                          
031400*01  -COPY R3LINE20              -PRE R3-                                 
031500*01  -COPY R3HEAD20              -PRE R3-                                 
031600*01  -COPY R3INIT20              -PRE R3-                                 
031700*01  -COPY W57073                -PRE LOGG-                               
031800*01  -COPY W57070                -PRE AVST-                               
031900*01  -COPY W517RW1               -PRE RW1-                                
032000*01  -COPY W517RW2               -PRE RW2-                                
032100*01  -COPY W51310                -PRE INV-                                
032200     EJECT                                                                
032300                                                                          
032400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032500 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
032600                                                                          
032700 01  NYCKLAR-TILL-DLI.                                                    
032800     03  W-WDH501KY-X.                                                    
032900         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
033000         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
033100     03  W-KDEKSHT-X.                                                     
033200         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
033300     03  W-KDEKNIVA-X.                                                    
033400         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
033500     03  W-WDH531KY-X.                                                    
033600         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
033700         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
033800     03  W-IDRADNR-X.                                                     
033900         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
034000                                                                          
034100     03  W-IDGMT-KEY.                                                     
034200         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
034300         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
034400                                                                          
034500     03  W-WDB101KY-X.                                                    
034600         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
034700         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
034800                                                                          
034900     03  W-WDGXKEY-5121-X.                                                
035000         05  FILLER               PIC X(4)    VALUE '5121'.               
035100         05  FILLER               PIC X(2)    VALUE '61'.                 
035200         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
035300     03  W-WDGXKEY-5122-X.                                                
035400         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
035500         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
035600     03  W-WDGXKEY-5122-MIN-X.                                            
035700         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
035800         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
035900     03  W-WDGXKEY-5122-MAX-X.                                            
036000         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
036100         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
036200                                                                          
036300     03  W-IDDC-B6-X.                                                     
036400         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
036500                                                                          
036600     03  W-IDARTNR-X.                                                     
036700         05 W-IDARTNR             PIC S9(9) COMP-3.                       
036800                                                                          
036900     03  W-IDFAKT-X.                                                      
037000         05 W-IDFAKT              PIC S9(7) COMP-3.                       
037100                                                                          
037200     03  W-IDLEVNR-X.                                                     
037300         05  W-IDLEVNR            PIC X(5)    VALUE SPACE.                
037400                                                                          
037500     EJECT                                                                
037600                                                                          
037700*    --- STATUS-KOD FRÅN IMS                                              
037800 01  STATUS-WS                    PIC XX.                                 
037900     88  SEGMENT-FINNS                        VALUE '  '.                 
038000     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
038100                                                                          
038200 01  GODK-STATUSKODER.                                                    
038300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038400                                                                          
038500 01  SSA1                         PIC X(128).                             
038600 01  SSA2                         PIC X(64).                              
038700 01  SSA3                         PIC X(64).                              
038800     EJECT                                                                
038900                                                                          
039000*    --- IMS FUNKTIONSKODER                                               
039100*01  -COPY W0003                                                          
039200     EJECT                                                                
039300                                                                          
039400*    ---  DLI INPUT-OUTPUT AREA                                           
039500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
039600 01  DLI-IO-WDH501.                                                       
039700*    03  -COPY WDH501                                                     
039800     EJECT                                                                
039900                                                                          
040000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
040100 01  DLI-IO-WDH511.                                                       
040200*    03  -COPY WDH511                                                     
040300     EJECT                                                                
040400                                                                          
040500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
040600 01  DLI-IO-WDH521.                                                       
040700*    03  -COPY WDH521                                                     
040800     EJECT                                                                
040900                                                                          
041000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
041100 01  DLI-IO-WDH531.                                                       
041200*    03  -COPY WDH531                                                     
041300     EJECT                                                                
041400                                                                          
041500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
041600 01  DLI-IO-WLBETC01.                                                     
041700*    03  -COPY WDB101                                                     
041800     EJECT                                                                
041900                                                                          
042000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
042100 01  DLI-IO-WLGMTA01.                                                     
042200*    03  -COPY WDB201                                                     
042300     EJECT                                                                
042400                                                                          
042500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
042600 01  DLI-IO-WDGX5121.                                                     
042700*    03  -COPY WDGX5121                                                   
042800     EJECT                                                                
042900                                                                          
043000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
043100 01  DLI-IO-WDGX5122.                                                     
043200*    03  -COPY WDGX5122                                                   
043300     EJECT                                                                
043400                                                                          
043500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
043600 01   DLI-IO-AREA-B601.                                                   
043700*     03  -COPY WDB601                                                    
043800     EJECT                                                                
043900 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
044000 01   DLI-IO-WDB617.                                                      
044100*     03  -COPY WDB617                                                    
044200     EJECT                                                                
044300 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
044400 01   DLI-IO-AREA-L601.                                                   
044500*     03  -COPY WDL601                                                    
044600     EJECT                                                                
044700 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
044800 01   DLI-IO-AREA-L611.                                                   
044900*     03  -COPY WDL611                                                    
045000     EJECT                                                                
045100 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6C1'.                
045200     SKIP3                                                                
045300 01  FILLER               PIC X(16)   VALUE 'WDF101 AREA'.                
045400 01  DLI-IO-WDF101.                                                       
045500*    03  -COPY WDF101                                                     
045600     EJECT                                                                
045700 01  FILLER               PIC X(16)   VALUE 'WDF106 AREA'.                
045800 01  DLI-IO-WDF106.                                                       
045900*    03  -COPY WDF106                                                     
046000     EJECT                                                                
046100 LINKAGE SECTION.                                                         
046200*01  -COPY W0008  -PRE WDH5-                                              
046300     05  FILLER                  PIC X.                                   
046400                                                                          
046500*01  -COPY W0008  -PRE GMTA-                                              
046600     05  FILLER                  PIC X.                                   
046700                                                                          
046800*01  -COPY W0008  -PRE BETC-                                              
046900     05  FILLER                  PIC X.                                   
047000                                                                          
047100*01  -COPY W0008  -PRE 5121-                                              
047200     05  FILLER                  PIC X.                                   
047300                                                                          
047400*01  -COPY W0008  -PRE WDG2-                                              
047500     05  FILLER                  PIC X.                                   
047600                                                                          
047700*01  -COPY W0008  -PRE WDB6-                                              
047800     05  FILLER                  PIC X.                                   
047900                                                                          
048000*01  -COPY W0008  -PRE WDL6-                                              
048100     05  FILLER                  PIC X.                                   
048200                                                                          
048300*01  -COPY W0008  -PRE WDF1-                                              
048400     05  FILLER                  PIC X.                                   
048500                                                                          
048600     EJECT                                                                
048700                                                                          
048800 PROCEDURE DIVISION  USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
048900                           WDG2-PCB WDB6-PCB WDL6-PCB WDF1-PCB.           
049000 MAIN SECTION.                                                            
049100     ENTRY 'DLITCBL' USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
049200                           WDG2-PCB WDB6-PCB WDL6-PCB WDF1-PCB.           
049300                                                                          
049400     PERFORM A-INIT                                                       
049500                                                                          
049600     PERFORM S01-READ-W57066                                              
049700     PERFORM UNTIL END-OF-W57066                                          
049800*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
049900       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
050000       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
050100       AND WS-NEW-MONTH > 01                                              
050200         PERFORM S60-WRITE-W5705NM                                        
050300       ELSE                                                               
050400         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
050500         PERFORM S30-READ-DATABASE-B2-B1                                  
050600         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
050700           PERFORM C-EXECUTE                                              
050800         END-IF                                                           
050900       END-IF                                                             
051000       PERFORM S01-READ-W57066                                            
051100     END-PERFORM                                                          
051200                                                                          
051300     PERFORM Z-FINI                                                       
051400                                                                          
051500     MOVE ZERO TO RETURN-CODE                                             
051600     GOBACK                                                               
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000 A-INIT SECTION.                                                          
052100     OPEN INPUT  W57066                                                   
052200                                                                          
052300     OPEN OUTPUT W57058M                                                  
052400                 W57051M                                                  
052500                 W57052M                                                  
052600                 W57053M                                                  
052700                 W57055M                                                  
052800                 W5705NM                                                  
052900                 W51350M                                                  
053000                                                                          
053100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
053200     MOVE 20               TO RW1-DAVVREG(1:2)                            
053300     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
053400                              RW1-DAVVREG(3:2)                            
053500                              W-DATE-AAMM(1:2)                            
053600                              WS-TIAA                                     
053700     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
053800                              W-DATE-AAMM(3:2)                            
053900                              WS-TIMM                                     
054000                              WS-NEW-MONTH                                
054100     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
054200     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
054300     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
054400                                                                          
054500*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
054600*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
054700     IF WS-NEW-MONTH = 12                                                 
054800       MOVE 1              TO WS-NEW-MONTH                                
054900     ELSE                                                                 
055000       ADD 1               TO WS-NEW-MONTH                                
055100*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
055200***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
055300***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
055400***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
055500***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
055600       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
055700       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
055800         ADD 1             TO WS-NEW-MONTH                                
055900         ADD 1             TO WS-TIMM                                     
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
057100     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
057200     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
057300     MOVE 'M'                   TO CURR-KDVALTYP                          
057400                                                                          
057500     MOVE WS-KDVALISO-MX        TO CURR-KDVALISO-ROW                      
057600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
057700     IF CURR-KDSVAR = ' '                                                 
057800       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-MX                           
057900     ELSE                                                                 
058000       MOVE 1                   TO WS-PRKURS-MX                           
058100     END-IF                                                               
058200     COMPUTE WS-PRKURS-MX2 ROUNDED = 1 / WS-PRKURS-MX                     
058300     MOVE WS-PRKURS-MX          TO WS-PRKURS-MX3                          
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700 C-EXECUTE SECTION.                                                       
058800     MOVE WC-IDFTG-MX           TO W-IDFTG                                
058900     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
059000     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
059100     IF IN-EKH-KDEKNIVA = 'TDET'                                          
059200       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
059300     END-IF                                                               
059400     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
059500     PERFORM IMS-GU-WDH521                                                
059600     PERFORM IMS-GNP-WDH531                                               
059700                                                                          
059800     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
059900     IF IN-EKH-IDDISTR > ZERO                                             
060000       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
060100     ELSE                                                                 
060200       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
060300     END-IF                                                               
060400     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
060500                                                                          
060600* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
060700* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
060800* BOKFÖRINGEN I SAP SKER DOCK ALLTID I MXN, DÄRFÖR BYTET HÄR:             
060900*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
061000*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '106')               
061100*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '107')               
061200*      MOVE 'MXN'               TO WS-KDVALISO                            
061300*    END-IF                                                               
061400                                                                          
061500     IF  ((IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                               
061600     AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                                
061700     OR (IN-EKH-KDEKHHT = '303'                                           
061800     AND IN-EKH-KDEKSHT = '301')                                          
061900     OR (IN-EKH-KDEKHHT = '303'                                           
062000     AND IN-EKH-KDEKSHT = '307'))                                         
062100       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
062200     ELSE                                                                 
062300       MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                             
062400       MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                             
062500       IF WS-LOP = 9                                                      
062600         MOVE ZERO  TO WS-LOP                                             
062700       ELSE                                                               
062800         ADD +1     TO WS-LOP                                             
062900       END-IF                                                             
063000       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
063100     END-IF                                                               
063200* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
063300     IF SYST-IDPTYP = '210'                                               
063400       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
063500     ELSE                                                                 
063600       IF SYST-IDPTYP = '310'                                             
063700         PERFORM CC-CREATE-WRITE-HEADER-AR                                
063800       ELSE                                                               
063900* TEST OM BRYTNING PÅ VERIFIKATION                                        
064000         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
064100         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
064200         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
064300         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
064400           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
064500           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
064600           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
064700           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
064800           IF (IN-EKH-KDEKHHT = '102'                                     
064900           AND IN-EKH-KDEKSHT = '121')                                    
065000           OR (IN-EKH-KDEKHHT = '102'                                     
065100           AND IN-EKH-KDEKSHT = '122')                                    
065200           OR (IN-EKH-KDEKHHT = '102'                                     
065300           AND IN-EKH-KDEKSHT = '126')                                    
065310           OR (IN-EKH-KDEKHHT = '102'                                     
065320           AND IN-EKH-KDEKSHT = '127')                                    
065400           OR (IN-EKH-KDEKHHT = '102'                                     
065410           AND IN-EKH-KDEKSHT = '128')                                    
065420           OR (IN-EKH-KDEKHHT = '102'                                     
065500           AND IN-EKH-KDEKSHT = '131')                                    
065600           OR (IN-EKH-KDEKHHT = '102'                                     
065700           AND IN-EKH-KDEKSHT = '132')                                    
065800             PERFORM S80-GET-CURRENCY-RATE                                
065900           END-IF                                                         
066000           IF (IN-EKH-KDEKHHT = '303'                                     
066100           AND IN-EKH-KDEKSHT = '301')                                    
066110           OR (IN-EKH-KDEKHHT = '303'                                     
066120           AND IN-EKH-KDEKSHT = '307')                                    
066130           OR (IN-EKH-KDEKHHT = '303'                                     
066140           AND IN-EKH-KDEKSHT = '371')                                    
066150           OR (IN-EKH-KDEKHHT = '303'                                     
066160           AND IN-EKH-KDEKSHT = '3XX')                                    
066200             PERFORM S81-GET-CURRENCY-RATE                                
066300           END-IF                                                         
066400*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
066500*   HEADER-POST TILL HUVUDBOKEN                                           
066600           IF (IN-EKH-KDEKHHT = '102'                                     
066700           AND IN-EKH-KDEKSHT = '120')                                    
066800           OR (IN-EKH-KDEKHHT = '102'                                     
066900           AND IN-EKH-KDEKSHT = '124')                                    
067000           OR (IN-EKH-KDEKHHT = '102'                                     
067100           AND IN-EKH-KDEKSHT = '125')                                    
067200           OR (IN-EKH-KDEKHHT = '102'                                     
067300           AND IN-EKH-KDEKSHT = '130')                                    
067400           OR (IN-EKH-KDEKHHT = '102'                                     
067500           AND IN-EKH-KDEKSHT = '134')                                    
067600           OR (IN-EKH-KDEKHHT = '103'                                     
067700           AND IN-EKH-KDEKSHT = '102')                                    
067800           OR (IN-EKH-KDEKHHT = '103'                                     
067900           AND IN-EKH-KDEKSHT = '106')                                    
068000           OR (IN-EKH-KDEKHHT = '103'                                     
068100           AND IN-EKH-KDEKSHT = '107')                                    
068200           OR (IN-EKH-KDEKHHT = '204'                                     
068300           AND IN-EKH-KDEKSHT = '301')                                    
068400           OR (IN-EKH-KDEKHHT = '303'                                     
068500           AND IN-EKH-KDEKSHT = '301')                                    
068600           OR (IN-EKH-KDEKHHT = '303'                                     
068700           AND IN-EKH-KDEKSHT = '307')                                    
068800           OR (IN-EKH-KDEKHHT = '303'                                     
068900           AND IN-EKH-KDEKSHT = '3XX')                                    
069000           OR (IN-EKH-KDEKHHT = '303'                                     
069100           AND IN-EKH-KDEKSHT = '371')                                    
069200             CONTINUE                                                     
069300           ELSE                                                           
069400             PERFORM CA-CREATE-WRITE-HEADER-GL                            
069500           END-IF                                                         
069600         END-IF                                                           
069700       END-IF                                                             
069800     END-IF                                                               
069900                                                                          
070000**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
070100     MOVE WS-STATUS TO STATUS-WS                                          
070200     PERFORM UNTIL SEGMENT-SAKNAS                                         
070300       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
070400                                                                          
070500* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
070600       IF SYST-IDPTYP = '610'                                             
070700         PERFORM CD-BUILD-COMMON-610-PART                                 
070800         PERFORM CE-SCHEDULE-LINE-GL                                      
070900       ELSE                                                               
071000         IF SYST-IDPTYP = '210'                                           
071100           PERFORM CF-BUILD-COMMON-210-PART                               
071200           PERFORM CG-SCHEDULE-LINE-AP                                    
071300         ELSE                                                             
071400           IF SYST-IDPTYP = '310'                                         
071500             PERFORM CH-BUILD-COMMON-310-PART                             
071600             PERFORM CI-SCHEDULE-LINE-AR                                  
071700           END-IF                                                         
071800         END-IF                                                           
071900       END-IF                                                             
072000       PERFORM IMS-GNP-WDH531                                             
072100     END-PERFORM                                                          
072200     .                                                                    
072300     EJECT                                                                
072400                                                                          
072500 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
072600     MOVE SPACE                   TO R3-HEAD-R3                           
072700     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
072800     MOVE 'MX10'                  TO R3-HEAD-COMPANY-CODE                 
072900     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
073000     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
073100     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
073200     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
073300     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
073400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
073500     ELSE                                                                 
073600       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
073700     END-IF                                                               
073800     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
073900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
074000     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
074100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
074200     IF WS-KDVALISO = 'MXN'                                               
074300       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
074400     ELSE                                                                 
074500       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
074600       MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                     
074700       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
074800       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
074900       IF CURR-KDSVAR = ' '                                               
075000         IF IN-EKH-IDDISTR > ZERO                                         
075100           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
075200         ELSE                                                             
075300           MOVE 1               TO WS-PRKURS                              
075400         END-IF                                                           
075500       ELSE                                                               
075600         MOVE 1                 TO WS-PRKURS                              
075700       END-IF                                                             
075800       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
075900                                       CURR-REVALUTA-TO                   
076000       IF CURR-REVALUTA-TO = +1                                           
076100         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
076200       END-IF                                                             
076300       IF CURR-REVALUTA-TO = +10                                          
076400         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
076500       END-IF                                                             
076600       IF CURR-REVALUTA-TO = +100                                         
076700         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
076800       END-IF                                                             
076900     END-IF                                                               
077000     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
077100     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
077200     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
077300     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
077400     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
077500     MOVE JA                      TO WS-HEADER-SW                         
077600     MOVE NEJ                     TO WS-LINE-SW                           
077700                                                                          
077800* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
077900     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
078000     OR (IN-EKH-KDEKHHT = '303'                                           
078100     AND IN-EKH-KDEKSHT = '391')                                          
078200     OR (IN-EKH-KDEKHHT = '102'                                           
078300     AND IN-EKH-KDEKSHT = '121')                                          
078400     OR (IN-EKH-KDEKHHT = '102'                                           
078500     AND IN-EKH-KDEKSHT = '131')                                          
078600       PERFORM S004-WRITE-W57053M-HEAD                                    
078700     ELSE                                                                 
078800       PERFORM S002-WRITE-W57051M-HEAD                                    
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200                                                                          
079300 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
079400     MOVE SPACE                   TO R3-HEAD-R3                           
079500     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
079600     MOVE 'MX10'                  TO R3-HEAD-COMPANY-CODE                 
079700                                     R3-HEAD-CONTROL-AREA                 
079800     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
079900     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
080000     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
080100     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
080200     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
080300     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
080400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
080500     ELSE                                                                 
080600       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
080700     END-IF                                                               
080800     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
080900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
081000     IF (IN-EKH-KDEKHHT = '103'                                           
081100     AND IN-EKH-KDEKSHT = '102')                                          
081200     OR (IN-EKH-KDEKHHT = '103'                                           
081300     AND IN-EKH-KDEKSHT = '106')                                          
081400     OR (IN-EKH-KDEKHHT = '103'                                           
081500     AND IN-EKH-KDEKSHT = '107')                                          
081600       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
081700       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
081800     ELSE                                                                 
081900       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
082000       MOVE WS-PRKURS-MX2         TO R3-HEAD-EXCHANGE-RATE                
082100       MOVE 'MXN'                 TO CURR-KDVALISO-ROW                    
082200       IF IN-FIL-IDPGM = 'W4183300'                                       
082300         IF IN-EKH-DAAVIDAT > ZERO                                        
082400           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
082500           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
082600         ELSE                                                             
082700           MOVE WS-TIAA              TO WS-TIAA-CR                        
082800           MOVE WS-TIMM              TO WS-TIMM-CR                        
082900         END-IF                                                           
083000       ELSE                                                               
083100         MOVE WS-TIAA                TO WS-TIAA-CR                        
083200         MOVE WS-TIMM                TO WS-TIMM-CR                        
083300       END-IF                                                             
083400       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
083500       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
083600       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
083700       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
083800       IF CURR-KDSVAR = ' '                                               
083900         IF IN-EKH-IDDISTR > ZERO                                         
084000           MOVE CURR-PRKURS-NEW TO WS-PRKURS-MX                           
084100         ELSE                                                             
084200           IF WS-PRKURS = ZERO                                            
084300             MOVE 1             TO WS-PRKURS-MX                           
084400           END-IF                                                         
084500         END-IF                                                           
084600       ELSE                                                               
084700         MOVE 1                 TO WS-PRKURS-MX                           
084800       END-IF                                                             
084900       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-MX *                     
085000                                       CURR-REVALUTA-TO                   
085100       END-COMPUTE                                                        
085200       IF CURR-REVALUTA-TO = +1                                           
085300         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
085400       END-IF                                                             
085500       IF CURR-REVALUTA-TO = +10                                          
085600         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
085700       END-IF                                                             
085800       IF CURR-REVALUTA-TO = +100                                         
085900         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
086000       END-IF                                                             
086100     END-IF                                                               
086200     IF (IN-EKH-KDEKHHT = '102'                                           
086300     AND IN-EKH-KDEKSHT = '120')                                          
086400     OR (IN-EKH-KDEKHHT = '102'                                           
086500     AND IN-EKH-KDEKSHT = '124')                                          
086600     OR (IN-EKH-KDEKHHT = '102'                                           
086700     AND IN-EKH-KDEKSHT = '125')                                          
086800     OR (IN-EKH-KDEKHHT = '102'                                           
086900     AND IN-EKH-KDEKSHT = '130')                                          
087000     OR (IN-EKH-KDEKHHT = '102'                                           
087100     AND IN-EKH-KDEKSHT = '134')                                          
087200     OR (IN-EKH-KDEKHHT = '303'                                           
087300     AND IN-EKH-KDEKSHT = '301')                                          
087400     OR (IN-EKH-KDEKHHT = '303'                                           
087500     AND IN-EKH-KDEKSHT = '307')                                          
087600     OR (IN-EKH-KDEKHHT = '303'                                           
087700     AND IN-EKH-KDEKSHT = '371')                                          
087800     OR (IN-EKH-KDEKHHT = '303'                                           
087900     AND IN-EKH-KDEKSHT = '3XX')                                          
088000       MOVE 'MXN'                 TO R3-HEAD-CURRENCY                     
088100       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
088200     END-IF                                                               
088300     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
088400     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
088500     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
088600     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
088700     MOVE JA                      TO WS-HEADER-SW                         
088800     MOVE NEJ                     TO WS-LINE-SW                           
088900                                                                          
089000* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57053M                       
089100       PERFORM S004-WRITE-W57053M-HEAD                                    
089200     .                                                                    
089300     EJECT                                                                
089400                                                                          
089500 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
089600     MOVE SPACE                   TO R3-HEAD-R3                           
089700     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
089800     MOVE 'MX10'                  TO R3-HEAD-COMPANY-CODE                 
089900                                     R3-HEAD-CONTROL-AREA                 
090000     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
090100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
090200     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
090300     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
090400     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
090500     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
090600       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
090700     ELSE                                                                 
090800       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
090900     END-IF                                                               
091000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
091100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
091200     IF (IN-EKH-KDEKHHT = '204'                                           
091300     AND IN-EKH-KDEKSHT = '301')                                          
091400       MOVE 'MXN'                 TO R3-HEAD-CURRENCY                     
091500       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
091600     ELSE                                                                 
091700       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
091800       MOVE WS-PRKURS-MX2         TO R3-HEAD-EXCHANGE-RATE                
091900       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
092000       IF IN-FIL-IDPGM = 'W4183300'                                       
092100         IF IN-EKH-DAAVIDAT > ZERO                                        
092200           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
092300           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
092400         ELSE                                                             
092500           MOVE WS-TIAA              TO WS-TIAA-CR                        
092600           MOVE WS-TIMM              TO WS-TIMM-CR                        
092700         END-IF                                                           
092800       ELSE                                                               
092900         MOVE WS-TIAA                TO WS-TIAA-CR                        
093000         MOVE WS-TIMM                TO WS-TIMM-CR                        
093100       END-IF                                                             
093200       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
093300       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
093400       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
093500       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
093600       IF CURR-KDSVAR = ' '                                               
093700         IF IN-EKH-IDDISTR > ZERO                                         
093800           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
093900         ELSE                                                             
094000           IF WS-PRKURS = ZERO                                            
094100             MOVE 1             TO WS-PRKURS                              
094200           END-IF                                                         
094300         END-IF                                                           
094400       ELSE                                                               
094500         MOVE 1                 TO WS-PRKURS                              
094600       END-IF                                                             
094700       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
094800                                       CURR-REVALUTA-TO                   
094900       END-COMPUTE                                                        
095000       IF CURR-REVALUTA-TO = +1                                           
095100         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
095200       END-IF                                                             
095300       IF CURR-REVALUTA-TO = +10                                          
095400         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
095500       END-IF                                                             
095600       IF CURR-REVALUTA-TO = +100                                         
095700         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
095800       END-IF                                                             
095900     END-IF                                                               
096000     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
096100     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
096200     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
096300     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
096400     MOVE JA                      TO WS-HEADER-SW                         
096500     MOVE NEJ                     TO WS-LINE-SW                           
096600                                                                          
096700* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57053M                       
096800       PERFORM S004-WRITE-W57053M-HEAD                                    
096900     .                                                                    
097000     EJECT                                                                
097100                                                                          
097200 CD-BUILD-COMMON-610-PART SECTION.                                        
097300     MOVE SPACE               TO R3-LINE-R3                               
097400     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
097500                                 R3-LINE-DUE-DATE                         
097600                                 R3-LINE-AMOUNT                           
097700                                 R3-LINE-AMOUNT-LC                        
097800                                 R3-LINE-TAX-AMOUNT                       
097900                                 R3-LINE-TAX-AMOUNT-LC                    
098000                                 R3-LINE-NUMBER-OF-DAYS                   
098100                                 R3-LINE-QUANTITY                         
098200                                 R3-LINE-SAMNR                            
098300     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
098400     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
098500     MOVE 'MX10'              TO R3-LINE-COMPANY-CODE                     
098600     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
098700     IF SYST-KDPOST = '50'                                                
098800       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
098900     ELSE                                                                 
099000       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
099100     END-IF                                                               
099200     IF SYST-IDPRCTR NOT = SPACE                                          
099300       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
099400       IF WS-PRCTR-PRODSL = '??'                                          
099500         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
099600         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
099700       END-IF                                                             
099800       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
099900     END-IF                                                               
100000     .                                                                    
100100     EJECT                                                                
100200                                                                          
100300 CE-SCHEDULE-LINE-GL SECTION.                                             
100400     MOVE NEJ                     TO WS-HEADER-SW                         
100500     MOVE JA                      TO WS-LINE-SW                           
100600     EVALUATE IN-EKH-KDEKHHT                                              
100700     WHEN '102'                                                           
100800          PERFORM CEB-MAIN-EVENT-102                                      
100900     WHEN '103'                                                           
101000          PERFORM CEC-MAIN-EVENT-103                                      
101100     WHEN '201'                                                           
101200          PERFORM CED-MAIN-EVENT-201                                      
101300     WHEN '203'                                                           
101400          PERFORM CEF-MAIN-EVENT-203                                      
101500     WHEN '204'                                                           
101600          PERFORM CEG-MAIN-EVENT-204                                      
101700     WHEN '302'                                                           
101800          PERFORM CEI-MAIN-EVENT-302                                      
101900     WHEN '303'                                                           
102000          PERFORM CEJ-MAIN-EVENT-303                                      
102100     WHEN '401'                                                           
102200          PERFORM CEK-MAIN-EVENT-401                                      
102300     WHEN '402'                                                           
102400          PERFORM CEL-MAIN-EVENT-402                                      
102500     WHEN '403'                                                           
102600          PERFORM CEM-MAIN-EVENT-403                                      
102700     WHEN '404'                                                           
102800          PERFORM CEN-MAIN-EVENT-404                                      
102900     END-EVALUATE                                                         
103000     .                                                                    
103100     EJECT                                                                
103200                                                                          
103300 CEB-MAIN-EVENT-102 SECTION.                                              
103400     EVALUATE IN-EKH-KDEKSHT                                              
103500     WHEN '102'                                                           
103600          PERFORM CEBB-SUB-EVENT-102-102                                  
103700     WHEN '120'                                                           
103800          PERFORM CEBD-SUB-EVENT-102-120                                  
103900     WHEN '121'                                                           
104000          PERFORM CEBD-SUB-EVENT-102-121                                  
104100     WHEN '122'                                                           
104200          PERFORM CEBD-SUB-EVENT-102-122                                  
104300     WHEN '123'                                                           
104400          PERFORM CEBD-SUB-EVENT-102-123                                  
104500     WHEN '124'                                                           
104600          PERFORM CEBD-SUB-EVENT-102-124                                  
104700     WHEN '125'                                                           
104800          PERFORM CEBD-SUB-EVENT-102-125                                  
104900     WHEN '126'                                                           
105000          PERFORM CEBD-SUB-EVENT-102-126                                  
105100     WHEN '127'                                                           
105200          PERFORM CEBD-SUB-EVENT-102-127                                  
105300     WHEN '128'                                                           
105400          PERFORM CEBD-SUB-EVENT-102-128                                  
105500     WHEN '130'                                                           
105600          PERFORM CEBE-SUB-EVENT-102-130                                  
105700     WHEN '131'                                                           
105800          PERFORM CEBE-SUB-EVENT-102-131                                  
105900     WHEN '132'                                                           
106000          PERFORM CEBE-SUB-EVENT-102-132                                  
106100     WHEN '134'                                                           
106200          PERFORM CEBE-SUB-EVENT-102-134                                  
106300     END-EVALUATE                                                         
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700 CEBB-SUB-EVENT-102-102 SECTION.                                          
106800     EVALUATE IN-EKH-KDEKNIVA                                             
106900     WHEN 'DET'                                                           
107000       IF SYST-IDSEKVNR = 1                                               
107100         IF IN-EKH-KVANTAL > 0                                            
107200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
107300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
107400           COMPUTE R3-LINE-AMOUNT-LC =                                    
107500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
107600           IF IN-EKH-KDVALISO = 'MXN'                                     
107700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
107800           END-IF                                                         
107900           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
108000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
108100           MOVE SPACE               TO WS-ALLOCATE-REF                    
108200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
108300           PERFORM S02-WRITE-W57051M                                      
108400         END-IF                                                           
108500       END-IF                                                             
108600                                                                          
108700       IF SYST-IDSEKVNR = 2                                               
108800         IF IN-EKH-KVANTAL < 0                                            
108900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
109000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
109100           COMPUTE R3-LINE-AMOUNT-LC =                                    
109200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
109300           IF IN-EKH-KDVALISO = 'MXN'                                     
109400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
109500           END-IF                                                         
109600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
109700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
109800           MOVE SPACE               TO WS-ALLOCATE-REF                    
109900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
110000           PERFORM S02-WRITE-W57051M                                      
110100         END-IF                                                           
110200       END-IF                                                             
110300                                                                          
110400       IF SYST-IDSEKVNR = 3                                               
110500         IF IN-EKH-KVANTAL < 0                                            
110600           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
110700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
110800           COMPUTE R3-LINE-AMOUNT-LC =                                    
110900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
111000           IF IN-EKH-KDVALISO = 'MXN'                                     
111100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
111200           END-IF                                                         
111300           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
111310           MOVE SPACE               TO WS-ALLOCATE-DC                     
111320           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
111330           MOVE SPACE               TO WS-ALLOCATE-REF                    
111340           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
111400           PERFORM S02-WRITE-W57051M                                      
111500         END-IF                                                           
111600       END-IF                                                             
111700                                                                          
111800       IF SYST-IDSEKVNR = 4                                               
111900         IF IN-EKH-KVANTAL > 0                                            
112000           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
112100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
112200           COMPUTE R3-LINE-AMOUNT-LC =                                    
112300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
112400           IF IN-EKH-KDVALISO = 'MXN'                                     
112500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
112600           END-IF                                                         
112700           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
112710           MOVE SPACE               TO WS-ALLOCATE-DC                     
112720           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
112730           MOVE SPACE               TO WS-ALLOCATE-REF                    
112740           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
112800           PERFORM S02-WRITE-W57051M                                      
112900         END-IF                                                           
113000       END-IF                                                             
113100                                                                          
113200     END-EVALUATE                                                         
113300     .                                                                    
113400     EJECT                                                                
113500                                                                          
113600 CEBD-SUB-EVENT-102-120 SECTION.                                          
113700     EVALUATE IN-EKH-KDEKNIVA                                             
113800     WHEN 'DET'                                                           
113900       IF SYST-IDSEKVNR = 1                                               
114000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
114100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
114200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
114300          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX * -1            
114400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
114500         PERFORM S03-WRITE-W57072                                         
114600       END-IF                                                             
114700                                                                          
114800     WHEN 'FÖRS'                                                          
114900     WHEN 'FRAKT'                                                         
115000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
115100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
115200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
115300               IN-EKH-SUBEL / WS-PRKURS-MX  * -1                          
115400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
115500       PERFORM S04-WRITE-W57053M                                          
115600                                                                          
115700     WHEN 'EMB'                                                           
115800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
115900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
116000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
116100               IN-EKH-SUBEL / WS-PRKURS-MX  * -1                          
116200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
116300       PERFORM S04-WRITE-W57053M                                          
116400                                                                          
116500     WHEN 'DDI'                                                           
116600       IF IN-EKH-SUBEL > ZERO                                             
116700         IF SYST-IDSEKVNR = 1                                             
116800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
116900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
117000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
117100                   IN-EKH-SUBEL                                           
117200           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
117300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
117400           PERFORM S04-WRITE-W57053M                                      
117500         END-IF                                                           
117600       ELSE                                                               
117700         IF SYST-IDSEKVNR = 2                                             
117800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
117900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
118000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
118100                   IN-EKH-SUBEL                                           
118200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
118300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
118400           PERFORM S04-WRITE-W57053M                                      
118500         END-IF                                                           
118600       END-IF                                                             
118700     END-EVALUATE                                                         
118800     .                                                                    
118900     EJECT                                                                
119000                                                                          
119100 CEBD-SUB-EVENT-102-121 SECTION.                                          
119200     EVALUATE IN-EKH-KDEKNIVA                                             
119300     WHEN 'DET'                                                           
119400       IF SYST-IDSEKVNR = 1                                               
119500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
119600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
119700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
119800             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-MX3            
119900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
120000         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-1                      
120100         PERFORM S03-WRITE-W57072                                         
120200       END-IF                                                             
120300                                                                          
120400       IF SYST-IDSEKVNR = 2                                               
120500         PERFORM S13-GET-LANDING-COST                                     
120600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
120700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
120800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
120900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
121000            (IN-EKH-KVANTAL *                                             
121100            IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                  
121200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
121300         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-2                      
121400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
121500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
121600         MOVE SPACE               TO WS-ALLOCATE-REF                      
121700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
121800         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
121900         PERFORM S03-WRITE-W57072                                         
122000       END-IF                                                             
122100                                                                          
122200       IF SYST-IDSEKVNR = 3                                               
122300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
122400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
122500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
122600            WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
122700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
122800         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
122900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
123000         MOVE SPACE               TO WS-ALLOCATE-REF                      
123100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
123200         PERFORM S03-WRITE-W57072                                         
123300       END-IF                                                             
123400                                                                          
123500     WHEN 'FÖRS'                                                          
123600     WHEN 'FRAKT'                                                         
123700       IF SYST-IDSEKVNR = 1                                               
123800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
123900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
124000         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
124100                 IN-EKH-SUBEL / WS-PRKURS-MX3                             
124200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
124300         PERFORM S04-WRITE-W57053M                                        
124400       END-IF                                                             
124500                                                                          
124600       IF SYST-IDSEKVNR = 2                                               
124700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
124800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
124900         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
125000                 IN-EKH-SUBEL / WS-PRKURS-MX3                             
125100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
125200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
125300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
125400         MOVE SPACE               TO WS-ALLOCATE-REF                      
125500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
125600         PERFORM S04-WRITE-W57053M                                        
125700       END-IF                                                             
125800                                                                          
125900     WHEN 'EMB'                                                           
126000       IF SYST-IDSEKVNR = 1                                               
126100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
126200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
126300         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
126400                 IN-EKH-SUBEL / WS-PRKURS-MX3                             
126500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
126600         PERFORM S04-WRITE-W57053M                                        
126700       END-IF                                                             
126800                                                                          
126900       IF SYST-IDSEKVNR = 2                                               
127000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
127100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
127200         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
127300                 IN-EKH-SUBEL / WS-PRKURS-MX3                             
127400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
127500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
127600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
127700         MOVE SPACE               TO WS-ALLOCATE-REF                      
127800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
127900         PERFORM S04-WRITE-W57053M                                        
128000       END-IF                                                             
128100     END-EVALUATE                                                         
128200                                                                          
128300     .                                                                    
128400     EJECT                                                                
128500                                                                          
128600 CEBD-SUB-EVENT-102-122 SECTION.                                          
128700     EVALUATE IN-EKH-KDEKNIVA                                             
128800     WHEN 'DET'                                                           
128900       IF IN-EKH-KVANTAL > 0                                              
129000         IF SYST-IDSEKVNR = 1                                             
129100           PERFORM S13-GET-LANDING-COST                                   
129200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
129300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
129400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
129500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
129600            (IN-EKH-KVANTAL *                                             
129700             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
129800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
129900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
130000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
130100           MOVE SPACE               TO WS-ALLOCATE-REF                    
130200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
130300           PERFORM S02-WRITE-W57051M                                      
130400         END-IF                                                           
130500                                                                          
130600         IF SYST-IDSEKVNR = 4                                             
130700           PERFORM S13-GET-LANDING-COST                                   
130800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
130900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
131000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
131100            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
131200            (IN-EKH-KVANTAL *                                             
131300             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
131400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
131500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
131600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
131700           MOVE SPACE               TO WS-ALLOCATE-REF                    
131800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
131900           MOVE SPACE               TO R3-LINE-COST-CENTER                
132000           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
132100           PERFORM S02-WRITE-W57051M                                      
132200         END-IF                                                           
132300       END-IF                                                             
132400                                                                          
132500                                                                          
132600       IF IN-EKH-KVANTAL < 0                                              
132700         IF SYST-IDSEKVNR = 2                                             
132800           PERFORM S13-GET-LANDING-COST                                   
132900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
133000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
133100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
133200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
133300            (IN-EKH-KVANTAL *                                             
133400             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
133500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
133600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
133700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
133800           MOVE SPACE               TO WS-ALLOCATE-REF                    
133900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
134000           MOVE SPACE             TO R3-LINE-COST-CENTER                  
134100           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
134200           PERFORM S02-WRITE-W57051M                                      
134300         END-IF                                                           
134400                                                                          
134500         IF SYST-IDSEKVNR = 3                                             
134600           PERFORM S13-GET-LANDING-COST                                   
134700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
134800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
134900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
135000            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
135100            (IN-EKH-KVANTAL *                                             
135200             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
135300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
135400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
135500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
135600           MOVE SPACE               TO WS-ALLOCATE-REF                    
135700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
135800           PERFORM S02-WRITE-W57051M                                      
135900         END-IF                                                           
136000       END-IF                                                             
136100                                                                          
136200     END-EVALUATE                                                         
136300     .                                                                    
136400     EJECT                                                                
136500                                                                          
136600 CEBD-SUB-EVENT-102-123 SECTION.                                          
136700     EVALUATE IN-EKH-KDEKNIVA                                             
136800     WHEN 'DET'                                                           
136900       IF SYST-IDSEKVNR = 1                                               
137000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
137100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
137200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
137300              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
137400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
137500         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
137600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
137700         MOVE SPACE               TO WS-ALLOCATE-REF                      
137800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
137900         PERFORM S02-WRITE-W57051M                                        
138000       END-IF                                                             
138100                                                                          
138200       IF SYST-IDSEKVNR = 2                                               
138300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
138400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
138500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
138600             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
138700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
138800         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
138900         PERFORM S02-WRITE-W57051M                                        
139000       END-IF                                                             
139100     END-EVALUATE                                                         
139200     .                                                                    
139300     EJECT                                                                
139400                                                                          
139500 CEBD-SUB-EVENT-102-124 SECTION.                                          
139600     EVALUATE IN-EKH-KDEKNIVA                                             
139700                                                                          
139800     WHEN 'DET'                                                           
139900       IF SYST-IDSEKVNR = 1                                               
139910         PERFORM S13-GET-LANDING-COST                                     
140000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
140100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
140200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
140300         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX   * -1           
140400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
140500                                   WS-102-124-DET-1                       
140600         MOVE SPACE               TO WS-ALLOCATE-DC                       
140700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
140800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
140900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
141000         PERFORM S03-WRITE-W57072                                         
141100*** CALC THE VALUE WITH THE LANDED COST                                   
141200         COMPUTE WS-102-124-DET-2 = WS-102-124-DET-1 * WS-MARKUP          
141300       END-IF                                                             
141400                                                                          
141500       IF SYST-IDSEKVNR = 2                                               
141600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
141700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
141800         MOVE WS-102-124-DET-2    TO R3-LINE-AMOUNT-LC                    
141900         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
142000         MOVE SPACE               TO WS-ALLOCATE-DC                       
142100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
142200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
142300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
142400         PERFORM S03-WRITE-W57072                                         
142500       END-IF                                                             
142600                                                                          
142700       IF SYST-IDSEKVNR = 3                                               
142800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
142900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
143000         MOVE WS-102-124-DET-2    TO R3-LINE-AMOUNT-LC                    
143100         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
143200         MOVE SPACE               TO WS-ALLOCATE-DC                       
143300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
143400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
143500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
143600         PERFORM S03-WRITE-W57072                                         
143700         MOVE ZERO              TO WS-102-124-DET-2                       
143800       END-IF                                                             
143900                                                                          
144000     WHEN 'FÖRS'                                                          
144100     WHEN 'FRAKT'                                                         
144200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
144300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
144400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
144500               IN-EKH-SUBEL * -1                                          
144600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
144700       MOVE SPACE               TO WS-ALLOCATE-DC                         
144800       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
144900       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
145000       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
145100       PERFORM S04-WRITE-W57053M                                          
145200                                                                          
145300     WHEN 'EMB'                                                           
145400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
145500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
145600       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
145700               (IN-EKH-SUBEL / WS-PRKURS-MX) * -1                         
145800       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
145900       MOVE SPACE               TO WS-ALLOCATE-DC                         
146000       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
146100       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
146200       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
146300       PERFORM S04-WRITE-W57053M                                          
146400                                                                          
146500     WHEN 'DDI'                                                           
146600       IF IN-EKH-SUBEL > ZERO                                             
146700         IF SYST-IDSEKVNR = 1                                             
146800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
146900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
147000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
147100                   IN-EKH-SUBEL                                           
147200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
147300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
147400           MOVE SPACE               TO WS-ALLOCATE-DC                     
147500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
147600           MOVE SPACE               TO WS-ALLOCATE-REF                    
147700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
147800           PERFORM S04-WRITE-W57053M                                      
147900         END-IF                                                           
148000       ELSE                                                               
148100         IF SYST-IDSEKVNR = 2                                             
148200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
148300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
148400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
148500                   IN-EKH-SUBEL                                           
148600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
148700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
148800           MOVE SPACE               TO WS-ALLOCATE-DC                     
148900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
149000           MOVE SPACE               TO WS-ALLOCATE-REF                    
149100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
149200           PERFORM S04-WRITE-W57053M                                      
149300         END-IF                                                           
149400       END-IF                                                             
149500     END-EVALUATE                                                         
149600     .                                                                    
149700     EJECT                                                                
149800                                                                          
149900 CEBD-SUB-EVENT-102-125 SECTION.                                          
150000     EVALUATE IN-EKH-KDEKNIVA                                             
150100     WHEN 'DET'                                                           
150200       IF SYST-IDSEKVNR = 1                                               
150300         PERFORM S13-GET-LANDING-COST                                     
150400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
150500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
150600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
150700         (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX) +              
150800         (IN-EKH-KVANTAL *                                                
150900          IN-EKH-PRARTNTO / WS-PRKURS-MX * WS-MARKUP)                     
151000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
151100         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
151200         PERFORM S03-WRITE-W57072                                         
151300       END-IF                                                             
151400                                                                          
151500       IF SYST-IDSEKVNR = 2                                               
151600         PERFORM S13-GET-LANDING-COST                                     
151700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
151800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
151900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
152000            (IN-EKH-KVANTAL *                                             
152100             IN-EKH-PRARTNTO / WS-PRKURS-MX * WS-MARKUP)                  
152200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
152300         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
152400         PERFORM S03-WRITE-W57072                                         
152500       END-IF                                                             
152600                                                                          
152700     WHEN 'FÖRS'                                                          
152800     WHEN 'FRAKT'                                                         
152900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
153000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
153100       MOVE SPACE             TO R3-LINE-COST-CENTER                      
153200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
153300          IN-EKH-SUBEL / WS-PRKURS-MX  * -1                               
153400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
153500       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
153600       PERFORM S04-WRITE-W57053M                                          
153700                                                                          
153800     WHEN 'EMB'                                                           
153900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
154000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
154100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
154200          IN-EKH-SUBEL / WS-PRKURS-MX  * -1                               
154300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
154400       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
154500       PERFORM S04-WRITE-W57053M                                          
154600                                                                          
154700     WHEN 'DDI'                                                           
154800       IF SPAR-SUMMA-102-125 < ZERO                                       
154900         IF SYST-IDSEKVNR = 1                                             
155000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
155100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
155200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
155300                   SPAR-SUMMA-102-125                                     
155400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
155500           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
155510           MOVE SPACE               TO WS-ALLOCATE-DC                     
155520           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
155530           MOVE SPACE               TO WS-ALLOCATE-REF                    
155540           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
155600           PERFORM S04-WRITE-W57053M                                      
155700         END-IF                                                           
155800       END-IF                                                             
155900       IF SPAR-SUMMA-102-125 > ZERO                                       
156000         IF SYST-IDSEKVNR = 2                                             
156100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
156200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
156300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
156400                   SPAR-SUMMA-102-125                                     
156500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
156600           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
156610           MOVE SPACE               TO WS-ALLOCATE-DC                     
156620           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
156630           MOVE SPACE               TO WS-ALLOCATE-REF                    
156640           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
156700           PERFORM S04-WRITE-W57053M                                      
156800         END-IF                                                           
156900       END-IF                                                             
157000                                                                          
157100     END-EVALUATE                                                         
157200     .                                                                    
157300     EJECT                                                                
157400                                                                          
157500 CEBD-SUB-EVENT-102-126 SECTION.                                          
157600     EVALUATE IN-EKH-KDEKNIVA                                             
157700     WHEN 'DET'                                                           
157800         IF SYST-IDSEKVNR = 1                                             
157810           PERFORM S13-GET-LANDING-COST                                   
157900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
158000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
158100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
158200            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-MX3))          
158300            + (IN-EKH-KVANTAL *                                           
158400            (IN-EKH-PRARTNTO / WS-PRKURS-MX3) * WS-MARKUP)                
158500           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
158600           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-126-1               
158700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
158800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
158900           MOVE SPACE               TO WS-ALLOCATE-REF                    
159000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
159100           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
159200           PERFORM S02-WRITE-W57051M                                      
159300         END-IF                                                           
159400                                                                          
159500         IF SYST-IDSEKVNR = 2                                             
159600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
159700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
159800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
159900            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-MX3))          
160000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
160100           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-126-2               
160200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
160300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
160400           MOVE SPACE               TO WS-ALLOCATE-REF                    
160500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
160600           MOVE SPACE               TO R3-LINE-COST-CENTER                
160700           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
160800           PERFORM S02-WRITE-W57051M                                      
160900         END-IF                                                           
161000                                                                          
161100         IF SYST-IDSEKVNR = 3                                             
161200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
161300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
161400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
161500              WS-LINE-AMOUNT-126-1 - WS-LINE-AMOUNT-126-2                 
161600           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
161700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
161800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
161900           MOVE SPACE               TO WS-ALLOCATE-REF                    
162000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
162100           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
162200           PERFORM S02-WRITE-W57051M                                      
162300         END-IF                                                           
162400                                                                          
162500     END-EVALUATE                                                         
162600     .                                                                    
162700     EJECT                                                                
162800                                                                          
162900 CEBD-SUB-EVENT-102-127 SECTION.                                          
163000     EVALUATE IN-EKH-KDEKNIVA                                             
163100     WHEN 'DET'                                                           
163200         IF SYST-IDSEKVNR = 1                                             
163210           PERFORM S13-GET-LANDING-COST                                   
163300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
163400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
163500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
163600            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-MX3))          
163700            + (IN-EKH-KVANTAL *                                           
163800            (IN-EKH-PRARTNTO / WS-PRKURS-MX3) * WS-MARKUP)                
163900           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
164000           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-127-1               
164100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
164200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
164300           MOVE SPACE               TO WS-ALLOCATE-REF                    
164400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
164500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
164600           PERFORM S02-WRITE-W57051M                                      
164700         END-IF                                                           
164800                                                                          
164900         IF SYST-IDSEKVNR = 2                                             
165000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
165100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
165200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
165300            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-MX3))          
165400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
165500           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-127-2               
165600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
165700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
165800           MOVE SPACE               TO WS-ALLOCATE-REF                    
165900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
166000           MOVE SPACE               TO R3-LINE-COST-CENTER                
166100           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
166200           PERFORM S02-WRITE-W57051M                                      
166300         END-IF                                                           
166400                                                                          
166500         IF SYST-IDSEKVNR = 3                                             
166600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
166700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
166800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
166900              WS-LINE-AMOUNT-127-1 - WS-LINE-AMOUNT-127-2                 
167000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
167100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
167200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
167300           MOVE SPACE               TO WS-ALLOCATE-REF                    
167400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
167500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
167600           PERFORM S02-WRITE-W57051M                                      
167700         END-IF                                                           
167800                                                                          
167900     END-EVALUATE                                                         
168000     .                                                                    
168100     EJECT                                                                
168200                                                                          
168300 CEBD-SUB-EVENT-102-128 SECTION.                                          
168400     EVALUATE IN-EKH-KDEKNIVA                                             
168500     WHEN 'DET'                                                           
168600         IF SYST-IDSEKVNR = 1                                             
168610           PERFORM S13-GET-LANDING-COST                                   
168700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
168800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
168900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
169000            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-MX3))          
169100            + (IN-EKH-KVANTAL *                                           
169200            (IN-EKH-PRARTNTO / WS-PRKURS-MX3) * WS-MARKUP)                
169300           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
169400           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-128-1               
169500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
169600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
169700           MOVE SPACE               TO WS-ALLOCATE-REF                    
169800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
169900           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
170000           PERFORM S02-WRITE-W57051M                                      
170100         END-IF                                                           
170200                                                                          
170300         IF SYST-IDSEKVNR = 2                                             
170400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
170500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
170600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
170700            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-MX3))          
170800           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
170900           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-128-2               
171000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
171100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
171200           MOVE SPACE               TO WS-ALLOCATE-REF                    
171300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
171400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
171500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
171600           PERFORM S02-WRITE-W57051M                                      
171700         END-IF                                                           
171800                                                                          
171900         IF SYST-IDSEKVNR = 3                                             
172000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
172100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
172200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
172300              WS-LINE-AMOUNT-128-1 - WS-LINE-AMOUNT-128-2                 
172400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
172500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
172600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
172700           MOVE SPACE               TO WS-ALLOCATE-REF                    
172800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
172810           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
172900           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
173000           PERFORM S02-WRITE-W57051M                                      
173100         END-IF                                                           
173200                                                                          
173300     END-EVALUATE                                                         
173400     .                                                                    
173500     EJECT                                                                
173600                                                                          
173700 CEBE-SUB-EVENT-102-130 SECTION.                                          
173800     EVALUATE IN-EKH-KDEKNIVA                                             
173900     WHEN 'DET'                                                           
174000       IF SYST-IDSEKVNR = 1                                               
174100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
174200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
174300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
174400          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX * -1            
174500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
174600         PERFORM S03-WRITE-W57072                                         
174700       END-IF                                                             
174800                                                                          
174900     WHEN 'EMB'                                                           
175000     WHEN 'FÖRS'                                                          
175100     WHEN 'FRAKT'                                                         
175200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
175300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
175400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
175500               IN-EKH-SUBEL / WS-PRKURS-MX  * -1                          
175600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
175700       PERFORM S04-WRITE-W57053M                                          
175800                                                                          
175900     WHEN 'DDI'                                                           
176000       IF IN-EKH-SUBEL > ZERO                                             
176100         IF SYST-IDSEKVNR = 1                                             
176200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
176300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
176400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
176500                   IN-EKH-SUBEL                                           
176600           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
176700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
176710           MOVE SPACE               TO WS-ALLOCATE-DC                     
176720           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
176730           MOVE SPACE               TO WS-ALLOCATE-REF                    
176740           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
176800           PERFORM S04-WRITE-W57053M                                      
176900         END-IF                                                           
177000       ELSE                                                               
177100         IF SYST-IDSEKVNR = 2                                             
177200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
177300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
177400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
177500                   IN-EKH-SUBEL                                           
177600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
177700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
177710           MOVE SPACE               TO WS-ALLOCATE-DC                     
177720           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
177730           MOVE SPACE               TO WS-ALLOCATE-REF                    
177740           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
177800           PERFORM S04-WRITE-W57053M                                      
177900         END-IF                                                           
178000       END-IF                                                             
178100     END-EVALUATE                                                         
178200     .                                                                    
178300     EJECT                                                                
178400                                                                          
178500 CEBE-SUB-EVENT-102-131 SECTION.                                          
178600     EVALUATE IN-EKH-KDEKNIVA                                             
178700     WHEN 'DET'                                                           
178800       IF SYST-IDSEKVNR = 1                                               
178900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
179000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
179100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
179200             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-MX3            
179300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
179400         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-131-1                      
179500         PERFORM S03-WRITE-W57072                                         
179600       END-IF                                                             
179700                                                                          
179800       IF SYST-IDSEKVNR = 2                                               
179900         PERFORM S13-GET-LANDING-COST                                     
180000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
180100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
180200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
180300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
180400            (IN-EKH-KVANTAL *                                             
180500            IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                  
180600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
180700         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-131-2                      
180800         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
180900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
181000         MOVE SPACE               TO WS-ALLOCATE-REF                      
181100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
181200         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
181300         PERFORM S03-WRITE-W57072                                         
181400       END-IF                                                             
181500                                                                          
181600       IF SYST-IDSEKVNR = 3                                               
181700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
181800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
181900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
182000            WS-LINE-AMOUNT-131-2 - WS-LINE-AMOUNT-131-1                   
182100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
182200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
182300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
182400         MOVE SPACE               TO WS-ALLOCATE-REF                      
182500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
182600         PERFORM S03-WRITE-W57072                                         
182700       END-IF                                                             
182800                                                                          
182900     WHEN 'EMB'                                                           
183000     WHEN 'FÖRS'                                                          
183100     WHEN 'FRAKT'                                                         
183200       IF SYST-IDSEKVNR = 1                                               
183300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
183400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
183500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
183600                 IN-EKH-SUBEL / WS-PRKURS-MX3                             
183700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
183800         PERFORM S04-WRITE-W57053M                                        
183900       END-IF                                                             
184000                                                                          
184100       IF SYST-IDSEKVNR = 2                                               
184200         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
184300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
184400         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
184500                 IN-EKH-SUBEL / WS-PRKURS-MX3                             
184600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
184700         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
184800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
184900         MOVE SPACE               TO WS-ALLOCATE-REF                      
185000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
185100         PERFORM S04-WRITE-W57053M                                        
185200       END-IF                                                             
185300                                                                          
185400     END-EVALUATE                                                         
185500     .                                                                    
185600     EJECT                                                                
185700                                                                          
185800 CEBE-SUB-EVENT-102-132 SECTION.                                          
185900     EVALUATE IN-EKH-KDEKNIVA                                             
186000     WHEN 'DET'                                                           
186100       IF IN-EKH-KVANTAL > 0                                              
186200         IF SYST-IDSEKVNR = 1                                             
186300           PERFORM S13-GET-LANDING-COST                                   
186400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
186500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
186600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
186700            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
186800            (IN-EKH-KVANTAL *                                             
186900             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
187000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
187100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
187200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
187300           MOVE SPACE               TO WS-ALLOCATE-REF                    
187400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
187500           PERFORM S02-WRITE-W57051M                                      
187600         END-IF                                                           
187700                                                                          
187800         IF SYST-IDSEKVNR = 4                                             
187900           PERFORM S13-GET-LANDING-COST                                   
188000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
188100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
188200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
188300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
188400            (IN-EKH-KVANTAL *                                             
188500             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
188600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
188700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
188800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
188900           MOVE SPACE               TO WS-ALLOCATE-REF                    
189000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
189100           MOVE SPACE               TO R3-LINE-COST-CENTER                
189200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
189300           PERFORM S02-WRITE-W57051M                                      
189400         END-IF                                                           
189500       END-IF                                                             
189600                                                                          
189700                                                                          
189800       IF IN-EKH-KVANTAL < 0                                              
189900         IF SYST-IDSEKVNR = 2                                             
190000           PERFORM S13-GET-LANDING-COST                                   
190100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
190200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
190300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
190400            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
190500            (IN-EKH-KVANTAL *                                             
190600             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
190700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
190800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
190900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
191000           MOVE SPACE               TO WS-ALLOCATE-REF                    
191100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
191200           MOVE SPACE             TO R3-LINE-COST-CENTER                  
191300           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
191400           PERFORM S02-WRITE-W57051M                                      
191500         END-IF                                                           
191600                                                                          
191700         IF SYST-IDSEKVNR = 3                                             
191800           PERFORM S13-GET-LANDING-COST                                   
191900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
192000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
192100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
192200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3) +          
192300            (IN-EKH-KVANTAL *                                             
192400             IN-EKH-PRARTNTO / WS-PRKURS-MX3 * WS-MARKUP)                 
192500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
192600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
192700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
192800           MOVE SPACE               TO WS-ALLOCATE-REF                    
192900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
193000           PERFORM S02-WRITE-W57051M                                      
193100         END-IF                                                           
193200       END-IF                                                             
193300                                                                          
193400     END-EVALUATE                                                         
193500     .                                                                    
193600     EJECT                                                                
193700                                                                          
193800 CEBE-SUB-EVENT-102-134 SECTION.                                          
193900     EVALUATE IN-EKH-KDEKNIVA                                             
194000     WHEN 'DET'                                                           
194100       IF SYST-IDSEKVNR = 1                                               
194110         PERFORM S13-GET-LANDING-COST                                     
194200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
194300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
194400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
194500         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX   * -1           
194600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
194700                                   WS-102-134-DET-1                       
194800         MOVE SPACE               TO WS-ALLOCATE-DC                       
194900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
195000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
195100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
195200         PERFORM S03-WRITE-W57072                                         
195300*** CALC THE VALUE WITH THE LANDED COST                                   
195400         COMPUTE WS-102-134-DET-2 = WS-102-134-DET-1 * WS-MARKUP          
195500       END-IF                                                             
195600                                                                          
195700       IF SYST-IDSEKVNR = 2                                               
195800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
195900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
196000         MOVE WS-102-134-DET-2    TO R3-LINE-AMOUNT-LC                    
196100         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
196200         MOVE SPACE               TO WS-ALLOCATE-DC                       
196300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
196400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
196500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
196600         PERFORM S03-WRITE-W57072                                         
196700       END-IF                                                             
196800                                                                          
196900       IF SYST-IDSEKVNR = 3                                               
197000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
197100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
197200         MOVE WS-102-134-DET-2    TO R3-LINE-AMOUNT-LC                    
197300         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
197400         MOVE SPACE               TO WS-ALLOCATE-DC                       
197500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
197600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
197700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
197800         PERFORM S03-WRITE-W57072                                         
197900         MOVE ZERO              TO WS-102-134-DET-2                       
198000       END-IF                                                             
198100                                                                          
198200     WHEN 'FÖRS'                                                          
198300     WHEN 'FRAKT'                                                         
198400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
198500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
198600       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
198700               IN-EKH-SUBEL * -1                                          
198800       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
198900       MOVE SPACE               TO WS-ALLOCATE-DC                         
199000       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
199100       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
199200       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
199300       PERFORM S04-WRITE-W57053M                                          
199400                                                                          
199500     WHEN 'EMB'                                                           
199600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
199700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
199800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
199900               (IN-EKH-SUBEL / WS-PRKURS-MX) * -1                         
200000       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
200100       MOVE SPACE               TO WS-ALLOCATE-DC                         
200200       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
200300       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
200400       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
200500       PERFORM S04-WRITE-W57053M                                          
200600                                                                          
200700     WHEN 'DDI'                                                           
200800       IF IN-EKH-SUBEL > ZERO                                             
200900         IF SYST-IDSEKVNR = 1                                             
201000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
201100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
201200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
201300                   IN-EKH-SUBEL                                           
201400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
201500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
201600           MOVE SPACE               TO WS-ALLOCATE-DC                     
201700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
201800           MOVE SPACE               TO WS-ALLOCATE-REF                    
201900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
202000           PERFORM S04-WRITE-W57053M                                      
202100         END-IF                                                           
202200       ELSE                                                               
202300         IF SYST-IDSEKVNR = 2                                             
202400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
202500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
202600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
202700                   IN-EKH-SUBEL                                           
202800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
202900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
203000           MOVE SPACE               TO WS-ALLOCATE-DC                     
203100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
203200           MOVE SPACE               TO WS-ALLOCATE-REF                    
203300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
203400           PERFORM S04-WRITE-W57053M                                      
203500         END-IF                                                           
203600       END-IF                                                             
203700     END-EVALUATE                                                         
203800     .                                                                    
203900     EJECT                                                                
204000 CEC-MAIN-EVENT-103 SECTION.                                              
204100     EVALUATE IN-EKH-KDEKSHT                                              
204200     WHEN '102'                                                           
204300          PERFORM CECB-SUB-EVENT-103-102                                  
204400     WHEN '106'                                                           
204500          PERFORM CECB-SUB-EVENT-103-106                                  
204600     WHEN '107'                                                           
204700          PERFORM CECB-SUB-EVENT-103-107                                  
204800     END-EVALUATE                                                         
204900     .                                                                    
205000     EJECT                                                                
205100                                                                          
205200 CECB-SUB-EVENT-103-102 SECTION.                                          
205300     EVALUATE IN-EKH-KDEKNIVA                                             
205400     WHEN 'DET'                                                           
205500       IF SYST-IDSEKVNR = 1                                               
205600         IF IN-EKH-KVANTAL < 0                                            
205700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
205800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
205900           COMPUTE R3-LINE-AMOUNT-LC =                                    
206000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
206100           IF IN-EKH-KDVALISO = 'MXN'                                     
206200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
206300           END-IF                                                         
206400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
206500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
206600           MOVE SPACE               TO WS-ALLOCATE-REF                    
206700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
206800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
206900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
207000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
207100           PERFORM S04-WRITE-W57053M                                      
207200         END-IF                                                           
207300       END-IF                                                             
207400                                                                          
207500       IF SYST-IDSEKVNR = 2                                               
207600         IF IN-EKH-KVANTAL > 0                                            
207700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
207800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
207900           COMPUTE R3-LINE-AMOUNT-LC =                                    
208000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
208100           IF IN-EKH-KDVALISO = 'MXN'                                     
208200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
208300           END-IF                                                         
208400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
208500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
208600           MOVE SPACE               TO WS-ALLOCATE-REF                    
208700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
208800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
208900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
209000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
209100           PERFORM S04-WRITE-W57053M                                      
209200         END-IF                                                           
209300       END-IF                                                             
216600                                                                          
216700     WHEN 'DDI'                                                           
216800       IF IN-EKH-SUBEL > ZERO                                             
216900         IF SYST-IDSEKVNR = 1                                             
217000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
217100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
217200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
217300                   IN-EKH-SUBEL                                           
217400           MOVE ZEROES              TO R3-LINE-AMOUNT                     
217500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
217600           MOVE SPACE               TO WS-ALLOCATE-DC                     
217700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
217800           MOVE SPACE               TO WS-ALLOCATE-REF                    
217900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
218000           PERFORM S04-WRITE-W57053M                                      
218100         END-IF                                                           
218200       ELSE                                                               
218300         IF SYST-IDSEKVNR = 2                                             
218400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
218500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
218600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
218700                   IN-EKH-SUBEL                                           
218800           MOVE ZEROES              TO R3-LINE-AMOUNT                     
218900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
219000           MOVE SPACE               TO WS-ALLOCATE-DC                     
219100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
219200           MOVE SPACE               TO WS-ALLOCATE-REF                    
219300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
219400           PERFORM S04-WRITE-W57053M                                      
219500         END-IF                                                           
219600       END-IF                                                             
219700     END-EVALUATE                                                         
219800     .                                                                    
219900     EJECT                                                                
220000                                                                          
220100 CECB-SUB-EVENT-103-106 SECTION.                                          
220200     EVALUATE IN-EKH-KDEKNIVA                                             
220300     WHEN 'DET'                                                           
220400       IF SYST-IDSEKVNR = 1                                               
220500         IF IN-EKH-KVANTAL < 0                                            
220600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
220700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
220800           COMPUTE R3-LINE-AMOUNT-LC =                                    
220900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
221000           IF IN-EKH-KDVALISO = 'MXN'                                     
221100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
221200           END-IF                                                         
221300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
221400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
221500           MOVE SPACE               TO WS-ALLOCATE-REF                    
221600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
221700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
221800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
221900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
222000           PERFORM S04-WRITE-W57053M                                      
222100         END-IF                                                           
222200       END-IF                                                             
222300                                                                          
231600     WHEN 'DDI'                                                           
231700       IF IN-EKH-SUBEL < ZERO                                             
231800         IF SYST-IDSEKVNR = 1                                             
231900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
232000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
232100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
232200                   IN-EKH-SUBEL                                           
232300           MOVE ZEROES              TO R3-LINE-AMOUNT                     
232400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
232500           MOVE SPACE               TO WS-ALLOCATE-DC                     
232600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
232700           MOVE SPACE               TO WS-ALLOCATE-REF                    
232800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
232900           PERFORM S04-WRITE-W57053M                                      
233000         END-IF                                                           
233100       ELSE                                                               
233200         IF SYST-IDSEKVNR = 2                                             
233300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
233400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
233500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
233600                   IN-EKH-SUBEL                                           
233700           MOVE ZEROES              TO R3-LINE-AMOUNT                     
233800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
233900           MOVE SPACE               TO WS-ALLOCATE-DC                     
234000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
234100           MOVE SPACE               TO WS-ALLOCATE-REF                    
234200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
234300           PERFORM S04-WRITE-W57053M                                      
234400         END-IF                                                           
234500       END-IF                                                             
234600                                                                          
234700     END-EVALUATE                                                         
234800     .                                                                    
234900     EJECT                                                                
235000                                                                          
235100 CECB-SUB-EVENT-103-107 SECTION.                                          
235200     EVALUATE IN-EKH-KDEKNIVA                                             
235300     WHEN 'DET'                                                           
235400       IF SYST-IDSEKVNR = 1                                               
235500         IF IN-EKH-KVANTAL > 0                                            
235600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
235700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
235800           COMPUTE R3-LINE-AMOUNT-LC =                                    
235900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
236000           IF IN-EKH-KDVALISO = 'MXN'                                     
236100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
236200           END-IF                                                         
236300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
236400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
236500           MOVE SPACE               TO WS-ALLOCATE-REF                    
236600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
236700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
236800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
236900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
237000           PERFORM S04-WRITE-W57053M                                      
237100         END-IF                                                           
237200       END-IF                                                             
237300                                                                          
246600     WHEN 'DDI'                                                           
246700       IF IN-EKH-SUBEL < ZERO                                             
246800         IF SYST-IDSEKVNR = 1                                             
246900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
247000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
247100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
247200                   IN-EKH-SUBEL                                           
247300           MOVE ZEROES              TO R3-LINE-AMOUNT                     
247400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
247500           MOVE SPACE               TO WS-ALLOCATE-DC                     
247600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
247700           MOVE SPACE               TO WS-ALLOCATE-REF                    
247800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
247900           PERFORM S04-WRITE-W57053M                                      
248000         END-IF                                                           
248100       ELSE                                                               
248200         IF SYST-IDSEKVNR = 2                                             
248300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
248400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
248500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
248600                   IN-EKH-SUBEL                                           
248700           MOVE ZEROES              TO R3-LINE-AMOUNT                     
248800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
248900           MOVE SPACE               TO WS-ALLOCATE-DC                     
249000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
249100           MOVE SPACE               TO WS-ALLOCATE-REF                    
249200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
249300           PERFORM S04-WRITE-W57053M                                      
249400         END-IF                                                           
249500       END-IF                                                             
249600                                                                          
249700     END-EVALUATE                                                         
249800     .                                                                    
249900     EJECT                                                                
250000                                                                          
250100 CED-MAIN-EVENT-201 SECTION.                                              
250200     EVALUATE IN-EKH-KDEKSHT                                              
250300     WHEN '201'                                                           
250400          PERFORM CEDA-SUB-EVENT-201-201                                  
250500     END-EVALUATE                                                         
250600     .                                                                    
250700     EJECT                                                                
250800                                                                          
250900 CEDA-SUB-EVENT-201-201 SECTION.                                          
251000     EVALUATE IN-EKH-KDEKNIVA                                             
251100     WHEN 'DET'                                                           
251200       IF SYST-IDSEKVNR = 1                                               
251300         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
251400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
251500         COMPUTE R3-LINE-AMOUNT-LC =                                      
251600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
251700         IF IN-EKH-KDVALISO = 'MXN'                                       
251800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
251900         END-IF                                                           
252000         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
252100         MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                  
252300         PERFORM S03-WRITE-W57072                                         
252400       END-IF                                                             
252500                                                                          
252600       IF SYST-IDSEKVNR = 2                                               
252700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
252800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
252900         COMPUTE R3-LINE-AMOUNT-LC =                                      
253000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
253100         IF IN-EKH-KDVALISO = 'MXN'                                       
253200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
253300         END-IF                                                           
253400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
253500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
253600         MOVE SPACE               TO WS-ALLOCATE-REF                      
253700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
253800         MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                  
254000         PERFORM S03-WRITE-W57072                                         
254100       END-IF                                                             
254200     END-EVALUATE                                                         
254300     .                                                                    
254400     EJECT                                                                
254500                                                                          
254600 CEF-MAIN-EVENT-203 SECTION.                                              
254700     EVALUATE IN-EKH-KDEKSHT                                              
254800     WHEN '201'                                                           
254900          PERFORM CEFA-SUB-EVENT-203-201                                  
255000     END-EVALUATE                                                         
255100     .                                                                    
255200     EJECT                                                                
255300                                                                          
255400 CEFA-SUB-EVENT-203-201 SECTION.                                          
255500     EVALUATE IN-EKH-KDEKNIVA                                             
255600     WHEN 'DET'                                                           
255700       IF SYST-IDSEKVNR = 1                                               
255800         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
255900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
256000         COMPUTE R3-LINE-AMOUNT-LC =                                      
256100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
256200         IF IN-EKH-KDVALISO = 'MXN'                                       
256300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
256400         END-IF                                                           
256500         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
256600         MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                  
256700         MOVE SPACE               TO WS-ALLOCATE-DC                       
256710         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
256720         MOVE SPACE               TO WS-ALLOCATE-REF                      
256730         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
256800         PERFORM S03-WRITE-W57072                                         
256900       END-IF                                                             
257000                                                                          
257100       IF SYST-IDSEKVNR = 2                                               
257200         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
257300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
257400         COMPUTE R3-LINE-AMOUNT-LC =                                      
257500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
257600         IF IN-EKH-KDVALISO = 'MXN'                                       
257700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
257800         END-IF                                                           
257900         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
258000         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
258100         MOVE SPACE             TO WS-ALLOCATE-REF                        
258200         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
258300         MOVE 0000379164        TO R3-LINE-PA-CUSTOMER                    
258500         PERFORM S03-WRITE-W57072                                         
258600       END-IF                                                             
258700     END-EVALUATE                                                         
258800     .                                                                    
258900     EJECT                                                                
259000                                                                          
259100 CEG-MAIN-EVENT-204 SECTION.                                              
259200     EVALUATE IN-EKH-KDEKSHT                                              
259300     WHEN '201'                                                           
259400          PERFORM CEGA-SUB-EVENT-204-201                                  
259500     WHEN '301'                                                           
259600          PERFORM CEGB-SUB-EVENT-204-301                                  
259700     END-EVALUATE                                                         
259800     .                                                                    
259900     EJECT                                                                
260000                                                                          
260100 CEGA-SUB-EVENT-204-201 SECTION.                                          
260200     EVALUATE IN-EKH-KDEKNIVA                                             
260300     WHEN 'DET'                                                           
260400         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
260500* R-FAKTURA                                                               
260600       IF SYST-IDSEKVNR = 1                                               
260700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
260800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
260900         COMPUTE R3-LINE-AMOUNT-LC =                                      
261000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
261100         IF IN-EKH-KDVALISO = 'MXN'                                       
261200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
261300         END-IF                                                           
261400         MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                  
261600         MOVE SPACE               TO WS-ALLOCATE-DC                       
261700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
261800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
261900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
262000         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
262100         PERFORM S03-WRITE-W57072                                         
262200       END-IF                                                             
262300                                                                          
262400       IF SYST-IDSEKVNR = 2                                               
262500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
262600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
262700         COMPUTE R3-LINE-AMOUNT-LC =                                      
262800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
262900         IF IN-EKH-KDVALISO = 'MXN'                                       
263000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
263100         END-IF                                                           
263200         MOVE SPACE               TO WS-ALLOCATE-DC                       
263300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
263400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
263500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
263600         MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                  
263800         PERFORM S03-WRITE-W57072                                         
263900       END-IF                                                             
264000     END-EVALUATE                                                         
264100     .                                                                    
264200     EJECT                                                                
264300                                                                          
264400 CEGB-SUB-EVENT-204-301 SECTION.                                          
264500     EVALUATE IN-EKH-KDEKNIVA                                             
264600     WHEN 'DET'                                                           
264700* R-FAKTURA                                                               
264800       IF SYST-IDSEKVNR = 1                                               
264900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
265000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
265100         COMPUTE R3-LINE-AMOUNT-LC =                                      
265200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
265300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
265400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
265500         IF BET-KDTRADP(3:2) NOT = SPACE                                  
265600            MOVE '1'             TO WS-ACCOUNT-4                          
265700            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
265800         ELSE                                                             
265900            MOVE '3'             TO WS-ACCOUNT-4                          
266000            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
266100         END-IF                                                           
266200         PERFORM S03-WRITE-W57072                                         
266300       END-IF                                                             
266400                                                                          
266500       IF SYST-IDSEKVNR = 2                                               
266600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
266700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
266800         COMPUTE R3-LINE-AMOUNT-LC =                                      
266900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
267000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
267100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
267200         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
267300         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
267400         MOVE SPACE             TO WS-ALLOCATE-REF                        
267500         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
267600         PERFORM S03-WRITE-W57072                                         
267700       END-IF                                                             
267800                                                                          
267900       IF SYST-IDSEKVNR = 3                                               
268000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
268100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
268200         COMPUTE R3-LINE-AMOUNT-LC =                                      
268300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
268400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
268500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
268600         IF BET-KDTRADP(3:2) NOT = SPACE                                  
268700            MOVE '1'             TO WS-ACCOUNT-4                          
268800            MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER               
268900         ELSE                                                             
269000            MOVE '3'             TO WS-ACCOUNT-4                          
269100            MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER              
269200         END-IF                                                           
269300         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
269400         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
269500         MOVE SPACE             TO WS-ALLOCATE-REF                        
269600         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
269700         PERFORM S03-WRITE-W57072                                         
269800       END-IF                                                             
269900                                                                          
270000     WHEN 'EMB'                                                           
270100     WHEN 'FÖRS'                                                          
270200     WHEN 'FRAKT'                                                         
270300       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
270400       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
270500       MOVE SYST-IDKST          TO WS-RED-IDKST                           
270600       IF WS-RED-IDKST > SPACE                                            
270700         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
270800       ELSE                                                               
270900         MOVE SPACE             TO R3-LINE-COST-CENTER                    
271000       END-IF                                                             
271100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
271200               IN-EKH-SUBEL * -1                                          
271300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
271400       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
271500       PERFORM S04-WRITE-W57053M                                          
271600                                                                          
271700     END-EVALUATE                                                         
271800     .                                                                    
271900     EJECT                                                                
272000                                                                          
272100 CEI-MAIN-EVENT-302 SECTION.                                              
272200     EVALUATE IN-EKH-KDEKSHT                                              
272300     WHEN '301'                                                           
272400          PERFORM CEIA-SUB-EVENT-302-301                                  
272500     WHEN '302'                                                           
272600          PERFORM CEIB-SUB-EVENT-302-302                                  
272700     END-EVALUATE                                                         
272800     .                                                                    
272900     EJECT                                                                
273000                                                                          
273100 CEIA-SUB-EVENT-302-301 SECTION.                                          
273200     EVALUATE IN-EKH-KDEKNIVA                                             
273300     WHEN 'DET'                                                           
273400       IF SYST-IDSEKVNR = 1                                               
273500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
273600         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
273700         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
273800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
273900         COMPUTE R3-LINE-AMOUNT-LC =                                      
274000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
274100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
274200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
274300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
274400         MOVE SPACE               TO WS-ALLOCATE-REF                      
274500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
274600         PERFORM S02-WRITE-W57051M                                        
274700       END-IF                                                             
274800                                                                          
274900       IF SYST-IDSEKVNR = 2                                               
275000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
275100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
275200         MOVE SPACE           TO R3-LINE-COST-CENTER                      
275300         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
275400         COMPUTE R3-LINE-AMOUNT-LC =                                      
275500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
275600         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
275700         MOVE SPACE               TO WS-LINE-TEXT                         
275800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
275900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
276000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
276100         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
276200         PERFORM S02-WRITE-W57051M                                        
276300       END-IF                                                             
276400     END-EVALUATE                                                         
276500     .                                                                    
276600     EJECT                                                                
276700                                                                          
276800 CEIB-SUB-EVENT-302-302 SECTION.                                          
276900     EVALUATE IN-EKH-KDEKNIVA                                             
277000     WHEN 'DET'                                                           
277100       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
277200         IF SYST-IDSEKVNR = 1                                             
277300           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
277400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
277500           IF BET-KDTRADP(3:2) NOT = SPACE                                
277600             MOVE '1'             TO WS-ACCOUNT-4                         
277700           ELSE                                                           
277800             MOVE '3'             TO WS-ACCOUNT-4                         
277900           END-IF                                                         
278000           COMPUTE R3-LINE-AMOUNT-LC  =                                   
278100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
278200           IF IN-EKH-KDVALISO = 'MXN'                                     
278300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
278400           END-IF                                                         
278500           MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                
278700           PERFORM S02-WRITE-W57051M                                      
278800         END-IF                                                           
278900                                                                          
279000         IF SYST-IDSEKVNR = 4                                             
279100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
279200           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
279300           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
279400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
279500           COMPUTE R3-LINE-AMOUNT-LC  =                                   
279600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
279700           IF IN-EKH-KDVALISO = 'MXN'                                     
279800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
279900           END-IF                                                         
280000           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
280100           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
280200           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
280300           MOVE SPACE             TO WS-ALLOCATE-REF                      
280400           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
280500           MOVE 0000379164        TO R3-LINE-PA-CUSTOMER                  
280700           PERFORM S02-WRITE-W57051M                                      
280800         END-IF                                                           
280900       ELSE                                                               
281000         IF SYST-IDSEKVNR = 2                                             
281100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
281200           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
281300           IF BET-KDTRADP(3:2) NOT = SPACE                                
281400             MOVE '1'             TO WS-ACCOUNT-4                         
281500           ELSE                                                           
281600             MOVE '3'             TO WS-ACCOUNT-4                         
281700           END-IF                                                         
281800           COMPUTE R3-LINE-AMOUNT-LC  =                                   
281900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
282000           IF IN-EKH-KDVALISO = 'MXN'                                     
282100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
282200           END-IF                                                         
282300           MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                
282500           PERFORM S02-WRITE-W57051M                                      
282600         END-IF                                                           
282700                                                                          
282800         IF SYST-IDSEKVNR = 3                                             
282900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
283000           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
283100           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
283200           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
283300           COMPUTE R3-LINE-AMOUNT-LC  =                                   
283400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
283500           IF IN-EKH-KDVALISO = 'MXN'                                     
283600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
283700           END-IF                                                         
283800           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
283900           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
284000           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
284100           MOVE SPACE             TO WS-ALLOCATE-REF                      
284200           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
284300           MOVE 0000379164        TO R3-LINE-PA-CUSTOMER                  
284500           PERFORM S02-WRITE-W57051M                                      
284600         END-IF                                                           
284700       END-IF                                                             
284800     END-EVALUATE                                                         
284900     .                                                                    
285000     EJECT                                                                
285100                                                                          
285200 CEJ-MAIN-EVENT-303 SECTION.                                              
285300     EVALUATE IN-EKH-KDEKSHT                                              
285400     WHEN '3XX'                                                           
285500          PERFORM CEJ301-SUB-EVENT-303-3XX                                
285600     WHEN '301'                                                           
285700          PERFORM CEJ301-SUB-EVENT-303-301                                
285800     WHEN '307'                                                           
285900          PERFORM CEJ307-SUB-EVENT-303-307                                
286000     WHEN '310'                                                           
286100          PERFORM CEJ310-SUB-EVENT-303-310                                
286200     WHEN '311'                                                           
286300          PERFORM CEJ311-SUB-EVENT-303-311                                
286400     WHEN '391'                                                           
286500          PERFORM CEJ301-SUB-EVENT-303-391                                
286600     WHEN '371'                                                           
286700          PERFORM CEJ303-SUB-EVENT-303-371                                
286800     END-EVALUATE                                                         
286900     .                                                                    
287000     EJECT                                                                
287100                                                                          
287200 CEJ303-SUB-EVENT-303-371 SECTION.                                        
287300     EVALUATE IN-EKH-KDEKNIVA                                             
287400     WHEN 'DET'                                                           
287500       IF SYST-IDSEKVNR = 1                                               
287600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
287700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
287800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
287900         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX * -1             
288000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
288100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
288200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
288300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
288400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
288500         MOVE SPACE               TO WS-ALLOCATE-DC                       
288600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
288700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
288710         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
288720         PERFORM S03-WRITE-W57072                                         
288730       END-IF                                                             
288740                                                                          
288750       IF SYST-IDSEKVNR = 2                                               
288751         PERFORM S13-GET-LANDING-COST                                     
288760         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
288770         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
288780         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
288790         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX * -1             
288791         * WS-MARKUP                                                      
288792         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
288793         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
288794         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
288795         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
288796         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
288797         MOVE SPACE               TO WS-ALLOCATE-DC                       
288798         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
288799         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
288800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
288801         PERFORM S03-WRITE-W57072                                         
288802       END-IF                                                             
288803                                                                          
288804       IF SYST-IDSEKVNR = 3                                               
288805         PERFORM S13-GET-LANDING-COST                                     
288806         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
288807         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
288808         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
288809         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX * -1             
288810         * WS-MARKUP                                                      
288811         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
288812         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
288813         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
288814         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
288815         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
288816         MOVE SPACE               TO WS-ALLOCATE-DC                       
288817         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
288818         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
288819         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
288820         PERFORM S03-WRITE-W57072                                         
288821       END-IF                                                             
288830                                                                          
288900     WHEN 'LAND'                                                          
289000       IF SYST-IDSEKVNR = 1                                               
289100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
289200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
289300         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
289400         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
289500               R3-LINE-AMOUNT-LC / WS-PRKURS-MX3                          
289600         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
289700         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
289800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
289900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
290000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
290100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
290200         PERFORM S02-WRITE-W57051M                                        
290300       END-IF                                                             
290400                                                                          
290500     WHEN 'DDI'                                                           
290600       IF IN-EKH-SUBEL < ZERO                                             
290700         IF SYST-IDSEKVNR = 1                                             
290800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
290900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
291000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
291100                   IN-EKH-SUBEL                                           
291200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
291300           MOVE SPACE               TO WS-ALLOCATE-DC                     
291400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
291500           MOVE SPACE               TO WS-ALLOCATE-REF                    
291600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
291700           PERFORM S02-WRITE-W57051M                                      
291800         END-IF                                                           
291900       ELSE                                                               
292000         IF SYST-IDSEKVNR = 2                                             
292100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
292200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
292300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
292400                   IN-EKH-SUBEL                                           
292500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
292600           MOVE SPACE               TO WS-ALLOCATE-DC                     
292700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
292800           MOVE SPACE               TO WS-ALLOCATE-REF                    
292900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
293000           PERFORM S02-WRITE-W57051M                                      
293100         END-IF                                                           
293200       END-IF                                                             
293300     END-EVALUATE                                                         
293400     .                                                                    
293500     EJECT                                                                
293600 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
293700     EVALUATE IN-EKH-KDEKNIVA                                             
293800                                                                          
293900     WHEN 'LAND'                                                          
294000       IF SYST-IDSEKVNR = 1                                               
294100         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
294200         MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                          
294300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
294400                 IN-EKH-SUBEL * -1  / WS-PRKURS-MX3                       
294500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
294600         MOVE SPACE           TO WS-ALLOCATE-DC                           
294700         MOVE SPACE           TO WS-ALLOCATE-DISTR                        
294800         MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                        
294900         MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                         
295000         PERFORM S03-WRITE-W57072                                         
295100       END-IF                                                             
295200                                                                          
295300     WHEN 'DDI'                                                           
295400       IF IN-EKH-SUBEL < ZERO                                             
295500         IF SYST-IDSEKVNR = 1                                             
295600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
295700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
295800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
295900                   IN-EKH-SUBEL                                           
296000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
296100           MOVE SPACE               TO WS-ALLOCATE-DC                     
296200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
296300           MOVE SPACE               TO WS-ALLOCATE-REF                    
296400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
296500           PERFORM S04-WRITE-W57053M                                      
296600         END-IF                                                           
296700       ELSE                                                               
296800         IF SYST-IDSEKVNR = 2                                             
296900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
297000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
297100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
297200                   IN-EKH-SUBEL                                           
297300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
297400           MOVE SPACE               TO WS-ALLOCATE-DC                     
297500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
297600           MOVE SPACE               TO WS-ALLOCATE-REF                    
297700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
297800           PERFORM S04-WRITE-W57053M                                      
297900         END-IF                                                           
298000       END-IF                                                             
298100     END-EVALUATE                                                         
298200     .                                                                    
298300     EJECT                                                                
298400                                                                          
298500 CEJ301-SUB-EVENT-303-301 SECTION.                                        
298600                                                                          
298700     EVALUATE IN-EKH-KDEKNIVA                                             
298800     WHEN 'DET'                                                           
298900       IF SYST-IDSEKVNR = 1                                               
299000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
299100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
299200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
299300         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3 * -1            
299400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
299500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
299600         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
299700         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
299800         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
299900         MOVE SPACE               TO WS-ALLOCATE-DC                       
300000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
300100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
300200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
300300         PERFORM S03-WRITE-W57072                                         
300400       END-IF                                                             
300500                                                                          
300600       IF SYST-IDSEKVNR = 2                                               
300610         PERFORM S13-GET-LANDING-COST                                     
300700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
300800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
300900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
301000         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3 * -1            
301100         * WS-MARKUP                                                      
301200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
301300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
301400         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
301500         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
301600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
301700         MOVE SPACE               TO WS-ALLOCATE-DC                       
301800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
301900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
302000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
302100         PERFORM S03-WRITE-W57072                                         
302200       END-IF                                                             
302300                                                                          
302400       IF SYST-IDSEKVNR = 3                                               
302410         PERFORM S13-GET-LANDING-COST                                     
302500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
302600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
302700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
302800         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3 * -1            
302900         * WS-MARKUP                                                      
303000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
303100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
303200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
303300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
303400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
303500         MOVE SPACE               TO WS-ALLOCATE-DC                       
303600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
303700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
303800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
303900         PERFORM S03-WRITE-W57072                                         
304000       END-IF                                                             
304100                                                                          
304200     END-EVALUATE                                                         
304300     .                                                                    
304400     EJECT                                                                
304500                                                                          
304600 CEJ307-SUB-EVENT-303-307 SECTION.                                        
304700                                                                          
304800     EVALUATE IN-EKH-KDEKNIVA                                             
304900     WHEN 'DET'                                                           
305000       IF SYST-IDSEKVNR = 1                                               
305100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
305200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
305300         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
305400         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3 * -1            
305500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
305600         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
305700         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
305800         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
305900         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
306000         MOVE SPACE               TO WS-ALLOCATE-DC                       
306100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
306200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
306300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
306400         PERFORM S03-WRITE-W57072                                         
306500       END-IF                                                             
306600                                                                          
306700       IF SYST-IDSEKVNR = 2                                               
306710         PERFORM S13-GET-LANDING-COST                                     
306800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
306900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
307000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
307100         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3 * -1            
307200         * WS-MARKUP                                                      
307300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
307400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
307500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
307600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
307700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
307800         MOVE SPACE               TO WS-ALLOCATE-DC                       
307900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
308000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
308100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
308200         PERFORM S03-WRITE-W57072                                         
308300       END-IF                                                             
308400                                                                          
308500       IF SYST-IDSEKVNR = 3                                               
308510         PERFORM S13-GET-LANDING-COST                                     
308600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
308700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
308800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
308900         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MX3 * -1            
309000         * WS-MARKUP                                                      
309100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
309200         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
309300         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
309400         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
309500         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
309600         MOVE SPACE               TO WS-ALLOCATE-DC                       
309700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
309800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
309900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
310000         PERFORM S03-WRITE-W57072                                         
310100       END-IF                                                             
310200                                                                          
310300     END-EVALUATE                                                         
310400     .                                                                    
310500     EJECT                                                                
310600                                                                          
310700 CEJ310-SUB-EVENT-303-310 SECTION.                                        
310800     EVALUATE IN-EKH-KDEKNIVA                                             
310900     WHEN 'DET'                                                           
311000       IF SYST-IDSEKVNR = 1                                               
311100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
311200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
311300         COMPUTE R3-LINE-AMOUNT-LC =                                      
311400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
311500         IF IN-EKH-KDVALISO = 'MXN'                                       
311600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
311700         END-IF                                                           
311800         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
311900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
312000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
312100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
312200         MOVE SPACE               TO WS-LINE-TEXT                         
312300         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
312400         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
312500         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
312600         PERFORM S02-WRITE-W57051M                                        
312700       END-IF                                                             
312800                                                                          
312900       IF SYST-IDSEKVNR = 2                                               
313000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
313100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
313200         COMPUTE R3-LINE-AMOUNT-LC =                                      
313300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
313400         IF IN-EKH-KDVALISO = 'MXN'                                       
313500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
313600         END-IF                                                           
313700         MOVE SPACE               TO WS-LINE-TEXT                         
313800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
313900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
314000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
314100         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
314200         MOVE SPACE               TO WS-ALLOCATE-DC                       
314300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
314400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
314500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
314600         PERFORM S02-WRITE-W57051M                                        
314700       END-IF                                                             
314800     END-EVALUATE                                                         
314900     .                                                                    
315000     EJECT                                                                
315100                                                                          
315200 CEJ311-SUB-EVENT-303-311 SECTION.                                        
315300     EVALUATE IN-EKH-KDEKNIVA                                             
315400     WHEN 'DET'                                                           
315500        IF SYST-IDSEKVNR = 1                                              
315600          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
315700          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
315800          COMPUTE R3-LINE-AMOUNT-LC =                                     
315900                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
316000          IF IN-EKH-KDVALISO = 'MXN'                                      
316100            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
316200          END-IF                                                          
316300          MOVE SPACE               TO R3-LINE-TRADING-PARTNER             
316400          MOVE SPACE               TO WS-LINE-TEXT                        
316500          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
316600          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
316700          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
316800          MOVE SPACE               TO WS-ALLOCATE-DC                      
316900          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
317000          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
317100          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
317110          MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                 
317200          PERFORM S02-WRITE-W57051M                                       
317300        END-IF                                                            
317400                                                                          
317500        IF SYST-IDSEKVNR = 2                                              
317600          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
317700          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
317800          COMPUTE R3-LINE-AMOUNT-LC =                                     
317900                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
318000          IF IN-EKH-KDVALISO = 'MXN'                                      
318100            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
318200          END-IF                                                          
318300          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
318400          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
318500          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
318600          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
318700          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
318800          MOVE SPACE               TO WS-LINE-TEXT                        
318900          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
319000          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
319100          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
319200          PERFORM S02-WRITE-W57051M                                       
319300        END-IF                                                            
319400     END-EVALUATE                                                         
319500     .                                                                    
319600     EJECT                                                                
319700                                                                          
319800 CEJ301-SUB-EVENT-303-391 SECTION.                                        
319900     EVALUATE IN-EKH-KDEKNIVA                                             
320000     WHEN 'DET'                                                           
320100       IF SYST-IDSEKVNR = 1                                               
320200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
320300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
320400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
320500              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
320600         IF IN-EKH-KDVALISO = 'MXN'                                       
320700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
320800         END-IF                                                           
320900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
321000         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
321100         MOVE SPACE               TO WS-LINE-TEXT                         
321200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
321300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
321400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
321500         MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                  
321700         MOVE SPACE               TO WS-ALLOCATE-DC                       
321800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
321900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
322000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
322100         PERFORM S03-WRITE-W57072                                         
322200       END-IF                                                             
322300                                                                          
322400       IF SYST-IDSEKVNR = 2                                               
322500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
322600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
322700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
322800              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
322900         IF IN-EKH-KDVALISO = 'MXN'                                       
323000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
323100         END-IF                                                           
323200         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
323300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
323400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
323500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
323600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
323700         MOVE SPACE               TO WS-LINE-TEXT                         
323800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
323900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
324000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
324100         MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                  
324300         PERFORM S03-WRITE-W57072                                         
324400       END-IF                                                             
324500                                                                          
324600     END-EVALUATE                                                         
324700     .                                                                    
324800     EJECT                                                                
324900                                                                          
325000 CEK-MAIN-EVENT-401 SECTION.                                              
325100     EVALUATE IN-EKH-KDEKNIVA                                             
325200                                                                          
325300* PRISÄNDRING LÖPANDE                                                     
325400     WHEN 'DET'                                                           
325500       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
325600                           IN-EKH-PRARTSTD                                
325700       IF SYST-IDSEKVNR = 1                                               
325800* PRISHÖJNING                                                             
325900         IF WS-BELOPP > 0                                                 
326000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
326100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
326200           COMPUTE R3-LINE-AMOUNT-LC =                                    
326300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
326400           IF IN-EKH-KDVALISO = 'MXN'                                     
326500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
326600           END-IF                                                         
326700           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
326800           MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                
327000           PERFORM S02-WRITE-W57051M                                      
327100         END-IF                                                           
327200       END-IF                                                             
327300                                                                          
327400       IF SYST-IDSEKVNR = 2                                               
327500* PRISSÄKNING                                                             
327600         IF WS-BELOPP < 0                                                 
327700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
327800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
327900           COMPUTE R3-LINE-AMOUNT-LC =                                    
328000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
328100           IF IN-EKH-KDVALISO = 'MXN'                                     
328200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
328300           END-IF                                                         
328400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
328500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
328600           MOVE SPACE               TO WS-ALLOCATE-REF                    
328700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
328800           MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                
329000           PERFORM S02-WRITE-W57051M                                      
329100         END-IF                                                           
329200       END-IF                                                             
329300                                                                          
329400       IF SYST-IDSEKVNR = 3                                               
329500* PRISSÄNKNING                                                            
329600         IF WS-BELOPP < 0                                                 
329700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
329800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
329900           COMPUTE R3-LINE-AMOUNT-LC =                                    
330000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
330100           IF IN-EKH-KDVALISO = 'MXN'                                     
330200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
330300           END-IF                                                         
330400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
330500           MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                
330700           PERFORM S02-WRITE-W57051M                                      
330800         END-IF                                                           
330900       END-IF                                                             
331000                                                                          
331100       IF SYST-IDSEKVNR = 4                                               
331200* PRISHÖJNING                                                             
331300         IF WS-BELOPP > 0                                                 
331400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
331500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
331600           COMPUTE R3-LINE-AMOUNT-LC =                                    
331700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
331800           IF IN-EKH-KDVALISO = 'MXN'                                     
331900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
332000           END-IF                                                         
332100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
332200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
332300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
332400           MOVE SPACE               TO WS-ALLOCATE-REF                    
332500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
332600           MOVE 0000379164          TO R3-LINE-PA-CUSTOMER                
332800           PERFORM S02-WRITE-W57051M                                      
332900         END-IF                                                           
333000       END-IF                                                             
333100     END-EVALUATE                                                         
333200     .                                                                    
333300     EJECT                                                                
333400                                                                          
333500 CEL-MAIN-EVENT-402 SECTION.                                              
333600     EVALUATE IN-EKH-KDEKNIVA                                             
333700     WHEN 'DET'                                                           
333800       IF SYST-IDSEKVNR = 1                                               
333900         IF IN-EKH-KVANTAL > 0                                            
334000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
334100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
334200           COMPUTE R3-LINE-AMOUNT-LC =                                    
334300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
334400           IF IN-EKH-KDVALISO = 'MXN'                                     
334500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
334600           END-IF                                                         
334700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
334800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
334900           MOVE SPACE               TO WS-ALLOCATE-REF                    
335000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
335100           PERFORM S02-WRITE-W57051M                                      
335200         END-IF                                                           
335300       END-IF                                                             
335400                                                                          
335500       IF SYST-IDSEKVNR = 2                                               
335600         IF IN-EKH-KVANTAL < 0                                            
335700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
335800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
335900           COMPUTE R3-LINE-AMOUNT-LC =                                    
336000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
336100           IF IN-EKH-KDVALISO = 'MXN'                                     
336200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
336300           END-IF                                                         
336400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
336500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
336600           MOVE SPACE               TO WS-ALLOCATE-REF                    
336700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
336800           PERFORM S02-WRITE-W57051M                                      
336900         END-IF                                                           
337000       END-IF                                                             
337100     END-EVALUATE                                                         
337200     .                                                                    
337300     EJECT                                                                
337400                                                                          
337500 CEM-MAIN-EVENT-403 SECTION.                                              
337600     EVALUATE IN-EKH-KDEKSHT                                              
337700     WHEN '401'                                                           
337800     WHEN '402'                                                           
337900     WHEN '403'                                                           
338000     WHEN '404'                                                           
338100     WHEN '405'                                                           
338200     WHEN '407'                                                           
338300     WHEN '408'                                                           
338400     WHEN '409'                                                           
338500          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
338600     END-EVALUATE                                                         
338700     .                                                                    
338800     EJECT                                                                
338900                                                                          
339000 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
339100     EVALUATE IN-EKH-KDEKNIVA                                             
339200     WHEN 'DET'                                                           
339300       IF IN-EKH-KVANTAL > 0                                              
339400         IF SYST-IDSEKVNR = 1                                             
339500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
339600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
339700           COMPUTE R3-LINE-AMOUNT-LC =                                    
339800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
339900           IF IN-EKH-KDVALISO = 'MXN'                                     
340000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
340100           END-IF                                                         
340200           PERFORM S02-WRITE-W57051M                                      
340300         END-IF                                                           
340400                                                                          
340500         IF SYST-IDSEKVNR = 4                                             
340600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
340700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
340800           COMPUTE R3-LINE-AMOUNT-LC =                                    
340900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
341000           IF IN-EKH-KDVALISO = 'MXN'                                     
341100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
341200           END-IF                                                         
341300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
341400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
341500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
341600           MOVE SPACE               TO WS-ALLOCATE-REF                    
341700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
341800           PERFORM S02-WRITE-W57051M                                      
341900         END-IF                                                           
342000       END-IF                                                             
342100                                                                          
342200       IF IN-EKH-KVANTAL < 0                                              
342300         IF SYST-IDSEKVNR = 2                                             
342400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
342500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
342600           COMPUTE R3-LINE-AMOUNT-LC =                                    
342700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
342800           IF IN-EKH-KDVALISO = 'MXN'                                     
342900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
343000           END-IF                                                         
343100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
343200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
343300           MOVE SPACE               TO WS-ALLOCATE-REF                    
343400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
343500           PERFORM S02-WRITE-W57051M                                      
343600         END-IF                                                           
343700                                                                          
343800         IF SYST-IDSEKVNR = 3                                             
343900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
344000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
344100           COMPUTE R3-LINE-AMOUNT-LC =                                    
344200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
344300           IF IN-EKH-KDVALISO = 'MXN'                                     
344400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
344500           END-IF                                                         
344600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
344700           PERFORM S02-WRITE-W57051M                                      
344800         END-IF                                                           
344900       END-IF                                                             
345000     END-EVALUATE                                                         
345100     .                                                                    
345200     EJECT                                                                
345300                                                                          
345400 CEN-MAIN-EVENT-404 SECTION.                                              
345500     EVALUATE IN-EKH-KDEKNIVA                                             
345600     WHEN 'DET'                                                           
345700       IF SYST-IDSEKVNR = 1                                               
345800* KONTO EJ MANUELLT REGISTRERAT                                           
345900         IF IN-EKH-IDKONTO = 0                                            
346000           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
346100           IF DIST18-SCRAP-NDC-SC                                         
346200           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
346400             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
346500             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
346600             COMPUTE R3-LINE-AMOUNT-LC =                                  
346700                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
346800             IF IN-EKH-KDVALISO = 'MXN'                                   
346900               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
347000             END-IF                                                       
347100             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
347200             PERFORM S02-WRITE-W57051M                                    
347300           END-IF                                                         
347400         END-IF                                                           
347500       END-IF                                                             
347600                                                                          
347700       IF SYST-IDSEKVNR = 2                                               
347800* KONTO MANUELLT REGISTRERAT                                              
347900         IF IN-EKH-IDKONTO > 0                                            
348000           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
348100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
348200           COMPUTE R3-LINE-AMOUNT-LC =                                    
348300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
348400           IF IN-EKH-KDVALISO = 'MXN'                                     
348500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
348600           END-IF                                                         
348700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
348800           PERFORM S02-WRITE-W57051M                                      
348900         END-IF                                                           
349000       END-IF                                                             
349100                                                                          
349200       IF SYST-IDSEKVNR = 3                                               
349300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
349400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
349500         COMPUTE R3-LINE-AMOUNT-LC =                                      
349600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
349700         IF IN-EKH-KDVALISO = 'MXN'                                       
349800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
349900         END-IF                                                           
350000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
350100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
350200         MOVE SPACE               TO WS-ALLOCATE-REF                      
350300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
350400         PERFORM S02-WRITE-W57051M                                        
350500       END-IF                                                             
350600                                                                          
350610       IF SYST-IDSEKVNR = 4                                               
350620         IF IN-EKH-IDKONTO = 0                                            
350630           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
350640           IF DIST18-SCRAP-NDC-QUAL                                       
350650             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
350660             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
350670             COMPUTE R3-LINE-AMOUNT-LC =                                  
350680                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
350690             IF IN-EKH-KDVALISO = 'MXN'                                   
350691               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
350692             END-IF                                                       
350693             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
350694             PERFORM S02-WRITE-W57051M                                    
350695           END-IF                                                         
350696         END-IF                                                           
350697       END-IF                                                             
350698                                                                          
350700                                                                          
350800     END-EVALUATE                                                         
350900     .                                                                    
351000     EJECT                                                                
351100                                                                          
351200 CF-BUILD-COMMON-210-PART SECTION.                                        
351300     MOVE SPACE              TO R3-LINE-R3                                
351400     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
351500                                R3-LINE-DUE-DATE                          
351600                                R3-LINE-AMOUNT                            
351700                                R3-LINE-AMOUNT-LC                         
351800                                R3-LINE-TAX-AMOUNT                        
351900                                R3-LINE-TAX-AMOUNT-LC                     
352000                                R3-LINE-NUMBER-OF-DAYS                    
352100                                R3-LINE-QUANTITY                          
352200                                R3-LINE-SAMNR                             
352300     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
352400     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
352500     MOVE 'MX10'             TO R3-LINE-COMPANY-CODE                      
352600     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
352700     IF SYST-KDPOST = '31'                                                
352800       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
352900     ELSE                                                                 
353000       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
353100     END-IF                                                               
353200     .                                                                    
353300     EJECT                                                                
353400                                                                          
353500 CG-SCHEDULE-LINE-AP SECTION.                                             
353600     MOVE NEJ                     TO WS-HEADER-SW                         
353700     MOVE JA                      TO WS-LINE-SW                           
353800     EVALUATE IN-EKH-KDEKHHT                                              
353900     WHEN '102'                                                           
354000       IF IN-EKH-KDEKSHT = '130'                                          
354100       OR IN-EKH-KDEKSHT = '134'                                          
354200         IF IN-EKH-KDEKSHT = '130'                                        
354300           PERFORM CGA-MAIN-EVENT-102-130                                 
354400         ELSE                                                             
354500           PERFORM CGA-MAIN-EVENT-102-134                                 
354600         END-IF                                                           
354700       ELSE                                                               
354800         IF IN-EKH-KDEKSHT = '120'                                        
354900         OR IN-EKH-KDEKSHT = '124'                                        
355000         OR IN-EKH-KDEKSHT = '125'                                        
355100           IF IN-EKH-KDEKSHT = '125'                                      
355200             PERFORM CGA-MAIN-EVENT-102-125                               
355300           ELSE                                                           
355400             PERFORM CGA-MAIN-EVENT-102-12X                               
355500           END-IF                                                         
355600         ELSE                                                             
355700           PERFORM CGA-MAIN-EVENT-102                                     
355800         END-IF                                                           
355900       END-IF                                                             
356000     WHEN '103'                                                           
356100         PERFORM CGA-MAIN-EVENT-103                                       
356200     WHEN '303'                                                           
356300       IF IN-EKH-KDEKSHT = '371'                                          
356400         PERFORM S81-GET-CURRENCY-RATE                                    
356500         PERFORM CGA-MAIN-EVENT-303-371                                   
356600       ELSE                                                               
356610         IF IN-EKH-KDEKSHT = '3XX'                                        
356620           PERFORM S81-GET-CURRENCY-RATE                                  
356700           PERFORM CGA-MAIN-EVENT-303-3XX                                 
356701         ELSE                                                             
356710           PERFORM CGA-MAIN-EVENT-303                                     
356720         END-IF                                                           
356800       END-IF                                                             
356900     END-EVALUATE                                                         
357000     .                                                                    
357100     EJECT                                                                
357200                                                                          
357300                                                                          
357400 CGA-MAIN-EVENT-102     SECTION.                                          
357500     EVALUATE IN-EKH-KDEKNIVA                                             
357600     WHEN 'SUM'                                                           
357700       IF IN-EKH-SUBEL > ZERO                                             
357800         IF SYST-IDSEKVNR = 1                                             
357900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
358000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
358100            IN-EKH-SUBEL                                                  
358200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
358300           PERFORM S10-VATCODE                                            
358400           IF IN-EKH-SUVAT = ZERO                                         
358500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
358600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
358700           ELSE                                                           
358800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
358900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
359000           END-IF                                                         
359100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
359200                                                                          
359300           PERFORM S04-WRITE-W57053M                                      
359400         END-IF                                                           
359500       END-IF                                                             
359600                                                                          
359700       IF IN-EKH-SUBEL < ZERO                                             
359800         IF SYST-IDSEKVNR = 2                                             
359900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
360000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
360100           IN-EKH-SUBEL                                                   
360200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
360300           PERFORM S10-VATCODE                                            
360400           IF IN-EKH-SUVAT = ZERO                                         
360500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
360600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
360700           ELSE                                                           
360800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
360900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
361000           END-IF                                                         
361100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
361200                                                                          
361300           PERFORM S04-WRITE-W57053M                                      
361400         END-IF                                                           
361500       END-IF                                                             
361600     END-EVALUATE                                                         
361700     .                                                                    
361800     EJECT                                                                
361900                                                                          
362000 CGA-MAIN-EVENT-102-12X SECTION.                                          
362100     EVALUATE IN-EKH-KDEKNIVA                                             
362200     WHEN 'SUM'                                                           
362300       IF IN-EKH-SUBEL > ZERO                                             
362400         IF SYST-IDSEKVNR = 1                                             
362500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
362600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
362700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
362800                   R3-LINE-AMOUNT    / WS-PRKURS-MX  * -1                 
362900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
363000           MOVE 'I3'     TO R3-LINE-TAX-CODE                              
363100           IF IN-EKH-SUVAT = ZERO                                         
363200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
363300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
363400           ELSE                                                           
363500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
363600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
363700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MX  * -1           
363800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
363900           END-IF                                                         
364000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
364100                                                                          
364200           PERFORM S04-WRITE-W57053M                                      
364300         END-IF                                                           
364400       END-IF                                                             
364500                                                                          
364600       IF IN-EKH-SUBEL < ZERO                                             
364700         IF SYST-IDSEKVNR = 2                                             
364800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
364900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
365000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
365100                   R3-LINE-AMOUNT    / WS-PRKURS-MX  * -1                 
365200           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
365300           MOVE 'I3'     TO R3-LINE-TAX-CODE                              
365400           IF IN-EKH-SUVAT = ZERO                                         
365500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
365600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
365700           ELSE                                                           
365800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
365900             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
366000                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MX  * -1           
366100             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
366200           END-IF                                                         
366300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
366400           MOVE SPACE           TO R3-LINE-COST-CENTER                    
366500                                                                          
366600           PERFORM S04-WRITE-W57053M                                      
366700         END-IF                                                           
366800       END-IF                                                             
366900     END-EVALUATE                                                         
367000     .                                                                    
367100     EJECT                                                                
367200                                                                          
367300                                                                          
367400 CGA-MAIN-EVENT-102-125 SECTION.                                          
367500     EVALUATE IN-EKH-KDEKNIVA                                             
367600     WHEN 'SUM'                                                           
367700       IF IN-EKH-SUBEL > ZERO                                             
367800         IF SYST-IDSEKVNR = 1                                             
367900           MOVE ZERO TO SPAR-SUMMA-102-125                                
368000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
368100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
368200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
368300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
368400                   R3-LINE-AMOUNT    / WS-PRKURS-MX  * -1                 
368500           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
368600           MOVE 'I3'     TO R3-LINE-TAX-CODE                              
368700           IF IN-EKH-SUVAT = ZERO                                         
368800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
368900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
369000           ELSE                                                           
369100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
369200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
369300               R3-LINE-TAX-AMOUNT / WS-PRKURS-MX  * -1                    
369400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
369500           END-IF                                                         
369600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
369700           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
369800                                                                          
369900           PERFORM S04-WRITE-W57053M                                      
370000         END-IF                                                           
370100       END-IF                                                             
370200                                                                          
370300       IF IN-EKH-SUBEL < ZERO                                             
370400         IF SYST-IDSEKVNR = 2                                             
370500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
370600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
370700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
370800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
370900                   R3-LINE-AMOUNT    / WS-PRKURS-MX  * -1                 
371000           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
371100           MOVE 'I3'     TO R3-LINE-TAX-CODE                              
371200           IF IN-EKH-SUVAT = ZERO                                         
371300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
371400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
371500           ELSE                                                           
371600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
371700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
371800               R3-LINE-TAX-AMOUNT / WS-PRKURS-MX * -1                     
371900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
372000           END-IF                                                         
372100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
372200           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
372300                                                                          
372400           PERFORM S04-WRITE-W57053M                                      
372500         END-IF                                                           
372600       END-IF                                                             
372700     END-EVALUATE                                                         
372800     .                                                                    
372900     EJECT                                                                
373000 CGA-MAIN-EVENT-102-130 SECTION.                                          
373100     EVALUATE IN-EKH-KDEKNIVA                                             
373200     WHEN 'SUM'                                                           
373300       IF IN-EKH-SUBEL > ZERO                                             
373400         IF SYST-IDSEKVNR = 1                                             
373500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
373600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
373700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
373800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
373900                   R3-LINE-AMOUNT    / WS-PRKURS-MX  * -1                 
374000           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
374100           MOVE 'I3'     TO R3-LINE-TAX-CODE                              
374200           IF IN-EKH-SUVAT = ZERO                                         
374300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
374400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
374500           ELSE                                                           
374600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
374700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
374800                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MX  * -1           
374900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
375000           END-IF                                                         
375100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
375200                                                                          
375300           PERFORM S04-WRITE-W57053M                                      
375400         END-IF                                                           
375500       END-IF                                                             
375600                                                                          
375700       IF IN-EKH-SUBEL < ZERO                                             
375800         IF SYST-IDSEKVNR = 2                                             
375900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
376000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
376100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
376200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
376300                   R3-LINE-AMOUNT    / WS-PRKURS-MX                       
376400           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
376500           MOVE 'I3'     TO R3-LINE-TAX-CODE                              
376600           IF IN-EKH-SUVAT = ZERO                                         
376700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
376800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
376900           ELSE                                                           
377000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
377100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
377200                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MX                 
377300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
377400           END-IF                                                         
377500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
377600                                                                          
377700           PERFORM S04-WRITE-W57053M                                      
377800         END-IF                                                           
377900       END-IF                                                             
378000     END-EVALUATE                                                         
378100     .                                                                    
378200     EJECT                                                                
378300                                                                          
378400 CGA-MAIN-EVENT-102-134 SECTION.                                          
378500     EVALUATE IN-EKH-KDEKNIVA                                             
378600     WHEN 'SUM'                                                           
378700       IF IN-EKH-SUBEL > ZERO                                             
378800         IF SYST-IDSEKVNR = 1                                             
378900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
379000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
379100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
379200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
379300                   R3-LINE-AMOUNT    / WS-PRKURS-MX  * -1                 
379400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
379500           MOVE 'I3'     TO R3-LINE-TAX-CODE                              
379600           IF IN-EKH-SUVAT = ZERO                                         
379700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
379800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
379900           ELSE                                                           
380000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
380100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
380200                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MX  * -1           
380300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
380400           END-IF                                                         
380500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
380600                                                                          
380700           PERFORM S04-WRITE-W57053M                                      
380800         END-IF                                                           
380900       END-IF                                                             
381000     END-EVALUATE                                                         
381100     .                                                                    
381200     EJECT                                                                
381300                                                                          
381400 CGA-MAIN-EVENT-103     SECTION.                                          
381500     EVALUATE IN-EKH-KDEKNIVA                                             
381600     WHEN 'SUM'                                                           
381700       MOVE IN-EKH-IDLEVNR         TO W-IDLEVNR                           
381800       PERFORM IMS-GET-WDF101                                             
381900       PERFORM IMS-GNP-WDF106                                             
382000       IF IN-EKH-SUBEL > ZERO                                             
382100         IF SYST-IDSEKVNR = 1                                             
382200           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
382300           IF ADR-IDLANDX2 = 'MX'                                         
382400             PERFORM S10-VATCODE                                          
382500           ELSE                                                           
382600             MOVE 'I3'             TO R3-LINE-TAX-CODE                    
382700             MOVE ZERO             TO IN-EKH-SUVAT                        
382800           END-IF                                                         
382900           IF IN-EKH-SUVAT = ZERO                                         
383000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
383100                                      R3-LINE-TAX-AMOUNT-LC               
383200           ELSE                                                           
383300             IF IN-EKH-KDVALISO = 'MXN'                                   
383400               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
383500                                      R3-LINE-TAX-AMOUNT-LC               
383600             ELSE                                                         
383700               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
383800                                      R3-LINE-TAX-AMOUNT-LC               
383900               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
384000                       R3-LINE-TAX-AMOUNT * WS-PRKURS                     
384100             END-IF                                                       
384200           END-IF                                                         
384300**** CALCULATE NEW SUM WITH VAT                                           
384400           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
384500                   IN-EKH-SUVAT                                           
384600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
384700           IF IN-EKH-KDVALISO = 'MXN'                                     
384800             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
384900           ELSE                                                           
385000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
385100                     R3-LINE-AMOUNT * WS-PRKURS                           
385200           END-IF                                                         
385300           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
385400                                                                          
385500           PERFORM S04-WRITE-W57053M                                      
385600         END-IF                                                           
385700       END-IF                                                             
385800                                                                          
385900       IF IN-EKH-SUBEL < ZERO                                             
386000         IF SYST-IDSEKVNR = 1                                             
386100           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
386200           IF ADR-IDLANDX2 = 'MX'                                         
386300             MOVE 'D1'             TO R3-LINE-TAX-CODE                    
386400             COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.16           
386500           ELSE                                                           
386600             MOVE 'I3'             TO R3-LINE-TAX-CODE                    
386700             MOVE ZERO             TO IN-EKH-SUVAT                        
386800           END-IF                                                         
386900           IF IN-EKH-SUVAT = ZERO                                         
387000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
387100                                      R3-LINE-TAX-AMOUNT-LC               
387200           ELSE                                                           
387300             IF IN-EKH-KDVALISO = 'MXN'                                   
387400               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
387500                                      R3-LINE-TAX-AMOUNT-LC               
387600             ELSE                                                         
387700               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
387800                                      R3-LINE-TAX-AMOUNT-LC               
387900               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
388000                       R3-LINE-TAX-AMOUNT * WS-PRKURS                     
388100             END-IF                                                       
388200           END-IF                                                         
388300**** CALCULATE NEW SUM WITH VAT                                           
388400           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
388500                   IN-EKH-SUVAT                                           
388600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
388700           IF IN-EKH-KDVALISO = 'MXN'                                     
388800             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
388900           ELSE                                                           
389000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
389100                     R3-LINE-AMOUNT * WS-PRKURS                           
389200           END-IF                                                         
389300           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
389400                                                                          
389500           PERFORM S04-WRITE-W57053M                                      
389600         END-IF                                                           
389700       END-IF                                                             
389800     END-EVALUATE                                                         
389900     .                                                                    
390000     EJECT                                                                
390100                                                                          
390200 CGA-MAIN-EVENT-303 SECTION.                                              
390300     EVALUATE IN-EKH-KDEKNIVA                                             
390400     WHEN 'SUM'                                                           
390500       IF SYST-IDSEKVNR = 1                                               
390600         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
390700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
390800         (IN-EKH-SUBEL / WS-PRKURS-MX)                                    
390900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
391000         MOVE '  '     TO R3-LINE-TAX-CODE                                
391100         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
391200         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
391300                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-MX                     
391400         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
391500                                                                          
391600         PERFORM S04-WRITE-W57053M                                        
391700       END-IF                                                             
391800     END-EVALUATE                                                         
391900     .                                                                    
392000     EJECT                                                                
392100                                                                          
392110 CGA-MAIN-EVENT-303-3XX SECTION.                                          
392120     EVALUATE IN-EKH-KDEKNIVA                                             
392130     WHEN 'SUM'                                                           
392140       IF SYST-IDSEKVNR = 1                                               
392150         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
392160         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
392170         (IN-EKH-SUBEL / WS-PRKURS-MX3)                                   
392180         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
392190         MOVE '  '     TO R3-LINE-TAX-CODE                                
392191         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
392192         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
392193                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-MX3                    
392194         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
392195                                                                          
392196         PERFORM S04-WRITE-W57053M                                        
392197       END-IF                                                             
392198     END-EVALUATE                                                         
392199     .                                                                    
392200     EJECT                                                                
392201                                                                          
392210 CGA-MAIN-EVENT-303-371 SECTION.                                          
392300     EVALUATE IN-EKH-KDEKNIVA                                             
392400     WHEN 'SUM'                                                           
392500       IF SYST-IDSEKVNR = 1                                               
392600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
392700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
392800         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
392900         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
393000               R3-LINE-AMOUNT-LC / WS-PRKURS-MX3                          
393100         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
393200         MOVE 'I3'                 TO R3-LINE-TAX-CODE                    
393300         MOVE ZERO                 TO IN-EKH-SUVAT                        
393400         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
393500         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
393600                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-MX3                    
393700         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
393800                                                                          
393900         PERFORM S04-WRITE-W57053M                                        
394000       END-IF                                                             
394100     END-EVALUATE                                                         
394200     .                                                                    
394300     EJECT                                                                
394400 CH-BUILD-COMMON-310-PART SECTION.                                        
394500     MOVE SPACE              TO R3-LINE-R3                                
394600     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
394700                                R3-LINE-DUE-DATE                          
394800                                R3-LINE-AMOUNT                            
394900                                R3-LINE-AMOUNT-LC                         
395000                                R3-LINE-TAX-AMOUNT                        
395100                                R3-LINE-TAX-AMOUNT-LC                     
395200                                R3-LINE-NUMBER-OF-DAYS                    
395300                                R3-LINE-QUANTITY                          
395400                                R3-LINE-SAMNR                             
395500     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
395600     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
395700     MOVE 'MX10'             TO R3-LINE-COMPANY-CODE                      
395800     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
395900     IF SYST-KDPOST = '31'                                                
396000       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
396100     ELSE                                                                 
396200       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
396300     END-IF                                                               
396400     .                                                                    
396500     EJECT                                                                
396600                                                                          
396700 CI-SCHEDULE-LINE-AR SECTION.                                             
396800     MOVE NEJ                     TO WS-HEADER-SW                         
396900     MOVE JA                      TO WS-LINE-SW                           
397000     EVALUATE IN-EKH-KDEKHHT                                              
397100     WHEN '204'                                                           
397200         PERFORM CIA-MAIN-EVENT-204                                       
397300     END-EVALUATE                                                         
397400     .                                                                    
397500     EJECT                                                                
397600                                                                          
397700 CIA-MAIN-EVENT-204     SECTION.                                          
397800     EVALUATE IN-EKH-KDEKNIVA                                             
397900     WHEN 'SUM'                                                           
398000       IF IN-EKH-SUBEL > ZERO                                             
398100         IF SYST-IDSEKVNR = 1                                             
398200           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
398300           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
398400           IF IN-EKH-KDVALISO = 'MXN'                                     
398500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
398600           END-IF                                                         
398700           IF IN-EKH-KDEKSHT = '301'                                      
398800             MOVE 'I3'             TO R3-LINE-TAX-CODE                    
398900           ELSE                                                           
399000             PERFORM S10-VATCODE                                          
399100           END-IF                                                         
399200           IF IN-EKH-SUVAT = ZERO                                         
399300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
399400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
399500           ELSE                                                           
399600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
399700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
399800           END-IF                                                         
399900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
400000                                                                          
400100           PERFORM S04-WRITE-W57053M                                      
400200         END-IF                                                           
400300       END-IF                                                             
400400                                                                          
400500     END-EVALUATE                                                         
400600     .                                                                    
400700     EJECT                                                                
400800                                                                          
400900 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
401000     MOVE ZERO             TO LOGG-W57073                                 
401100     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
401200     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
401300     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
401400     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
401500     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
401600     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
401700     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
401800     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
401900     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
402000     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
402100     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
402200     MOVE 'MX10'           TO LOGG-KDTRADP                                
402300                                                                          
402400****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
402500     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
402600     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
402700     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
402800     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
402900     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
403000     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
403100     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
403200     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
403300     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
403400     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
403500     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
403600     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
403700     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
403800     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
403900     .                                                                    
404000     EJECT                                                                
404100                                                                          
404200 Z-FINI SECTION.                                                          
404300     CLOSE W57066                                                         
404400           W57058M                                                        
404500           W57051M                                                        
404600           W57052M                                                        
404700           W57053M                                                        
404800           W57055M                                                        
404900           W5705NM                                                        
405000           W51350M                                                        
405100                                                                          
405200     MOVE 'S' TO POSTSUM-OPKOD                                            
405300     CALL POSTSUM USING POSTSUM-PARM                                      
405400     .                                                                    
405500     EJECT                                                                
405600                                                                          
405700 S01-READ-W57066  SECTION.                                                
405800     READ W57066 INTO IN-AREA                                             
405900     AT END                                                               
406000        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
406100        SET END-OF-W57066 TO TRUE                                         
406200                                                                          
406300     NOT AT END                                                           
406400        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
406500        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
406600        MOVE 'W57079D1'   TO POSTSUM-DDNAMN2                              
406700        CALL POSTSUM USING POSTSUM-PARM                                   
406800                                                                          
406900        IF IN-EKH-KDPRODSL NOT = 0                                        
407000          MOVE IN-EKH-KDPRODSL TO WS-KDPRODSL-SAVE                        
407100        END-IF                                                            
407200     END-READ                                                             
407300     .                                                                    
407400                                                                          
407500 S02-WRITE-W57051M SECTION.                                               
407600     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
407700     MOVE SPACE                 TO 71LINE-POST                            
407800     IF WS-LINE-SW = JA                                                   
407900       IF IN-EKH-KDSORT = 'SW'                                            
408000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
408100         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
408200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
408300       ELSE                                                               
408400         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
408500         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
408600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
408700       END-IF                                                             
408800       WRITE 71LINE-POST        FROM R3-LINE-R3                           
408900       PERFORM S20-CREATE-WRITE-LOG                                       
409000     ELSE                                                                 
409100       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
409200     END-IF                                                               
409300                                                                          
409400     IF WS-LINE-SW = JA                                                   
409500       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
409600     ELSE                                                                 
409700       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
409800     END-IF                                                               
409900     MOVE 'W57051M'             TO POSTSUM-FDNAMN                         
410000     MOVE 'W57079D2'            TO POSTSUM-DDNAMN2                        
410100     CALL POSTSUM USING POSTSUM-PARM                                      
410200     .                                                                    
410300                                                                          
410400 S002-WRITE-W57051M-HEAD SECTION.                                         
410500     MOVE SPACE                 TO 71LINE-POST                            
410600     IF IN-EKH-KDSORT = 'SW'                                              
410700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
410800       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
410900       MOVE WS-TEXT           TO R3-LINE-TEXT                             
411000     ELSE                                                                 
411100       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
411200       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
411300       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
411400     END-IF                                                               
411500     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
411600                                                                          
411700     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
411800     MOVE 'W57051M'             TO POSTSUM-FDNAMN                         
411900     MOVE 'W57079D2'            TO POSTSUM-DDNAMN2                        
412000     CALL POSTSUM USING POSTSUM-PARM                                      
412100     .                                                                    
412200                                                                          
412300 S03-WRITE-W57072 SECTION.                                                
412400     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
412500     IF IN-EKH-KDSORT = 'SW'                                              
412600       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
412700       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
412800       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
412900     ELSE                                                                 
413000       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
413100       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
413200       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
413300     END-IF                                                               
413400     WRITE 72LINE-POST        FROM R3-LINE-R3                             
413500                                                                          
413600     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
413700     MOVE 'W57052M'           TO POSTSUM-FDNAMN                           
413800     MOVE 'W57079D3'          TO POSTSUM-DDNAMN2                          
413900     CALL POSTSUM USING POSTSUM-PARM                                      
414000                                                                          
414100     PERFORM S20-CREATE-WRITE-LOG                                         
414200     .                                                                    
414300                                                                          
414400 S04-WRITE-W57053M SECTION.                                               
414500     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
414600     MOVE SPACE                 TO 73LINE-POST                            
414700     IF WS-LINE-SW = JA                                                   
414800       IF IN-EKH-KDSORT = 'SW'                                            
414900         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
415000         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
415100         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
415200       ELSE                                                               
415300         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
415400         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
415500         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
415600       END-IF                                                             
415700       WRITE 73LINE-POST        FROM R3-LINE-R3                           
415800       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
415900     ELSE                                                                 
416000       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
416100       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
416200     END-IF                                                               
416300                                                                          
416400     MOVE 'W57053M'             TO POSTSUM-FDNAMN                         
416500     MOVE 'W57079D4'            TO POSTSUM-DDNAMN2                        
416600     CALL POSTSUM USING POSTSUM-PARM                                      
416700                                                                          
416800     IF WS-LINE-SW = JA                                                   
416900       PERFORM S20-CREATE-WRITE-LOG                                       
417000     END-IF                                                               
417100     .                                                                    
417200                                                                          
417300 S004-WRITE-W57053M-HEAD SECTION.                                         
417400     MOVE SPACE                 TO 73LINE-POST                            
417500     IF IN-EKH-KDSORT = 'SW'                                              
417600       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
417700       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
417800       MOVE WS-TEXT           TO R3-LINE-TEXT                             
417900     ELSE                                                                 
418000       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
418100       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
418200       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
418300     END-IF                                                               
418400     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
418500                                                                          
418600     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
418700     MOVE 'W57053M'             TO POSTSUM-FDNAMN                         
418800     MOVE 'W57079D4'            TO POSTSUM-DDNAMN2                        
418900     CALL POSTSUM USING POSTSUM-PARM                                      
419000                                                                          
419100     .                                                                    
419200                                                                          
419300 S10-VATCODE SECTION.                                                     
419400     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
419500     PERFORM IMS-GU-WDB601                                                
419600     IF DCS-KDDC = SPACE                                                  
419700       MOVE NEJ              TO WDB6-A-SW                                 
419800     ELSE                                                                 
419900       MOVE JA               TO WDB6-A-SW                                 
420000     END-IF                                                               
420100                                                                          
420200     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
420300     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
420400     IF IN-EKH-SUVAT = ZERO                                               
420500       MOVE 'D1'     TO R3-LINE-TAX-CODE                                  
420600**** HERE WE ADD VAT FOR 16% TO BOOKING                                   
420700       COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.16                 
420800     ELSE                                                                 
420900       MOVE 'I3'     TO R3-LINE-TAX-CODE                                  
421000     END-IF                                                               
421100     IF IN-EKH-BEVAT = 'XX'                                               
421200       MOVE 'I3'     TO R3-LINE-TAX-CODE                                  
421300     END-IF                                                               
421400     .                                                                    
421500     EJECT                                                                
421600                                                                          
421700 S13-GET-LANDING-COST SECTION.                                            
421710**** SET CORRECT DC HERE                                                  
421720     MOVE WC-NDC-MX          TO W-IDDC-B6                                 
421900     PERFORM IMS-GU-WDB601                                                
422000     IF SEGMENT-FINNS                                                     
422100       PERFORM IMS-GNP-WDB617                                             
422200       IF SEGMENT-FINNS                                                   
422300         IF PROC-TILANDCO >  IN-EKH-DAVERDAT                              
422400           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
422500         ELSE                                                             
422600           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
422700         END-IF                                                           
422800       END-IF                                                             
422900     END-IF                                                               
423000     .                                                                    
423100     EJECT                                                                
423200 S20-CREATE-WRITE-LOG SECTION.                                            
423300     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
423400     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
423500     IF SYST-IDPTYP = '610'                                               
423600       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
423700     ELSE                                                                 
423800       MOVE ZERO                      TO WS-IDLEVNR                       
423900       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
424000                          FOR CHARACTERS BEFORE INITIAL SPACE             
424100       IF WS-IDLEVNR   > ZERO                                             
424200          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
424300                                      TO LOGG-IDKONTO                     
424400       END-IF                                                             
424500     END-IF                                                               
424600     IF R3-LINE-COST-CENTER NOT = SPACE                                   
424700       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
424800     END-IF                                                               
424900     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
425000     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
425100     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
425200     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
425300     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
425400                                                                          
425500     PERFORM S21-WRITE-W57055M                                            
425600     PERFORM S22-WRITE-W57058M                                            
425700                                                                          
425800     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
425900       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
426000       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
426100       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
426200                                                                          
426300       PERFORM S21-WRITE-W57055M                                          
426400     END-IF                                                               
426500     .                                                                    
426600     EJECT                                                                
426700                                                                          
426800 S21-WRITE-W57055M SECTION.                                               
426900     IF DCS-IDDC NOT = LOGG-IDDC                                          
427000        MOVE LOGG-IDDC TO W-IDDC-B6                                       
427100        PERFORM IMS-GU-WDB601                                             
427200     END-IF                                                               
427300     IF DCS-KDDC = SPACE                                                  
427400       MOVE NEJ              TO WDB6-A-SW                                 
427500     ELSE                                                                 
427600       MOVE JA               TO WDB6-A-SW                                 
427700     END-IF                                                               
427800                                                                          
427900     IF  WDB6-A-FINNS                                                     
428000     AND DCS-DDC                                                          
428100       MOVE 'N'       TO LOGG-FLLSBOK                                     
428200     END-IF                                                               
428300     WRITE LOGG-POST FROM LOGG-W57073                                     
428400                                                                          
428500     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
428600     MOVE 'W57055M'   TO POSTSUM-FDNAMN                                   
428700     MOVE 'W57079D5'  TO POSTSUM-DDNAMN2                                  
428800     CALL POSTSUM USING POSTSUM-PARM                                      
428900     .                                                                    
429000                                                                          
429100 S22-WRITE-W57058M SECTION.                                               
429200     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
429300     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
429400     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
429500                                                                          
429600     IF R3-LINE-AMOUNT-SIGN = '+'                                         
429700       IF AVST-SUBEL < +0                                                 
429800         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
429900       END-IF                                                             
430000       IF AVST-KVANTAL < +0                                               
430100         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
430200       END-IF                                                             
430300     ELSE                                                                 
430400       IF AVST-SUBEL > +0                                                 
430500         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
430600       END-IF                                                             
430700       IF AVST-KVANTAL > +0                                               
430800         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
430900       END-IF                                                             
431000     END-IF                                                               
431100                                                                          
431200     IF DCS-IDDC NOT = AVST-IDDC                                          
431300        MOVE AVST-IDDC  TO W-IDDC-B6                                      
431400        PERFORM IMS-GU-WDB601                                             
431500     END-IF                                                               
431600     IF DCS-KDDC = SPACE                                                  
431700       MOVE NEJ              TO WDB6-A-SW                                 
431800     ELSE                                                                 
431900       MOVE JA               TO WDB6-A-SW                                 
432000     END-IF                                                               
432100                                                                          
432200     IF  WDB6-A-FINNS                                                     
432300     AND DCS-DDC                                                          
432400       MOVE 'N'                 TO AVST-FLLSBOK                           
432500     END-IF                                                               
432600                                                                          
432700     IF AVST-IDKONTO(1:4) = '1454'                                        
432800       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
432900       WRITE AVST-POST FROM AVST-W57070                                   
433000                                                                          
433100       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
433200       MOVE 'W57058M'           TO POSTSUM-FDNAMN                         
433300       MOVE 'W57079D6'          TO POSTSUM-DDNAMN2                        
433400       CALL POSTSUM USING POSTSUM-PARM                                    
433500     END-IF                                                               
433600     .                                                                    
433700     EJECT                                                                
433800                                                                          
433900 S30-READ-DATABASE-B2-B1 SECTION.                                         
434000                                                                          
434100     IF IN-EKH-IDLEVNR = '1441'                                           
434200       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
434300     ELSE                                                                 
434400       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
434500       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
434600       PERFORM IMS-GU-WDB201                                              
434700       IF SEGMENT-SAKNAS                                                  
434800         MOVE 'MX99999'       TO W-WDB1-IDPARTNR                          
434900       ELSE                                                               
435000         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
435100       END-IF                                                             
435200     END-IF                                                               
435300     MOVE WC-IDFTG-MX         TO W-WDB1-IDFTG                             
435400     PERFORM IMS-GU-WDB101                                                
435500     IF SEGMENT-SAKNAS                                                    
435600       DISPLAY 'BETALARUPPG. SAKNAS '                                     
435700       DISPLAY IN-EKH-IDVERGL                                             
435800       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
435900       DISPLAY GMT-IDPARTNR                                               
436000                                                                          
436100       MOVE SPACE         TO BET-KDTRADP                                  
436200       MOVE ZERO          TO BET-IDPARTNR                                 
436300       MOVE '????'        TO WS-KDBETVIL                                  
436400       MOVE '???'         TO WS-KDVALISO-WDB1                             
436500     ELSE                                                                 
436600       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
436700     END-IF                                                               
436800     MOVE 'MXN'           TO WS-KDVALISO-WDB1                             
436900                                                                          
437000     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
437100     MOVE ZERO TO TALLY                                                   
437200     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
437300                 FOR CHARACTERS BEFORE INITIAL SPACE                      
437400     IF TALLY = ZERO                                                      
437500       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
437600     ELSE                                                                 
437700       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
437800                                TO W-BET-IDPARTNR-NUM                     
437900     END-IF                                                               
438000     .                                                                    
438100     EJECT                                                                
438200                                                                          
438300 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
438400     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
438500     PERFORM IMS-GU-WDB601                                                
438600     IF DCS-KDDC = SPACE                                                  
438700       MOVE NEJ              TO WDB6-A-SW                                 
438800     ELSE                                                                 
438900       MOVE JA               TO WDB6-A-SW                                 
439000     END-IF                                                               
439100                                                                          
439200     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
439300       IF IN-EKH-KDEKSHT NOT = '406'                                      
439400         IF IN-EKH-FLDCET = NEJ                                           
439500           PERFORM S42-SKAPA-RW2-INV-POSTER                               
439600         END-IF                                                           
439700       END-IF                                                             
439800     END-IF                                                               
439900                                                                          
440000     IF IN-EKH-KDEKNIVA = 'DET'                                           
440100       IF  IN-EKH-KDEKHHT = '204'                                         
440200       AND (IN-EKH-KDEKSHT = '201')                                       
440300         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
440400       END-IF                                                             
440500                                                                          
440600       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
440700       AND (WDB6-A-FINNS                                                  
440800       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
440900       OR   DCS-DDC OR DCS-NDC-PF))                                       
441000         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
441100       END-IF                                                             
441200                                                                          
441300       IF IN-FIL-IDPGM = 'W4183000'                                       
441400       AND (WDB6-A-FINNS                                                  
441500       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
441600       OR   DCS-DDC OR DCS-NDC-PF))                                       
441700         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
441800       END-IF                                                             
441900     END-IF                                                               
442000     .                                                                    
442100     EJECT                                                                
442200                                                                          
442300 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
442400     MOVE 'RW2'              TO RW2-IDPTYP                                
442500     MOVE 'RW2'              TO WS-IDPTYP                                 
442600     MOVE ZERO               TO RW2-IDDISTR                               
442700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
442800       MOVE WC-CDC-SE        TO RW2-IDDC                                  
442900     ELSE                                                                 
443000       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
443100     END-IF                                                               
443200     IF IN-EKH-KVANTAL < +0                                               
443300       MOVE '0422'           TO RW2-KDWRTYP                               
443400     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
443500     ELSE                                                                 
443600       MOVE '0421'           TO RW2-KDWRTYP                               
443700       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
443800     END-IF                                                               
443900                                                                          
444000     IF RW2-SUARTSTD NOT = +0                                             
444100       PERFORM S70-WRITE-W51350M                                          
444200     END-IF                                                               
444300     .                                                                    
444400     EJECT                                                                
444500                                                                          
444600 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
444700     MOVE '0110'             TO RW1-KDWRTYP                               
444800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
444900       MOVE WC-CDC-SE        TO RW1-IDDC                                  
445000     ELSE                                                                 
445100       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
445200     END-IF                                                               
445300     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
445400     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
445500     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
445600                                                                          
445700     IF RW1-SUARTSTD NOT = +0                                             
445800       MOVE 'RW1' TO WS-IDPTYP                                            
445900       PERFORM S70-WRITE-W51350M                                          
446000     END-IF                                                               
446100     .                                                                    
446200     EJECT                                                                
446300                                                                          
446400 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
446500     MOVE '0110'             TO RW1-KDWRTYP                               
446600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
446700       MOVE WC-CDC-SE        TO RW1-IDDC                                  
446800     ELSE                                                                 
446900       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
447000     END-IF                                                               
447100     IF IN-EKH-KDANMORS = '30'                                            
447200       MOVE ZERO             TO RW1-SUARTSTD                              
447300     ELSE                                                                 
447400      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
447500     END-IF                                                               
447600     IF IN-EKH-KDANMORS = '30' OR '80'                                    
447700       MOVE ZERO             TO RW1-SUARTSJK                              
447800     ELSE                                                                 
447900      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
448000     END-IF                                                               
448100     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
448200                                                                          
448300     IF RW1-SUARTSTD NOT = +0                                             
448400       MOVE 'RW1' TO WS-IDPTYP                                            
448500       PERFORM S70-WRITE-W51350M                                          
448600     END-IF                                                               
448700     .                                                                    
448800     EJECT                                                                
448900                                                                          
449000 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
449100     MOVE '0110'             TO RW1-KDWRTYP                               
449200     IF DCS-KDDC = SPACE OR DCS-DDC                                       
449300       MOVE WC-CDC-SE        TO RW1-IDDC                                  
449400     ELSE                                                                 
449500       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
449600     END-IF                                                               
449700     IF IN-EKH-KDEKSHT = '310'                                            
449800*** SKROTNING KDANMORS  13 O 23                                           
449900       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
450000     ELSE                                                                 
450100      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
450200     END-IF                                                               
450300                                                                          
450400     MOVE ZERO               TO RW1-SUARTSJK                              
450500                                RW1-SUARTFSG                              
450600     IF RW1-SUARTSTD NOT = +0                                             
450700       MOVE 'RW1' TO WS-IDPTYP                                            
450800       PERFORM S70-WRITE-W51350M                                          
450900     END-IF                                                               
451000     .                                                                    
451100     EJECT                                                                
451200                                                                          
451300 S60-WRITE-W5705NM SECTION.                                               
451400     WRITE SAPUT-POST  FROM IN-AREA                                       
451500                                                                          
451600     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
451700     MOVE 'W5705NM'           TO POSTSUM-FDNAMN                           
451800     MOVE 'W57079D7'          TO POSTSUM-DDNAMN2                          
451900     CALL POSTSUM USING POSTSUM-PARM                                      
452000     .                                                                    
452100     EJECT                                                                
452200                                                                          
452300 S70-WRITE-W51350M SECTION.                                               
452400     IF WS-IDPTYP  = 'RW2'                                                
452500       IF DCS-KDDC = SPACE OR DCS-DDC                                     
452600         MOVE WC-CDC-SE        TO INV-IDDC                                
452700       ELSE                                                               
452800         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
452900       END-IF                                                             
453000       IF IN-EKH-KVANTAL < +0                                             
453100         MOVE '003'            TO INV-IDPTYP                              
453200       COMPUTE INV-SUARTSTD =                                             
453300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
453400       ELSE                                                               
453500         MOVE '002'            TO INV-IDPTYP                              
453600         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
453700       END-IF                                                             
453800       MOVE SPACE TO WS-IDPTYP                                            
453900       MOVE 0                  TO INV-ADLAGOMR                            
454000       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
454100       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
454200     END-IF                                                               
454300     IF WS-IDPTYP  = 'RW1'                                                
454400       IF DCS-KDDC = SPACE OR DCS-DDC                                     
454500         MOVE WC-CDC-SE        TO INV-IDDC                                
454600       ELSE                                                               
454700         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
454800       END-IF                                                             
454900       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
455000       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
455100       MOVE 0                  TO INV-ADLAGOMR                            
455200       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
455300       MOVE '001'              TO INV-IDPTYP                              
455400       MOVE SPACE              TO WS-IDPTYP                               
455500     END-IF                                                               
455600     WRITE INV-POST  FROM INV-W51310                                      
455700                                                                          
455800     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
455900     MOVE 'W51350M'           TO POSTSUM-FDNAMN                           
456000     MOVE 'W57079D8'          TO POSTSUM-DDNAMN2                          
456100     CALL POSTSUM USING POSTSUM-PARM                                      
456200     .                                                                    
456300     EJECT                                                                
456400                                                                          
456500 S80-GET-CURRENCY-RATE SECTION.                                           
456600     MOVE +0                  TO W-ANT                                    
456700     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
456800             BEFORE INITIAL ' '                                           
456900     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
457000     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
457100     PERFORM IMS-GU-WDL601                                                
457200     IF SEGMENT-SAKNAS                                                    
457300       CONTINUE                                                           
457400     ELSE                                                                 
457500       PERFORM IMS-GNP-WDL611                                             
457600       IF SEGMENT-SAKNAS                                                  
457700         CONTINUE                                                         
457800       ELSE                                                               
457900         IF (IN-EKH-KDEKHHT = '102'                                       
458000         AND IN-EKH-KDEKSHT = '121')                                      
458010         OR (IN-EKH-KDEKHHT = '102'                                       
458020         AND IN-EKH-KDEKSHT = '122')                                      
458030         OR (IN-EKH-KDEKHHT = '102'                                       
458040         AND IN-EKH-KDEKSHT = '126')                                      
458041         OR (IN-EKH-KDEKHHT = '102'                                       
458042         AND IN-EKH-KDEKSHT = '127')                                      
458050         OR (IN-EKH-KDEKHHT = '102'                                       
458060         AND IN-EKH-KDEKSHT = '128')                                      
458090         OR (IN-EKH-KDEKHHT = '102'                                       
458091         AND IN-EKH-KDEKSHT = '131')                                      
458092         OR (IN-EKH-KDEKHHT = '102'                                       
458093         AND IN-EKH-KDEKSHT = '132')                                      
458100**** EVENT 102-126 CREATES A NEW POST ON WDL611                           
458200**** WITH A DIFFERENT DAINLEV, SO WE NEED TO GET THE CURRENCY             
458300**** RATE FROM THE ORIGINAL POST AND THAT IS SAVED                        
458400**** IF THEY DO BINNING 102-121 OR DEVIATION 102-122 AT THE SAME          
458500**** TIME AS THEY REPORT HAC 102-126 AND THEN SAY THAT THEY               
458600**** RECIEVED THE GOODS BACK FROM CUSTOME 102-127 OR 102-128              
458700**** 102-121 AND 102-122 CAN ALSO GET WRONG CURRENCY RATE                 
458710           IF INL-PRKURS = ZERO                                           
458811             COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                 
458812                                       - INL-DAINLEV                      
458813             MOVE WS-FAKTURA-DATUM2   TO WS-FAKTURA-DATUM                 
458814             MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)               
458815             MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)               
458816             MOVE W-DATE-AAMM         TO CURR-TIAAMM                      
458817             MOVE WS-KDVALISO-MX      TO CURR-KDVALISO-ROW                
458818             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
458819             IF CURR-KDSVAR = ' '                                         
458820               MOVE CURR-PRKURS-NEW   TO WS-PRKURS-MX3                    
458821             ELSE                                                         
458822               MOVE +1                TO WS-PRKURS-MX3                    
458823             END-IF                                                       
458824           ELSE                                                           
458825             MOVE INL-PRKURS        TO WS-PRKURS-MX3                      
458830           END-IF                                                         
458900         ELSE                                                             
459000           COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                   
459100                                     - INL-DAINLEV                        
459200           MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                 
459300           MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                 
459400           MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                 
459500           MOVE W-DATE-AAMM           TO CURR-TIAAMM                      
459600           MOVE WS-KDVALISO-MX        TO CURR-KDVALISO-ROW                
459700           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
459800           IF CURR-KDSVAR = ' '                                           
459900             MOVE CURR-PRKURS-NEW     TO WS-PRKURS-MX3                    
460000           ELSE                                                           
460100             MOVE +1                  TO WS-PRKURS-MX3                    
460200           END-IF                                                         
460300         END-IF                                                           
460400       END-IF                                                             
460500     END-IF                                                               
460600     .                                                                    
460700     EJECT                                                                
460800                                                                          
460900 S81-GET-CURRENCY-RATE SECTION.                                           
461000     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
461100     MOVE 'MXN'               TO CURR-KDVALISO-ROW                        
461200     IF IN-FIL-IDPGM = 'W4183300'                                         
461300       IF IN-EKH-DAAVIDAT > ZERO                                          
461400         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
461500         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
461600       ELSE                                                               
461700         MOVE WS-TIAA            TO WS-TIAA-CR                            
461800         MOVE WS-TIMM            TO WS-TIMM-CR                            
461900       END-IF                                                             
462000     ELSE                                                                 
462100       MOVE WS-TIAA              TO WS-TIAA-CR                            
462200       MOVE WS-TIMM              TO WS-TIMM-CR                            
462300     END-IF                                                               
462400     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
462500     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
462600     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
462700     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
462800     IF CURR-KDSVAR = ' '                                                 
462900       IF IN-EKH-IDDISTR > ZERO                                           
463000         MOVE CURR-PRKURS-NEW TO WS-PRKURS-MX3                            
463100       ELSE                                                               
463200         IF WS-PRKURS = ZERO                                              
463300           MOVE 1           TO WS-PRKURS-MX3                              
463400         END-IF                                                           
463500       END-IF                                                             
463600     ELSE                                                                 
463700       MOVE 1               TO WS-PRKURS-MX3                              
463800     END-IF                                                               
463900     .                                                                    
464000     EJECT                                                                
464100                                                                          
464200* --- IMS SECTIONS ---                                                    
464300                                                                          
464400 IMS-GU-WDH521 SECTION.                                                   
464500     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
464600          DELIMITED BY SIZE INTO SSA1                                     
464700     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
464800          DELIMITED BY SIZE INTO SSA2                                     
464900     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
465000          DELIMITED BY SIZE INTO SSA3                                     
465100     MOVE '  '              TO GODK-STATUSKODER                           
465200     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
465300                                                   SSA2                   
465400                                                   SSA3                   
465500     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
465600                                                                          
465700     PERFORM IMS-STATUS-CONTROL                                           
465800     .                                                                    
465900                                                                          
466000 IMS-GNP-WDH531 SECTION.                                                  
466100     MOVE 'WDH531  '        TO SSA1                                       
466200     MOVE '  GE'            TO GODK-STATUSKODER                           
466300     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
466400     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
466500                               WS-STATUS                                  
466600     PERFORM IMS-STATUS-CONTROL                                           
466700     .                                                                    
466800     EJECT                                                                
466900                                                                          
467000 IMS-GU-WDB201 SECTION.                                                   
467100     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-KEY ')'                         
467200          DELIMITED BY SIZE INTO SSA1                                     
467300     MOVE '  GE'                 TO GODK-STATUSKODER                      
467400     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
467500      MOVE GMTA-STATUS-CODE      TO STATUS-WS                             
467600     PERFORM IMS-STATUS-CONTROL                                           
467700     .                                                                    
467800     EJECT                                                                
467900                                                                          
468000 IMS-GU-WDB101 SECTION.                                                   
468100     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
468200          DELIMITED BY SIZE INTO SSA1                                     
468300     MOVE '  GE'               TO GODK-STATUSKODER                        
468400     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
468500     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
468600     PERFORM IMS-STATUS-CONTROL                                           
468700     .                                                                    
468800     EJECT                                                                
468900                                                                          
469000 IMS-GU-5122 SECTION.                                                     
469100     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
469200            DELIMITED BY SIZE INTO SSA1                                   
469300     STRING 'WDGX5122(KEY5122  =' W-WDGXKEY-5122-X ')'                    
469400            DELIMITED BY SIZE INTO SSA2                                   
469500     MOVE '  GE'           TO GODK-STATUSKODER                            
469600     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5122 SSA1 SSA2            
469700     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
469800     PERFORM IMS-STATUS-CONTROL                                           
469900     .                                                                    
470000                                                                          
470100 IMS-GU-5121 SECTION.                                                     
470200     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
470300            DELIMITED BY SIZE INTO SSA1                                   
470400     MOVE '    '           TO GODK-STATUSKODER                            
470500     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5121 SSA1                 
470600     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
470700     PERFORM IMS-STATUS-CONTROL                                           
470800     .                                                                    
470900                                                                          
471000 IMS-GNP-5122 SECTION.                                                    
471100     STRING 'WDGX5122(KEY5122 >=' W-WDGXKEY-5122-MIN-X                    
471200                    '&KEY5122 <=' W-WDGXKEY-5122-MAX-X ')'                
471300            DELIMITED BY SIZE INTO SSA1                                   
471400     MOVE '  GE'           TO GODK-STATUSKODER                            
471500     CALL CBLTDLI USING GNP 5121-PCB DLI-IO-WDGX5122 SSA1                 
471600     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
471700     PERFORM IMS-STATUS-CONTROL                                           
471800     .                                                                    
471900     EJECT                                                                
472000                                                                          
472100 IMS-GU-WDB601    SECTION.                                                
472200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
472300          DELIMITED BY SIZE INTO SSA1                                     
472400     MOVE '  GE' TO GODK-STATUSKODER                                      
472500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
472600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
472700     PERFORM IMS-STATUS-CONTROL                                           
472800     IF SEGMENT-SAKNAS                                                    
472900        MOVE SPACE TO DCS-KDDC                                            
473000     END-IF                                                               
473100     .                                                                    
473200     EJECT                                                                
473300                                                                          
473400 IMS-GNP-WDB617 SECTION.                                                  
473500     MOVE 'WDB617   ' TO SSA1                                             
473600     MOVE '  GE'        TO GODK-STATUSKODER                               
473700     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
473800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
473900     PERFORM IMS-STATUS-CONTROL                                           
474000     .                                                                    
474100     SKIP3                                                                
474200                                                                          
474300 IMS-GU-WDL601   SECTION.                                                 
474400     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
474500          DELIMITED BY SIZE INTO SSA1                                     
474600     MOVE '  GE' TO GODK-STATUSKODER                                      
474700     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
474800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
474900     PERFORM IMS-STATUS-CONTROL                                           
475000     .                                                                    
475100     SKIP3                                                                
475200                                                                          
475300 IMS-GNP-WDL611   SECTION.                                                
475400     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
475500          DELIMITED BY SIZE INTO SSA1                                     
475600     MOVE '  GE' TO GODK-STATUSKODER                                      
475700     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
475800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
475900     PERFORM IMS-STATUS-CONTROL                                           
476000     .                                                                    
476100     SKIP3                                                                
476200                                                                          
476300 IMS-GET-WDF101 SECTION.                                                  
476400     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
476500             DELIMITED BY SIZE INTO SSA1                                  
476600     MOVE '  GE'                 TO GODK-STATUSKODER                      
476700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
476800     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
476900     PERFORM IMS-STATUS-CONTROL                                           
477000     .                                                                    
477100                                                                          
477200 IMS-GNP-WDF106 SECTION.                                                  
477300     MOVE 'WDF106   '            TO SSA1                                  
477400     MOVE '  GE'                 TO GODK-STATUSKODER                      
477500     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF106 SSA1                   
477600     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
477700     PERFORM IMS-STATUS-CONTROL                                           
477800     .                                                                    
477900                                                                          
478000 IMS-STATUS-CONTROL SECTION.                                              
478100     SET STATUS-IX TO 1                                                   
478200     SEARCH GODK-STATUS                                                   
478300       AT END                                                             
478400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
478500           DELIMITED BY SIZE INTO FELTEXT                                 
478600         DISPLAY FELTEXT                                                  
478700         CALL FELLOG                                                      
478800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
478900         CONTINUE                                                         
479000     END-SEARCH                                                           
479100     .                                                                    
