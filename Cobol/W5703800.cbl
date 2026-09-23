000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5703800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   20211220.                                                
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
004600     SELECT W57066                     ASSIGN TO W57038D1.                
004700                                                                          
004800*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
004900     SELECT W57031A                    ASSIGN TO W57038D2.                
005000                                                                          
005100*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005200     SELECT W57032A                    ASSIGN TO W57038D3.                
005300                                                                          
005400*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005500     SELECT W57033A                    ASSIGN TO W57038D4.                
005600                                                                          
005700*          --- LOGG TILL ON-DEMAND                                        
005800     SELECT W57035                     ASSIGN TO W57038D5.                
005900                                                                          
006000*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006100     SELECT W57038                     ASSIGN TO W57038D6.                
006200                                                                          
006300*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006400     SELECT W5703N                     ASSIGN TO W57038D7.                
006500                                                                          
006600*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006700     SELECT W51380                     ASSIGN TO W57038D8.                
006800     EJECT                                                                
006900                                                                          
007000 DATA DIVISION.                                                           
007100                                                                          
007200 FILE SECTION.                                                            
007300 FD  W57066                                                               
007400     RECORDING       F                                                    
007500     BLOCK CONTAINS  0.                                                   
007600 01  SAP-POST.                                                            
007700*    03  -COPY WDR801        -L.                                          
007800     03 FILLER                   PIC X(6).                                
007900                                                                          
008000 FD  W57031A                                                              
008100     RECORDING       V                                                    
008200     BLOCK CONTAINS  0.                                                   
008300*01  71INIT-POST -COPY R3INIT20               -L.                         
008400*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008500*01  71LINE-POST -COPY R3LINE20               -L.                         
008600                                                                          
008700 FD  W57032A                                                              
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000*01  72LINE-POST -COPY R3LINE20               -L.                         
009100                                                                          
009200 FD  W57033A                                                              
009300     RECORDING       V                                                    
009400     BLOCK CONTAINS  0.                                                   
009500*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009600*01  73LINE-POST -COPY R3LINE20               -L.                         
009700                                                                          
009800 FD  W57035                                                               
009900     RECORDING       F                                                    
010000     BLOCK CONTAINS  0.                                                   
010100*01  LOGG-POST   -COPY W57073                 -L.                         
010200                                                                          
010300 FD  W57038                                                               
010400     RECORDING       F                                                    
010500     BLOCK CONTAINS  0.                                                   
010600*01  AVST-POST   -COPY W57070                 -L.                         
010700                                                                          
010800 FD  W5703N                                                               
010900     RECORDING       F                                                    
011000     BLOCK CONTAINS  0.                                                   
011100                                                                          
011200 01  SAPUT-POST.                                                          
011300*    03  -COPY WDR801        -L.                                          
011400     03 FILLER                   PIC X(6).                                
011500                                                                          
011600 FD  W51380                                                               
011700     RECORDING       F                                                    
011800     BLOCK CONTAINS  0.                                                   
011900*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012000                                                                          
012100     EJECT                                                                
012200 WORKING-STORAGE SECTION.                                                 
012300*    -- CHECKED BY WY2000                                                 
012400 77  IDPGM                        PIC X(8)    VALUE 'W5703800'.           
012500 77  JA                           PIC X       VALUE 'J'.                  
012600 77  NEJ                          PIC X       VALUE 'N'.                  
012700 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
012800 77  W57066-EOF-SW                PIC X       VALUE 'N'.                  
012900     88  END-OF-W57066                        VALUE 'J'.                  
013000 77  WS-HEADER-SW                 PIC X       VALUE 'N'.                  
013100 77  WS-LINE-SW                   PIC X       VALUE 'N'.                  
013200 77  WS-STATUS                    PIC XX      VALUE '  '.                 
013300 77  WS-LINE-AMOUNT               PIC S9(13)V99 COMP-3.                   
013400 77  WS-LINE-AMOUNT-121-1         PIC S9(13)V99 COMP-3.                   
013500 77  WS-LINE-AMOUNT-121-2         PIC S9(13)V99 COMP-3.                   
013600 77  WS-LINE-AMOUNT-122-1         PIC S9(13)V99 COMP-3.                   
013700 77  WS-LINE-AMOUNT-122-2         PIC S9(13)V99 COMP-3.                   
013800 77  WS-LINE-AMOUNT-126-1         PIC S9(13)V99 COMP-3.                   
013900 77  WS-LINE-AMOUNT-126-2         PIC S9(13)V99 COMP-3.                   
014000 77  WS-LINE-AMOUNT-127-1         PIC S9(13)V99 COMP-3.                   
014100 77  WS-LINE-AMOUNT-127-2         PIC S9(13)V99 COMP-3.                   
014200 77  WS-LINE-AMOUNT-128-1         PIC S9(13)V99 COMP-3.                   
014300 77  WS-LINE-AMOUNT-128-2         PIC S9(13)V99 COMP-3.                   
014400 77  SPAR-SUMMA-102-125         PIC S9(13)V99  COMP-3 VALUE ZERO.         
014500 77  WS-BELOPP                  PIC S9(13)V99  COMP-3.                    
014600 77  WS-LOP                       PIC 9       VALUE ZERO.                 
014700 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
014800 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
014900 77  WS-SPAR-IDVERGL              PIC X(10) VALUE SPACE.                  
015000 77  WS-SPAR-DAVERDAT             PIC 9(8) VALUE ZERO.                    
015100 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
015200 77  SPAR-LINE-ORDER              PIC X(12).                              
015300 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
015400 77  WS-RED-IDKST                 PIC X(10).                              
015500 77  WS-IDPTYP                    PIC X(3).                               
015600 77  WS-FAKTURA-DATUM             PIC X(16).                              
015700 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
015800 77  SPAR-SUMMA                 PIC S9(13)V99  COMP-3 VALUE ZERO.         
015900 77  SPAR-PRDMTRL               PIC S9(13)V99  COMP-3 VALUE ZERO.         
016000 77  SPAR-PROVRPAL              PIC S9(13)V99  COMP-3 VALUE ZERO.         
016100 77  SPAR-PRDIRLON              PIC S9(13)V99  COMP-3 VALUE ZERO.         
016200 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
016300 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
016400 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
016500 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
016600 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
016700 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
016800 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
016900 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
017000                                                                          
017100 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
017200       88  WDB6-A-FINNS                       VALUE 'J'.                  
017300       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
017400                                                                          
017500*01  -COPY WWPRODSL                                                       
017600                                                                          
017700*01  -COPY WWDCKONS                                                       
017800     EJECT                                                                
017900                                                                          
018000 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
018100*01  -COPY WWIDFTG                                                        
018200     EJECT                                                                
018300                                                                          
018400 01  FELTEXT                      PIC X(80).                              
018500 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
018600*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
018700     EJECT                                                                
018800                                                                          
018900 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
019000 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
019100     EJECT                                                                
019200 01  WS-IDDISTR-IDKUNDNR.                                                 
019300     03  FILLER                   PIC X(2)    VALUE SPACE.                
019400     03  WS-IDDISTR               PIC 9(4).                               
019500     03  WS-IDKUNDNR              PIC 9(6).                               
019600                                                                          
019700 01  WS-KDBETVIL                  PIC X(4).                               
019800 01  WS-KDVALISO-WDB1             PIC X(3).                               
019900 01  WS-KDVALISO                  PIC X(3).                               
020000 01  WS-KDVALISO-TR               PIC X(3) VALUE 'TRY'.                   
020100 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
020200 01  WS-PRKURS-TR                 PIC S9(6)V9(5) COMP-3.                  
020300 01  WS-PRKURS-TR2                PIC S9(6)V9(5) COMP-3.                  
020400 01  WS-PRKURS-TR3                PIC S9(6)V9(5) COMP-3.                  
020500 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
020600 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
020700                                                                          
020800 01  WS-ALLOCATE.                                                         
020900     03  WS-ALLOCATE-DC           PIC X(2).                               
021000     03  WS-ALLOCATE-DISTR        PIC X(5).                               
021100     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
021200     03  FILLER                   PIC X(4)    VALUE SPACE.                
021300                                                                          
021400 01  WS-TEXT.                                                             
021500     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
021600     03  WS-TEXT-KDEKHHT          PIC X(3).                               
021700     03  WS-TEXT-KDEKSHT          PIC X(3).                               
021800     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
021900     03  FILLER                   PIC X(7)    VALUE SPACE.                
022000                                                                          
022100 01  WS-LINE-TEXT.                                                        
022200     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
022300     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
022400     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
022500     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
022600     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
022700     03  FILLER                   PIC X(22)   VALUE SPACE.                
022800                                                                          
022900 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
023000 01  WS-PRCTR.                                                            
023100     03  WS-PRCTR-PRODSL          PIC X(2).                               
023200     03  FILLER                   PIC X(1).                               
023300     03  FILLER                   PIC X(7).                               
023400                                                                          
023500 01  WS-R3-ACCOUNT.                                                       
023600     03  WS-R3-ACCOUNT-ALFA.                                              
023700         05 FILLER                PIC X(4).                               
023800         05 WS-R3-ACCOUNT-6       PIC X(6).                               
023900     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
024000         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
024100                                                                          
024200 01  WS-ACCOUNT.                                                          
024300     03  FILLER                   PIC X(7).                               
024400     03  WS-ACCOUNT-4             PIC X(1).                               
024500     03  FILLER                   PIC X(2).                               
024600                                                                          
024700 01  SPAR-AREA.                                                           
024800     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
024900     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
025000     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
025100     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
025200                                                                          
025300 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
025400 01  FILLER REDEFINES DAGENS-DATUM.                                       
025500     03  DAGENS-DATUM-AAR         PIC 9(2).                               
025600     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
025700     03  DAGENS-DATUM-DAG         PIC 9(2).                               
025800                                                                          
025900 01  WS-NEW-MONTH                 PIC 9(2).                               
026000                                                                          
026100 01  WS-DAREGDAT.                                                         
026200     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
026300     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
026400                                                                          
026500 01  WS-TIREGDAT-TOT.                                                     
026600     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
026700     03  WS-TIREGDAT              PIC 9(6).                               
026800                                                                          
026900 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
027000 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
027100     EJECT                                                                
027200                                                                          
027300 01  DYNAMISKA-SUBPROGRAM.                                                
027400     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
027500     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
027600     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
027700     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
027800     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
027900     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
028000                                                                          
028100*    --- PARAMETRAR TILL ABEND                                            
028200 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
028300 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
028400 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
028500     EJECT                                                                
028600                                                                          
028700*    --- PARAMETRAR TILL DATKORT                                          
028800 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57038'.             
028900                                                                          
029000 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
029100*01  -COPY WDATKORT                                                       
029200     EJECT                                                                
029300                                                                          
029400*    --- PARAMETRAR TILL POSTSUM                                          
029500*01  -COPY W0005   -PRE  POSTSUM-                                         
029600     EJECT                                                                
029700                                                                          
029800 01  FILLER                          PIC X(16) VALUE 'W510CURR '.         
029900*01  -COPY W510CURR                                                       
030000     EJECT                                                                
030100                                                                          
030200 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
030300*01  AREA -COPY WDR801           -PRE IN-                                 
030400*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
030500         05   IN-EKH-IDSYSMOT     PIC X(6).                               
030600                                                                          
030700     EJECT                                                                
030800 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
030900                                                                          
031000*01  -COPY R3LINE20              -PRE R3-                                 
031100*01  -COPY R3HEAD20              -PRE R3-                                 
031200*01  -COPY R3INIT20              -PRE R3-                                 
031300*01  -COPY W57073                -PRE LOGG-                               
031400*01  -COPY W57070                -PRE AVST-                               
031500*01  -COPY W517RW1               -PRE RW1-                                
031600*01  -COPY W517RW2               -PRE RW2-                                
031700*01  -COPY W51310                -PRE INV-                                
031800     EJECT                                                                
031900                                                                          
032000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032100 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
032200                                                                          
032300 01  NYCKLAR-TILL-DLI.                                                    
032400     03  W-WDH501KY-X.                                                    
032500         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
032600         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
032700     03  W-KDEKSHT-X.                                                     
032800         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
032900     03  W-KDEKNIVA-X.                                                    
033000         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
033100     03  W-WDH531KY-X.                                                    
033200         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
033300         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
033400     03  W-IDRADNR-X.                                                     
033500         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
033600                                                                          
033700     03  W-IDGMT-KEY.                                                     
033800         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
033900         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
034000                                                                          
034100     03  W-WDB101KY-X.                                                    
034200         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
034300         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
034400                                                                          
034500     03  W-IDDC-B6-X.                                                     
034600         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
034700                                                                          
034800     03  W-IDARTNR-X.                                                     
034900         05 W-IDARTNR             PIC S9(9) COMP-3.                       
035000                                                                          
035100     03  W-IDFAKT-X.                                                      
035200         05 W-IDFAKT              PIC S9(7) COMP-3.                       
035300                                                                          
035400     03  W-IDLEVNR-X.                                                     
035500         05  W-IDLEVNR            PIC X(5)    VALUE SPACE.                
035600     EJECT                                                                
035700                                                                          
035800*    --- STATUS-KOD FRÅN IMS                                              
035900 01  STATUS-WS                    PIC XX.                                 
036000     88  SEGMENT-FINNS                        VALUE '  '.                 
036100     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
036200                                                                          
036300 01  GODK-STATUSKODER.                                                    
036400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036500                                                                          
036600 01  SSA1                         PIC X(128).                             
036700 01  SSA2                         PIC X(64).                              
036800 01  SSA3                         PIC X(64).                              
036900     EJECT                                                                
037000                                                                          
037100*    --- IMS FUNKTIONSKODER                                               
037200*01  -COPY W0003                                                          
037300     EJECT                                                                
037400                                                                          
037500*    ---  DLI INPUT-OUTPUT AREA                                           
037600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
037700 01  DLI-IO-WDH501.                                                       
037800*    03  -COPY WDH501                                                     
037900     EJECT                                                                
038000                                                                          
038100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
038200 01  DLI-IO-WDH511.                                                       
038300*    03  -COPY WDH511                                                     
038400     EJECT                                                                
038500                                                                          
038600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
038700 01  DLI-IO-WDH521.                                                       
038800*    03  -COPY WDH521                                                     
038900     EJECT                                                                
039000                                                                          
039100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
039200 01  DLI-IO-WDH531.                                                       
039300*    03  -COPY WDH531                                                     
039400     EJECT                                                                
039500                                                                          
039600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
039700 01  DLI-IO-WDB101.                                                       
039800*    03  -COPY WDB101                                                     
039900     EJECT                                                                
040000                                                                          
040100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
040200 01  DLI-IO-WDB201.                                                       
040300*    03  -COPY WDB201                                                     
040400     EJECT                                                                
040500                                                                          
040600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040700 01   DLI-IO-AREA-B601.                                                   
040800*     03  -COPY WDB601                                                    
040900     EJECT                                                                
041000 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
041100 01   DLI-IO-WDB617.                                                      
041200*     03  -COPY WDB617                                                    
041300     EJECT                                                                
041400 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
041500 01   DLI-IO-AREA-L601.                                                   
041600*     03  -COPY WDL601                                                    
041700     EJECT                                                                
041800 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
041900 01   DLI-IO-AREA-L611.                                                   
042000*     03  -COPY WDL611                                                    
042100     EJECT                                                                
042200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6C1'.                
042300     SKIP3                                                                
042400     EJECT                                                                
042500 LINKAGE SECTION.                                                         
042600*01  -COPY W0008  -PRE WDH5-                                              
042700     05  FILLER                  PIC X.                                   
042800                                                                          
042900*01  -COPY W0008  -PRE WDB2-                                              
043000     05  FILLER                  PIC X.                                   
043100                                                                          
043200*01  -COPY W0008  -PRE WDB1-                                              
043300     05  FILLER                  PIC X.                                   
043400                                                                          
043500*01  -COPY W0008  -PRE WDG2-                                              
043600     05  FILLER                  PIC X.                                   
043700                                                                          
043800*01  -COPY W0008  -PRE WDB6-                                              
043900     05  FILLER                  PIC X.                                   
044000                                                                          
044100*01  -COPY W0008  -PRE WDL6-                                              
044200     05  FILLER                  PIC X.                                   
044300                                                                          
044400                                                                          
044500     EJECT                                                                
044600                                                                          
044700 PROCEDURE DIVISION  USING WDH5-PCB WDB2-PCB WDB1-PCB                     
044800                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
044900 MAIN SECTION.                                                            
045000     ENTRY 'DLITCBL' USING WDH5-PCB WDB2-PCB WDB1-PCB                     
045100                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
045200                                                                          
045300     PERFORM A-INIT                                                       
045400                                                                          
045500     PERFORM S01-READ-W57066                                              
045600     PERFORM UNTIL END-OF-W57066                                          
045700*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
045800       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
045900       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
046000       AND WS-NEW-MONTH > 01                                              
046100         PERFORM S60-WRITE-W5703N                                         
046200       ELSE                                                               
046300         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
046400         PERFORM S30-READ-DATABASE-B2-B1                                  
046500         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
046600           PERFORM C-EXECUTE                                              
046700         END-IF                                                           
046800       END-IF                                                             
046900       PERFORM S01-READ-W57066                                            
047000     END-PERFORM                                                          
047100                                                                          
047200     PERFORM Z-FINI                                                       
047300                                                                          
047400     MOVE ZERO TO RETURN-CODE                                             
047500     GOBACK                                                               
047600     .                                                                    
047700     EJECT                                                                
047800                                                                          
047900 A-INIT SECTION.                                                          
048000     OPEN INPUT  W57066                                                   
048100                                                                          
048200     OPEN OUTPUT W57038                                                   
048300                 W57031A                                                  
048400                 W57032A                                                  
048500                 W57033A                                                  
048600                 W57035                                                   
048700                 W5703N                                                   
048800                 W51380                                                   
048900                                                                          
049000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
049100     MOVE 20               TO RW1-DAVVREG(1:2)                            
049200     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
049300                              RW1-DAVVREG(3:2)                            
049400                              W-DATE-AAMM(1:2)                            
049500                              WS-TIAA                                     
049600     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
049700                              W-DATE-AAMM(3:2)                            
049800                              WS-TIMM                                     
049900                              WS-NEW-MONTH                                
050000     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
050100     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
050200     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
050300                                                                          
050400*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
050500*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
050600     IF WS-NEW-MONTH = 12                                                 
050700       MOVE 1              TO WS-NEW-MONTH                                
050800     ELSE                                                                 
050900       ADD 1               TO WS-NEW-MONTH                                
051000*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
051100***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
051200***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
051300***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
051400***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
051500       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
051600       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
051700         ADD 1             TO WS-NEW-MONTH                                
051800         ADD 1             TO WS-TIMM                                     
051900         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
052000       END-IF                                                             
052100     END-IF                                                               
052200                                                                          
052300     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
052400                                                                          
052500     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
052600                                                                          
052700     ACCEPT DAGENS-KLOCKA FROM TIME                                       
052800     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
052900                                                                          
053000     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
053100     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
053200     MOVE 'M'                   TO CURR-KDVALTYP                          
053300                                                                          
053400     MOVE WS-KDVALISO-TR        TO CURR-KDVALISO-ROW                      
053500     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
053600     IF CURR-KDSVAR = ' '                                                 
053700       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-TR                           
053800     ELSE                                                                 
053900       MOVE 1                   TO WS-PRKURS-TR                           
054000     END-IF                                                               
054100     COMPUTE WS-PRKURS-TR2 ROUNDED = 1 / WS-PRKURS-TR                     
054200     MOVE WS-PRKURS-TR          TO WS-PRKURS-TR3                          
054300     .                                                                    
054400     EJECT                                                                
054500                                                                          
054600 C-EXECUTE SECTION.                                                       
054700     MOVE WC-IDFTG-TR           TO W-IDFTG                                
054800     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
054900     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
055000     IF IN-EKH-KDEKNIVA = 'TDET'                                          
055100       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
055200     END-IF                                                               
055300     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
055400     PERFORM IMS-GU-WDH521                                                
055500     PERFORM IMS-GNP-WDH531                                               
055600                                                                          
055700     PERFORM S13-GET-LANDING-COST                                         
055800                                                                          
055900     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
056000     IF IN-EKH-IDDISTR > ZERO                                             
056100       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
056200     ELSE                                                                 
056300       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
056400     END-IF                                                               
056500     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
056600                                                                          
056700* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
056800* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
056900* BOKFÖRINGEN I SAP SKER DOCK ALLTID I TR, DÄRFÖR BYTET HÄR:              
057000*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
057100*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '106')               
057200*    OR (IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '107')               
057300*      MOVE 'TRY'               TO WS-KDVALISO                            
057400*    END-IF                                                               
057500                                                                          
057600     IF IN-EKH-IDVERGL = WS-SPAR-IDVERGL                                  
057700     AND (IN-EKH-DAVERDAT = WS-SPAR-DAVERDAT)                             
057800       IF  (IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                              
057900       AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                              
058000       OR (IN-EKH-KDEKHHT = '303')                                        
058100         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
058200       ELSE                                                               
058300         MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                           
058400         MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                           
058500         IF WS-LOP = 9                                                    
058600           MOVE ZERO  TO WS-LOP                                           
058700         ELSE                                                             
058800           ADD +1     TO WS-LOP                                           
058900         END-IF                                                           
059000         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
059100       END-IF                                                             
059200     ELSE                                                                 
059300       MOVE IN-EKH-IDVERGL  TO WS-SPAR-IDVERGL                            
059400       MOVE IN-EKH-KDEKHHT  TO WS-SPAR-KDEKHHT                            
059500       MOVE IN-EKH-KDEKSHT  TO WS-SPAR-KDEKSHT                            
059600       MOVE IN-EKH-DAVERDAT TO WS-SPAR-DAVERDAT                           
059700       IF WS-LOP = 9                                                      
059800         MOVE ZERO  TO WS-LOP                                             
059900       ELSE                                                               
060000         ADD +1     TO WS-LOP                                             
060100       END-IF                                                             
060200       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
060300     END-IF                                                               
060400* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
060500     IF SYST-IDPTYP = '210'                                               
060600       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
060700     ELSE                                                                 
060800       IF SYST-IDPTYP = '310'                                             
060900         PERFORM CC-CREATE-WRITE-HEADER-AR                                
061000       ELSE                                                               
061100* TEST OM BRYTNING PÅ VERIFIKATION                                        
061200         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
061300         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
061400         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
061500         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
061600           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
061700           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
061800           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
061900           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
062000           IF (IN-EKH-KDEKHHT = '102'                                     
062100           AND IN-EKH-KDEKSHT = '121')                                    
062200           OR (IN-EKH-KDEKHHT = '102'                                     
062300           AND IN-EKH-KDEKSHT = '122')                                    
062400           OR (IN-EKH-KDEKHHT = '102'                                     
062500           AND IN-EKH-KDEKSHT = '126')                                    
062600           OR (IN-EKH-KDEKHHT = '102'                                     
062700           AND IN-EKH-KDEKSHT = '127')                                    
062800           OR (IN-EKH-KDEKHHT = '102'                                     
062900           AND IN-EKH-KDEKSHT = '128')                                    
063000           OR (IN-EKH-KDEKHHT = '102'                                     
063100           AND IN-EKH-KDEKSHT = '131')                                    
063200           OR (IN-EKH-KDEKHHT = '102'                                     
063300           AND IN-EKH-KDEKSHT = '132')                                    
063400             PERFORM S80-GET-CURRENCY-RATE                                
063500           END-IF                                                         
063600           IF (IN-EKH-KDEKHHT = '303'                                     
063700           AND IN-EKH-KDEKSHT = '301')                                    
063800           OR (IN-EKH-KDEKHHT = '303'                                     
063900           AND IN-EKH-KDEKSHT = '307')                                    
064000           OR (IN-EKH-KDEKHHT = '303'                                     
064100           AND IN-EKH-KDEKSHT = '371')                                    
064200           OR (IN-EKH-KDEKHHT = '303'                                     
064300           AND IN-EKH-KDEKSHT = '3XX')                                    
064400             PERFORM S81-GET-CURRENCY-RATE                                
064500           END-IF                                                         
064600*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
064700*   HEADER-POST TILL HUVUDBOKEN                                           
064800           IF (IN-EKH-KDEKHHT = '102'                                     
064900           AND IN-EKH-KDEKSHT = '120')                                    
065000           OR (IN-EKH-KDEKHHT = '102'                                     
065100           AND IN-EKH-KDEKSHT = '124')                                    
065200           OR (IN-EKH-KDEKHHT = '102'                                     
065300           AND IN-EKH-KDEKSHT = '125')                                    
065400           OR (IN-EKH-KDEKHHT = '102'                                     
065500           AND IN-EKH-KDEKSHT = '130')                                    
065600           OR (IN-EKH-KDEKHHT = '102'                                     
065700           AND IN-EKH-KDEKSHT = '134')                                    
065800           OR (IN-EKH-KDEKHHT = '103'                                     
065900           AND IN-EKH-KDEKSHT = '106')                                    
066000           OR (IN-EKH-KDEKHHT = '103'                                     
066100           AND IN-EKH-KDEKSHT = '107')                                    
066200           OR (IN-EKH-KDEKHHT = '204'                                     
066300           AND IN-EKH-KDEKSHT = '301')                                    
066400           OR (IN-EKH-KDEKHHT = '303'                                     
066500           AND IN-EKH-KDEKSHT = '301')                                    
066600           OR (IN-EKH-KDEKHHT = '303'                                     
066700           AND IN-EKH-KDEKSHT = '307')                                    
066800           OR (IN-EKH-KDEKHHT = '303'                                     
066900           AND IN-EKH-KDEKSHT = '371')                                    
067000           OR (IN-EKH-KDEKHHT = '303'                                     
067100           AND IN-EKH-KDEKSHT = '3XX')                                    
067200             CONTINUE                                                     
067300           ELSE                                                           
067400             PERFORM CA-CREATE-WRITE-HEADER-GL                            
067500           END-IF                                                         
067600         END-IF                                                           
067700       END-IF                                                             
067800     END-IF                                                               
067900                                                                          
068000**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
068100     MOVE WS-STATUS TO STATUS-WS                                          
068200     PERFORM UNTIL SEGMENT-SAKNAS                                         
068300       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
068400                                                                          
068500* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
068600       IF SYST-IDPTYP = '610'                                             
068700         PERFORM CD-BUILD-COMMON-610-PART                                 
068800         PERFORM CE-SCHEDULE-LINE-GL                                      
068900       ELSE                                                               
069000         IF SYST-IDPTYP = '210'                                           
069100           PERFORM CF-BUILD-COMMON-210-PART                               
069200           PERFORM CG-SCHEDULE-LINE-AP                                    
069300         ELSE                                                             
069400           IF SYST-IDPTYP = '310'                                         
069500             PERFORM CH-BUILD-COMMON-310-PART                             
069600             PERFORM CI-SCHEDULE-LINE-AR                                  
069700           END-IF                                                         
069800         END-IF                                                           
069900       END-IF                                                             
070000       PERFORM IMS-GNP-WDH531                                             
070100     END-PERFORM                                                          
070200     .                                                                    
070300     EJECT                                                                
070400                                                                          
070500 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
070600     MOVE SPACE                   TO R3-HEAD-R3                           
070700     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
070800     MOVE 'TR02'                  TO R3-HEAD-COMPANY-CODE                 
070900     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
071000     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
071100     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
071200     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
071300     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
071400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
071500     ELSE                                                                 
071600       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
071700     END-IF                                                               
071800     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
071900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
072000     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
072100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
072200     IF WS-KDVALISO = 'TRY'                                               
072300       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
072400     ELSE                                                                 
072500       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
072600       MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                     
072700       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
072800       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
072900       IF CURR-KDSVAR = ' '                                               
073000         IF IN-EKH-IDDISTR > ZERO                                         
073100           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
073200         ELSE                                                             
073300           MOVE 1               TO WS-PRKURS                              
073400         END-IF                                                           
073500       ELSE                                                               
073600         MOVE 1                 TO WS-PRKURS                              
073700       END-IF                                                             
073800       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
073900                                       CURR-REVALUTA-TO                   
074000       IF CURR-REVALUTA-TO = +1                                           
074100         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
074200       END-IF                                                             
074300       IF CURR-REVALUTA-TO = +10                                          
074400         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
074500       END-IF                                                             
074600       IF CURR-REVALUTA-TO = +100                                         
074700         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
074800       END-IF                                                             
074900     END-IF                                                               
075000     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
075100     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
075200     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
075300     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
075400     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
075500     MOVE JA                      TO WS-HEADER-SW                         
075600     MOVE NEJ                     TO WS-LINE-SW                           
075700                                                                          
075800* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
075900     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
076000     OR (IN-EKH-KDEKHHT = '303'                                           
076100     AND IN-EKH-KDEKSHT = '391')                                          
076200     OR (IN-EKH-KDEKHHT = '102'                                           
076300     AND IN-EKH-KDEKSHT = '121')                                          
076400     OR (IN-EKH-KDEKHHT = '102'                                           
076500     AND IN-EKH-KDEKSHT = '131')                                          
076600     OR (IN-EKH-KDEKHHT = '103'                                           
076700     AND IN-EKH-KDEKSHT = '102')                                          
076800       PERFORM S004-WRITE-W57033A-HEAD                                    
076900     ELSE                                                                 
077000       PERFORM S002-WRITE-W57031A-HEAD                                    
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400                                                                          
077500 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
077600     MOVE SPACE                   TO R3-HEAD-R3                           
077700     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
077800     MOVE 'TR02'                  TO R3-HEAD-COMPANY-CODE                 
077900                                     R3-HEAD-CONTROL-AREA                 
078000     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
078100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
078200     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
078300     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
078400     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
078500     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
078600       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
078700     ELSE                                                                 
078800       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
078900     END-IF                                                               
079000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
079100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
079200     IF (IN-EKH-KDEKHHT = '103'                                           
079300     AND IN-EKH-KDEKSHT = '106')                                          
079400     OR (IN-EKH-KDEKHHT = '103'                                           
079500     AND IN-EKH-KDEKSHT = '107')                                          
079600       MOVE 'TRY'                 TO R3-HEAD-CURRENCY                     
079700       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
079800     ELSE                                                                 
079900       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
080000       MOVE WS-PRKURS-TR2         TO R3-HEAD-EXCHANGE-RATE                
080100       MOVE 'TRY'                 TO CURR-KDVALISO-ROW                    
080200       IF IN-FIL-IDPGM = 'W4183300'                                       
080300         IF IN-EKH-DAAVIDAT > ZERO                                        
080400           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
080500           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
080600         ELSE                                                             
080700           MOVE WS-TIAA              TO WS-TIAA-CR                        
080800           MOVE WS-TIMM              TO WS-TIMM-CR                        
080900         END-IF                                                           
081000       ELSE                                                               
081100         MOVE WS-TIAA                TO WS-TIAA-CR                        
081200         MOVE WS-TIMM                TO WS-TIMM-CR                        
081300       END-IF                                                             
081400       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
081500       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
081600       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
081700       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
081800       IF CURR-KDSVAR = ' '                                               
081900         IF IN-EKH-IDDISTR > ZERO                                         
082000           MOVE CURR-PRKURS-NEW TO WS-PRKURS-TR                           
082100         ELSE                                                             
082200           IF WS-PRKURS = ZERO                                            
082300             MOVE 1             TO WS-PRKURS-TR                           
082400           END-IF                                                         
082500         END-IF                                                           
082600       ELSE                                                               
082700         MOVE 1                 TO WS-PRKURS-TR                           
082800       END-IF                                                             
082900       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-TR *                     
083000                                       CURR-REVALUTA-TO                   
083100       END-COMPUTE                                                        
083200       IF CURR-REVALUTA-TO = +1                                           
083300         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
083400       END-IF                                                             
083500       IF CURR-REVALUTA-TO = +10                                          
083600         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
083700       END-IF                                                             
083800       IF CURR-REVALUTA-TO = +100                                         
083900         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
084000       END-IF                                                             
084100     END-IF                                                               
084200     IF (IN-EKH-KDEKHHT = '102'                                           
084300     AND IN-EKH-KDEKSHT = '120')                                          
084400     OR (IN-EKH-KDEKHHT = '102'                                           
084500     AND IN-EKH-KDEKSHT = '124')                                          
084600     OR (IN-EKH-KDEKHHT = '102'                                           
084700     AND IN-EKH-KDEKSHT = '125')                                          
084800     OR (IN-EKH-KDEKHHT = '102'                                           
084900     AND IN-EKH-KDEKSHT = '130')                                          
085000     OR (IN-EKH-KDEKHHT = '102'                                           
085100     AND IN-EKH-KDEKSHT = '134')                                          
085200     OR (IN-EKH-KDEKHHT = '303'                                           
085300     AND IN-EKH-KDEKSHT = '301')                                          
085400     OR (IN-EKH-KDEKHHT = '303'                                           
085500     AND IN-EKH-KDEKSHT = '307')                                          
085600     OR (IN-EKH-KDEKHHT = '303'                                           
085700     AND IN-EKH-KDEKSHT = '3XX')                                          
085800       MOVE 'TRY'                 TO R3-HEAD-CURRENCY                     
085900       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
086000     END-IF                                                               
086100     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
086200     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
086300     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
086400     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
086500     MOVE JA                      TO WS-HEADER-SW                         
086600     MOVE NEJ                     TO WS-LINE-SW                           
086700                                                                          
086800* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57033A                       
086900       PERFORM S004-WRITE-W57033A-HEAD                                    
087000     .                                                                    
087100     EJECT                                                                
087200                                                                          
087300 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
087400     MOVE SPACE                   TO R3-HEAD-R3                           
087500     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
087600     MOVE 'TR02'                  TO R3-HEAD-COMPANY-CODE                 
087700                                     R3-HEAD-CONTROL-AREA                 
087800     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
087900     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
088000     MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                                 
088100     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
088200     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
088300     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
088400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
088500     ELSE                                                                 
088600       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
088700     END-IF                                                               
088800     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
088900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
089000     IF (IN-EKH-KDEKHHT = '204'                                           
089100     AND IN-EKH-KDEKSHT = '301')                                          
089200       MOVE 'TRY'                 TO R3-HEAD-CURRENCY                     
089300       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
089400     ELSE                                                                 
089500       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
089600       MOVE WS-PRKURS-TR2         TO R3-HEAD-EXCHANGE-RATE                
089700       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
089800       IF IN-FIL-IDPGM = 'W4183300'                                       
089900         IF IN-EKH-DAAVIDAT > ZERO                                        
090000           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
090100           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
090200         ELSE                                                             
090300           MOVE WS-TIAA              TO WS-TIAA-CR                        
090400           MOVE WS-TIMM              TO WS-TIMM-CR                        
090500         END-IF                                                           
090600       ELSE                                                               
090700         MOVE WS-TIAA                TO WS-TIAA-CR                        
090800         MOVE WS-TIMM                TO WS-TIMM-CR                        
090900       END-IF                                                             
091000       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
091100       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
091200       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
091300       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
091400       IF CURR-KDSVAR = ' '                                               
091500         IF IN-EKH-IDDISTR > ZERO                                         
091600           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
091700         ELSE                                                             
091800           IF WS-PRKURS = ZERO                                            
091900             MOVE 1             TO WS-PRKURS                              
092000           END-IF                                                         
092100         END-IF                                                           
092200       ELSE                                                               
092300         MOVE 1                 TO WS-PRKURS                              
092400       END-IF                                                             
092500       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
092600                                       CURR-REVALUTA-TO                   
092700       END-COMPUTE                                                        
092800       IF CURR-REVALUTA-TO = +1                                           
092900         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
093000       END-IF                                                             
093100       IF CURR-REVALUTA-TO = +10                                          
093200         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
093300       END-IF                                                             
093400       IF CURR-REVALUTA-TO = +100                                         
093500         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
093600       END-IF                                                             
093700     END-IF                                                               
093800     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
093900     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
094000     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
094100     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
094200     MOVE JA                      TO WS-HEADER-SW                         
094300     MOVE NEJ                     TO WS-LINE-SW                           
094400                                                                          
094500* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57033A                       
094600       PERFORM S004-WRITE-W57033A-HEAD                                    
094700     .                                                                    
094800     EJECT                                                                
094900                                                                          
095000 CD-BUILD-COMMON-610-PART SECTION.                                        
095100     MOVE SPACE               TO R3-LINE-R3                               
095200     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
095300                                 R3-LINE-DUE-DATE                         
095400                                 R3-LINE-AMOUNT                           
095500                                 R3-LINE-AMOUNT-LC                        
095600                                 R3-LINE-TAX-AMOUNT                       
095700                                 R3-LINE-TAX-AMOUNT-LC                    
095800                                 R3-LINE-NUMBER-OF-DAYS                   
095900                                 R3-LINE-QUANTITY                         
096000                                 R3-LINE-SAMNR                            
096100     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
096200     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
096300     MOVE 'TR02'              TO R3-LINE-COMPANY-CODE                     
096400     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
096500     IF SYST-KDPOST = '50'                                                
096600       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
096700     ELSE                                                                 
096800       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
096900     END-IF                                                               
097000     IF SYST-IDPRCTR NOT = SPACE                                          
097100       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
097200       IF WS-PRCTR-PRODSL = '??'                                          
097300         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
097400         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
097500       END-IF                                                             
097600       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
097700     END-IF                                                               
097800     .                                                                    
097900     EJECT                                                                
098000                                                                          
098100 CE-SCHEDULE-LINE-GL SECTION.                                             
098200     MOVE NEJ                     TO WS-HEADER-SW                         
098300     MOVE JA                      TO WS-LINE-SW                           
098400     EVALUATE IN-EKH-KDEKHHT                                              
098500     WHEN '102'                                                           
098600          PERFORM CEB-MAIN-EVENT-102                                      
098700     WHEN '103'                                                           
098800          PERFORM CEC-MAIN-EVENT-103                                      
098900     WHEN '201'                                                           
099000          PERFORM CED-MAIN-EVENT-201                                      
099100     WHEN '203'                                                           
099200          PERFORM CEF-MAIN-EVENT-203                                      
099300     WHEN '204'                                                           
099400          PERFORM CEG-MAIN-EVENT-204                                      
099500     WHEN '302'                                                           
099600          PERFORM CEI-MAIN-EVENT-302                                      
099700     WHEN '303'                                                           
099800          PERFORM CEJ-MAIN-EVENT-303                                      
099900     WHEN '401'                                                           
100000          PERFORM CEK-MAIN-EVENT-401                                      
100100     WHEN '402'                                                           
100200          PERFORM CEL-MAIN-EVENT-402                                      
100300     WHEN '403'                                                           
100400          PERFORM CEM-MAIN-EVENT-403                                      
100500     WHEN '404'                                                           
100600          PERFORM CEN-MAIN-EVENT-404                                      
100700     END-EVALUATE                                                         
100800     .                                                                    
100900     EJECT                                                                
101000                                                                          
101100 CEB-MAIN-EVENT-102 SECTION.                                              
101200     EVALUATE IN-EKH-KDEKSHT                                              
101300     WHEN '102'                                                           
101400          PERFORM CEBB-SUB-EVENT-102-102                                  
101500     WHEN '120'                                                           
101600          PERFORM CEBD-SUB-EVENT-102-120                                  
101700     WHEN '121'                                                           
101800          PERFORM CEBD-SUB-EVENT-102-121                                  
101900     WHEN '122'                                                           
102000          PERFORM CEBD-SUB-EVENT-102-122                                  
102100     WHEN '123'                                                           
102200          PERFORM CEBD-SUB-EVENT-102-123                                  
102300     WHEN '124'                                                           
102400          PERFORM CEBD-SUB-EVENT-102-124                                  
102500     WHEN '125'                                                           
102600          PERFORM CEBD-SUB-EVENT-102-125                                  
102700     WHEN '126'                                                           
102800          PERFORM CEBD-SUB-EVENT-102-126                                  
102900     WHEN '127'                                                           
103000          PERFORM CEBD-SUB-EVENT-102-127                                  
103100     WHEN '128'                                                           
103200          PERFORM CEBD-SUB-EVENT-102-128                                  
103300     WHEN '130'                                                           
103400          PERFORM CEBE-SUB-EVENT-102-130                                  
103500     WHEN '131'                                                           
103600          PERFORM CEBE-SUB-EVENT-102-131                                  
103700     WHEN '132'                                                           
103800          PERFORM CEBE-SUB-EVENT-102-132                                  
103900     WHEN '134'                                                           
104000          PERFORM CEBE-SUB-EVENT-102-134                                  
104100     END-EVALUATE                                                         
104200     .                                                                    
104300     EJECT                                                                
104400                                                                          
104500 CEBB-SUB-EVENT-102-102 SECTION.                                          
104600     EVALUATE IN-EKH-KDEKNIVA                                             
104700     WHEN 'DET'                                                           
104800       IF SYST-IDSEKVNR = 1                                               
104900         IF IN-EKH-KVANTAL > 0                                            
105000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
105100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
105200           COMPUTE R3-LINE-AMOUNT-LC =                                    
105300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
105400           IF IN-EKH-KDVALISO = 'TRY'                                     
105500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
105600           END-IF                                                         
105700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
105800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
105900           MOVE SPACE               TO WS-ALLOCATE-REF                    
106000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
106100           PERFORM S02-WRITE-W57031A                                      
106200         END-IF                                                           
106300       END-IF                                                             
106400                                                                          
106500       IF SYST-IDSEKVNR = 2                                               
106600         IF IN-EKH-KVANTAL < 0                                            
106700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
106800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
106900           COMPUTE R3-LINE-AMOUNT-LC =                                    
107000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
107100           IF IN-EKH-KDVALISO = 'TRY'                                     
107200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
107300           END-IF                                                         
107400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
107500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
107600           MOVE SPACE               TO WS-ALLOCATE-REF                    
107700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
107800           PERFORM S02-WRITE-W57031A                                      
107900         END-IF                                                           
108000       END-IF                                                             
108100                                                                          
108200       IF SYST-IDSEKVNR = 3                                               
108300         IF IN-EKH-KVANTAL < 0                                            
108400           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
108500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
108600           COMPUTE R3-LINE-AMOUNT-LC =                                    
108700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
108800           IF IN-EKH-KDVALISO = 'TRY'                                     
108900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
109000           END-IF                                                         
109100           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
109200           PERFORM S02-WRITE-W57031A                                      
109300         END-IF                                                           
109400       END-IF                                                             
109500                                                                          
109600       IF SYST-IDSEKVNR = 4                                               
109700         IF IN-EKH-KVANTAL > 0                                            
109800           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
109900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
110000           COMPUTE R3-LINE-AMOUNT-LC =                                    
110100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
110200           IF IN-EKH-KDVALISO = 'TRY'                                     
110300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
110400           END-IF                                                         
110500           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
110600           PERFORM S02-WRITE-W57031A                                      
110700         END-IF                                                           
110800       END-IF                                                             
110900                                                                          
111000     END-EVALUATE                                                         
111100     .                                                                    
111200     EJECT                                                                
111300                                                                          
111400 CEBD-SUB-EVENT-102-120 SECTION.                                          
111500     EVALUATE IN-EKH-KDEKNIVA                                             
111600     WHEN 'DET'                                                           
111700       IF SYST-IDSEKVNR = 1                                               
111800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
111900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
112000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
112100          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR * -1            
112200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
112300         PERFORM S03-WRITE-W57032                                         
112400       END-IF                                                             
112500                                                                          
112600     WHEN 'FÖRS'                                                          
112700     WHEN 'FRAKT'                                                         
112800     WHEN 'EMB'                                                           
112900     WHEN 'LEG'                                                           
113000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
113100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
113200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
113300               IN-EKH-SUBEL / WS-PRKURS-TR   * -1                         
113400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
113500       PERFORM S04-WRITE-W57033A                                          
113600                                                                          
113700     WHEN 'DDI'                                                           
113800       IF IN-EKH-SUBEL > ZERO                                             
113900         IF SYST-IDSEKVNR = 1                                             
114000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
114100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
114200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
114300                   IN-EKH-SUBEL                                           
114400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
114500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
114600           PERFORM S04-WRITE-W57033A                                      
114700         END-IF                                                           
114800       ELSE                                                               
114900         IF SYST-IDSEKVNR = 2                                             
115000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
115100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
115200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
115300                   IN-EKH-SUBEL                                           
115400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
115500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
115600           PERFORM S04-WRITE-W57033A                                      
115700         END-IF                                                           
115800       END-IF                                                             
115900     END-EVALUATE                                                         
116000     .                                                                    
116100     EJECT                                                                
116200                                                                          
116300 CEBD-SUB-EVENT-102-121 SECTION.                                          
116400     EVALUATE IN-EKH-KDEKNIVA                                             
116500     WHEN 'DET'                                                           
116600       IF SYST-IDSEKVNR = 1                                               
116700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
116800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
116900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
117000             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-TR3            
117100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
117200         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-1                      
117300         PERFORM S03-WRITE-W57032                                         
117400       END-IF                                                             
117500                                                                          
117600       IF SYST-IDSEKVNR = 2                                               
117700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
117800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
117900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
118000            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3              
118100            + IN-EKH-KVANTAL *                                            
118200            IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP                   
118300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
118400         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-2                      
118500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
118600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
118700         MOVE SPACE               TO WS-ALLOCATE-REF                      
118800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
118900         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
119000         PERFORM S03-WRITE-W57032                                         
119100       END-IF                                                             
119200                                                                          
119300       IF SYST-IDSEKVNR = 3                                               
119400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
119500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
119600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
119700            WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
119800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
119900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
120000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
120100         MOVE SPACE               TO WS-ALLOCATE-REF                      
120200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
120300         PERFORM S03-WRITE-W57032                                         
120400       END-IF                                                             
120500                                                                          
120600     WHEN 'FÖRS'                                                          
120700     WHEN 'FRAKT'                                                         
120800     WHEN 'EMB'                                                           
120900     WHEN 'LEG'                                                           
121000       IF SYST-IDSEKVNR = 1                                               
121100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
121200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
121300         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
121400                 IN-EKH-SUBEL / WS-PRKURS-TR3                             
121500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
121600         PERFORM S04-WRITE-W57033A                                        
121700       END-IF                                                             
121800                                                                          
121900       IF SYST-IDSEKVNR = 2                                               
122000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
122100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
122200         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
122300                 IN-EKH-SUBEL / WS-PRKURS-TR3                             
122400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
122500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
122600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
122700         MOVE SPACE               TO WS-ALLOCATE-REF                      
122800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
122900         PERFORM S04-WRITE-W57033A                                        
123000       END-IF                                                             
123100                                                                          
123200     END-EVALUATE                                                         
123300                                                                          
123400     .                                                                    
123500     EJECT                                                                
123600                                                                          
123700 CEBD-SUB-EVENT-102-122 SECTION.                                          
123800     EVALUATE IN-EKH-KDEKNIVA                                             
123900     WHEN 'DET'                                                           
124000       IF IN-EKH-KVANTAL > 0                                              
124100         IF SYST-IDSEKVNR = 1                                             
124200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
124300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
124400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
124500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)            
124600            + (IN-EKH-KVANTAL *                                           
124700            IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                  
124800           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
124900           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-122-1               
125000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
125100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
125200           MOVE SPACE               TO WS-ALLOCATE-REF                    
125300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
125400           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
125500           PERFORM S02-WRITE-W57031A                                      
125600         END-IF                                                           
125700                                                                          
125800         IF SYST-IDSEKVNR = 4                                             
125900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
126000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
126100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
126200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)            
126300           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
126400           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-122-2               
126500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
126600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
126700           MOVE SPACE               TO WS-ALLOCATE-REF                    
126800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
126900           MOVE SPACE               TO R3-LINE-COST-CENTER                
127000           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
127100           PERFORM S02-WRITE-W57031A                                      
127200         END-IF                                                           
127300                                                                          
127400         IF SYST-IDSEKVNR = 5                                             
127500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
127600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
127700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
127800              WS-LINE-AMOUNT-122-1 - WS-LINE-AMOUNT-122-2                 
127900           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
128000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
128100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
128200           MOVE SPACE               TO WS-ALLOCATE-REF                    
128300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
128400           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
128500           PERFORM S02-WRITE-W57031A                                      
128600         END-IF                                                           
128700       END-IF                                                             
128800                                                                          
128900                                                                          
129000       IF IN-EKH-KVANTAL < 0                                              
129100         IF SYST-IDSEKVNR = 2                                             
129200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
129300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
129400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
129500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)            
129600            + (IN-EKH-KVANTAL *                                           
129700            IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                  
129800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
129900           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-122-1               
130000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
130100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
130200           MOVE SPACE               TO WS-ALLOCATE-REF                    
130300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
130400           MOVE SPACE             TO R3-LINE-COST-CENTER                  
130500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
130600           PERFORM S02-WRITE-W57031A                                      
130700         END-IF                                                           
130800                                                                          
130900         IF SYST-IDSEKVNR = 3                                             
131000           IF IN-EKH-CMD = 'DAM'                                          
131100             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
131200             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
131300             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
131400              (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)          
131500              + (IN-EKH-KVANTAL *                                         
131600              IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                
131700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
131800             MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-122-1             
131900             MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                   
132000             MOVE SPACE               TO WS-ALLOCATE-DISTR                
132100             MOVE SPACE               TO WS-ALLOCATE-REF                  
132200             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
132300             MOVE SPACE             TO R3-LINE-COST-CENTER                
132400             MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                    
132500             PERFORM S02-WRITE-W57031A                                    
132600           ELSE                                                           
132700             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
132800             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
132900             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
133000              (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)          
133100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
133200             MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-122-2             
133300             MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                   
133400             MOVE SPACE               TO WS-ALLOCATE-DISTR                
133500             MOVE SPACE               TO WS-ALLOCATE-REF                  
133600             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
133700             MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                    
133800             PERFORM S02-WRITE-W57031A                                    
133900           END-IF                                                         
134000         END-IF                                                           
134100                                                                          
134200         IF SYST-IDSEKVNR = 6                                             
134300           IF IN-EKH-CMD = 'DAM'                                          
134400             CONTINUE                                                     
134500           ELSE                                                           
134600             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
134700             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
134800             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
134900                WS-LINE-AMOUNT-122-1 - WS-LINE-AMOUNT-122-2               
135000             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
135100             MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                   
135200             MOVE SPACE               TO WS-ALLOCATE-DISTR                
135300             MOVE SPACE               TO WS-ALLOCATE-REF                  
135400             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
135500             MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                    
135600             PERFORM S02-WRITE-W57031A                                    
135700           END-IF                                                         
135800         END-IF                                                           
135900                                                                          
136000         IF SYST-IDSEKVNR = 7                                             
136100           IF IN-EKH-CMD = 'DAM'                                          
136200             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
136300             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
136400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
136500              (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)          
136600              + (IN-EKH-KVANTAL *                                         
136700              IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                
136800             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
136900             MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                   
137000             MOVE SPACE               TO WS-ALLOCATE-DISTR                
137100             MOVE SPACE               TO WS-ALLOCATE-REF                  
137200             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
137300             MOVE SPACE               TO R3-LINE-COST-CENTER              
137400             MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                    
137500             PERFORM S02-WRITE-W57031A                                    
137600           END-IF                                                         
137700         END-IF                                                           
137800                                                                          
137900         IF SYST-IDSEKVNR = 8                                             
138000           IF IN-EKH-CMD = 'DAM'                                          
138100             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
138200             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
138300             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
138400              (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)          
138500              + (IN-EKH-KVANTAL *                                         
138600               IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)               
138700             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
138800             MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                   
138900             MOVE SPACE               TO WS-ALLOCATE-DISTR                
139000             MOVE SPACE               TO WS-ALLOCATE-REF                  
139100             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
139200             MOVE SPACE               TO R3-LINE-COST-CENTER              
139300             MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                    
139400             PERFORM S02-WRITE-W57031A                                    
139500           END-IF                                                         
139600         END-IF                                                           
139700       END-IF                                                             
139800                                                                          
139900     END-EVALUATE                                                         
140000     .                                                                    
140100     EJECT                                                                
140200                                                                          
140300 CEBD-SUB-EVENT-102-123 SECTION.                                          
140400     EVALUATE IN-EKH-KDEKNIVA                                             
140500     WHEN 'DET'                                                           
140600       IF SYST-IDSEKVNR = 1                                               
140700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
140800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
140900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
141000              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
141100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
141200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
141300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
141400         MOVE SPACE               TO WS-ALLOCATE-REF                      
141500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
141600         PERFORM S02-WRITE-W57031A                                        
141700       END-IF                                                             
141800                                                                          
141900       IF SYST-IDSEKVNR = 2                                               
142000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
142100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
142200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
142300             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
142400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
142500         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
142600         PERFORM S02-WRITE-W57031A                                        
142700       END-IF                                                             
142800     END-EVALUATE                                                         
142900     .                                                                    
143000     EJECT                                                                
143100                                                                          
143200 CEBD-SUB-EVENT-102-124 SECTION.                                          
143300     EVALUATE IN-EKH-KDEKNIVA                                             
143400     WHEN 'DET'                                                           
143500       IF SYST-IDSEKVNR = 1                                               
143600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
143700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
143800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
143900         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR   * -1           
144000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
144100         MOVE SPACE               TO WS-ALLOCATE-DC                       
144200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
144300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
144400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
144500         PERFORM S03-WRITE-W57032                                         
144600       END-IF                                                             
144700                                                                          
144800     WHEN 'FÖRS'                                                          
144900     WHEN 'FRAKT'                                                         
145000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
145100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
145200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
145300               IN-EKH-SUBEL * -1                                          
145400       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
145500       MOVE SPACE               TO WS-ALLOCATE-DC                         
145600       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
145700       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
145800       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
145900       PERFORM S04-WRITE-W57033A                                          
146000                                                                          
146100     WHEN 'EMB'                                                           
146200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
146300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
146400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
146500               (IN-EKH-SUBEL / WS-PRKURS-TR) * -1                         
146600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
146700       MOVE SPACE               TO WS-ALLOCATE-DC                         
146800       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
146900       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
147000       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
147100       PERFORM S04-WRITE-W57033A                                          
147200                                                                          
147300     WHEN 'DDI'                                                           
147400       IF IN-EKH-SUBEL > ZERO                                             
147500         IF SYST-IDSEKVNR = 1                                             
147600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
147700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
147800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
147900                   IN-EKH-SUBEL                                           
148000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
148100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
148200           MOVE SPACE               TO WS-ALLOCATE-DC                     
148300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
148400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
148500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
148600           PERFORM S04-WRITE-W57033A                                      
148700         END-IF                                                           
148800       ELSE                                                               
148900         IF SYST-IDSEKVNR = 2                                             
149000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
149100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
149200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
149300                   IN-EKH-SUBEL                                           
149400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
149500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
149600           MOVE SPACE               TO WS-ALLOCATE-DC                     
149700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
149800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
149900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
150000           PERFORM S04-WRITE-W57033A                                      
150100         END-IF                                                           
150200       END-IF                                                             
150300     END-EVALUATE                                                         
150400     .                                                                    
150500     EJECT                                                                
150600                                                                          
150700 CEBD-SUB-EVENT-102-125 SECTION.                                          
150800     EVALUATE IN-EKH-KDEKNIVA                                             
150900     WHEN 'DET'                                                           
151000       IF SYST-IDSEKVNR = 1                                               
151100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
151200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
151300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
151400         (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-TR)) +            
151500         (IN-EKH-KVANTAL *                                                
151600         (IN-EKH-PRARTNTO / WS-PRKURS-TR) * WS-MARKUP)                    
151700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
151800         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
151900         PERFORM S03-WRITE-W57032                                         
152000       END-IF                                                             
152100                                                                          
152200       IF SYST-IDSEKVNR = 2                                               
152300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
152400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
152500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
152600            (IN-EKH-KVANTAL *                                             
152700            (IN-EKH-PRARTNTO / WS-PRKURS-TR) * WS-MARKUP)                 
152800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
152900         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
153000         PERFORM S03-WRITE-W57032                                         
153100       END-IF                                                             
153200                                                                          
153300     WHEN 'FÖRS'                                                          
153400     WHEN 'FRAKT'                                                         
153500     WHEN 'LEG'                                                           
153600     WHEN 'EMB'                                                           
153700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
153800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
153900       MOVE SPACE             TO R3-LINE-COST-CENTER                      
154000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
154100          IN-EKH-SUBEL / WS-PRKURS-TR   * -1                              
154200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
154300       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
154400       PERFORM S04-WRITE-W57033A                                          
154500                                                                          
154600     WHEN 'DDI'                                                           
154700       IF SPAR-SUMMA-102-125 < ZERO                                       
154800         IF SYST-IDSEKVNR = 1                                             
154900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
155000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
155100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
155200                   SPAR-SUMMA-102-125                                     
155300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
155400           PERFORM S04-WRITE-W57033A                                      
155500         END-IF                                                           
155600       END-IF                                                             
155700       IF SPAR-SUMMA-102-125 > ZERO                                       
155800         IF SYST-IDSEKVNR = 2                                             
155900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
156000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
156100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
156200                   SPAR-SUMMA-102-125                                     
156300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
156400           PERFORM S04-WRITE-W57033A                                      
156500         END-IF                                                           
156600       END-IF                                                             
156700                                                                          
156800     END-EVALUATE                                                         
156900     .                                                                    
157000     EJECT                                                                
157100                                                                          
157200 CEBD-SUB-EVENT-102-126 SECTION.                                          
157300     EVALUATE IN-EKH-KDEKNIVA                                             
157400     WHEN 'DET'                                                           
157500         IF SYST-IDSEKVNR = 1                                             
157600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
157700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
157800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
157900            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-TR3))          
158000            + (IN-EKH-KVANTAL *                                           
158100            (IN-EKH-PRARTNTO / WS-PRKURS-TR3) * WS-MARKUP)                
158200           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
158300           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-126-1               
158400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
158500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
158600           MOVE SPACE               TO WS-ALLOCATE-REF                    
158700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
158800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
158900           PERFORM S02-WRITE-W57031A                                      
159000         END-IF                                                           
159100                                                                          
159200         IF SYST-IDSEKVNR = 2                                             
159300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
159400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
159500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
159600            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-TR3))          
159700           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
159800           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-126-2               
159900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
160000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
160100           MOVE SPACE               TO WS-ALLOCATE-REF                    
160200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
160300           MOVE SPACE               TO R3-LINE-COST-CENTER                
160400           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
160500           PERFORM S02-WRITE-W57031A                                      
160600         END-IF                                                           
160700                                                                          
160800         IF SYST-IDSEKVNR = 3                                             
160900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
161000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
161100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
161200              WS-LINE-AMOUNT-126-1 - WS-LINE-AMOUNT-126-2                 
161300           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
161400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
161500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
161600           MOVE SPACE               TO WS-ALLOCATE-REF                    
161700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
161800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
161900           PERFORM S02-WRITE-W57031A                                      
162000         END-IF                                                           
162100                                                                          
162200     END-EVALUATE                                                         
162300     .                                                                    
162400     EJECT                                                                
162500                                                                          
162600 CEBD-SUB-EVENT-102-127 SECTION.                                          
162700     EVALUATE IN-EKH-KDEKNIVA                                             
162800     WHEN 'DET'                                                           
162900         IF SYST-IDSEKVNR = 1                                             
163000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
163100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
163200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
163300            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-TR3))          
163400            + (IN-EKH-KVANTAL *                                           
163500            (IN-EKH-PRARTNTO / WS-PRKURS-TR3) * WS-MARKUP)                
163600           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
163700           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-127-1               
163800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
163900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
164000           MOVE SPACE               TO WS-ALLOCATE-REF                    
164100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
164200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
164300           PERFORM S02-WRITE-W57031A                                      
164400         END-IF                                                           
164500                                                                          
164600         IF SYST-IDSEKVNR = 2                                             
164700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
164800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
164900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
165000            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-TR3))          
165100           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
165200           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-127-2               
165300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
165400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
165500           MOVE SPACE               TO WS-ALLOCATE-REF                    
165600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
165700           MOVE SPACE               TO R3-LINE-COST-CENTER                
165800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
165900           PERFORM S02-WRITE-W57031A                                      
166000         END-IF                                                           
166100                                                                          
166200         IF SYST-IDSEKVNR = 3                                             
166300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
166400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
166500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
166600              WS-LINE-AMOUNT-127-1 - WS-LINE-AMOUNT-127-2                 
166700           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
166800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
166900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
167000           MOVE SPACE               TO WS-ALLOCATE-REF                    
167100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
167200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
167300           PERFORM S02-WRITE-W57031A                                      
167400         END-IF                                                           
167500                                                                          
167600     END-EVALUATE                                                         
167700     .                                                                    
167800     EJECT                                                                
167900                                                                          
168000 CEBD-SUB-EVENT-102-128 SECTION.                                          
168100     EVALUATE IN-EKH-KDEKNIVA                                             
168200     WHEN 'DET'                                                           
168300         IF SYST-IDSEKVNR = 1                                             
168400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
168500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
168600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
168700            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-TR3))          
168800            + (IN-EKH-KVANTAL *                                           
168900            (IN-EKH-PRARTNTO / WS-PRKURS-TR3) * WS-MARKUP)                
169000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
169100           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-128-1               
169200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
169300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
169400           MOVE SPACE               TO WS-ALLOCATE-REF                    
169500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
169600           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
169700           PERFORM S02-WRITE-W57031A                                      
169800         END-IF                                                           
169900                                                                          
170000         IF SYST-IDSEKVNR = 2                                             
170100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
170200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
170300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
170400            (IN-EKH-KVANTAL * (IN-EKH-PRARTNTO / WS-PRKURS-TR3))          
170500           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
170600           MOVE R3-LINE-AMOUNT      TO WS-LINE-AMOUNT-128-2               
170700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
170800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
170900           MOVE SPACE               TO WS-ALLOCATE-REF                    
171000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
171100           MOVE SPACE               TO R3-LINE-COST-CENTER                
171200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
171300           PERFORM S02-WRITE-W57031A                                      
171400         END-IF                                                           
171500                                                                          
171600         IF SYST-IDSEKVNR = 3                                             
171700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
171800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
171900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
172000              WS-LINE-AMOUNT-128-1 - WS-LINE-AMOUNT-128-2                 
172100           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
172200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
172300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
172400           MOVE SPACE               TO WS-ALLOCATE-REF                    
172500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
172600           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
172700           PERFORM S02-WRITE-W57031A                                      
172800         END-IF                                                           
172900                                                                          
173000     END-EVALUATE                                                         
173100     .                                                                    
173200     EJECT                                                                
173300                                                                          
173400 CEBE-SUB-EVENT-102-130 SECTION.                                          
173500     EVALUATE IN-EKH-KDEKNIVA                                             
173600     WHEN 'DET'                                                           
173700       IF SYST-IDSEKVNR = 1                                               
173800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
173900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
174000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
174100         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR  * -1            
174200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
174300         PERFORM S03-WRITE-W57032                                         
174400       END-IF                                                             
174500                                                                          
174600     WHEN 'FÖRS'                                                          
174700     WHEN 'FRAKT'                                                         
174800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
174900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
175000       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
175100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
175200               IN-EKH-SUBEL / WS-PRKURS-TR  * -1                          
175300       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
175400       PERFORM S04-WRITE-W57033A                                          
175500                                                                          
175600     WHEN 'EMB'                                                           
175700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
175800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
175900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
176000               IN-EKH-SUBEL / WS-PRKURS-TR  * -1                          
176100       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
176200       PERFORM S04-WRITE-W57033A                                          
176300                                                                          
176400     WHEN 'DDI'                                                           
176500       IF IN-EKH-SUBEL > ZERO                                             
176600         IF SYST-IDSEKVNR = 1                                             
176700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
176800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
176900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
177000                   IN-EKH-SUBEL                                           
177100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
177200           PERFORM S04-WRITE-W57033A                                      
177300         END-IF                                                           
177400       ELSE                                                               
177500         IF SYST-IDSEKVNR = 2                                             
177600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
177700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
177800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
177900                   IN-EKH-SUBEL                                           
178000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
178100           PERFORM S04-WRITE-W57033A                                      
178200         END-IF                                                           
178300       END-IF                                                             
178400                                                                          
178500     END-EVALUATE                                                         
178600     .                                                                    
178700     EJECT                                                                
178800                                                                          
178900 CEBE-SUB-EVENT-102-131 SECTION.                                          
179000     EVALUATE IN-EKH-KDEKNIVA                                             
179100     WHEN 'DET'                                                           
179200       IF SYST-IDSEKVNR = 1                                               
179300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
179400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
179500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
179600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3)            
179700         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-1                   
179800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
179900         PERFORM S03-WRITE-W57032                                         
180000       END-IF                                                             
180100                                                                          
180200       IF SYST-IDSEKVNR = 2                                               
180300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
180400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
180500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
180600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3) +          
180700            (IN-EKH-KVANTAL *                                             
180800             IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                 
180900         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-2                   
181000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
181100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
181200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
181300         MOVE SPACE               TO WS-ALLOCATE-REF                      
181400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
181500         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
181600         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
181700         PERFORM S03-WRITE-W57032                                         
181800       END-IF                                                             
181900                                                                          
182000       IF SYST-IDSEKVNR = 3                                               
182100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
182200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
182300         COMPUTE R3-LINE-AMOUNT-LC =                                      
182400                 WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1              
182500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
182600         PERFORM S03-WRITE-W57032                                         
182700       END-IF                                                             
182800                                                                          
182900     WHEN 'FÖRS'                                                          
183000     WHEN 'FRAKT'                                                         
183100       IF SYST-IDSEKVNR = 1                                               
183200         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
183300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
183400         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
183500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
183600                 IN-EKH-SUBEL / WS-PRKURS-TR3                             
183700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
183800         PERFORM S04-WRITE-W57033A                                        
183900       END-IF                                                             
184000                                                                          
184100       IF SYST-IDSEKVNR = 2                                               
184200         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
184300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
184400         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
184500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
184600                 IN-EKH-SUBEL / WS-PRKURS-TR3                             
184700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
184800         PERFORM S04-WRITE-W57033A                                        
184900       END-IF                                                             
185000                                                                          
185100     WHEN 'EMB'                                                           
185200       IF SYST-IDSEKVNR = 1                                               
185300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
185400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
185500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
185600                 IN-EKH-SUBEL / WS-PRKURS-TR3                             
185700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
185800         PERFORM S04-WRITE-W57033A                                        
185900       END-IF                                                             
186000                                                                          
186100       IF SYST-IDSEKVNR = 2                                               
186200         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
186300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
186400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
186500                 IN-EKH-SUBEL / WS-PRKURS-TR3                             
186600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
186700         PERFORM S04-WRITE-W57033A                                        
186800       END-IF                                                             
186900     END-EVALUATE                                                         
187000     .                                                                    
187100     EJECT                                                                
187200                                                                          
187300 CEBE-SUB-EVENT-102-132 SECTION.                                          
187400     EVALUATE IN-EKH-KDEKNIVA                                             
187500     WHEN 'DET'                                                           
187600       IF SYST-IDSEKVNR = 1                                               
187700         IF IN-EKH-KVANTAL > 0                                            
187800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
187900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
188000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
188100            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3) +          
188200            (IN-EKH-KVANTAL *                                             
188300             IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                 
188400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
188500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
188600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
188700           MOVE SPACE               TO WS-ALLOCATE-REF                    
188800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
188900           PERFORM S02-WRITE-W57031A                                      
189000         END-IF                                                           
189100       END-IF                                                             
189200                                                                          
189300       IF SYST-IDSEKVNR = 2                                               
189400         IF IN-EKH-KVANTAL < 0                                            
189500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
189600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
189700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
189800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3) +          
189900            (IN-EKH-KVANTAL *                                             
190000             IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                 
190100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
190200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
190300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
190400           MOVE SPACE               TO WS-ALLOCATE-REF                    
190500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
190600           PERFORM S02-WRITE-W57031A                                      
190700         END-IF                                                           
190800       END-IF                                                             
190900                                                                          
191000       IF SYST-IDSEKVNR = 3                                               
191100         IF IN-EKH-KVANTAL < 0                                            
191200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
191300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
191400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
191500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3) +          
191600            (IN-EKH-KVANTAL *                                             
191700             IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                 
191800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
191900           PERFORM S02-WRITE-W57031A                                      
192000         END-IF                                                           
192100       END-IF                                                             
192200                                                                          
192300       IF SYST-IDSEKVNR = 4                                               
192400         IF IN-EKH-KVANTAL > 0                                            
192500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
192600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
192700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
192800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3) +          
192900            (IN-EKH-KVANTAL *                                             
193000             IN-EKH-PRARTNTO / WS-PRKURS-TR3 * WS-MARKUP)                 
193100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
193200           PERFORM S02-WRITE-W57031A                                      
193300         END-IF                                                           
193400       END-IF                                                             
193500     END-EVALUATE                                                         
193600     .                                                                    
193700     EJECT                                                                
193800                                                                          
193900 CEBE-SUB-EVENT-102-134 SECTION.                                          
194000     EVALUATE IN-EKH-KDEKNIVA                                             
194100     WHEN 'DET'                                                           
194200       IF SYST-IDSEKVNR = 1                                               
194300         IF IN-EKH-KVANTAL > 0                                            
194400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
194500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
194600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
194700           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR  * -1          
194800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
194900           MOVE SPACE               TO WS-ALLOCATE-DC                     
195000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
195100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
195200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
195300           PERFORM S03-WRITE-W57032                                       
195400         END-IF                                                           
195500       END-IF                                                             
195600                                                                          
195700       IF SYST-IDSEKVNR = 2                                               
195800         IF IN-EKH-KVANTAL < 0                                            
195900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
196000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
196100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
196200           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR  * -1          
196300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
196400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
196500           MOVE SPACE               TO WS-ALLOCATE-DC                     
196600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
196700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
196800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
196900           PERFORM S03-WRITE-W57032                                       
197000         END-IF                                                           
197100       END-IF                                                             
197200                                                                          
197300     WHEN 'EMB'                                                           
197400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
197500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
197600       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
197700               IN-EKH-SUBEL / WS-PRKURS-TR   * -1                         
197800       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
197900       MOVE SPACE               TO WS-ALLOCATE-DC                         
198000       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
198100       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
198200       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
198300       PERFORM S04-WRITE-W57033A                                          
198400                                                                          
198500     WHEN 'DDI'                                                           
198600       IF IN-EKH-SUBEL > ZERO                                             
198700         IF SYST-IDSEKVNR = 1                                             
198800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
198900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
199000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
199100                   IN-EKH-SUBEL                                           
199200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
199300           MOVE SPACE               TO WS-ALLOCATE-DC                     
199400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
199500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
199600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
199700           PERFORM S04-WRITE-W57033A                                      
199800         END-IF                                                           
199900       ELSE                                                               
200000         IF SYST-IDSEKVNR = 2                                             
200100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
200200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
200300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
200400                   IN-EKH-SUBEL                                           
200500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
200600           MOVE SPACE               TO WS-ALLOCATE-DC                     
200700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
200800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
200900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
201000           PERFORM S04-WRITE-W57033A                                      
201100         END-IF                                                           
201200       END-IF                                                             
201300                                                                          
201400     END-EVALUATE                                                         
201500     .                                                                    
201600     EJECT                                                                
201700                                                                          
201800 CEC-MAIN-EVENT-103 SECTION.                                              
201900     EVALUATE IN-EKH-KDEKSHT                                              
202000     WHEN '102'                                                           
202100          PERFORM CECB-SUB-EVENT-103-102                                  
202200     WHEN '106'                                                           
202300          PERFORM CECB-SUB-EVENT-103-106                                  
202400     WHEN '107'                                                           
202500          PERFORM CECB-SUB-EVENT-103-107                                  
202600     END-EVALUATE                                                         
202700     .                                                                    
202800     EJECT                                                                
202900                                                                          
203000 CECB-SUB-EVENT-103-102 SECTION.                                          
203100     EVALUATE IN-EKH-KDEKNIVA                                             
203200     WHEN 'DET'                                                           
203300       IF SYST-IDSEKVNR = 1                                               
203400         IF IN-EKH-KVANTAL < 0                                            
203500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
203600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
203700           COMPUTE R3-LINE-AMOUNT-LC =                                    
203800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
203900           IF IN-EKH-KDVALISO = 'TRY'                                     
204000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
204100           END-IF                                                         
204200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
204300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
204400           MOVE SPACE               TO WS-ALLOCATE-REF                    
204500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
204600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
204700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
204800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
204900           PERFORM S04-WRITE-W57033A                                      
205000         END-IF                                                           
205100       END-IF                                                             
205200                                                                          
205300       IF SYST-IDSEKVNR = 2                                               
205400         IF IN-EKH-KVANTAL > 0                                            
205500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
205600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
205700           COMPUTE R3-LINE-AMOUNT-LC =                                    
205800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
205900           IF IN-EKH-KDVALISO = 'TRY'                                     
206000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
206100           END-IF                                                         
206200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
206300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
206400           MOVE SPACE               TO WS-ALLOCATE-REF                    
206500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
206600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
206700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
206800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
206900           PERFORM S04-WRITE-W57033A                                      
207000         END-IF                                                           
207100       END-IF                                                             
207200                                                                          
207300     WHEN 'KALK'                                                          
207400       IF SYST-IDSEKVNR = 1                                               
207500         IF IN-EKH-SUBEL > 0                                              
207600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
207700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
207800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
207900           IF IN-EKH-KDVALISO = 'TRY'                                     
208000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
208100           END-IF                                                         
208200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
208300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
208400           MOVE SPACE               TO WS-ALLOCATE-REF                    
208500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
208600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
208700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
208800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
208900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
209000           PERFORM S04-WRITE-W57033A                                      
209100         END-IF                                                           
209200       END-IF                                                             
209300                                                                          
209400       IF SYST-IDSEKVNR = 2                                               
209500         IF IN-EKH-SUBEL < 0                                              
209600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
209700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
209800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
209900           IF IN-EKH-KDVALISO = 'TRY'                                     
210000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
210100           END-IF                                                         
210200           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
210300           MOVE SPACE               TO WS-ALLOCATE-DC                     
210400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
210500           MOVE SPACE               TO WS-ALLOCATE-REF                    
210600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
210700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
210800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
210900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
211000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
211100           PERFORM S04-WRITE-W57033A                                      
211200         END-IF                                                           
211300       END-IF                                                             
211400                                                                          
211500       IF SYST-IDSEKVNR = 3                                               
211600         IF IN-EKH-SUBEL > 0                                              
211700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
211800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
211900           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
212000           IF IN-EKH-KDVALISO = 'TRY'                                     
212100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
212200           END-IF                                                         
212300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
212400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
212500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
212600           PERFORM S04-WRITE-W57033A                                      
212700         END-IF                                                           
212800       END-IF                                                             
212900                                                                          
213000       IF SYST-IDSEKVNR = 4                                               
213100         IF IN-EKH-SUBEL < 0                                              
213200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
213300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
213400           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
213500           IF IN-EKH-KDVALISO = 'TRY'                                     
213600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
213700           END-IF                                                         
213800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
213900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
214000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
214100           PERFORM S04-WRITE-W57033A                                      
214200         END-IF                                                           
214300       END-IF                                                             
214400                                                                          
214500     WHEN 'SUM'                                                           
214600       IF IN-EKH-SUBEL > ZERO                                             
214700         IF SYST-IDSEKVNR = 1                                             
214800           MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                    
214900           MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                     
215000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
215100           IF IN-EKH-KDVALISO = 'TRY'                                     
215200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
215300           END-IF                                                         
215400           PERFORM S10-VATCODE                                            
215500           IF IN-EKH-SUVAT = ZERO                                         
215600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
215700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
215800           ELSE                                                           
215900             IF IN-EKH-KDVALISO = 'TRY'                                   
216000               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
216100                                      R3-LINE-TAX-AMOUNT-LC               
216200             ELSE                                                         
216300               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
216400                                      R3-LINE-TAX-AMOUNT-LC               
216500               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
216600                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
216700             END-IF                                                       
216800           END-IF                                                         
216900**** CALCULATE NEW SUM WITH VAT                                           
217000           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
217100                   IN-EKH-SUVAT                                           
217200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
217300           IF IN-EKH-KDVALISO = 'TRY'                                     
217400             MOVE R3-LINE-AMOUNT TO R3-LINE-AMOUNT-LC                     
217500           ELSE                                                           
217600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
217700                     R3-LINE-AMOUNT * WS-PRKURS                           
217800           END-IF                                                         
217900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
218000                                                                          
218100           MOVE R3-LINE-TEXT       TO WS-LINE-TEXT                        
218200           MOVE IN-EKH-IDKUNDRF    TO WS-LINE-TEXT-IDKUNDRF               
218300           MOVE WS-LINE-TEXT       TO R3-LINE-TEXT                        
218400           PERFORM S04-WRITE-W57033A                                      
218500         END-IF                                                           
218600       END-IF                                                             
218700                                                                          
218800       IF IN-EKH-SUBEL < ZERO                                             
218900         IF SYST-IDSEKVNR = 2                                             
219000           MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                    
219100           MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                     
219200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
219300           IF IN-EKH-KDVALISO = 'TRY'                                     
219400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
219500           END-IF                                                         
219600           PERFORM S10-VATCODE                                            
219700           IF IN-EKH-SUVAT = ZERO                                         
219800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
219900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
220000           ELSE                                                           
220100             IF IN-EKH-KDVALISO = 'TRY'                                   
220200               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
220300                                      R3-LINE-TAX-AMOUNT-LC               
220400             ELSE                                                         
220500               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
220600                                      R3-LINE-TAX-AMOUNT-LC               
220700               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
220800                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
220900             END-IF                                                       
221000           END-IF                                                         
221100**** CALCULATE NEW SUM WITH VAT                                           
221200           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
221300                   IN-EKH-SUVAT                                           
221400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
221500           IF IN-EKH-KDVALISO = 'TRY'                                     
221600             MOVE R3-LINE-AMOUNT TO R3-LINE-AMOUNT-LC                     
221700           ELSE                                                           
221800             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
221900                     R3-LINE-AMOUNT * WS-PRKURS                           
222000           END-IF                                                         
222100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
222200                                                                          
222300           MOVE R3-LINE-TEXT       TO WS-LINE-TEXT                        
222400           MOVE IN-EKH-IDKUNDRF    TO WS-LINE-TEXT-IDKUNDRF               
222500           MOVE WS-LINE-TEXT       TO R3-LINE-TEXT                        
222600           PERFORM S04-WRITE-W57033A                                      
222700         END-IF                                                           
222800       END-IF                                                             
222900                                                                          
223000     WHEN 'DDI'                                                           
223100       IF IN-EKH-SUBEL > ZERO                                             
223200         IF SYST-IDSEKVNR = 1                                             
223300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
223400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
223500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
223600                   IN-EKH-SUBEL                                           
223700           MOVE ZEROES              TO R3-LINE-AMOUNT                     
223800           IF IN-EKH-KDVALISO = 'TRY'                                     
223900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
224000           END-IF                                                         
224100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
224200           MOVE SPACE               TO R3-LINE-ALLOCATE                   
224300           PERFORM S04-WRITE-W57033A                                      
224400         END-IF                                                           
224500       ELSE                                                               
224600         IF SYST-IDSEKVNR = 2                                             
224700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
224800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
224900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
225000                   IN-EKH-SUBEL                                           
225100           MOVE ZEROES              TO R3-LINE-AMOUNT                     
225200           IF IN-EKH-KDVALISO = 'TRY'                                     
225300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
225400           END-IF                                                         
225500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
225600           MOVE SPACE               TO R3-LINE-ALLOCATE                   
225700           PERFORM S04-WRITE-W57033A                                      
225800         END-IF                                                           
225900       END-IF                                                             
226000                                                                          
226100     END-EVALUATE                                                         
226200     .                                                                    
226300     EJECT                                                                
226400                                                                          
226500 CECB-SUB-EVENT-103-106 SECTION.                                          
226600     EVALUATE IN-EKH-KDEKNIVA                                             
226700     WHEN 'DET'                                                           
226800       IF SYST-IDSEKVNR = 1                                               
226900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
227000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
227100         COMPUTE R3-LINE-AMOUNT-LC =                                      
227200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
227300         IF IN-EKH-KDVALISO = 'TRY'                                       
227400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
227500         END-IF                                                           
227600         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
227700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
227800         MOVE SPACE               TO WS-ALLOCATE-REF                      
227900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
228000         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
228100         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
228200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
228300         PERFORM S04-WRITE-W57033A                                        
228400       END-IF                                                             
228500                                                                          
228600                                                                          
228700     WHEN 'KALK'                                                          
228800       IF SYST-IDSEKVNR = 1                                               
228900         IF IN-EKH-SUBEL > 0                                              
229000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
229100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
229200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
229300           IF IN-EKH-KDVALISO = 'TRY'                                     
229400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
229500           END-IF                                                         
229600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
229700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
229800           MOVE SPACE               TO WS-ALLOCATE-REF                    
229900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
230000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
230100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
230200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
230300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
230400           PERFORM S04-WRITE-W57033A                                      
230500         END-IF                                                           
230600       END-IF                                                             
230700                                                                          
230800       IF SYST-IDSEKVNR = 2                                               
230900         IF IN-EKH-SUBEL < 0                                              
231000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
231100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
231200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
231300           IF IN-EKH-KDVALISO = 'TRY'                                     
231400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
231500           END-IF                                                         
231600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
231700           MOVE SPACE               TO WS-ALLOCATE-DC                     
231800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
231900           MOVE SPACE               TO WS-ALLOCATE-REF                    
232000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
232100           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
232200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
232300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
232400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
232500           PERFORM S04-WRITE-W57033A                                      
232600         END-IF                                                           
232700       END-IF                                                             
232800                                                                          
232900       IF SYST-IDSEKVNR = 3                                               
233000         IF IN-EKH-SUBEL > 0                                              
233100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
233200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
233300           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
233400           IF IN-EKH-KDVALISO = 'TRY'                                     
233500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
233600           END-IF                                                         
233700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
233800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
233900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
234000           PERFORM S04-WRITE-W57033A                                      
234100         END-IF                                                           
234200       END-IF                                                             
234300                                                                          
234400       IF SYST-IDSEKVNR = 4                                               
234500         IF IN-EKH-SUBEL < 0                                              
234600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
234700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
234800           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
234900           IF IN-EKH-KDVALISO = 'TRY'                                     
235000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
235100           END-IF                                                         
235200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
235300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
235400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
235500           PERFORM S04-WRITE-W57033A                                      
235600         END-IF                                                           
235700       END-IF                                                             
235800                                                                          
235900     END-EVALUATE                                                         
236000     .                                                                    
236100     EJECT                                                                
236200                                                                          
236300 CECB-SUB-EVENT-103-107 SECTION.                                          
236400     EVALUATE IN-EKH-KDEKNIVA                                             
236500     WHEN 'DET'                                                           
236600       IF SYST-IDSEKVNR = 1                                               
236700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
236800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
236900         COMPUTE R3-LINE-AMOUNT-LC =                                      
237000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
237100         IF IN-EKH-KDVALISO = 'TRY'                                       
237200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
237300         END-IF                                                           
237400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
237500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
237600         MOVE SPACE               TO WS-ALLOCATE-REF                      
237700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
237800         MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                         
237900         MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF                
238000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
238100         PERFORM S04-WRITE-W57033A                                        
238200       END-IF                                                             
238300                                                                          
238400     WHEN 'KALK'                                                          
238500       IF SYST-IDSEKVNR = 1                                               
238600         IF IN-EKH-SUBEL > 0                                              
238700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
238800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
238900           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
239000           IF IN-EKH-KDVALISO = 'TRY'                                     
239100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
239200           END-IF                                                         
239300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
239400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
239500           MOVE SPACE               TO WS-ALLOCATE-REF                    
239600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
239700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
239800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
239900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
240000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
240100           PERFORM S04-WRITE-W57033A                                      
240200         END-IF                                                           
240300       END-IF                                                             
240400                                                                          
240500       IF SYST-IDSEKVNR = 2                                               
240600         IF IN-EKH-SUBEL < 0                                              
240700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
240800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
240900           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
241000           IF IN-EKH-KDVALISO = 'TRY'                                     
241100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
241200           END-IF                                                         
241300           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
241400           MOVE SPACE               TO WS-ALLOCATE-DC                     
241500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
241600           MOVE SPACE               TO WS-ALLOCATE-REF                    
241700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
241800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
241900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
242000           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
242100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
242200           PERFORM S04-WRITE-W57033A                                      
242300         END-IF                                                           
242400       END-IF                                                             
242500                                                                          
242600       IF SYST-IDSEKVNR = 3                                               
242700         IF IN-EKH-SUBEL > 0                                              
242800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
242900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
243000           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
243100           IF IN-EKH-KDVALISO = 'TRY'                                     
243200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
243300           END-IF                                                         
243400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
243500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
243600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
243700           PERFORM S04-WRITE-W57033A                                      
243800         END-IF                                                           
243900       END-IF                                                             
244000                                                                          
244100       IF SYST-IDSEKVNR = 4                                               
244200         IF IN-EKH-SUBEL < 0                                              
244300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
244400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
244500           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
244600           IF IN-EKH-KDVALISO = 'TRY'                                     
244700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
244800           END-IF                                                         
244900           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
245000           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
245100           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
245200           PERFORM S04-WRITE-W57033A                                      
245300         END-IF                                                           
245400       END-IF                                                             
245500                                                                          
245600     END-EVALUATE                                                         
245700     .                                                                    
245800     EJECT                                                                
245900                                                                          
246000 CED-MAIN-EVENT-201 SECTION.                                              
246100     EVALUATE IN-EKH-KDEKSHT                                              
246200     WHEN '201'                                                           
246300          PERFORM CEDA-SUB-EVENT-201-201                                  
246400     END-EVALUATE                                                         
246500     .                                                                    
246600     EJECT                                                                
246700                                                                          
246800 CEDA-SUB-EVENT-201-201 SECTION.                                          
246900     EVALUATE IN-EKH-KDEKNIVA                                             
247000     WHEN 'DET'                                                           
247100       IF SYST-IDSEKVNR = 1                                               
247200         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
247300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
247400         COMPUTE R3-LINE-AMOUNT-LC =                                      
247500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
247600         IF IN-EKH-KDVALISO = 'TRY'                                       
247700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
247800         END-IF                                                           
247900         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
248000         MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                  
248100         PERFORM S03-WRITE-W57032                                         
248200       END-IF                                                             
248300                                                                          
248400       IF SYST-IDSEKVNR = 2                                               
248500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
248600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
248700         COMPUTE R3-LINE-AMOUNT-LC =                                      
248800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
248900         IF IN-EKH-KDVALISO = 'TRY'                                       
249000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
249100         END-IF                                                           
249200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
249300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
249400         MOVE SPACE               TO WS-ALLOCATE-REF                      
249500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
249600         MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                  
249700         PERFORM S03-WRITE-W57032                                         
249800       END-IF                                                             
249900     END-EVALUATE                                                         
250000     .                                                                    
250100     EJECT                                                                
250200                                                                          
250300 CEF-MAIN-EVENT-203 SECTION.                                              
250400     EVALUATE IN-EKH-KDEKSHT                                              
250500     WHEN '201'                                                           
250600          PERFORM CEFA-SUB-EVENT-203-201                                  
250700     END-EVALUATE                                                         
250800     .                                                                    
250900     EJECT                                                                
251000                                                                          
251100 CEFA-SUB-EVENT-203-201 SECTION.                                          
251200     EVALUATE IN-EKH-KDEKNIVA                                             
251300     WHEN 'DET'                                                           
251400       IF SYST-IDSEKVNR = 1                                               
251500         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
251600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
251700         COMPUTE R3-LINE-AMOUNT-LC =                                      
251800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
251900         IF IN-EKH-KDVALISO = 'TRY'                                       
252000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
252100         END-IF                                                           
252200         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
252300         MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                  
252400         PERFORM S03-WRITE-W57032                                         
252500       END-IF                                                             
252600                                                                          
252700       IF SYST-IDSEKVNR = 2                                               
252800         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
252900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
253000         COMPUTE R3-LINE-AMOUNT-LC =                                      
253100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
253200         IF IN-EKH-KDVALISO = 'TRY'                                       
253300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
253400         END-IF                                                           
253500         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
253600         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
253700         MOVE SPACE             TO WS-ALLOCATE-REF                        
253800         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
253900         MOVE 0000480009        TO R3-LINE-PA-CUSTOMER                    
254000         PERFORM S03-WRITE-W57032                                         
254100       END-IF                                                             
254200     END-EVALUATE                                                         
254300     .                                                                    
254400     EJECT                                                                
254500                                                                          
254600 CEG-MAIN-EVENT-204 SECTION.                                              
254700     EVALUATE IN-EKH-KDEKSHT                                              
254800     WHEN '201'                                                           
254900          PERFORM CEGA-SUB-EVENT-204-201                                  
255000     WHEN '301'                                                           
255100          PERFORM CEGB-SUB-EVENT-204-301                                  
255200     END-EVALUATE                                                         
255300     .                                                                    
255400     EJECT                                                                
255500                                                                          
255600 CEGA-SUB-EVENT-204-201 SECTION.                                          
255700     EVALUATE IN-EKH-KDEKNIVA                                             
255800     WHEN 'DET'                                                           
255900         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
256000* R-FAKTURA                                                               
256100       IF SYST-IDSEKVNR = 1                                               
256200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
256300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
256400         COMPUTE R3-LINE-AMOUNT-LC =                                      
256500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
256600         IF IN-EKH-KDVALISO = 'TRY'                                       
256700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
256800         END-IF                                                           
256900         MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                  
257000         MOVE 'TR  '              TO R3-LINE-TRADING-PARTNER              
257100         MOVE SPACE               TO WS-ALLOCATE-DC                       
257200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
257300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
257400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
257500         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
257600         PERFORM S03-WRITE-W57032                                         
257700       END-IF                                                             
257800                                                                          
257900       IF SYST-IDSEKVNR = 2                                               
258000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
258100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
258200         COMPUTE R3-LINE-AMOUNT-LC =                                      
258300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
258400         IF IN-EKH-KDVALISO = 'TRY'                                       
258500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
258600         END-IF                                                           
258700         MOVE SPACE               TO WS-ALLOCATE-DC                       
258800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
258900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
259000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
259100         MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                  
259200         PERFORM S03-WRITE-W57032                                         
259300       END-IF                                                             
259400                                                                          
259500       IF SYST-IDSEKVNR = 3                                               
259600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
259700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
259800         COMPUTE R3-LINE-AMOUNT-LC =                                      
259900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
260000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
260100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
260200         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
260300         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
260400         MOVE SPACE             TO WS-ALLOCATE-REF                        
260500         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
260600         PERFORM S03-WRITE-W57032                                         
260700       END-IF                                                             
260800                                                                          
260900     END-EVALUATE                                                         
261000     .                                                                    
261100     EJECT                                                                
261200                                                                          
261300 CEGB-SUB-EVENT-204-301 SECTION.                                          
261400     EVALUATE IN-EKH-KDEKNIVA                                             
261500     WHEN 'DET'                                                           
261600* R-FAKTURA                                                               
261700       IF SYST-IDSEKVNR = 1                                               
261800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
261900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
262000         COMPUTE R3-LINE-AMOUNT-LC =                                      
262100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
262200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
262300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
262400         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
262500         PERFORM S03-WRITE-W57032                                         
262600       END-IF                                                             
262700                                                                          
262800       IF SYST-IDSEKVNR = 2                                               
262900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
263000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
263100         COMPUTE R3-LINE-AMOUNT-LC =                                      
263200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
263300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
263400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
263500         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
263600         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
263700         MOVE SPACE             TO WS-ALLOCATE-REF                        
263800         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
263900         PERFORM S03-WRITE-W57032                                         
264000       END-IF                                                             
264100                                                                          
264200       IF SYST-IDSEKVNR = 3                                               
264300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
264400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
264500         COMPUTE R3-LINE-AMOUNT-LC =                                      
264600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
264700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
264800         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
264900         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
265000         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
265100         MOVE SPACE             TO WS-ALLOCATE-REF                        
265200         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
265300         PERFORM S03-WRITE-W57032                                         
265400       END-IF                                                             
265500                                                                          
265600     WHEN 'FÖRS'                                                          
265700     WHEN 'FRAKT'                                                         
265800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
265900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
266000       MOVE SYST-IDKST          TO WS-RED-IDKST                           
266100       IF WS-RED-IDKST > SPACE                                            
266200         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
266300       ELSE                                                               
266400         MOVE SPACE             TO R3-LINE-COST-CENTER                    
266500       END-IF                                                             
266600       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
266700               IN-EKH-SUBEL * -1                                          
266800       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
266900       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
267000       PERFORM S04-WRITE-W57033A                                          
267100                                                                          
267200     WHEN 'EMB'                                                           
267300       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
267400       MOVE SYST-IDKST          TO WS-RED-IDKST                           
267500       IF WS-RED-IDKST > SPACE                                            
267600         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
267700       ELSE                                                               
267800         MOVE SPACE             TO R3-LINE-COST-CENTER                    
267900       END-IF                                                             
268000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
268100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
268200               IN-EKH-SUBEL * -1                                          
268300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
268400       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
268500       PERFORM S04-WRITE-W57033A                                          
268600                                                                          
268700     END-EVALUATE                                                         
268800     .                                                                    
268900     EJECT                                                                
269000                                                                          
269100 CEI-MAIN-EVENT-302 SECTION.                                              
269200     EVALUATE IN-EKH-KDEKSHT                                              
269300     WHEN '301'                                                           
269400          PERFORM CEIA-SUB-EVENT-302-301                                  
269500     WHEN '302'                                                           
269600          PERFORM CEIB-SUB-EVENT-302-302                                  
269700     END-EVALUATE                                                         
269800     .                                                                    
269900     EJECT                                                                
270000                                                                          
270100 CEIA-SUB-EVENT-302-301 SECTION.                                          
270200     EVALUATE IN-EKH-KDEKNIVA                                             
270300     WHEN 'DET'                                                           
270400       IF SYST-IDSEKVNR = 1                                               
270500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
270600         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
270700         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
270800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
270900         COMPUTE R3-LINE-AMOUNT-LC =                                      
271000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
271100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
271200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
271300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
271400         MOVE SPACE               TO WS-ALLOCATE-REF                      
271500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
271600         PERFORM S02-WRITE-W57031A                                        
271700       END-IF                                                             
271800                                                                          
271900       IF SYST-IDSEKVNR = 2                                               
272000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
272100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
272200         MOVE SPACE           TO R3-LINE-COST-CENTER                      
272300         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
272400         COMPUTE R3-LINE-AMOUNT-LC =                                      
272500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
272600         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
272700         MOVE SPACE               TO WS-LINE-TEXT                         
272800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
272900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
273000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
273100         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
273200         PERFORM S02-WRITE-W57031A                                        
273300       END-IF                                                             
273400     END-EVALUATE                                                         
273500     .                                                                    
273600     EJECT                                                                
273700                                                                          
273800 CEIB-SUB-EVENT-302-302 SECTION.                                          
273900     EVALUATE IN-EKH-KDEKNIVA                                             
274000     WHEN 'DET'                                                           
274100       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
274200         IF SYST-IDSEKVNR = 1                                             
274300           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
274400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
274500           IF BET-KDTRADP(3:2) NOT = SPACE                                
274600             MOVE '1'             TO WS-ACCOUNT-4                         
274700           ELSE                                                           
274800             MOVE '3'             TO WS-ACCOUNT-4                         
274900           END-IF                                                         
275000           COMPUTE R3-LINE-AMOUNT-LC  =                                   
275100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
275200           IF IN-EKH-KDVALISO = 'TRY'                                     
275300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
275400           END-IF                                                         
275500           MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                
275600           PERFORM S02-WRITE-W57031A                                      
275700         END-IF                                                           
275800                                                                          
275900         IF SYST-IDSEKVNR = 4                                             
276000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
276100           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
276200           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
276300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
276400           COMPUTE R3-LINE-AMOUNT-LC  =                                   
276500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
276600           IF IN-EKH-KDVALISO = 'TRY'                                     
276700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
276800           END-IF                                                         
276900           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
277000           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
277100           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
277200           MOVE SPACE             TO WS-ALLOCATE-REF                      
277300           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
277400           MOVE 0000480009        TO R3-LINE-PA-CUSTOMER                  
277500           MOVE 'TR  '            TO R3-LINE-TRADING-PARTNER              
277600                                                                          
277700           PERFORM S02-WRITE-W57031A                                      
277800         END-IF                                                           
277900       ELSE                                                               
278000         IF SYST-IDSEKVNR = 2                                             
278100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
278200           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
278300           IF BET-KDTRADP(3:2) NOT = SPACE                                
278400             MOVE '1'             TO WS-ACCOUNT-4                         
278500           ELSE                                                           
278600             MOVE '3'             TO WS-ACCOUNT-4                         
278700           END-IF                                                         
278800           COMPUTE R3-LINE-AMOUNT-LC  =                                   
278900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
279000           IF IN-EKH-KDVALISO = 'TRY'                                     
279100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
279200           END-IF                                                         
279300           MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                
279400           PERFORM S02-WRITE-W57031A                                      
279500         END-IF                                                           
279600                                                                          
279700         IF SYST-IDSEKVNR = 3                                             
279800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
279900           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
280000           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
280100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
280200           COMPUTE R3-LINE-AMOUNT-LC  =                                   
280300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
280400           IF IN-EKH-KDVALISO = 'TRY'                                     
280500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
280600           END-IF                                                         
280700           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
280800           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
280900           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
281000           MOVE SPACE             TO WS-ALLOCATE-REF                      
281100           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
281200           MOVE 0000480009        TO R3-LINE-PA-CUSTOMER                  
281300           MOVE 'TR  '            TO R3-LINE-TRADING-PARTNER              
281400           PERFORM S02-WRITE-W57031A                                      
281500         END-IF                                                           
281600       END-IF                                                             
281700     END-EVALUATE                                                         
281800     .                                                                    
281900     EJECT                                                                
282000                                                                          
282100 CEJ-MAIN-EVENT-303 SECTION.                                              
282200     EVALUATE IN-EKH-KDEKSHT                                              
282300     WHEN '3XX'                                                           
282400          PERFORM CEJ301-SUB-EVENT-303-3XX                                
282500     WHEN '301'                                                           
282600          PERFORM CEJ301-SUB-EVENT-303-301                                
282700     WHEN '307'                                                           
282800          PERFORM CEJ307-SUB-EVENT-303-307                                
282900     WHEN '310'                                                           
283000          PERFORM CEJ310-SUB-EVENT-303-310                                
283100     WHEN '311'                                                           
283200          PERFORM CEJ311-SUB-EVENT-303-311                                
283300     WHEN '371'                                                           
283400          PERFORM CEJ371-SUB-EVENT-303-371                                
283500     WHEN '391'                                                           
283600          PERFORM CEJ301-SUB-EVENT-303-391                                
283700     END-EVALUATE                                                         
283800     .                                                                    
283900     EJECT                                                                
284000                                                                          
284100 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
284200     EVALUATE IN-EKH-KDEKNIVA                                             
284300                                                                          
284400     WHEN 'FÖRS'                                                          
284500     WHEN 'FRAKT'                                                         
284600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
284700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
284800       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
284900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
285000              (IN-EKH-SUBEL * -1) / WS-PRKURS-TR3                         
285100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
285200       MOVE SPACE               TO WS-ALLOCATE-DC                         
285300       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
285400       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
285500       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
285600       PERFORM S03-WRITE-W57032                                           
285700                                                                          
285800     WHEN 'LAND'                                                          
285900       MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                         
286000       IF SYST-IDSEKVNR = 1                                               
286100         IF DIST18-SCRAP-NDC-QUAL                                         
286200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
286300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
286400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
286500                  (IN-EKH-SUBEL * -1) / WS-PRKURS-TR3                     
286600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
286700           MOVE SPACE           TO WS-ALLOCATE-DC                         
286800           MOVE SPACE           TO WS-ALLOCATE-DISTR                      
286900           MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                      
287000           MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                       
287100           PERFORM S03-WRITE-W57032                                       
287200         END-IF                                                           
287300       END-IF                                                             
287400                                                                          
287500       IF SYST-IDSEKVNR = 2                                               
287600         IF DIST18-SCRAP-NDC-QUAL                                         
287700           CONTINUE                                                       
287800         ELSE                                                             
287900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
288000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
288100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
288200                  (IN-EKH-SUBEL * -1) / WS-PRKURS-TR3                     
288300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
288400           MOVE SPACE           TO WS-ALLOCATE-DC                         
288500           MOVE SPACE           TO WS-ALLOCATE-DISTR                      
288600           MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                      
288700           MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                       
288800           PERFORM S03-WRITE-W57032                                       
288900         END-IF                                                           
289000       END-IF                                                             
289100                                                                          
289200     WHEN 'DDI'                                                           
289300       IF IN-EKH-SUBEL < ZERO                                             
289400         IF SYST-IDSEKVNR = 1                                             
289500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
289600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
289700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
289800                   IN-EKH-SUBEL                                           
289900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
290000           MOVE SPACE               TO WS-ALLOCATE-DC                     
290100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
290200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
290300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
290400           PERFORM S04-WRITE-W57033A                                      
290500         END-IF                                                           
290600       ELSE                                                               
290700         IF SYST-IDSEKVNR = 2                                             
290800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
290900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
291000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
291100                   IN-EKH-SUBEL                                           
291200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
291300           MOVE SPACE               TO WS-ALLOCATE-DC                     
291400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
291500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
291600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
291700           PERFORM S04-WRITE-W57033A                                      
291800         END-IF                                                           
291900       END-IF                                                             
292000     END-EVALUATE                                                         
292100     .                                                                    
292200     EJECT                                                                
292300                                                                          
292400 CEJ301-SUB-EVENT-303-301 SECTION.                                        
292500     EVALUATE IN-EKH-KDEKNIVA                                             
292600     WHEN 'DET'                                                           
292700       IF SYST-IDSEKVNR = 1                                               
292800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
292900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
293000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
293100         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3 * -1            
293200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
293300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
293400         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
293500         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
293600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
293700         MOVE SPACE               TO WS-ALLOCATE-DC                       
293800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
293900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
294000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
294100         PERFORM S03-WRITE-W57032                                         
294200       END-IF                                                             
294300                                                                          
294400     END-EVALUATE                                                         
294500     .                                                                    
294600     EJECT                                                                
294700                                                                          
294800 CEJ307-SUB-EVENT-303-307 SECTION.                                        
294900     EVALUATE IN-EKH-KDEKNIVA                                             
295000     WHEN 'DET'                                                           
295100       MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                         
295200       IF SYST-IDSEKVNR = 1                                               
295300         IF DIST18-SCRAP-NDC-QUAL                                         
295400           CONTINUE                                                       
295500         ELSE                                                             
295600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
295700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
295800           COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                           
295900           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3 * -1          
296000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
296100           MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                 
296200           MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                 
296300           MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                 
296400           MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                         
296500           MOVE SPACE               TO WS-ALLOCATE-DC                     
296600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
296700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
296800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
296900           PERFORM S03-WRITE-W57032                                       
297000         END-IF                                                           
297100       END-IF                                                             
297200                                                                          
297300       IF SYST-IDSEKVNR = 2                                               
297400         IF DIST18-SCRAP-NDC-QUAL                                         
297500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
297600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
297700           COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                           
297800           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3 * -1          
297900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
298000           MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                 
298100           MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                 
298200           MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                 
298300           MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                         
298400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
298500           MOVE SPACE               TO WS-ALLOCATE-DC                     
298600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
298700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
298800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
298900           PERFORM S03-WRITE-W57032                                       
299000         END-IF                                                           
299100       END-IF                                                             
299200                                                                          
299300     END-EVALUATE                                                         
299400     .                                                                    
299500     EJECT                                                                
299600                                                                          
299700 CEJ310-SUB-EVENT-303-310 SECTION.                                        
299800     EVALUATE IN-EKH-KDEKNIVA                                             
299900     WHEN 'DET'                                                           
300000       IF SYST-IDSEKVNR = 1                                               
300100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
300200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
300300         COMPUTE R3-LINE-AMOUNT-LC =                                      
300400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
300500         IF IN-EKH-KDVALISO = 'TRY'                                       
300600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
300700         END-IF                                                           
300800         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
300900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
301000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
301100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
301200         MOVE SPACE               TO WS-LINE-TEXT                         
301300         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
301400         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
301500         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
301600         PERFORM S02-WRITE-W57031A                                        
301700       END-IF                                                             
301800                                                                          
301900       IF SYST-IDSEKVNR = 2                                               
302000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
302100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
302200         COMPUTE R3-LINE-AMOUNT-LC =                                      
302300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
302400         IF IN-EKH-KDVALISO = 'TRY'                                       
302500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
302600         END-IF                                                           
302700         MOVE SPACE               TO WS-LINE-TEXT                         
302800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
302900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
303000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
303100         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
303200         MOVE SPACE               TO WS-ALLOCATE-DC                       
303300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
303400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
303500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
303600         PERFORM S02-WRITE-W57031A                                        
303700       END-IF                                                             
303800     END-EVALUATE                                                         
303900     .                                                                    
304000     EJECT                                                                
304100                                                                          
304200 CEJ311-SUB-EVENT-303-311 SECTION.                                        
304300     EVALUATE IN-EKH-KDEKNIVA                                             
304400     WHEN 'DET'                                                           
304500        IF SYST-IDSEKVNR = 1                                              
304600          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
304700          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
304800          COMPUTE R3-LINE-AMOUNT-LC =                                     
304900                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
305000          IF IN-EKH-KDVALISO = 'TRY'                                      
305100            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
305200          END-IF                                                          
305300          MOVE SPACE               TO WS-LINE-TEXT                        
305400          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
305500          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
305600          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
305700          MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                 
305800          MOVE SPACE               TO WS-ALLOCATE-DC                      
305900          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
306000          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
306100          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
306200          PERFORM S02-WRITE-W57031A                                       
306300        END-IF                                                            
306400                                                                          
306500        IF SYST-IDSEKVNR = 2                                              
306600          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
306700          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
306800          COMPUTE R3-LINE-AMOUNT-LC =                                     
306900                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
307000          IF IN-EKH-KDVALISO = 'TRY'                                      
307100            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
307200          END-IF                                                          
307300          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
307400          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
307500          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
307600          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
307700          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
307800          MOVE SPACE               TO WS-LINE-TEXT                        
307900          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
308000          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
308100          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
308200          MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                 
308300          MOVE 'TR  '              TO R3-LINE-TRADING-PARTNER             
308400          PERFORM S02-WRITE-W57031A                                       
308500        END-IF                                                            
308600     END-EVALUATE                                                         
308700     .                                                                    
308800     EJECT                                                                
308900                                                                          
309000 CEJ371-SUB-EVENT-303-371 SECTION.                                        
309100     EVALUATE IN-EKH-KDEKNIVA                                             
309200     WHEN 'DET'                                                           
309300       IF SYST-IDSEKVNR = 1                                               
309400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
309500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
309600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
309700           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TR3               
309800         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
309900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
310000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
310100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
310200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
310300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
310400         PERFORM S04-WRITE-W57033A                                        
310500       END-IF                                                             
310600                                                                          
310700     WHEN 'LAND'                                                          
310800       IF SYST-IDSEKVNR = 1                                               
310900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
311000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
311100         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
311200         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
311300               R3-LINE-AMOUNT-LC / WS-PRKURS-TR3                          
311400         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
311500         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
311600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
311700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
311800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
311900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
312000         PERFORM S04-WRITE-W57033A                                        
312100       END-IF                                                             
312200                                                                          
312300     WHEN 'DDI'                                                           
312400       IF IN-EKH-SUBEL > ZERO                                             
312500         IF SYST-IDSEKVNR = 1                                             
312600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
312700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
312800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
312900                   IN-EKH-SUBEL                                           
313000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
313100           MOVE SPACE               TO WS-ALLOCATE-DC                     
313200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
313300           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
313400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
313500           PERFORM S04-WRITE-W57033A                                      
313600         END-IF                                                           
313700       ELSE                                                               
313800         IF SYST-IDSEKVNR = 2                                             
313900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
314000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
314100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
314200                   IN-EKH-SUBEL                                           
314300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
314400           MOVE SPACE               TO WS-ALLOCATE-DC                     
314500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
314600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
314700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
314800           PERFORM S04-WRITE-W57033A                                      
314900         END-IF                                                           
315000       END-IF                                                             
315100     END-EVALUATE                                                         
315200     .                                                                    
315300     EJECT                                                                
315400                                                                          
315500 CEJ301-SUB-EVENT-303-391 SECTION.                                        
315600     EVALUATE IN-EKH-KDEKNIVA                                             
315700     WHEN 'DET'                                                           
315800       IF SYST-IDSEKVNR = 1                                               
315900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
316000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
316100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
316200              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
316300         IF IN-EKH-KDVALISO = 'TRY'                                       
316400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
316500         END-IF                                                           
316600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
316700         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
316800         MOVE SPACE               TO WS-LINE-TEXT                         
316900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
317000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
317100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
317200         MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                  
317300         MOVE 'TR  '              TO R3-LINE-TRADING-PARTNER              
317400         MOVE SPACE               TO WS-ALLOCATE-DC                       
317500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
317600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
317700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
317800         PERFORM S03-WRITE-W57032                                         
317900       END-IF                                                             
318000                                                                          
318100       IF SYST-IDSEKVNR = 2                                               
318200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
318300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
318400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
318500              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
318600         IF IN-EKH-KDVALISO = 'TRY'                                       
318700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
318800         END-IF                                                           
318900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
319000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
319100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
319200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
319300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
319400         MOVE SPACE               TO WS-LINE-TEXT                         
319500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
319600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
319700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
319800         MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                  
319900         PERFORM S03-WRITE-W57032                                         
320000       END-IF                                                             
320100                                                                          
320200     END-EVALUATE                                                         
320300     .                                                                    
320400     EJECT                                                                
320500                                                                          
320600 CEK-MAIN-EVENT-401 SECTION.                                              
320700     EVALUATE IN-EKH-KDEKNIVA                                             
320800                                                                          
320900* PRISÄNDRING LÖPANDE                                                     
321000     WHEN 'DET'                                                           
321100       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
321200                           IN-EKH-PRARTSTD                                
321300       IF WS-BELOPP > 0                                                   
321400* PRISHÖJNING                                                             
321500         IF SYST-IDSEKVNR = 1                                             
321600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
321700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
321800           COMPUTE R3-LINE-AMOUNT-LC =                                    
321900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
322000           IF IN-EKH-KDVALISO = 'TRY'                                     
322100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
322200           END-IF                                                         
322300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
322400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
322500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
322600           MOVE SPACE               TO WS-ALLOCATE-REF                    
322700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
322800           MOVE 'TR  '              TO R3-LINE-TRADING-PARTNER            
322900           MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                
323000           PERFORM S02-WRITE-W57031A                                      
323100         END-IF                                                           
323200                                                                          
323300* PRISHÖJNING                                                             
323400         IF SYST-IDSEKVNR = 2                                             
323500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
323600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
323700           COMPUTE R3-LINE-AMOUNT-LC =                                    
323800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
323900           IF IN-EKH-KDVALISO = 'TRY'                                     
324000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
324100           END-IF                                                         
324200           MOVE SPACES              TO R3-LINE-TRADING-PARTNER            
324300           MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                
324400           PERFORM S02-WRITE-W57031A                                      
324500         END-IF                                                           
324600       END-IF                                                             
324700                                                                          
324800       IF WS-BELOPP < 0                                                   
324900* PRISSÄNKNING                                                            
325000         IF SYST-IDSEKVNR = 3                                             
325100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
325200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
325300           COMPUTE R3-LINE-AMOUNT-LC =                                    
325400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
325500           IF IN-EKH-KDVALISO = 'TRY'                                     
325600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
325700           END-IF                                                         
325800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
325900           MOVE 'TR  '              TO R3-LINE-TRADING-PARTNER            
326000           MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                
326100           PERFORM S02-WRITE-W57031A                                      
326200         END-IF                                                           
326300                                                                          
326400* PRISSÄKNING                                                             
326500         IF SYST-IDSEKVNR = 4                                             
326600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
326700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
326800           COMPUTE R3-LINE-AMOUNT-LC =                                    
326900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
327000           IF IN-EKH-KDVALISO = 'TRY'                                     
327100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
327200           END-IF                                                         
327300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
327400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
327500           MOVE SPACE               TO WS-ALLOCATE-REF                    
327600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
327700           MOVE 0000480009          TO R3-LINE-PA-CUSTOMER                
327800           PERFORM S02-WRITE-W57031A                                      
327900         END-IF                                                           
328000       END-IF                                                             
328100                                                                          
328200     END-EVALUATE                                                         
328300     .                                                                    
328400     EJECT                                                                
328500                                                                          
328600 CEL-MAIN-EVENT-402 SECTION.                                              
328700     EVALUATE IN-EKH-KDEKNIVA                                             
328800     WHEN 'DET'                                                           
328900       IF SYST-IDSEKVNR = 1                                               
329000         IF IN-EKH-KVANTAL > 0                                            
329100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
329200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
329300           COMPUTE R3-LINE-AMOUNT-LC =                                    
329400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
329500           IF IN-EKH-KDVALISO = 'TRY'                                     
329600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
329700           END-IF                                                         
329800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
329900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
330000           MOVE SPACE               TO WS-ALLOCATE-REF                    
330100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
330200           PERFORM S02-WRITE-W57031A                                      
330300         END-IF                                                           
330400       END-IF                                                             
330500                                                                          
330600       IF SYST-IDSEKVNR = 2                                               
330700         IF IN-EKH-KVANTAL < 0                                            
330800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
330900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
331000           COMPUTE R3-LINE-AMOUNT-LC =                                    
331100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
331200           IF IN-EKH-KDVALISO = 'TRY'                                     
331300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
331400           END-IF                                                         
331500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
331600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
331700           MOVE SPACE               TO WS-ALLOCATE-REF                    
331800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
331900           PERFORM S02-WRITE-W57031A                                      
332000         END-IF                                                           
332100       END-IF                                                             
332200     END-EVALUATE                                                         
332300     .                                                                    
332400     EJECT                                                                
332500                                                                          
332600 CEM-MAIN-EVENT-403 SECTION.                                              
332700     EVALUATE IN-EKH-KDEKSHT                                              
332800     WHEN '401'                                                           
332900     WHEN '402'                                                           
333000     WHEN '403'                                                           
333100     WHEN '404'                                                           
333200     WHEN '405'                                                           
333300     WHEN '407'                                                           
333400     WHEN '408'                                                           
333500     WHEN '409'                                                           
333600          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
333700     END-EVALUATE                                                         
333800     .                                                                    
333900     EJECT                                                                
334000                                                                          
334100 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
334200     EVALUATE IN-EKH-KDEKNIVA                                             
334300     WHEN 'DET'                                                           
334400       IF IN-EKH-KVANTAL < 0                                              
334500         IF SYST-IDSEKVNR = 1                                             
334600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
334700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
334800           COMPUTE R3-LINE-AMOUNT-LC =                                    
334900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
335000           IF IN-EKH-KDVALISO = 'TRY'                                     
335100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
335200           END-IF                                                         
335300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
335400           PERFORM S02-WRITE-W57031A                                      
335500         END-IF                                                           
335600                                                                          
335700         IF SYST-IDSEKVNR = 2                                             
335800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
335900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
336000           COMPUTE R3-LINE-AMOUNT-LC =                                    
336100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
336200           IF IN-EKH-KDVALISO = 'TRY'                                     
336300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
336400           END-IF                                                         
336500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
336600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
336700           MOVE SPACE               TO WS-ALLOCATE-REF                    
336800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
336900           PERFORM S02-WRITE-W57031A                                      
337000         END-IF                                                           
337100       END-IF                                                             
337200                                                                          
337300       IF IN-EKH-KVANTAL > 0                                              
337400         IF SYST-IDSEKVNR = 3                                             
337500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
337600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
337700           COMPUTE R3-LINE-AMOUNT-LC =                                    
337800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
337900           IF IN-EKH-KDVALISO = 'TRY'                                     
338000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
338100           END-IF                                                         
338200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
338300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
338400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
338500           MOVE SPACE               TO WS-ALLOCATE-REF                    
338600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
338700           PERFORM S02-WRITE-W57031A                                      
338800         END-IF                                                           
338900                                                                          
339000         IF SYST-IDSEKVNR = 4                                             
339100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
339200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
339300           COMPUTE R3-LINE-AMOUNT-LC =                                    
339400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
339500           IF IN-EKH-KDVALISO = 'TRY'                                     
339600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
339700           END-IF                                                         
339800           PERFORM S02-WRITE-W57031A                                      
339900         END-IF                                                           
340000       END-IF                                                             
340100                                                                          
340200     END-EVALUATE                                                         
340300     .                                                                    
340400     EJECT                                                                
340500                                                                          
340600 CEN-MAIN-EVENT-404 SECTION.                                              
340700     EVALUATE IN-EKH-KDEKNIVA                                             
340800     WHEN 'DET'                                                           
340900       IF SYST-IDSEKVNR = 1                                               
341000* KONTO EJ MANUELLT REGISTRERAT                                           
341100         IF IN-EKH-IDKONTO = 0                                            
341200           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
341300           IF DIST18-SCRAP-NDC-SC                                         
341400           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
341500*          OR DIST18-SCRAP-NDC-QUAL                                       
341600             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
341700             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
341800             COMPUTE R3-LINE-AMOUNT-LC =                                  
341900                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
342000             IF IN-EKH-KDVALISO = 'TRY'                                   
342100               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
342200             END-IF                                                       
342300             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
342400             PERFORM S02-WRITE-W57031A                                    
342500           END-IF                                                         
342600         END-IF                                                           
342700       END-IF                                                             
342800                                                                          
342900       IF SYST-IDSEKVNR = 2                                               
343000* KONTO MANUELLT REGISTRERAT                                              
343100         IF IN-EKH-IDKONTO > 0                                            
343200           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
343300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
343400           COMPUTE R3-LINE-AMOUNT-LC =                                    
343500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
343600           IF IN-EKH-KDVALISO = 'TRY'                                     
343700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
343800           END-IF                                                         
343900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
344000           PERFORM S02-WRITE-W57031A                                      
344100         END-IF                                                           
344200       END-IF                                                             
344300                                                                          
344400       IF SYST-IDSEKVNR = 3                                               
344500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
344600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
344700         COMPUTE R3-LINE-AMOUNT-LC =                                      
344800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
344900         IF IN-EKH-KDVALISO = 'TRY'                                       
345000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
345100         END-IF                                                           
345200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
345300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
345400         MOVE SPACE               TO WS-ALLOCATE-REF                      
345500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
345600         PERFORM S02-WRITE-W57031A                                        
345700       END-IF                                                             
345800                                                                          
345900       IF SYST-IDSEKVNR = 4                                               
346000         IF IN-EKH-IDKONTO = 0                                            
346100           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
346200           IF DIST18-SCRAP-NDC-QUAL                                       
346300             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
346400             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
346500             COMPUTE R3-LINE-AMOUNT-LC =                                  
346600                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
346700             IF IN-EKH-KDVALISO = 'TRY'                                   
346800               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
346900             END-IF                                                       
347000             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
347100             PERFORM S02-WRITE-W57031A                                    
347200           END-IF                                                         
347300         END-IF                                                           
347400       END-IF                                                             
347500                                                                          
347600                                                                          
347700     END-EVALUATE                                                         
347800     .                                                                    
347900     EJECT                                                                
348000                                                                          
348100 CF-BUILD-COMMON-210-PART SECTION.                                        
348200     MOVE SPACE              TO R3-LINE-R3                                
348300     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
348400                                R3-LINE-DUE-DATE                          
348500                                R3-LINE-AMOUNT                            
348600                                R3-LINE-AMOUNT-LC                         
348700                                R3-LINE-TAX-AMOUNT                        
348800                                R3-LINE-TAX-AMOUNT-LC                     
348900                                R3-LINE-NUMBER-OF-DAYS                    
349000                                R3-LINE-QUANTITY                          
349100                                R3-LINE-SAMNR                             
349200     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
349300     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
349400     MOVE 'TR02'             TO R3-LINE-COMPANY-CODE                      
349500     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
349600     IF SYST-KDPOST = '31'                                                
349700       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
349800     ELSE                                                                 
349900       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
350000     END-IF                                                               
350100     .                                                                    
350200     EJECT                                                                
350300                                                                          
350400 CG-SCHEDULE-LINE-AP SECTION.                                             
350500     MOVE NEJ                     TO WS-HEADER-SW                         
350600     MOVE JA                      TO WS-LINE-SW                           
350700     EVALUATE IN-EKH-KDEKHHT                                              
350800     WHEN '102'                                                           
350900       IF IN-EKH-KDEKSHT = '130'                                          
351000       OR IN-EKH-KDEKSHT = '134'                                          
351100         IF IN-EKH-KDEKSHT = '130'                                        
351200           PERFORM CGA-MAIN-EVENT-102-130                                 
351300         ELSE                                                             
351400           PERFORM CGA-MAIN-EVENT-102-134                                 
351500         END-IF                                                           
351600       ELSE                                                               
351700         IF IN-EKH-KDEKSHT = '120'                                        
351800         OR IN-EKH-KDEKSHT = '124'                                        
351900         OR IN-EKH-KDEKSHT = '125'                                        
352000           IF IN-EKH-KDEKSHT = '125'                                      
352100             PERFORM CGA-MAIN-EVENT-102-125                               
352200           ELSE                                                           
352300             PERFORM CGA-MAIN-EVENT-102-12X                               
352400           END-IF                                                         
352500         ELSE                                                             
352600           PERFORM CGA-MAIN-EVENT-102                                     
352700         END-IF                                                           
352800       END-IF                                                             
352900     WHEN '103'                                                           
353000         PERFORM CGA-MAIN-EVENT-103                                       
353100     WHEN '303'                                                           
353200       IF IN-EKH-KDEKSHT = '371'                                          
353300         PERFORM S81-GET-CURRENCY-RATE                                    
353400         PERFORM CGA-MAIN-EVENT-303-371                                   
353500       ELSE                                                               
353600         IF IN-EKH-KDEKSHT = '3XX'                                        
353700           PERFORM S81-GET-CURRENCY-RATE                                  
353800           PERFORM CGA-MAIN-EVENT-303-3XX                                 
353900         ELSE                                                             
354000           PERFORM CGA-MAIN-EVENT-303                                     
354100         END-IF                                                           
354200       END-IF                                                             
354300     END-EVALUATE                                                         
354400     .                                                                    
354500     EJECT                                                                
354600                                                                          
354700 CGA-MAIN-EVENT-102     SECTION.                                          
354800     EVALUATE IN-EKH-KDEKNIVA                                             
354900     WHEN 'SUM'                                                           
355000       IF IN-EKH-SUBEL > ZERO                                             
355100         IF SYST-IDSEKVNR = 1                                             
355200           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
355300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
355400            IN-EKH-SUBEL                                                  
355500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
355600           PERFORM S10-VATCODE                                            
355700           IF IN-EKH-SUVAT = ZERO                                         
355800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
355900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
356000           ELSE                                                           
356100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
356200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
356300           END-IF                                                         
356400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
356500                                                                          
356600           PERFORM S04-WRITE-W57033A                                      
356700         END-IF                                                           
356800       END-IF                                                             
356900                                                                          
357000       IF IN-EKH-SUBEL < ZERO                                             
357100         IF SYST-IDSEKVNR = 2                                             
357200           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
357300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
357400           IN-EKH-SUBEL                                                   
357500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
357600           PERFORM S10-VATCODE                                            
357700           IF IN-EKH-SUVAT = ZERO                                         
357800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
357900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
358000           ELSE                                                           
358100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
358200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
358300           END-IF                                                         
358400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
358500                                                                          
358600           PERFORM S04-WRITE-W57033A                                      
358700         END-IF                                                           
358800       END-IF                                                             
358900     END-EVALUATE                                                         
359000     .                                                                    
359100     EJECT                                                                
359200                                                                          
359300 CGA-MAIN-EVENT-102-12X SECTION.                                          
359400     EVALUATE IN-EKH-KDEKNIVA                                             
359500     WHEN 'SUM'                                                           
359600       IF IN-EKH-SUBEL > ZERO                                             
359700         IF SYST-IDSEKVNR = 1                                             
359800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
359900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
360000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
360100                   R3-LINE-AMOUNT    / WS-PRKURS-TR  * -1                 
360200           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
360300           IF IN-EKH-KDEKSHT = '124'                                      
360400             MOVE 'O9'             TO R3-LINE-TAX-CODE                    
360500           ELSE                                                           
360600             MOVE 'T0'             TO R3-LINE-TAX-CODE                    
360700           END-IF                                                         
360800           IF IN-EKH-SUVAT = ZERO                                         
360900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
361000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
361100           ELSE                                                           
361200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
361300             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
361400                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TR  * -1           
361500             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
361600           END-IF                                                         
361700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
361800                                                                          
361900           PERFORM S04-WRITE-W57033A                                      
362000         END-IF                                                           
362100       END-IF                                                             
362200                                                                          
362300       IF IN-EKH-SUBEL < ZERO                                             
362400         IF SYST-IDSEKVNR = 2                                             
362500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
362600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
362700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
362800                   R3-LINE-AMOUNT    / WS-PRKURS-TR  * -1                 
362900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
363000           IF IN-EKH-KDEKSHT = '124'                                      
363100             MOVE 'O9'             TO R3-LINE-TAX-CODE                    
363200           ELSE                                                           
363300             MOVE 'T0'             TO R3-LINE-TAX-CODE                    
363400           END-IF                                                         
363500           IF IN-EKH-SUVAT = ZERO                                         
363600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
363700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
363800           ELSE                                                           
363900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
364000             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
364100                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TR  * -1           
364200             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
364300           END-IF                                                         
364400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
364500           MOVE SPACE           TO R3-LINE-COST-CENTER                    
364600                                                                          
364700           PERFORM S04-WRITE-W57033A                                      
364800         END-IF                                                           
364900       END-IF                                                             
365000     END-EVALUATE                                                         
365100     .                                                                    
365200     EJECT                                                                
365300                                                                          
365400                                                                          
365500 CGA-MAIN-EVENT-102-125 SECTION.                                          
365600     EVALUATE IN-EKH-KDEKNIVA                                             
365700     WHEN 'SUM'                                                           
365800       IF IN-EKH-SUBEL > ZERO                                             
365900         IF SYST-IDSEKVNR = 1                                             
366000           MOVE ZERO TO SPAR-SUMMA-102-125                                
366100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
366200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
366300           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
366400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
366500                   R3-LINE-AMOUNT    / WS-PRKURS-TR  * -1                 
366600           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
366700           MOVE 'T0'               TO R3-LINE-TAX-CODE                    
366800           IF IN-EKH-SUVAT = ZERO                                         
366900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
367000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
367100           ELSE                                                           
367200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
367300             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
367400               R3-LINE-TAX-AMOUNT / WS-PRKURS-TR  * -1                    
367500             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
367600           END-IF                                                         
367700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
367800           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
367900                                                                          
368000           PERFORM S04-WRITE-W57033A                                      
368100         END-IF                                                           
368200       END-IF                                                             
368300                                                                          
368400       IF IN-EKH-SUBEL < ZERO                                             
368500         IF SYST-IDSEKVNR = 2                                             
368600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
368700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
368800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
368900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
369000                   R3-LINE-AMOUNT    / WS-PRKURS-TR  * -1                 
369100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
369200           MOVE 'T0'               TO R3-LINE-TAX-CODE                    
369300           IF IN-EKH-SUVAT = ZERO                                         
369400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
369500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
369600           ELSE                                                           
369700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
369800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
369900               R3-LINE-TAX-AMOUNT / WS-PRKURS-TR * -1                     
370000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
370100           END-IF                                                         
370200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
370300           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
370400                                                                          
370500           PERFORM S04-WRITE-W57033A                                      
370600         END-IF                                                           
370700       END-IF                                                             
370800     END-EVALUATE                                                         
370900     .                                                                    
371000     EJECT                                                                
371100                                                                          
371200 CGA-MAIN-EVENT-102-130 SECTION.                                          
371300     EVALUATE IN-EKH-KDEKNIVA                                             
371400     WHEN 'SUM'                                                           
371500       IF IN-EKH-SUBEL > ZERO                                             
371600         IF SYST-IDSEKVNR = 1                                             
371700           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
371800           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
371900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
372000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
372100                   R3-LINE-AMOUNT    / WS-PRKURS-TR  * -1                 
372200           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
372300           PERFORM S10-VATCODE                                            
372400           IF IN-EKH-SUVAT = ZERO                                         
372500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
372600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
372700           ELSE                                                           
372800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
372900             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
373000                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TR  * -1           
373100             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
373200           END-IF                                                         
373300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
373400                                                                          
373500           PERFORM S04-WRITE-W57033A                                      
373600         END-IF                                                           
373700       END-IF                                                             
373800                                                                          
373900       IF IN-EKH-SUBEL < ZERO                                             
374000         IF SYST-IDSEKVNR = 2                                             
374100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
374200           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
374300           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
374400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
374500                   R3-LINE-AMOUNT    / WS-PRKURS-TR                       
374600           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
374700           PERFORM S10-VATCODE                                            
374800           IF IN-EKH-SUVAT = ZERO                                         
374900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
375000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
375100           ELSE                                                           
375200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
375300             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
375400                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TR                 
375500             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
375600           END-IF                                                         
375700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
375800                                                                          
375900           PERFORM S04-WRITE-W57033A                                      
376000         END-IF                                                           
376100       END-IF                                                             
376200     END-EVALUATE                                                         
376300     .                                                                    
376400     EJECT                                                                
376500                                                                          
376600 CGA-MAIN-EVENT-102-134 SECTION.                                          
376700     EVALUATE IN-EKH-KDEKNIVA                                             
376800     WHEN 'SUM'                                                           
376900       IF IN-EKH-SUBEL > ZERO                                             
377000         IF SYST-IDSEKVNR = 1                                             
377100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
377200           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
377300           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
377400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
377500                   R3-LINE-AMOUNT    / WS-PRKURS-TR  * -1                 
377600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
377700           PERFORM S10-VATCODE                                            
377800           IF IN-EKH-SUVAT = ZERO                                         
377900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
378000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
378100           ELSE                                                           
378200             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
378300             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
378400                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TR  * -1           
378500             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
378600           END-IF                                                         
378700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
378800                                                                          
378900           PERFORM S04-WRITE-W57033A                                      
379000         END-IF                                                           
379100       END-IF                                                             
379200                                                                          
379300       IF IN-EKH-SUBEL < ZERO                                             
379400         IF SYST-IDSEKVNR = 2                                             
379500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
379600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
379700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
379800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
379900                   R3-LINE-AMOUNT    / WS-PRKURS-TR                       
380000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
380100           PERFORM S10-VATCODE                                            
380200           IF IN-EKH-SUVAT = ZERO                                         
380300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
380400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
380500           ELSE                                                           
380600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
380700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
380800                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TR                 
380900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
381000           END-IF                                                         
381100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
381200                                                                          
381300           PERFORM S04-WRITE-W57033A                                      
381400         END-IF                                                           
381500       END-IF                                                             
381600     END-EVALUATE                                                         
381700     .                                                                    
381800     EJECT                                                                
381900                                                                          
382000 CGA-MAIN-EVENT-103     SECTION.                                          
382100     EVALUATE IN-EKH-KDEKNIVA                                             
382200     WHEN 'SUM'                                                           
382300       IF IN-EKH-SUBEL > ZERO                                             
382400         IF SYST-IDSEKVNR = 1                                             
382500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
382600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
382700           IF IN-EKH-KDVALISO = 'TRY'                                     
382800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
382900           END-IF                                                         
383000           PERFORM S10-VATCODE                                            
383100           IF IN-EKH-KDEKSHT = '102'                                      
383200             MOVE 'O3'             TO R3-LINE-TAX-CODE                    
383300             COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.20           
383400           END-IF                                                         
383500           IF IN-EKH-SUVAT = ZERO                                         
383600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
383700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
383800           ELSE                                                           
383900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
384000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
384100           END-IF                                                         
384200**** CALCULATE NEW SUM WITH VAT                                           
384300           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
384400                   IN-EKH-SUVAT                                           
384500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
384600           IF IN-EKH-KDVALISO = 'TRY'                                     
384700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
384800           END-IF                                                         
384900           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
385000                                                                          
385100           PERFORM S04-WRITE-W57033A                                      
385200         END-IF                                                           
385300       END-IF                                                             
385400                                                                          
385500       IF IN-EKH-SUBEL < ZERO                                             
385600         IF SYST-IDSEKVNR = 1                                             
385700           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
385800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
385900           IF IN-EKH-KDVALISO = 'TRY'                                     
386000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
386100           END-IF                                                         
386200           IF IN-EKH-KDEKSHT = '102'                                      
386300             MOVE 'O3'             TO R3-LINE-TAX-CODE                    
386400             COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.20           
386500           END-IF                                                         
386600           IF IN-EKH-SUVAT = ZERO                                         
386700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
386800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
386900           ELSE                                                           
387000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
387100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
387200           END-IF                                                         
387300           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
387400                   IN-EKH-SUVAT                                           
387500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
387600           IF IN-EKH-KDVALISO = 'TRY'                                     
387700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
387800           END-IF                                                         
387900           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
388000                                                                          
388100           PERFORM S04-WRITE-W57033A                                      
388200         END-IF                                                           
388300       END-IF                                                             
388400     END-EVALUATE                                                         
388500     .                                                                    
388600     EJECT                                                                
388700                                                                          
388800 CGA-MAIN-EVENT-303 SECTION.                                              
388900     EVALUATE IN-EKH-KDEKNIVA                                             
389000     WHEN 'SUM'                                                           
389100       IF SYST-IDSEKVNR = 1                                               
389200         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
389300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
389400         (IN-EKH-SUBEL / WS-PRKURS-TR)                                    
389500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
389600         MOVE '  '     TO R3-LINE-TAX-CODE                                
389700         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
389800         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
389900                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-TR                     
390000         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
390100                                                                          
390200         PERFORM S04-WRITE-W57033A                                        
390300       END-IF                                                             
390400     END-EVALUATE                                                         
390500     .                                                                    
390600     EJECT                                                                
390700                                                                          
390800 CGA-MAIN-EVENT-303-3XX SECTION.                                          
390900     EVALUATE IN-EKH-KDEKNIVA                                             
391000     WHEN 'SUM'                                                           
391100       IF SYST-IDSEKVNR = 1                                               
391200         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
391300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
391400         (IN-EKH-SUBEL / WS-PRKURS-TR3)                                   
391500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
391600         MOVE '  '     TO R3-LINE-TAX-CODE                                
391700         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
391800         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
391900                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-TR3                    
392000         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
392100                                                                          
392200         PERFORM S04-WRITE-W57033A                                        
392300       END-IF                                                             
392400     END-EVALUATE                                                         
392500     .                                                                    
392600     EJECT                                                                
392700                                                                          
392800 CGA-MAIN-EVENT-303-371 SECTION.                                          
392900     EVALUATE IN-EKH-KDEKNIVA                                             
393000     WHEN 'SUM'                                                           
393100       IF SYST-IDSEKVNR = 1                                               
393200         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
393300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
393400         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
393500         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
393600               R3-LINE-AMOUNT-LC / WS-PRKURS-TR3                          
393700         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
393800         PERFORM S10-VATCODE                                              
393900         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
394000         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
394100                 R3-LINE-TAX-AMOUNT-LC * WS-PRKURS-TR2                    
394200         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
394300                                                                          
394400         PERFORM S04-WRITE-W57033A                                        
394500       END-IF                                                             
394600     END-EVALUATE                                                         
394700     .                                                                    
394800     EJECT                                                                
394900                                                                          
395000 CH-BUILD-COMMON-310-PART SECTION.                                        
395100     MOVE SPACE              TO R3-LINE-R3                                
395200     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
395300                                R3-LINE-DUE-DATE                          
395400                                R3-LINE-AMOUNT                            
395500                                R3-LINE-AMOUNT-LC                         
395600                                R3-LINE-TAX-AMOUNT                        
395700                                R3-LINE-TAX-AMOUNT-LC                     
395800                                R3-LINE-NUMBER-OF-DAYS                    
395900                                R3-LINE-QUANTITY                          
396000                                R3-LINE-SAMNR                             
396100     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
396200     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
396300     MOVE 'TR02'             TO R3-LINE-COMPANY-CODE                      
396400     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
396500     IF SYST-KDPOST = '31'                                                
396600       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
396700     ELSE                                                                 
396800       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
396900     END-IF                                                               
397000     .                                                                    
397100     EJECT                                                                
397200                                                                          
397300 CI-SCHEDULE-LINE-AR SECTION.                                             
397400     MOVE NEJ                     TO WS-HEADER-SW                         
397500     MOVE JA                      TO WS-LINE-SW                           
397600     EVALUATE IN-EKH-KDEKHHT                                              
397700     WHEN '204'                                                           
397800         PERFORM CIA-MAIN-EVENT-204                                       
397900     END-EVALUATE                                                         
398000     .                                                                    
398100     EJECT                                                                
398200                                                                          
398300 CIA-MAIN-EVENT-204     SECTION.                                          
398400     EVALUATE IN-EKH-KDEKNIVA                                             
398500     WHEN 'SUM'                                                           
398600       IF IN-EKH-SUBEL > ZERO                                             
398700         IF SYST-IDSEKVNR = 1                                             
398800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
398900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
399000           IF IN-EKH-KDVALISO = 'TRY'                                     
399100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
399200           END-IF                                                         
399300           PERFORM S10-VATCODE                                            
399400           IF IN-EKH-SUVAT = ZERO                                         
399500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
399600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
399700           ELSE                                                           
399800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
399900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
400000           END-IF                                                         
400100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
400200                                                                          
400300           PERFORM S04-WRITE-W57033A                                      
400400         END-IF                                                           
400500       END-IF                                                             
400600                                                                          
400700     END-EVALUATE                                                         
400800     .                                                                    
400900     EJECT                                                                
401000                                                                          
401100 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
401200     MOVE ZERO             TO LOGG-W57073                                 
401300     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
401400     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
401500     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
401600     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
401700     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
401800     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
401900     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
402000     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
402100     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
402200     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
402300     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
402400     MOVE 'TR02'           TO LOGG-KDTRADP                                
402500                                                                          
402600****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
402700     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
402800     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
402900     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
403000     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
403100     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
403200     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
403300     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
403400     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
403500     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
403600     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
403700     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
403800     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
403900     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
404000     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
404100     .                                                                    
404200     EJECT                                                                
404300                                                                          
404400 Z-FINI SECTION.                                                          
404500     CLOSE W57066                                                         
404600           W57038                                                         
404700           W57031A                                                        
404800           W57032A                                                        
404900           W57033A                                                        
405000           W57035                                                         
405100           W5703N                                                         
405200           W51380                                                         
405300                                                                          
405400     MOVE 'S' TO POSTSUM-OPKOD                                            
405500     CALL POSTSUM USING POSTSUM-PARM                                      
405600     .                                                                    
405700     EJECT                                                                
405800                                                                          
405900 S01-READ-W57066  SECTION.                                                
406000     READ W57066 INTO IN-AREA                                             
406100     AT END                                                               
406200        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
406300        SET END-OF-W57066 TO TRUE                                         
406400                                                                          
406500     NOT AT END                                                           
406600        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
406700        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
406800        MOVE 'W57038D1'   TO POSTSUM-DDNAMN2                              
406900        CALL POSTSUM USING POSTSUM-PARM                                   
407000     END-READ                                                             
407100     .                                                                    
407200                                                                          
407300 S02-WRITE-W57031A SECTION.                                               
407400     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
407500     MOVE SPACE                 TO 71LINE-POST                            
407600     IF WS-LINE-SW = JA                                                   
407700       IF IN-EKH-KDSORT = 'SW'                                            
407800         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
407900         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
408000         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
408100       ELSE                                                               
408200         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
408300         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
408400         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
408500       END-IF                                                             
408600       WRITE 71LINE-POST        FROM R3-LINE-R3                           
408700       PERFORM S20-CREATE-WRITE-LOG                                       
408800     ELSE                                                                 
408900       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
409000     END-IF                                                               
409100                                                                          
409200     IF WS-LINE-SW = JA                                                   
409300       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
409400     ELSE                                                                 
409500       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
409600     END-IF                                                               
409700     MOVE 'W57031A'             TO POSTSUM-FDNAMN                         
409800     MOVE 'W57038D2'            TO POSTSUM-DDNAMN2                        
409900     CALL POSTSUM USING POSTSUM-PARM                                      
410000     .                                                                    
410100                                                                          
410200 S002-WRITE-W57031A-HEAD SECTION.                                         
410300     MOVE SPACE                 TO 71LINE-POST                            
410400     IF IN-EKH-KDSORT = 'SW'                                              
410500       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
410600       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
410700       MOVE WS-TEXT           TO R3-LINE-TEXT                             
410800     ELSE                                                                 
410900       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
411000       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
411100       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
411200     END-IF                                                               
411300     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
411400                                                                          
411500     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
411600     MOVE 'W57031A'             TO POSTSUM-FDNAMN                         
411700     MOVE 'W57038D2'            TO POSTSUM-DDNAMN2                        
411800     CALL POSTSUM USING POSTSUM-PARM                                      
411900     .                                                                    
412000                                                                          
412100 S03-WRITE-W57032 SECTION.                                                
412200     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
412300     IF IN-EKH-KDSORT = 'SW'                                              
412400       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
412500       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
412600       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
412700     ELSE                                                                 
412800       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
412900       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
413000       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
413100     END-IF                                                               
413200     WRITE 72LINE-POST        FROM R3-LINE-R3                             
413300                                                                          
413400     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
413500     MOVE 'W57032A'           TO POSTSUM-FDNAMN                           
413600     MOVE 'W57038D3'          TO POSTSUM-DDNAMN2                          
413700     CALL POSTSUM USING POSTSUM-PARM                                      
413800                                                                          
413900     PERFORM S20-CREATE-WRITE-LOG                                         
414000     .                                                                    
414100                                                                          
414200 S04-WRITE-W57033A SECTION.                                               
414300     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
414400     MOVE SPACE                 TO 73LINE-POST                            
414500     IF WS-LINE-SW = JA                                                   
414600       IF IN-EKH-KDSORT = 'SW'                                            
414700         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
414800         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
414900         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
415000       ELSE                                                               
415100         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
415200         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
415300         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
415400       END-IF                                                             
415500       WRITE 73LINE-POST        FROM R3-LINE-R3                           
415600       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
415700     ELSE                                                                 
415800       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
415900       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
416000     END-IF                                                               
416100                                                                          
416200     MOVE 'W57033A'             TO POSTSUM-FDNAMN                         
416300     MOVE 'W57038D4'            TO POSTSUM-DDNAMN2                        
416400     CALL POSTSUM USING POSTSUM-PARM                                      
416500                                                                          
416600     IF WS-LINE-SW = JA                                                   
416700       PERFORM S20-CREATE-WRITE-LOG                                       
416800     END-IF                                                               
416900     .                                                                    
417000                                                                          
417100 S004-WRITE-W57033A-HEAD SECTION.                                         
417200     MOVE SPACE                 TO 73LINE-POST                            
417300     IF IN-EKH-KDSORT = 'SW'                                              
417400       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
417500       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
417600       MOVE WS-TEXT           TO R3-LINE-TEXT                             
417700     ELSE                                                                 
417800       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
417900       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
418000       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
418100     END-IF                                                               
418200     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
418300                                                                          
418400     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
418500     MOVE 'W57033A'             TO POSTSUM-FDNAMN                         
418600     MOVE 'W57038D4'            TO POSTSUM-DDNAMN2                        
418700     CALL POSTSUM USING POSTSUM-PARM                                      
418800     .                                                                    
418900                                                                          
419000 S10-VATCODE SECTION.                                                     
419100     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
419200     PERFORM IMS-GU-WDB601                                                
419300     IF DCS-KDDC = SPACE                                                  
419400       MOVE NEJ              TO WDB6-A-SW                                 
419500     ELSE                                                                 
419600       MOVE JA               TO WDB6-A-SW                                 
419700     END-IF                                                               
419800                                                                          
419900     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
420000     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
420100     IF IN-EKH-SUVAT = ZERO                                               
420200       MOVE '  '     TO R3-LINE-TAX-CODE                                  
420300**** HERE WE ADD VAT FOR 10% TO BOOKING                                   
420400**     COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.1                  
420500     ELSE                                                                 
420600       MOVE 'T0'     TO R3-LINE-TAX-CODE                                  
420700     END-IF                                                               
420800     IF IN-EKH-BEVAT = 'XX'                                               
420900       MOVE 'T0'     TO R3-LINE-TAX-CODE                                  
421000     END-IF                                                               
421100     .                                                                    
421200     EJECT                                                                
421300                                                                          
421400 S20-CREATE-WRITE-LOG SECTION.                                            
421500     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
421600     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
421700     IF SYST-IDPTYP = '610'                                               
421800       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
421900     ELSE                                                                 
422000       MOVE ZERO                      TO WS-IDLEVNR                       
422100       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
422200                          FOR CHARACTERS BEFORE INITIAL SPACE             
422300       IF WS-IDLEVNR   > ZERO                                             
422400          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
422500                                      TO LOGG-IDKONTO                     
422600       END-IF                                                             
422700     END-IF                                                               
422800     IF R3-LINE-COST-CENTER NOT = SPACE                                   
422900       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
423000     END-IF                                                               
423100     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
423200     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
423300     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
423400     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
423500     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
423600                                                                          
423700     PERFORM S21-WRITE-W57035                                             
423800     PERFORM S22-WRITE-W57038                                             
423900                                                                          
424000     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
424100       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
424200       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
424300       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
424400                                                                          
424500       PERFORM S21-WRITE-W57035                                           
424600     END-IF                                                               
424700     .                                                                    
424800     EJECT                                                                
424900                                                                          
425000 S21-WRITE-W57035 SECTION.                                                
425100     IF DCS-IDDC NOT = LOGG-IDDC                                          
425200        MOVE LOGG-IDDC TO W-IDDC-B6                                       
425300        PERFORM IMS-GU-WDB601                                             
425400     END-IF                                                               
425500     IF DCS-KDDC = SPACE                                                  
425600       MOVE NEJ              TO WDB6-A-SW                                 
425700     ELSE                                                                 
425800       MOVE JA               TO WDB6-A-SW                                 
425900     END-IF                                                               
426000                                                                          
426100     IF  WDB6-A-FINNS                                                     
426200     AND DCS-DDC                                                          
426300       MOVE 'N'       TO LOGG-FLLSBOK                                     
426400     END-IF                                                               
426500     WRITE LOGG-POST FROM LOGG-W57073                                     
426600                                                                          
426700     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
426800     MOVE 'W57035'    TO POSTSUM-FDNAMN                                   
426900     MOVE 'W57038D5'  TO POSTSUM-DDNAMN2                                  
427000     CALL POSTSUM USING POSTSUM-PARM                                      
427100     .                                                                    
427200                                                                          
427300 S22-WRITE-W57038 SECTION.                                                
427400     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
427500     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
427600     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
427700                                                                          
427800     IF R3-LINE-AMOUNT-SIGN = '+'                                         
427900       IF AVST-SUBEL < +0                                                 
428000         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
428100       END-IF                                                             
428200       IF AVST-KVANTAL < +0                                               
428300         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
428400       END-IF                                                             
428500     ELSE                                                                 
428600       IF AVST-SUBEL > +0                                                 
428700         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
428800       END-IF                                                             
428900       IF AVST-KVANTAL > +0                                               
429000         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
429100       END-IF                                                             
429200     END-IF                                                               
429300                                                                          
429400     IF DCS-IDDC NOT = AVST-IDDC                                          
429500        MOVE AVST-IDDC  TO W-IDDC-B6                                      
429600        PERFORM IMS-GU-WDB601                                             
429700     END-IF                                                               
429800     IF DCS-KDDC = SPACE                                                  
429900       MOVE NEJ              TO WDB6-A-SW                                 
430000     ELSE                                                                 
430100       MOVE JA               TO WDB6-A-SW                                 
430200     END-IF                                                               
430300                                                                          
430400     IF  WDB6-A-FINNS                                                     
430500     AND DCS-DDC                                                          
430600       MOVE 'N'                 TO AVST-FLLSBOK                           
430700     END-IF                                                               
430800                                                                          
430900     IF AVST-IDKONTO(1:4) = '1454'                                        
431000       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
431100       WRITE AVST-POST FROM AVST-W57070                                   
431200                                                                          
431300       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
431400       MOVE 'W57038'            TO POSTSUM-FDNAMN                         
431500       MOVE 'W57038D6'          TO POSTSUM-DDNAMN2                        
431600       CALL POSTSUM USING POSTSUM-PARM                                    
431700     END-IF                                                               
431800     .                                                                    
431900     EJECT                                                                
432000                                                                          
432100 S30-READ-DATABASE-B2-B1 SECTION.                                         
432200     IF IN-EKH-IDLEVNR = '1441'                                           
432300       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
432400     ELSE                                                                 
432500       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
432600       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
432700       PERFORM IMS-GU-WDB201                                              
432800       IF SEGMENT-SAKNAS                                                  
432900         MOVE 'TR99999'       TO W-WDB1-IDPARTNR                          
433000       ELSE                                                               
433100         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
433200       END-IF                                                             
433300     END-IF                                                               
433400     MOVE WC-IDFTG-TR       TO W-WDB1-IDFTG                               
433500     PERFORM IMS-GU-WDB101                                                
433600     IF SEGMENT-SAKNAS                                                    
433700       DISPLAY 'BETALARUPPG. SAKNAS '                                     
433800       DISPLAY IN-EKH-IDVERGL                                             
433900       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
434000       DISPLAY GMT-IDPARTNR                                               
434100                                                                          
434200       MOVE SPACE         TO BET-KDTRADP                                  
434300       MOVE ZERO          TO BET-IDPARTNR                                 
434400       MOVE '????'        TO WS-KDBETVIL                                  
434500       MOVE '???'         TO WS-KDVALISO-WDB1                             
434600     ELSE                                                                 
434700       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
434800     END-IF                                                               
434900     MOVE 'TRY'           TO WS-KDVALISO-WDB1                             
435000                                                                          
435100     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
435200     MOVE ZERO TO TALLY                                                   
435300     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
435400                 FOR CHARACTERS BEFORE INITIAL SPACE                      
435500     IF TALLY = ZERO                                                      
435600       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
435700     ELSE                                                                 
435800       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
435900                                TO W-BET-IDPARTNR-NUM                     
436000     END-IF                                                               
436100     .                                                                    
436200     EJECT                                                                
436300                                                                          
436400 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
436500     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
436600     PERFORM IMS-GU-WDB601                                                
436700     IF DCS-KDDC = SPACE                                                  
436800       MOVE NEJ              TO WDB6-A-SW                                 
436900     ELSE                                                                 
437000       MOVE JA               TO WDB6-A-SW                                 
437100     END-IF                                                               
437200                                                                          
437300     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
437400       IF IN-EKH-KDEKSHT NOT = '406'                                      
437500         IF IN-EKH-FLDCET = NEJ                                           
437600           PERFORM S42-SKAPA-RW2-INV-POSTER                               
437700         END-IF                                                           
437800       END-IF                                                             
437900     END-IF                                                               
438000                                                                          
438100     IF IN-EKH-KDEKNIVA = 'DET'                                           
438200       IF  IN-EKH-KDEKHHT = '204'                                         
438300       AND (IN-EKH-KDEKSHT = '201')                                       
438400         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
438500       END-IF                                                             
438600                                                                          
438700       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
438800       AND (WDB6-A-FINNS                                                  
438900       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
439000       OR   DCS-DDC OR DCS-NDC-PF))                                       
439100         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
439200       END-IF                                                             
439300                                                                          
439400       IF IN-FIL-IDPGM = 'W4183000'                                       
439500       AND (WDB6-A-FINNS                                                  
439600       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
439700       OR   DCS-DDC OR DCS-NDC-PF))                                       
439800         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
439900       END-IF                                                             
440000     END-IF                                                               
440100     .                                                                    
440200     EJECT                                                                
440300                                                                          
440400 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
440500     MOVE 'RW2'              TO RW2-IDPTYP                                
440600     MOVE 'RW2'              TO WS-IDPTYP                                 
440700     MOVE ZERO               TO RW2-IDDISTR                               
440800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
440900       MOVE WC-CDC-SE        TO RW2-IDDC                                  
441000     ELSE                                                                 
441100       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
441200     END-IF                                                               
441300     IF IN-EKH-KVANTAL < +0                                               
441400       MOVE '0422'           TO RW2-KDWRTYP                               
441500     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
441600     ELSE                                                                 
441700       MOVE '0421'           TO RW2-KDWRTYP                               
441800       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
441900     END-IF                                                               
442000                                                                          
442100     IF RW2-SUARTSTD NOT = +0                                             
442200       PERFORM S70-WRITE-W51380                                           
442300     END-IF                                                               
442400     .                                                                    
442500     EJECT                                                                
442600                                                                          
442700 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
442800     MOVE '0110'             TO RW1-KDWRTYP                               
442900     IF DCS-KDDC = SPACE OR DCS-DDC                                       
443000       MOVE WC-CDC-SE        TO RW1-IDDC                                  
443100     ELSE                                                                 
443200       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
443300     END-IF                                                               
443400     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
443500     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
443600     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
443700                                                                          
443800     IF RW1-SUARTSTD NOT = +0                                             
443900       MOVE 'RW1' TO WS-IDPTYP                                            
444000       PERFORM S70-WRITE-W51380                                           
444100     END-IF                                                               
444200     .                                                                    
444300     EJECT                                                                
444400                                                                          
444500 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
444600     MOVE '0110'             TO RW1-KDWRTYP                               
444700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
444800       MOVE WC-CDC-SE        TO RW1-IDDC                                  
444900     ELSE                                                                 
445000       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
445100     END-IF                                                               
445200     IF IN-EKH-KDANMORS = '30'                                            
445300       MOVE ZERO             TO RW1-SUARTSTD                              
445400     ELSE                                                                 
445500      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
445600     END-IF                                                               
445700     IF IN-EKH-KDANMORS = '30' OR '80'                                    
445800       MOVE ZERO             TO RW1-SUARTSJK                              
445900     ELSE                                                                 
446000      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
446100     END-IF                                                               
446200     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
446300                                                                          
446400     IF RW1-SUARTSTD NOT = +0                                             
446500       MOVE 'RW1' TO WS-IDPTYP                                            
446600       PERFORM S70-WRITE-W51380                                           
446700     END-IF                                                               
446800     .                                                                    
446900     EJECT                                                                
447000                                                                          
447100 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
447200     MOVE '0110'             TO RW1-KDWRTYP                               
447300     IF DCS-KDDC = SPACE OR DCS-DDC                                       
447400       MOVE WC-CDC-SE        TO RW1-IDDC                                  
447500     ELSE                                                                 
447600       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
447700     END-IF                                                               
447800     IF IN-EKH-KDEKSHT = '310'                                            
447900*** SKROTNING KDANMORS  13 O 23                                           
448000       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
448100     ELSE                                                                 
448200      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
448300     END-IF                                                               
448400                                                                          
448500     MOVE ZERO               TO RW1-SUARTSJK                              
448600                                RW1-SUARTFSG                              
448700     IF RW1-SUARTSTD NOT = +0                                             
448800       MOVE 'RW1' TO WS-IDPTYP                                            
448900       PERFORM S70-WRITE-W51380                                           
449000     END-IF                                                               
449100     .                                                                    
449200     EJECT                                                                
449300                                                                          
449400 S60-WRITE-W5703N SECTION.                                                
449500     WRITE SAPUT-POST  FROM IN-AREA                                       
449600                                                                          
449700     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
449800     MOVE 'W5703N'            TO POSTSUM-FDNAMN                           
449900     MOVE 'W57038D7'          TO POSTSUM-DDNAMN2                          
450000     CALL POSTSUM USING POSTSUM-PARM                                      
450100     .                                                                    
450200     EJECT                                                                
450300                                                                          
450400 S70-WRITE-W51380 SECTION.                                                
450500     IF WS-IDPTYP  = 'RW2'                                                
450600       IF DCS-KDDC = SPACE OR DCS-DDC                                     
450700         MOVE WC-CDC-SE        TO INV-IDDC                                
450800       ELSE                                                               
450900         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
451000       END-IF                                                             
451100       IF IN-EKH-KVANTAL < +0                                             
451200         MOVE '003'            TO INV-IDPTYP                              
451300       COMPUTE INV-SUARTSTD =                                             
451400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
451500       ELSE                                                               
451600         MOVE '002'            TO INV-IDPTYP                              
451700         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
451800       END-IF                                                             
451900       MOVE SPACE TO WS-IDPTYP                                            
452000       MOVE 0                  TO INV-ADLAGOMR                            
452100       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
452200       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
452300     END-IF                                                               
452400     IF WS-IDPTYP  = 'RW1'                                                
452500       IF DCS-KDDC = SPACE OR DCS-DDC                                     
452600         MOVE WC-CDC-SE        TO INV-IDDC                                
452700       ELSE                                                               
452800         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
452900       END-IF                                                             
453000       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
453100       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
453200       MOVE 0                  TO INV-ADLAGOMR                            
453300       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
453400       MOVE '001'              TO INV-IDPTYP                              
453500       MOVE SPACE              TO WS-IDPTYP                               
453600     END-IF                                                               
453700     WRITE INV-POST  FROM INV-W51310                                      
453800                                                                          
453900     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
454000     MOVE 'W51380'            TO POSTSUM-FDNAMN                           
454100     MOVE 'W57038D8'          TO POSTSUM-DDNAMN2                          
454200     CALL POSTSUM USING POSTSUM-PARM                                      
454300     .                                                                    
454400     EJECT                                                                
454500                                                                          
454600 S13-GET-LANDING-COST SECTION.                                            
454700     MOVE '86'                   TO W-IDDC-B6                             
454800     PERFORM IMS-GU-WDB601                                                
454900     IF SEGMENT-FINNS                                                     
455000       PERFORM IMS-GNP-WDB617                                             
455100       IF SEGMENT-FINNS                                                   
455200         IF PROC-TILANDCO >  IN-EKH-DAVERDAT                              
455300           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
455400         ELSE                                                             
455500           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
455600         END-IF                                                           
455700       END-IF                                                             
455800     END-IF                                                               
455900     .                                                                    
456000     EJECT                                                                
456100 S80-GET-CURRENCY-RATE SECTION.                                           
456200     MOVE +0                  TO W-ANT                                    
456300     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
456400             BEFORE INITIAL ' '                                           
456500     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
456600     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
456700     PERFORM IMS-GU-WDL601                                                
456800     IF SEGMENT-SAKNAS                                                    
456900       CONTINUE                                                           
457000     ELSE                                                                 
457100       PERFORM IMS-GNP-WDL611                                             
457200       IF SEGMENT-SAKNAS                                                  
457300         CONTINUE                                                         
457400       ELSE                                                               
457500         IF (IN-EKH-KDEKHHT = '102'                                       
457600         AND IN-EKH-KDEKSHT = '126')                                      
457700         OR (IN-EKH-KDEKHHT = '102'                                       
457800         AND IN-EKH-KDEKSHT = '127')                                      
457900         OR (IN-EKH-KDEKHHT = '102'                                       
458000         AND IN-EKH-KDEKSHT = '128')                                      
458100**** EVENT 102-127 AND 102-128 CREATES A NEW POST ON WDL611               
458200**** WITH A DIFFERENT DAINLEV, SO WE NEED TO GET THE CURRENCY             
458300**** RATE FROM THE ORIGINAL POST AND THAT IS SAVED                        
458400**** IF THEY DO BINNING 102-121 OR DEVIATION 102-122 AT THE SAME          
458500**** TIME AS THEY REPORT HAC 102-126 AND THEN SAY THAT THEY               
458600**** RECIEVED THE GOODS BACK FROM CUSTOME 102-127 OR 102-128              
458700**** 102-121 AND 102-122 CAN ALSO GET WRONG CURRENCY RATE                 
458800           MOVE INL-PRKURS          TO WS-PRKURS-TR3                      
458900         ELSE                                                             
459000           COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                   
459100                                     - INL-DAINLEV                        
459200           MOVE WS-FAKTURA-DATUM2   TO WS-FAKTURA-DATUM                   
459300           MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                 
459400           MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                 
459500           MOVE W-DATE-AAMM         TO CURR-TIAAMM                        
459600           MOVE WS-KDVALISO-TR      TO CURR-KDVALISO-ROW                  
459700           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
459800           IF CURR-KDSVAR = ' '                                           
459900             MOVE CURR-PRKURS-NEW   TO WS-PRKURS-TR3                      
460000           ELSE                                                           
460100             MOVE +1                TO WS-PRKURS-TR3                      
460200           END-IF                                                         
460300         END-IF                                                           
460400       END-IF                                                             
460500     END-IF                                                               
460600     .                                                                    
460700     EJECT                                                                
460800                                                                          
460900 S81-GET-CURRENCY-RATE SECTION.                                           
461000     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
461100     MOVE 'TRY'               TO CURR-KDVALISO-ROW                        
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
463000         MOVE CURR-PRKURS-NEW TO WS-PRKURS-TR3                            
463100       ELSE                                                               
463200         IF WS-PRKURS = ZERO                                              
463300           MOVE 1           TO WS-PRKURS-TR3                              
463400         END-IF                                                           
463500       END-IF                                                             
463600     ELSE                                                                 
463700       MOVE 1               TO WS-PRKURS-TR3                              
463800     END-IF                                                               
463900     .                                                                    
464000     EJECT                                                                
464100* --- IMS SECTIONS ---                                                    
464200                                                                          
464300 IMS-GU-WDH521 SECTION.                                                   
464400     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
464500          DELIMITED BY SIZE INTO SSA1                                     
464600     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
464700          DELIMITED BY SIZE INTO SSA2                                     
464800     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
464900          DELIMITED BY SIZE INTO SSA3                                     
465000     MOVE '  '              TO GODK-STATUSKODER                           
465100     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
465200                                                   SSA2                   
465300                                                   SSA3                   
465400     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
465500                                                                          
465600     PERFORM IMS-STATUS-CONTROL                                           
465700     .                                                                    
465800                                                                          
465900 IMS-GNP-WDH531 SECTION.                                                  
466000     MOVE 'WDH531  '        TO SSA1                                       
466100     MOVE '  GE'            TO GODK-STATUSKODER                           
466200     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
466300     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
466400                               WS-STATUS                                  
466500     PERFORM IMS-STATUS-CONTROL                                           
466600     .                                                                    
466700     EJECT                                                                
466800                                                                          
466900 IMS-GU-WDB201 SECTION.                                                   
467000     STRING 'WDB201  (IDGMT    =' W-IDGMT-KEY ')'                         
467100          DELIMITED BY SIZE INTO SSA1                                     
467200     MOVE '  GE'                 TO GODK-STATUSKODER                      
467300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
467400      MOVE WDB2-STATUS-CODE      TO STATUS-WS                             
467500     PERFORM IMS-STATUS-CONTROL                                           
467600     .                                                                    
467700     EJECT                                                                
467800                                                                          
467900 IMS-GU-WDB101 SECTION.                                                   
468000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
468100          DELIMITED BY SIZE INTO SSA1                                     
468200     MOVE '  GE'               TO GODK-STATUSKODER                        
468300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
468400     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
468500     PERFORM IMS-STATUS-CONTROL                                           
468600     .                                                                    
468700     EJECT                                                                
468800                                                                          
468900 IMS-GU-WDB601    SECTION.                                                
469000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
469100          DELIMITED BY SIZE INTO SSA1                                     
469200     MOVE '  GE' TO GODK-STATUSKODER                                      
469300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
469400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
469500     PERFORM IMS-STATUS-CONTROL                                           
469600     IF SEGMENT-SAKNAS                                                    
469700        MOVE SPACE TO DCS-KDDC                                            
469800     END-IF                                                               
469900     .                                                                    
470000     EJECT                                                                
470100                                                                          
470200 IMS-GNP-WDB617 SECTION.                                                  
470300     MOVE 'WDB617   ' TO SSA1                                             
470400     MOVE '  GE'        TO GODK-STATUSKODER                               
470500     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
470600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
470700     PERFORM IMS-STATUS-CONTROL                                           
470800     .                                                                    
470900     SKIP3                                                                
471000                                                                          
471100 IMS-GU-WDL601   SECTION.                                                 
471200     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
471300          DELIMITED BY SIZE INTO SSA1                                     
471400     MOVE '  GE' TO GODK-STATUSKODER                                      
471500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
471600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
471700     PERFORM IMS-STATUS-CONTROL                                           
471800     .                                                                    
471900     SKIP3                                                                
472000                                                                          
472100 IMS-GNP-WDL611   SECTION.                                                
472200     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
472300          DELIMITED BY SIZE INTO SSA1                                     
472400     MOVE '  GE' TO GODK-STATUSKODER                                      
472500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
472600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
472700     PERFORM IMS-STATUS-CONTROL                                           
472800     .                                                                    
472900     SKIP3                                                                
473000                                                                          
473100 IMS-STATUS-CONTROL SECTION.                                              
473200     SET STATUS-IX TO 1                                                   
473300     SEARCH GODK-STATUS                                                   
473400       AT END                                                             
473500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
473600           DELIMITED BY SIZE INTO FELTEXT                                 
473700         DISPLAY FELTEXT                                                  
473800         CALL FELLOG                                                      
473900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
474000         CONTINUE                                                         
474100     END-SEARCH                                                           
474200     .                                                                    
