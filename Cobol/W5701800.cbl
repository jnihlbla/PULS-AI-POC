000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5701800.                                                
000300 AUTHOR.         BARSHARANI BISHOYE.                                      
000400 DATE-WRITTEN.   20230130.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        TAIWAN FINANCIAL EVENTS PROGRAM                                  
001000*                                                                         
001100*       -PGM LÄSER KONTROLLERADE/KOMPLETTERADE EKONOMISKA                 
001200*        HÄNDELSETRANSAKTIONER OCH MATCHAR DESSA MOT                      
001300*        EKONOMISKA STYRPARAMETRAR FÖR ATT I SLUTÄNDEN                    
001400*        PRODUCERA POSTER TILL R3 I FORM AV                               
001500*        1 "LINE RECORD" HUVUDBOK               (PTYP 610)                
001600*        2 "LINE RECORD" KUNDRESKONTRA          (PTYP 310)                
001700*        3 "LINE RECORD" LEVERANTÖRSRESKONTRA   (PTYP 210)                
001800*                                                                         
001900*       -PGM SKAPAR/SKRIVER ÄVEN FÖLJANDE POSTER TILL R3                  
002000*        1 "HEADER RECORD" HUVUDBOK             (PTYP 600)                
002100*        2 "HEADER RECORD" KUNDRESKONTRA        (PTYP 300)                
002200*        3 "HEADER RECORD" LEVERANTÖRSRESKONTRA (PTYP 200)                
002300*                                                                         
002400*       -PGM PLOCKAR UNDAN NY MÅNADS POSTER VID MÅNADSSKIFTE              
002500*        FÖR ATT TA IN DESSA VID NÄSTA KÖRNING.                           
002600*        (NY MÅNADS POSTER = DATUMKORTS MÅNAD + 1, OM DENNA ÄR            
002700*         LIKA MED IN-POSTENS DAVERDAT'S MÅNAD,                           
002800*         SKRIVS POSTEN PÅ UTFIL FÖR AT TAS IN NÄSTA KÖRNING).            
002900*                                                                         
003000*       -PROGRAMMET LÄSER      WDH5                                       
003100*                              WDB1                                       
003200*                              WDB2                                       
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
004700     SELECT W57066                     ASSIGN TO W57018D1.                
004800                                                                          
004900*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
005000     SELECT W57011A                    ASSIGN TO W57018D2.                
005100                                                                          
005200*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005300     SELECT W57012A                    ASSIGN TO W57018D3.                
005400                                                                          
005500*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005600     SELECT W57013A                    ASSIGN TO W57018D4.                
005700                                                                          
005800*          --- LOGG TILL ON-DEMAND                                        
005900     SELECT W57015                     ASSIGN TO W57018D5.                
006000                                                                          
006100*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006200     SELECT W57018                     ASSIGN TO W57018D6.                
006300                                                                          
006400*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006500     SELECT W5701N                     ASSIGN TO W57018D7.                
006600                                                                          
006700*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006800     SELECT W51310A                    ASSIGN TO W57018D8.                
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
008100 FD  W57011A                                                              
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400*01  71INIT-POST -COPY R3INIT20               -L.                         
008500*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008600*01  71LINE-POST -COPY R3LINE20               -L.                         
008700                                                                          
008800 FD  W57012A                                                              
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100*01  72LINE-POST -COPY R3LINE20               -L.                         
009200                                                                          
009300 FD  W57013A                                                              
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009700*01  73LINE-POST -COPY R3LINE20               -L.                         
009800                                                                          
009900 FD  W57015                                                               
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200*01  LOGG-POST   -COPY W57073                 -L.                         
010300                                                                          
010400 FD  W57018                                                               
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700*01  AVST-POST   -COPY W57070                 -L.                         
010800                                                                          
010900 FD  W5701N                                                               
011000     RECORDING       F                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300 01  SAPUT-POST.                                                          
011400*    03  -COPY WDR801        -L.                                          
011500     03 FILLER                   PIC X(6).                                
011600                                                                          
011700 FD  W51310A                                                              
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012100                                                                          
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400*    -- CHECKED BY WY2000                                                 
012500 77  IDPGM                        PIC X(8)    VALUE 'W5701800'.           
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
013900 77  WS-102-124-DET-1             PIC S9(13)V99 COMP-3.                   
014000 77  WS-102-124-DET-2             PIC S9(13)V99 COMP-3.                   
014100 77  WS-102-134-DET-1             PIC S9(13)V99 COMP-3.                   
014200 77  WS-102-134-DET-2             PIC S9(13)V99 COMP-3.                   
014300 77  WS-102-122-DET               PIC S9(13)V99 COMP-3.                   
014400 77  WS-102-132-DET               PIC S9(13)V99 COMP-3.                   
014500 77  SPAR-SUMMA-102-125         PIC S9(13)V99  COMP-3 VALUE ZERO.         
014600 77  WS-BELOPP                  PIC S9(13)V99  COMP-3.                    
014700 77  WS-LOP                       PIC 9       VALUE ZERO.                 
014800 77  WS-SPAR-CMD                  PIC X(3) VALUE SPACE.                   
014900 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
015000 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
015100 77  WS-SPAR-IDVERGL              PIC X(10) VALUE SPACE.                  
015200 77  WS-SPAR-DAVERDAT             PIC 9(8) VALUE ZERO.                    
015300 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
015400 77  SPAR-LINE-ORDER              PIC X(12).                              
015500 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
015600 77  WS-RED-IDKST                 PIC X(10).                              
015700 77  WS-IDPTYP                    PIC X(3).                               
015800 77  WS-FAKTURA-DATUM             PIC X(16).                              
015900 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
016000 77  SPAR-SUMMA                 PIC S9(13)V99  COMP-3 VALUE ZERO.         
016100 77  SPAR-PRDMTRL               PIC S9(13)V99  COMP-3 VALUE ZERO.         
016200 77  SPAR-PROVRPAL              PIC S9(13)V99  COMP-3 VALUE ZERO.         
016300 77  SPAR-PRDIRLON              PIC S9(13)V99  COMP-3 VALUE ZERO.         
016400 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
016500 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
016600 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
016700 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
016800 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
016900 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
017000 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
017100 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
017200                                                                          
017300 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
017400       88  WDB6-A-FINNS                       VALUE 'J'.                  
017500       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
017600                                                                          
017700*01  -COPY WWPRODSL                                                       
017800                                                                          
017900*01  -COPY WWDCKONS                                                       
018000     EJECT                                                                
018100                                                                          
018200 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
018300*01  -COPY WWIDFTG                                                        
018400     EJECT                                                                
018500                                                                          
018600 01  FELTEXT                      PIC X(80).                              
018700 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
018800*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
018900     EJECT                                                                
019000                                                                          
019100 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
019200 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
019300     EJECT                                                                
019400 01  WS-IDDISTR-IDKUNDNR.                                                 
019500     03  FILLER                   PIC X(2)    VALUE SPACE.                
019600     03  WS-IDDISTR               PIC 9(4).                               
019700     03  WS-IDKUNDNR              PIC 9(6).                               
019800                                                                          
019900 01  WS-KDBETVIL                  PIC X(4).                               
020000 01  WS-KDVALISO-WDB1             PIC X(3).                               
020100 01  WS-KDVALISO                  PIC X(3).                               
020200 01  WS-KDVALISO-TW               PIC X(3) VALUE 'TWD'.                   
020300 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
020400 01  WS-PRKURS-TW                 PIC S9(6)V9(5) COMP-3.                  
020500 01  WS-PRKURS-TW2                PIC S9(6)V9(5) COMP-3.                  
020600 01  WS-PRKURS-TW3                PIC S9(6)V9(5) COMP-3.                  
020700 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
020800 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
020900                                                                          
021000 01  WS-ALLOCATE.                                                         
021100     03  WS-ALLOCATE-DC           PIC X(2).                               
021200     03  WS-ALLOCATE-DISTR        PIC X(5).                               
021300     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
021400     03  FILLER                   PIC X(4)    VALUE SPACE.                
021500                                                                          
021600 01  WS-TEXT.                                                             
021700     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
021800     03  WS-TEXT-KDEKHHT          PIC X(3).                               
021900     03  WS-TEXT-KDEKSHT          PIC X(3).                               
022000     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
022100     03  FILLER                   PIC X(7)    VALUE SPACE.                
022200                                                                          
022300 01  WS-LINE-TEXT.                                                        
022400     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
022500     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
022600     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
022700     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
022800     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
022900     03  FILLER                   PIC X(22)   VALUE SPACE.                
023000                                                                          
023100 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
023200 01  WS-PRCTR.                                                            
023300     03  WS-PRCTR-PRODSL          PIC X(2).                               
023400     03  FILLER                   PIC X(1).                               
023500     03  FILLER                   PIC X(7).                               
023600 01  WS-PRODSL-PC                 PIC 9(2).                               
023700     88 KDPRODSL-PC-91            VALUE 91 THRU 94 98 99.                 
023800     88 KDPRODSL-PC-95            VALUE 95 THRU 97.                       
023900                                                                          
024000 01  WS-R3-ACCOUNT.                                                       
024100     03  WS-R3-ACCOUNT-ALFA.                                              
024200         05 FILLER                PIC X(4).                               
024300         05 WS-R3-ACCOUNT-6       PIC X(6).                               
024400     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
024500         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
024600                                                                          
024700 01  WS-ACCOUNT.                                                          
024800     03  FILLER                   PIC X(7).                               
024900     03  WS-ACCOUNT-4             PIC X(1).                               
025000     03  FILLER                   PIC X(2).                               
025100                                                                          
025200 01  SPAR-AREA.                                                           
025300     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
025400     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
025500     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
025600     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
025700                                                                          
025800 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
025900 01  FILLER REDEFINES DAGENS-DATUM.                                       
026000     03  DAGENS-DATUM-AAR         PIC 9(2).                               
026100     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
026200     03  DAGENS-DATUM-DAG         PIC 9(2).                               
026300                                                                          
026400 01  WS-NEW-MONTH                 PIC 9(2).                               
026500                                                                          
026600 01  WS-DAREGDAT.                                                         
026700     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
026800     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
026900                                                                          
027000 01  WS-TIREGDAT-TOT.                                                     
027100     03  WS-TIREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
027200     03  WS-TIREGDAT              PIC 9(6).                               
027300                                                                          
027400 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
027500 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
027600     EJECT                                                                
027700                                                                          
027800 01  DYNAMISKA-SUBPROGRAM.                                                
027900     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
028000     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
028100     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
028200     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
028300     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
028400     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
028500                                                                          
028600*    --- PARAMETRAR TILL ABEND                                            
028700 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
028800 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
028900 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
029000     EJECT                                                                
029100                                                                          
029200*    --- PARAMETRAR TILL DATKORT                                          
029300 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57018'.             
029400                                                                          
029500 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
029600*01  -COPY WDATKORT                                                       
029700     EJECT                                                                
029800                                                                          
029900*    --- PARAMETRAR TILL POSTSUM                                          
030000*01  -COPY W0005   -PRE  POSTSUM-                                         
030100     EJECT                                                                
030200                                                                          
030300 01  FILLER                          PIC X(16) VALUE 'W510CURR '.         
030400*01  -COPY W510CURR                                                       
030500     EJECT                                                                
030600                                                                          
030700 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
030800*01  AREA -COPY WDR801           -PRE IN-                                 
030900*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
031000         05   IN-EKH-IDSYSMOT     PIC X(6).                               
031100                                                                          
031200     EJECT                                                                
031300 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
031400                                                                          
031500*01  -COPY R3LINE20              -PRE R3-                                 
031600*01  -COPY R3HEAD20              -PRE R3-                                 
031700*01  -COPY R3INIT20              -PRE R3-                                 
031800*01  -COPY W57073                -PRE LOGG-                               
031900*01  -COPY W57070                -PRE AVST-                               
032000*01  -COPY W517RW1               -PRE RW1-                                
032100*01  -COPY W517RW2               -PRE RW2-                                
032200*01  -COPY W51310                -PRE INV-                                
032300     EJECT                                                                
032400                                                                          
032500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032600 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
032700                                                                          
032800 01  NYCKLAR-TILL-DLI.                                                    
032900     03  W-WDH501KY-X.                                                    
033000         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
033100         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
033200     03  W-KDEKSHT-X.                                                     
033300         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
033400     03  W-KDEKNIVA-X.                                                    
033500         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
033600     03  W-WDH531KY-X.                                                    
033700         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
033800         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
033900     03  W-IDRADNR-X.                                                     
034000         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
034100                                                                          
034200     03  W-IDGMT-KEY.                                                     
034300         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
034400         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
034500                                                                          
034600     03  W-WDB101KY-X.                                                    
034700         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
034800         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
034900                                                                          
035000     03  W-IDDC-B6-X.                                                     
035100         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
035200                                                                          
035300     03  W-KDSEGKEY-X.                                                    
035400         05  W-KDSEGKEY           PIC X(1)    VALUE '1'.                  
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
036500     03  W-IDFKNGRP-X.                                                    
036600         05  W-IDFKNGRP           PIC S9(5)   VALUE ZERO COMP-3.          
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
040700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
040800 01  DLI-IO-WDB101.                                                       
040900*    03  -COPY WDB101                                                     
041000     EJECT                                                                
041100                                                                          
041200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
041300 01  DLI-IO-WDB201.                                                       
041400*    03  -COPY WDB201                                                     
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
041800 01   DLI-IO-AREA-B601.                                                   
041900*     03  -COPY WDB601                                                    
042000     EJECT                                                                
042100 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
042200 01   DLI-IO-WDB617.                                                      
042300*     03  -COPY WDB617                                                    
042400     EJECT                                                                
042500 01  FILLER               PIC X(16)   VALUE 'WDB622 AREA'.                
042600 01   DLI-IO-WDB622.                                                      
042700*     03  -COPY WDB622                                                    
042800     EJECT                                                                
042900 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
043000 01   DLI-IO-AREA-L601.                                                   
043100*     03  -COPY WDL601                                                    
043200     EJECT                                                                
043300 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
043400 01   DLI-IO-AREA-L611.                                                   
043500*     03  -COPY WDL611                                                    
043600     EJECT                                                                
043700 01  FILLER               PIC X(16)   VALUE 'WDK601 AREA'.                
043800 01   DLI-IO-AREA-K601.                                                   
043900*     03  -COPY WDK601                                                    
044000     EJECT                                                                
044100     SKIP3                                                                
044200 LINKAGE SECTION.                                                         
044300*01  -COPY W0008  -PRE WDH5-                                              
044400     05  FILLER                  PIC X.                                   
044500                                                                          
044600*01  -COPY W0008  -PRE WDB2-                                              
044700     05  FILLER                  PIC X.                                   
044800                                                                          
044900*01  -COPY W0008  -PRE WDB1-                                              
045000     05  FILLER                  PIC X.                                   
045100                                                                          
045200*01  -COPY W0008  -PRE WDG2-                                              
045300     05  FILLER                  PIC X.                                   
045400                                                                          
045500*01  -COPY W0008  -PRE WDB6-                                              
045600     05  FILLER                  PIC X.                                   
045700                                                                          
045800*01  -COPY W0008  -PRE WDL6-                                              
045900     05  FILLER                  PIC X.                                   
046000                                                                          
046100*01  -COPY W0008  -PRE WDK6-                                              
046200     05  FILLER                  PIC X.                                   
046300                                                                          
046400                                                                          
046500     EJECT                                                                
046600                                                                          
046700 PROCEDURE DIVISION  USING WDH5-PCB WDB2-PCB WDB1-PCB                     
046800                           WDG2-PCB WDB6-PCB WDL6-PCB WDK6-PCB.           
046900 MAIN SECTION.                                                            
047000     ENTRY 'DLITCBL' USING WDH5-PCB WDB2-PCB WDB1-PCB                     
047100                           WDG2-PCB WDB6-PCB WDL6-PCB WDK6-PCB.           
047200                                                                          
047300     PERFORM A-INIT                                                       
047400                                                                          
047500     PERFORM S01-READ-W57066                                              
047600     PERFORM UNTIL END-OF-W57066                                          
047700*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
047800       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
047900       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
048000       AND WS-NEW-MONTH > 01                                              
048100         PERFORM S60-WRITE-W5701N                                         
048200       ELSE                                                               
048300         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
048400         PERFORM S30-READ-DATABASE-B2-B1                                  
048500         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
048600           PERFORM C-EXECUTE                                              
048700         END-IF                                                           
048800       END-IF                                                             
048900       PERFORM S01-READ-W57066                                            
049000     END-PERFORM                                                          
049100                                                                          
049200     PERFORM Z-FINI                                                       
049300                                                                          
049400     MOVE ZERO TO RETURN-CODE                                             
049500     GOBACK                                                               
049600     .                                                                    
049700     EJECT                                                                
049800                                                                          
049900 A-INIT SECTION.                                                          
050000     OPEN INPUT  W57066                                                   
050100                                                                          
050200     OPEN OUTPUT W57018                                                   
050300                 W57011A                                                  
050400                 W57012A                                                  
050500                 W57013A                                                  
050600                 W57015                                                   
050700                 W5701N                                                   
050800                 W51310A                                                  
050900                                                                          
051000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
051100     MOVE 20               TO RW1-DAVVREG(1:2)                            
051200     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
051300                              RW1-DAVVREG(3:2)                            
051400                              W-DATE-AAMM(1:2)                            
051500                              WS-TIAA                                     
051600     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
051700                              W-DATE-AAMM(3:2)                            
051800                              WS-TIMM                                     
051900                              WS-NEW-MONTH                                
052000     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
052100     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
052200     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
052300                                                                          
052400*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
052500*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
052600     IF WS-NEW-MONTH = 12                                                 
052700       MOVE 1              TO WS-NEW-MONTH                                
052800     ELSE                                                                 
052900       ADD 1               TO WS-NEW-MONTH                                
053000*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
053100***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
053200***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
053300***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
053400***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
053500       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
053600       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
053700         ADD 1             TO WS-NEW-MONTH                                
053800         ADD 1             TO WS-TIMM                                     
053900         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
054000       END-IF                                                             
054100     END-IF                                                               
054200                                                                          
054300     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
054400                                                                          
054500     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
054600                                                                          
054700     ACCEPT DAGENS-KLOCKA FROM TIME                                       
054800     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
054900                                                                          
055000     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
055100     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
055200     MOVE 'M'                   TO CURR-KDVALTYP                          
055300                                                                          
055400     MOVE WS-KDVALISO-TW        TO CURR-KDVALISO-ROW                      
055500     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
055600     IF CURR-KDSVAR = ' '                                                 
055700       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-TW                           
055800     ELSE                                                                 
055900       MOVE 1                   TO WS-PRKURS-TW                           
056000     END-IF                                                               
056100     COMPUTE WS-PRKURS-TW2 ROUNDED = 1 / WS-PRKURS-TW                     
056200     MOVE WS-PRKURS-TW          TO WS-PRKURS-TW3                          
056300                                                                          
056400     .                                                                    
056500     EJECT                                                                
056600                                                                          
056700 C-EXECUTE SECTION.                                                       
056800                                                                          
056900     MOVE IN-EKH-IDARTNR        TO W-IDARTNR                              
057000     PERFORM IMS-GU-WDK601                                                
057100     IF SEGMENT-FINNS                                                     
057200       MOVE ART-IDFKNGRP        TO W-IDFKNGRP                             
057300     END-IF                                                               
057400     MOVE WC-IDFTG-TW           TO W-IDFTG                                
057500     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
057600     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
057700     IF IN-EKH-KDEKNIVA = 'TDET'                                          
057800       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
057900     END-IF                                                               
058000     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
058100     PERFORM IMS-GU-WDH521                                                
058200     PERFORM IMS-GNP-WDH531                                               
058300                                                                          
058400     PERFORM S13-GET-LANDING-COST                                         
058500                                                                          
058600     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
058700     IF IN-EKH-IDDISTR > ZERO                                             
058800       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
058900     ELSE                                                                 
059000       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
059100     END-IF                                                               
059200     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
059300                                                                          
059400* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
059500* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
059600* BOKFÖRINGEN I SAP SKER DOCK ALLTID I TW, DÄRFÖR BYTET HÄR:              
059700*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
059800*      MOVE 'TWD'               TO WS-KDVALISO                            
059900*    END-IF                                                               
060000                                                                          
060100     IF IN-EKH-IDVERGL = WS-SPAR-IDVERGL                                  
060200     AND (IN-EKH-DAVERDAT = WS-SPAR-DAVERDAT)                             
060300       IF  (IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                              
060400       AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                              
060500       OR (IN-EKH-KDEKHHT = '303')                                        
060600         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
060700         IF IN-EKH-KDEKHHT = '103'                                        
060800           IF IN-EKH-CMD = WS-SPAR-CMD                                    
060900             MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                         
061000           ELSE                                                           
061100             IF WS-LOP = 9                                                
061200               MOVE ZERO  TO WS-LOP                                       
061300             ELSE                                                         
061400               ADD +1     TO WS-LOP                                       
061500             END-IF                                                       
061600             MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                         
061700             MOVE IN-EKH-CMD TO WS-SPAR-CMD                               
061800           END-IF                                                         
061900         END-IF                                                           
062000       ELSE                                                               
062100         MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                           
062200         MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                           
062300         IF WS-LOP = 9                                                    
062400           MOVE ZERO  TO WS-LOP                                           
062500         ELSE                                                             
062600           ADD +1     TO WS-LOP                                           
062700         END-IF                                                           
062800         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
062900       END-IF                                                             
063000     ELSE                                                                 
063100       MOVE IN-EKH-IDVERGL  TO WS-SPAR-IDVERGL                            
063200       MOVE IN-EKH-KDEKHHT  TO WS-SPAR-KDEKHHT                            
063300       MOVE IN-EKH-KDEKSHT  TO WS-SPAR-KDEKSHT                            
063400       MOVE IN-EKH-DAVERDAT TO WS-SPAR-DAVERDAT                           
063500       MOVE IN-EKH-CMD      TO WS-SPAR-CMD                                
063600       IF WS-LOP = 9                                                      
063700         MOVE ZERO  TO WS-LOP                                             
063800       ELSE                                                               
063900         ADD +1     TO WS-LOP                                             
064000       END-IF                                                             
064100       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
064200     END-IF                                                               
064300* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
064400     IF SYST-IDPTYP = '210'                                               
064500       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
064600     ELSE                                                                 
064700       IF SYST-IDPTYP = '310'                                             
064800         PERFORM CC-CREATE-WRITE-HEADER-AR                                
064900       ELSE                                                               
065000* TEST OM BRYTNING PÅ VERIFIKATION                                        
065100         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
065200         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
065300         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
065400         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
065500           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
065600           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
065700           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
065800           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
065900           IF (IN-EKH-KDEKHHT = '102'                                     
066000           AND IN-EKH-KDEKSHT = '121')                                    
066100           OR (IN-EKH-KDEKHHT = '102'                                     
066200           AND IN-EKH-KDEKSHT = '122')                                    
066300           OR (IN-EKH-KDEKHHT = '102'                                     
066400           AND IN-EKH-KDEKSHT = '124')                                    
066500           OR (IN-EKH-KDEKHHT = '102'                                     
066600           AND IN-EKH-KDEKSHT = '131')                                    
066700           OR (IN-EKH-KDEKHHT = '102'                                     
066800           AND IN-EKH-KDEKSHT = '132')                                    
066900           OR (IN-EKH-KDEKHHT = '102'                                     
067000           AND IN-EKH-KDEKSHT = '134')                                    
067100             PERFORM S80-GET-CURRENCY-RATE                                
067200           END-IF                                                         
067300           IF (IN-EKH-KDEKHHT = '303'                                     
067400           AND IN-EKH-KDEKSHT = '301')                                    
067410           OR (IN-EKH-KDEKHHT = '303'                                     
067420           AND IN-EKH-KDEKSHT = '307')                                    
067430           OR (IN-EKH-KDEKHHT = '303'                                     
067440           AND IN-EKH-KDEKSHT = '371')                                    
067450           OR (IN-EKH-KDEKHHT = '303'                                     
067460           AND IN-EKH-KDEKSHT = '3XX')                                    
067500             PERFORM S81-GET-CURRENCY-RATE                                
067600           END-IF                                                         
067700*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
067800*   HEADER-POST TILL HUVUDBOKEN                                           
067900           IF (IN-EKH-KDEKHHT = '102'                                     
068000           AND IN-EKH-KDEKSHT = '120')                                    
068100           OR (IN-EKH-KDEKHHT = '102'                                     
068200           AND IN-EKH-KDEKSHT = '124')                                    
068300           OR (IN-EKH-KDEKHHT = '102'                                     
068400           AND IN-EKH-KDEKSHT = '125')                                    
068500           OR (IN-EKH-KDEKHHT = '102'                                     
068600           AND IN-EKH-KDEKSHT = '130')                                    
068700           OR (IN-EKH-KDEKHHT = '102'                                     
068800           AND IN-EKH-KDEKSHT = '134')                                    
068900           OR (IN-EKH-KDEKHHT = '103'                                     
069000           AND IN-EKH-KDEKSHT = '102')                                    
069100           OR (IN-EKH-KDEKHHT = '103'                                     
069200           AND IN-EKH-KDEKSHT = '106')                                    
069300           OR (IN-EKH-KDEKHHT = '103'                                     
069400           AND IN-EKH-KDEKSHT = '107')                                    
069500           OR (IN-EKH-KDEKHHT = '204'                                     
069600           AND IN-EKH-KDEKSHT = '301')                                    
069700           OR (IN-EKH-KDEKHHT = '303'                                     
069800           AND IN-EKH-KDEKSHT = '301')                                    
069900           OR (IN-EKH-KDEKHHT = '303'                                     
070000           AND IN-EKH-KDEKSHT = '307')                                    
070100           OR (IN-EKH-KDEKHHT = '303'                                     
070200           AND IN-EKH-KDEKSHT = '3XX')                                    
070300           OR (IN-EKH-KDEKHHT = '303'                                     
070400           AND IN-EKH-KDEKSHT = '371')                                    
070500             CONTINUE                                                     
070600           ELSE                                                           
070700             PERFORM CA-CREATE-WRITE-HEADER-GL                            
070800           END-IF                                                         
070900         END-IF                                                           
071000       END-IF                                                             
071100     END-IF                                                               
071200                                                                          
071300**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
071400     MOVE WS-STATUS TO STATUS-WS                                          
071500     PERFORM UNTIL SEGMENT-SAKNAS                                         
071600       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
071700                                                                          
071800* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
071900       IF SYST-IDPTYP = '610'                                             
072000         PERFORM CD-BUILD-COMMON-610-PART                                 
072100         PERFORM CE-SCHEDULE-LINE-GL                                      
072200       ELSE                                                               
072300         IF SYST-IDPTYP = '210'                                           
072400           PERFORM CF-BUILD-COMMON-210-PART                               
072500           PERFORM CG-SCHEDULE-LINE-AP                                    
072600         ELSE                                                             
072700           IF SYST-IDPTYP = '310'                                         
072800             PERFORM CH-BUILD-COMMON-310-PART                             
072900             PERFORM CI-SCHEDULE-LINE-AR                                  
073000           END-IF                                                         
073100         END-IF                                                           
073200       END-IF                                                             
073300       PERFORM IMS-GNP-WDH531                                             
073400     END-PERFORM                                                          
073500     .                                                                    
073600     EJECT                                                                
073700                                                                          
073800 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
073900     MOVE SPACE                   TO R3-HEAD-R3                           
074000     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
074100     MOVE 'TW01'                  TO R3-HEAD-COMPANY-CODE                 
074200     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
074300     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
074400     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
074500     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
074600     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
074700       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
074800     ELSE                                                                 
074900       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
075000     END-IF                                                               
075100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
075200     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
075300     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
075400     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
075500     IF WS-KDVALISO = 'TWD'                                               
075600       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
075700     ELSE                                                                 
075800       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
075900       MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                     
076000       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
076100       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
076200       IF CURR-KDSVAR = ' '                                               
076300         IF IN-EKH-IDDISTR > ZERO                                         
076400           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
076500         ELSE                                                             
076600           MOVE 1               TO WS-PRKURS                              
076700         END-IF                                                           
076800       ELSE                                                               
076900         MOVE 1                 TO WS-PRKURS                              
077000       END-IF                                                             
077100       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
077200                                       CURR-REVALUTA-TO                   
077300       IF CURR-REVALUTA-TO = +1                                           
077400         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
077500       END-IF                                                             
077600       IF CURR-REVALUTA-TO = +10                                          
077700         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
077800       END-IF                                                             
077900       IF CURR-REVALUTA-TO = +100                                         
078000         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
078100       END-IF                                                             
078200     END-IF                                                               
078300     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
078400     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
078500     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
078600     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
078700     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
078800     MOVE JA                      TO WS-HEADER-SW                         
078900     MOVE NEJ                     TO WS-LINE-SW                           
079000                                                                          
079100* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
079200     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
079300     OR (IN-EKH-KDEKHHT = '303'                                           
079400     AND IN-EKH-KDEKSHT = '391')                                          
079500     OR (IN-EKH-KDEKHHT = '102'                                           
079600     AND IN-EKH-KDEKSHT = '121')                                          
079700     OR (IN-EKH-KDEKHHT = '102'                                           
079800     AND IN-EKH-KDEKSHT = '131')                                          
079900       PERFORM S004-WRITE-W57013A-HEAD                                    
080000     ELSE                                                                 
080100       PERFORM S002-WRITE-W57011A-HEAD                                    
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500                                                                          
080600 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
080700     MOVE SPACE                   TO R3-HEAD-R3                           
080800     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
080900     MOVE 'TW01'                  TO R3-HEAD-COMPANY-CODE                 
081000                                     R3-HEAD-CONTROL-AREA                 
081100     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
081200     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
081300     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
081400     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
081500     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
081600     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
081700       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
081800     ELSE                                                                 
081900       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
082000     END-IF                                                               
082100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
082200     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
082300     IF (IN-EKH-KDEKHHT = '103'                                           
082400     AND IN-EKH-KDEKSHT = '102')                                          
082500       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
082600       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
082700     ELSE                                                                 
082800       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
082900       MOVE WS-PRKURS-TW2         TO R3-HEAD-EXCHANGE-RATE                
083000       MOVE 'TWD'                 TO CURR-KDVALISO-ROW                    
083100       IF IN-FIL-IDPGM = 'W4183300'                                       
083200         IF IN-EKH-DAAVIDAT > ZERO                                        
083300           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
083400           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
083500         ELSE                                                             
083600           MOVE WS-TIAA              TO WS-TIAA-CR                        
083700           MOVE WS-TIMM              TO WS-TIMM-CR                        
083800         END-IF                                                           
083900       ELSE                                                               
084000         MOVE WS-TIAA                TO WS-TIAA-CR                        
084100         MOVE WS-TIMM                TO WS-TIMM-CR                        
084200       END-IF                                                             
084300       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
084400       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
084500       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
084600       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
084700       IF CURR-KDSVAR = ' '                                               
084800         IF IN-EKH-IDDISTR > ZERO                                         
084900           MOVE CURR-PRKURS-NEW TO WS-PRKURS-TW                           
085000         ELSE                                                             
085100           IF WS-PRKURS = ZERO                                            
085200             MOVE 1             TO WS-PRKURS-TW                           
085300           END-IF                                                         
085400         END-IF                                                           
085500       ELSE                                                               
085600         MOVE 1                 TO WS-PRKURS-TW                           
085700       END-IF                                                             
085800       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-TW *                     
085900                                       CURR-REVALUTA-TO                   
086000       END-COMPUTE                                                        
086100       IF CURR-REVALUTA-TO = +1                                           
086200         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
086300       END-IF                                                             
086400       IF CURR-REVALUTA-TO = +10                                          
086500         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
086600       END-IF                                                             
086700       IF CURR-REVALUTA-TO = +100                                         
086800         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
086900       END-IF                                                             
087000     END-IF                                                               
087100     IF (IN-EKH-KDEKHHT = '102'                                           
087200     AND IN-EKH-KDEKSHT = '120')                                          
087300     OR (IN-EKH-KDEKHHT = '102'                                           
087400     AND IN-EKH-KDEKSHT = '124')                                          
087500     OR (IN-EKH-KDEKHHT = '102'                                           
087600     AND IN-EKH-KDEKSHT = '125')                                          
087700     OR (IN-EKH-KDEKHHT = '102'                                           
087800     AND IN-EKH-KDEKSHT = '130')                                          
087900     OR (IN-EKH-KDEKHHT = '102'                                           
088000     AND IN-EKH-KDEKSHT = '134')                                          
088100     OR (IN-EKH-KDEKHHT = '303'                                           
088200     AND IN-EKH-KDEKSHT = '301')                                          
088300     OR (IN-EKH-KDEKHHT = '303'                                           
088400     AND IN-EKH-KDEKSHT = '307')                                          
088500     OR (IN-EKH-KDEKHHT = '303'                                           
088600     AND IN-EKH-KDEKSHT = '371')                                          
088700     OR (IN-EKH-KDEKHHT = '303'                                           
088800     AND IN-EKH-KDEKSHT = '3XX')                                          
088900       MOVE 'TWD'                 TO R3-HEAD-CURRENCY                     
089000       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
089100     END-IF                                                               
089200     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
089300     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
089400     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
089500     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
089600     MOVE JA                      TO WS-HEADER-SW                         
089700     MOVE NEJ                     TO WS-LINE-SW                           
089800                                                                          
089900* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57013A                       
090000     PERFORM S004-WRITE-W57013A-HEAD                                      
090100     .                                                                    
090200     EJECT                                                                
090300                                                                          
090400 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
090500     MOVE SPACE                   TO R3-HEAD-R3                           
090600     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
090700     MOVE 'TW01'                  TO R3-HEAD-COMPANY-CODE                 
090800                                     R3-HEAD-CONTROL-AREA                 
090900     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
091000     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
091100     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
091200     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
091300     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
091400     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
091500       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
091600     ELSE                                                                 
091700       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
091800     END-IF                                                               
091900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
092000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
092100     IF (IN-EKH-KDEKHHT = '204'                                           
092200     AND IN-EKH-KDEKSHT = '201')                                          
092300     OR (IN-EKH-KDEKHHT = '204'                                           
092400     AND IN-EKH-KDEKSHT = '301')                                          
092500       MOVE 'TWD'                 TO R3-HEAD-CURRENCY                     
092600       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
092700     ELSE                                                                 
092800       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
092900       MOVE WS-PRKURS-TW2         TO R3-HEAD-EXCHANGE-RATE                
093000       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
093100       IF IN-FIL-IDPGM = 'W4183300'                                       
093200         IF IN-EKH-DAAVIDAT > ZERO                                        
093300           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
093400           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
093500         ELSE                                                             
093600           MOVE WS-TIAA              TO WS-TIAA-CR                        
093700           MOVE WS-TIMM              TO WS-TIMM-CR                        
093800         END-IF                                                           
093900       ELSE                                                               
094000         MOVE WS-TIAA                TO WS-TIAA-CR                        
094100         MOVE WS-TIMM                TO WS-TIMM-CR                        
094200       END-IF                                                             
094300       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
094400       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
094500       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
094600       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
094700       IF CURR-KDSVAR = ' '                                               
094800         IF IN-EKH-IDDISTR > ZERO                                         
094900           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
095000         ELSE                                                             
095100           IF WS-PRKURS = ZERO                                            
095200             MOVE 1             TO WS-PRKURS                              
095300           END-IF                                                         
095400         END-IF                                                           
095500       ELSE                                                               
095600         MOVE 1                 TO WS-PRKURS                              
095700       END-IF                                                             
095800       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
095900                                       CURR-REVALUTA-TO                   
096000       END-COMPUTE                                                        
096100       IF CURR-REVALUTA-TO = +1                                           
096200         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
096300       END-IF                                                             
096400       IF CURR-REVALUTA-TO = +10                                          
096500         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
096600       END-IF                                                             
096700       IF CURR-REVALUTA-TO = +100                                         
096800         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
096900       END-IF                                                             
097000     END-IF                                                               
097100     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
097200     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
097300     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
097400     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
097500     MOVE JA                      TO WS-HEADER-SW                         
097600     MOVE NEJ                     TO WS-LINE-SW                           
097700                                                                          
097800* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57013A                       
097900       PERFORM S004-WRITE-W57013A-HEAD                                    
098000     .                                                                    
098100     EJECT                                                                
098200                                                                          
098300 CD-BUILD-COMMON-610-PART SECTION.                                        
098400     MOVE SPACE               TO R3-LINE-R3                               
098500     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
098600                                 R3-LINE-DUE-DATE                         
098700                                 R3-LINE-AMOUNT                           
098800                                 R3-LINE-AMOUNT-LC                        
098900                                 R3-LINE-TAX-AMOUNT                       
099000                                 R3-LINE-TAX-AMOUNT-LC                    
099100                                 R3-LINE-NUMBER-OF-DAYS                   
099200                                 R3-LINE-QUANTITY                         
099300                                 R3-LINE-SAMNR                            
099400     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
099500     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
099600     MOVE 'TW01'              TO R3-LINE-COMPANY-CODE                     
099700     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
099800     IF SYST-KDPOST = '50'                                                
099900       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
100000     ELSE                                                                 
100100       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
100200     END-IF                                                               
100300     IF SYST-IDPRCTR NOT = SPACE                                          
100400       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
100500       IF WS-PRCTR-PRODSL = '??'                                          
100600         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
100700                                      WS-PRODSL-PC                        
100800         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
100900       END-IF                                                             
101000       EVALUATE TRUE                                                      
101100       WHEN KDPRODSL-PC-91                                                
101200         MOVE 91                     TO R3-LINE-PROFIT-CENTER             
101300       WHEN KDPRODSL-PC-95                                                
101400         MOVE 95                     TO R3-LINE-PROFIT-CENTER             
101500       WHEN OTHER                                                         
101600         MOVE WS-PRCTR               TO R3-LINE-PROFIT-CENTER             
101700       END-EVALUATE                                                       
101800     END-IF                                                               
101900     .                                                                    
102000     EJECT                                                                
102100                                                                          
102200 CE-SCHEDULE-LINE-GL SECTION.                                             
102300     MOVE NEJ                     TO WS-HEADER-SW                         
102400     MOVE JA                      TO WS-LINE-SW                           
102500     EVALUATE IN-EKH-KDEKHHT                                              
102600     WHEN '102'                                                           
102700          PERFORM CEB-MAIN-EVENT-102                                      
102800     WHEN '103'                                                           
102900          PERFORM CEC-MAIN-EVENT-103                                      
103000     WHEN '201'                                                           
103100          PERFORM CED-MAIN-EVENT-201                                      
103200     WHEN '203'                                                           
103300          PERFORM CEF-MAIN-EVENT-203                                      
103400     WHEN '204'                                                           
103500          PERFORM CEG-MAIN-EVENT-204                                      
103600     WHEN '302'                                                           
103700          PERFORM CEI-MAIN-EVENT-302                                      
103800     WHEN '303'                                                           
103900          PERFORM CEJ-MAIN-EVENT-303                                      
104000     WHEN '401'                                                           
104100          PERFORM CEK-MAIN-EVENT-401                                      
104200     WHEN '402'                                                           
104300          PERFORM CEL-MAIN-EVENT-402                                      
104400     WHEN '403'                                                           
104500          PERFORM CEM-MAIN-EVENT-403                                      
104600     WHEN '404'                                                           
104700          PERFORM CEN-MAIN-EVENT-404                                      
104800     END-EVALUATE                                                         
104900     .                                                                    
105000     EJECT                                                                
105100                                                                          
105200 CEB-MAIN-EVENT-102 SECTION.                                              
105300     EVALUATE IN-EKH-KDEKSHT                                              
105400     WHEN '102'                                                           
105500          PERFORM CEBB-SUB-EVENT-102-102                                  
105600     WHEN '120'                                                           
105700          PERFORM CEBD-SUB-EVENT-102-120                                  
105800     WHEN '121'                                                           
105900          PERFORM CEBD-SUB-EVENT-102-121                                  
106000     WHEN '122'                                                           
106100          PERFORM CEBD-SUB-EVENT-102-122                                  
106200     WHEN '123'                                                           
106300          PERFORM CEBD-SUB-EVENT-102-123                                  
106400     WHEN '124'                                                           
106500          PERFORM CEBD-SUB-EVENT-102-124                                  
106600     WHEN '125'                                                           
106700          PERFORM CEBD-SUB-EVENT-102-125                                  
106800     WHEN '130'                                                           
106900          PERFORM CEBD-SUB-EVENT-102-130                                  
107000     WHEN '131'                                                           
107100          PERFORM CEBD-SUB-EVENT-102-131                                  
107200     WHEN '132'                                                           
107300          PERFORM CEBD-SUB-EVENT-102-132                                  
107400     WHEN '134'                                                           
107500          PERFORM CEBD-SUB-EVENT-102-134                                  
107600     END-EVALUATE                                                         
107700     .                                                                    
107800     EJECT                                                                
107900                                                                          
108000 CEBB-SUB-EVENT-102-102 SECTION.                                          
108100     EVALUATE IN-EKH-KDEKNIVA                                             
108200     WHEN 'DET'                                                           
108300       IF SYST-IDSEKVNR = 1                                               
108400         IF IN-EKH-KVANTAL > 0                                            
108500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
108600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
108700           COMPUTE R3-LINE-AMOUNT-LC =                                    
108800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
108900           IF IN-EKH-KDVALISO = 'TWD'                                     
109000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
109100           END-IF                                                         
109200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
109300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
109400           MOVE SPACE               TO WS-ALLOCATE-REF                    
109500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
109600           PERFORM S02-WRITE-W57011A                                      
109700         END-IF                                                           
109800       END-IF                                                             
109900                                                                          
110000       IF SYST-IDSEKVNR = 2                                               
110100         IF IN-EKH-KVANTAL < 0                                            
110200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
110300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
110400           COMPUTE R3-LINE-AMOUNT-LC =                                    
110500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
110600           IF IN-EKH-KDVALISO = 'TWD'                                     
110700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
110800           END-IF                                                         
110900           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
111000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
111100           MOVE SPACE               TO WS-ALLOCATE-REF                    
111200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
111300           PERFORM S02-WRITE-W57011A                                      
111400         END-IF                                                           
111500       END-IF                                                             
111600                                                                          
111700       IF SYST-IDSEKVNR = 3                                               
111800         IF IN-EKH-KVANTAL < 0                                            
111900           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
112000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
112100           COMPUTE R3-LINE-AMOUNT-LC =                                    
112200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
112300           IF IN-EKH-KDVALISO = 'TWD'                                     
112400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
112500           END-IF                                                         
112600           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
112700           PERFORM S02-WRITE-W57011A                                      
112800         END-IF                                                           
112900       END-IF                                                             
113000                                                                          
113100       IF SYST-IDSEKVNR = 4                                               
113200         IF IN-EKH-KVANTAL > 0                                            
113300           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
113400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
113500           COMPUTE R3-LINE-AMOUNT-LC =                                    
113600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
113700           IF IN-EKH-KDVALISO = 'TWD'                                     
113800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
113900           END-IF                                                         
114000           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
114100           PERFORM S02-WRITE-W57011A                                      
114200         END-IF                                                           
114300       END-IF                                                             
114400                                                                          
114500     END-EVALUATE                                                         
114600     .                                                                    
114700     EJECT                                                                
114800                                                                          
114900 CEBD-SUB-EVENT-102-120 SECTION.                                          
115000     EVALUATE IN-EKH-KDEKNIVA                                             
115100     WHEN 'DET'                                                           
115200       IF SYST-IDSEKVNR = 1                                               
115300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
115400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
115500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
115600          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW * -1            
115700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
115800         PERFORM S03-WRITE-W57012                                         
115900       END-IF                                                             
116000                                                                          
116100     WHEN 'FÖRS'                                                          
116200     WHEN 'FRAKT'                                                         
116300     WHEN 'EMB'                                                           
116400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
116500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
116600       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
116700               IN-EKH-SUBEL / WS-PRKURS-TW   * -1                         
116800       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
116900       PERFORM S04-WRITE-W57013A                                          
117000                                                                          
117100     WHEN 'DDI'                                                           
117200       IF IN-EKH-SUBEL > ZERO                                             
117300         IF SYST-IDSEKVNR = 1                                             
117400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
117500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
117600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
117700                   IN-EKH-SUBEL                                           
117800           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
117900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
118000           PERFORM S04-WRITE-W57013A                                      
118100         END-IF                                                           
118200       ELSE                                                               
118300         IF SYST-IDSEKVNR = 2                                             
118400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
118500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
118600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
118700                   IN-EKH-SUBEL                                           
118800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
118900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
119000           PERFORM S04-WRITE-W57013A                                      
119100         END-IF                                                           
119200       END-IF                                                             
119300     END-EVALUATE                                                         
119400     .                                                                    
119500     EJECT                                                                
119600                                                                          
119700 CEBD-SUB-EVENT-102-121 SECTION.                                          
119800     EVALUATE IN-EKH-KDEKNIVA                                             
119900     WHEN 'DET'                                                           
120000       IF SYST-IDSEKVNR = 1                                               
120100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
120200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
120300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
120400             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-TW3            
120500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
120600         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-1                      
120700         PERFORM S03-WRITE-W57012                                         
120800       END-IF                                                             
120900                                                                          
121000       IF SYST-IDSEKVNR = 2                                               
121100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
121200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
121300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
121400            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3              
121500            + IN-EKH-KVANTAL *                                            
121600            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP                   
121700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
121800         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-2                      
121900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
122000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
122100         MOVE SPACE               TO WS-ALLOCATE-REF                      
122200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
122300         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
122400         PERFORM S03-WRITE-W57012                                         
122500       END-IF                                                             
122600                                                                          
122700       IF SYST-IDSEKVNR = 3                                               
122800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
122900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
123000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
123100            WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
123200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
123300         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
123400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
123500         MOVE SPACE               TO WS-ALLOCATE-REF                      
123600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
123700         PERFORM S03-WRITE-W57012                                         
123800       END-IF                                                             
123900                                                                          
124000     WHEN 'FÖRS'                                                          
124100     WHEN 'FRAKT'                                                         
124200     WHEN 'EMB'                                                           
124300       IF SYST-IDSEKVNR = 1                                               
124400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
124500         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
124600         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
124700                 IN-EKH-SUBEL / WS-PRKURS-TW3                             
124800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
124900         PERFORM S04-WRITE-W57013A                                        
125000       END-IF                                                             
125100                                                                          
125200       IF SYST-IDSEKVNR = 2                                               
125300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
125400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
125500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
125600                 IN-EKH-SUBEL / WS-PRKURS-TW3                             
125700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
125800         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
125900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
126000         MOVE SPACE               TO WS-ALLOCATE-REF                      
126100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
126200         PERFORM S04-WRITE-W57013A                                        
126300       END-IF                                                             
126400                                                                          
126500     END-EVALUATE                                                         
126600                                                                          
126700     .                                                                    
126800     EJECT                                                                
126900                                                                          
127000 CEBD-SUB-EVENT-102-122 SECTION.                                          
127100     EVALUATE IN-EKH-KDEKNIVA                                             
127200     WHEN 'DET'                                                           
127300       IF IN-EKH-KVANTAL > 0                                              
127400         IF SYST-IDSEKVNR = 1                                             
127500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
127600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
127700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
127800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
127900            + (IN-EKH-KVANTAL *                                           
128000            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
128100           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
128200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
128300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
128400           MOVE SPACE               TO WS-ALLOCATE-REF                    
128500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
128600           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
128700           PERFORM S02-WRITE-W57011A                                      
128800         END-IF                                                           
128900                                                                          
129000         IF SYST-IDSEKVNR = 4                                             
129100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
129200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
129300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
129400            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
129500            + (IN-EKH-KVANTAL *                                           
129600            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
129700           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
129800           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
129900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
130000           MOVE SPACE               TO WS-ALLOCATE-REF                    
130100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
130200           MOVE SPACE               TO R3-LINE-COST-CENTER                
130300           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
130400           PERFORM S02-WRITE-W57011A                                      
130500         END-IF                                                           
130600       END-IF                                                             
130700                                                                          
130800       IF IN-EKH-KVANTAL < 0                                              
130900         IF SYST-IDSEKVNR = 2                                             
131000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
131100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
131200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
131300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
131400            + (IN-EKH-KVANTAL *                                           
131500            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
131600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
131700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
131800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
131900           MOVE SPACE               TO WS-ALLOCATE-REF                    
132000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
132100           MOVE SPACE             TO R3-LINE-COST-CENTER                  
132200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
132300           PERFORM S02-WRITE-W57011A                                      
132400         END-IF                                                           
132500                                                                          
132600         IF SYST-IDSEKVNR = 3                                             
132700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
132800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
132900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
133000            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
133100            + (IN-EKH-KVANTAL *                                           
133200            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
133300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
133400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
133500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
133600           MOVE SPACE               TO WS-ALLOCATE-REF                    
133700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
133800           MOVE SPACE             TO R3-LINE-COST-CENTER                  
133900           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
134000           PERFORM S02-WRITE-W57011A                                      
134100         END-IF                                                           
134200       END-IF                                                             
134300     END-EVALUATE                                                         
134400     .                                                                    
134500     EJECT                                                                
134600                                                                          
134700 CEBD-SUB-EVENT-102-123 SECTION.                                          
134800     EVALUATE IN-EKH-KDEKNIVA                                             
134900     WHEN 'DET'                                                           
135000       IF SYST-IDSEKVNR = 1                                               
135100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
135200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
135300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
135400              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
135500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
135600         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
135700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
135800         MOVE SPACE               TO WS-ALLOCATE-REF                      
135900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
136000         PERFORM S02-WRITE-W57011A                                        
136100       END-IF                                                             
136200                                                                          
136300       IF SYST-IDSEKVNR = 2                                               
136400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
136500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
136600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
136700             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
136800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
136900         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
137000         PERFORM S02-WRITE-W57011A                                        
137100       END-IF                                                             
137200     END-EVALUATE                                                         
137300     .                                                                    
137400     EJECT                                                                
137500                                                                          
137600 CEBD-SUB-EVENT-102-124 SECTION.                                          
137700                                                                          
137800     EVALUATE IN-EKH-KDEKNIVA                                             
137900     WHEN 'DET'                                                           
138000       IF SYST-IDSEKVNR = 1                                               
138100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
138200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
138300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
138400         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW   * -1           
138500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
138600                                   WS-102-124-DET-1                       
138700         MOVE SPACE               TO WS-ALLOCATE-DC                       
138800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
138900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
139000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
139100         PERFORM S03-WRITE-W57012                                         
139200*** CALC THE VALUE WITH THE LANDED COST                                   
139300         COMPUTE WS-102-124-DET-2 = WS-102-124-DET-1 * WS-MARKUP          
139400       END-IF                                                             
139500                                                                          
139600       IF SYST-IDSEKVNR = 2                                               
139700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
139800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
139900         MOVE WS-102-124-DET-2    TO R3-LINE-AMOUNT-LC                    
140000         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
140100         MOVE SPACE               TO WS-ALLOCATE-DC                       
140200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
140300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
140400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
140500         PERFORM S03-WRITE-W57012                                         
140600       END-IF                                                             
140700                                                                          
140800       IF SYST-IDSEKVNR = 3                                               
140900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
141000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
141100         MOVE WS-102-124-DET-2    TO R3-LINE-AMOUNT-LC                    
141200         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
141300         MOVE SPACE               TO WS-ALLOCATE-DC                       
141400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
141500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
141600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
141700         PERFORM S03-WRITE-W57012                                         
141800         MOVE ZERO              TO WS-102-124-DET-2                       
141900       END-IF                                                             
142000                                                                          
142100     WHEN 'EMB'                                                           
142200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
142300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
142400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
142500               (IN-EKH-SUBEL / WS-PRKURS-TW) * -1                         
142600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
142700       MOVE SPACE               TO WS-ALLOCATE-DC                         
142800       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
142900       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
143000       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
143100       PERFORM S04-WRITE-W57013A                                          
143200                                                                          
143300     WHEN 'DDI'                                                           
143400       IF IN-EKH-SUBEL > ZERO                                             
143500         IF SYST-IDSEKVNR = 1                                             
143600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
143700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
143800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
143900                   IN-EKH-SUBEL                                           
144000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
144100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
144200           MOVE SPACE               TO WS-ALLOCATE-DC                     
144300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
144400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
144500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
144600           PERFORM S04-WRITE-W57013A                                      
144700         END-IF                                                           
144800       ELSE                                                               
144900         IF SYST-IDSEKVNR = 2                                             
145000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
145100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
145200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
145300                   IN-EKH-SUBEL                                           
145400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
145500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
145600           MOVE SPACE               TO WS-ALLOCATE-DC                     
145700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
145800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
145900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
146000           PERFORM S04-WRITE-W57013A                                      
146100         END-IF                                                           
146200       END-IF                                                             
146300     END-EVALUATE                                                         
146400     .                                                                    
146500     EJECT                                                                
146600                                                                          
146700 CEBD-SUB-EVENT-102-125 SECTION.                                          
146800     EVALUATE IN-EKH-KDEKNIVA                                             
146900     WHEN 'DET'                                                           
147000       IF SYST-IDSEKVNR = 1                                               
147100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
147200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
147300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
147400         (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW) +              
147500         (IN-EKH-KVANTAL *                                                
147600          IN-EKH-PRARTNTO / WS-PRKURS-TW * WS-MARKUP)                     
147700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
147800         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
147900         PERFORM S03-WRITE-W57012                                         
148000       END-IF                                                             
148100                                                                          
148200       IF SYST-IDSEKVNR = 2                                               
148300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
148400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
148500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
148600            (IN-EKH-KVANTAL *                                             
148700             IN-EKH-PRARTNTO / WS-PRKURS-TW * WS-MARKUP)                  
148800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
148900         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
149000         PERFORM S03-WRITE-W57012                                         
149100       END-IF                                                             
149200                                                                          
149300     WHEN 'FÖRS'                                                          
149400     WHEN 'FRAKT'                                                         
149500     WHEN 'EMB'                                                           
149600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
149700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
149800       MOVE SPACE             TO R3-LINE-COST-CENTER                      
149900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
150000          IN-EKH-SUBEL / WS-PRKURS-TW   * -1                              
150100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
150200       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
150300       PERFORM S04-WRITE-W57013A                                          
150400                                                                          
150500     WHEN 'DDI'                                                           
150600       IF SPAR-SUMMA-102-125 < ZERO                                       
150700         IF SYST-IDSEKVNR = 1                                             
150800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
150900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
151000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
151100                   SPAR-SUMMA-102-125                                     
151200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
151300           PERFORM S04-WRITE-W57013A                                      
151400         END-IF                                                           
151500       END-IF                                                             
151600       IF SPAR-SUMMA-102-125 > ZERO                                       
151700         IF SYST-IDSEKVNR = 2                                             
151800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
151900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
152000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
152100                   SPAR-SUMMA-102-125                                     
152200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
152300           PERFORM S04-WRITE-W57013A                                      
152400         END-IF                                                           
152500       END-IF                                                             
152600                                                                          
152700     END-EVALUATE                                                         
152800     .                                                                    
152900     EJECT                                                                
153000                                                                          
153100 CEBD-SUB-EVENT-102-130 SECTION.                                          
153200     EVALUATE IN-EKH-KDEKNIVA                                             
153300     WHEN 'DET'                                                           
153400       IF SYST-IDSEKVNR = 1                                               
153500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
153600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
153700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
153800         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW  * -1            
153900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
154000         PERFORM S03-WRITE-W57012                                         
154100       END-IF                                                             
154200                                                                          
154300     WHEN 'FÖRS'                                                          
154400     WHEN 'FRAKT'                                                         
154500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
154600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
154700       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
155400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
155500               IN-EKH-SUBEL / WS-PRKURS-TW  * -1                          
155600       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
155700       PERFORM S04-WRITE-W57013A                                          
155800                                                                          
155900     WHEN 'EMB'                                                           
156000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
156100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
156200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
156300               IN-EKH-SUBEL / WS-PRKURS-TW  * -1                          
156400       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
156500       PERFORM S04-WRITE-W57013A                                          
156600                                                                          
156700     WHEN 'DDI'                                                           
156800       IF IN-EKH-SUBEL > ZERO                                             
156900         IF SYST-IDSEKVNR = 1                                             
157000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
157100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
157200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
157300                   IN-EKH-SUBEL                                           
157400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
157500           PERFORM S04-WRITE-W57013A                                      
157600         END-IF                                                           
157700       ELSE                                                               
157800         IF SYST-IDSEKVNR = 2                                             
157900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
158000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
158100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
158200                   IN-EKH-SUBEL                                           
158300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
158400           PERFORM S04-WRITE-W57013A                                      
158500         END-IF                                                           
158600       END-IF                                                             
158700                                                                          
158800     END-EVALUATE                                                         
158900     .                                                                    
159000     EJECT                                                                
159100                                                                          
159200 CEBD-SUB-EVENT-102-131 SECTION.                                          
159300     EVALUATE IN-EKH-KDEKNIVA                                             
159400     WHEN 'DET'                                                           
159500       IF SYST-IDSEKVNR = 1                                               
159600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
159700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
159800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
159900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
160000         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-1                   
160100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
160200         PERFORM S03-WRITE-W57012                                         
160300       END-IF                                                             
160400                                                                          
160500       IF SYST-IDSEKVNR = 2                                               
160600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
160700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
160800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
160900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3) +          
161000            (IN-EKH-KVANTAL *                                             
161100             IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                 
161200         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-2                   
161300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
161400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
161500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
161600         MOVE SPACE               TO WS-ALLOCATE-REF                      
161700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
161800         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
161900         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
162000         PERFORM S03-WRITE-W57012                                         
162100       END-IF                                                             
162200                                                                          
162300       IF SYST-IDSEKVNR = 3                                               
162400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
162500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
162600         COMPUTE R3-LINE-AMOUNT-LC =                                      
162700                 WS-LINE-AMOUNT-131-2 - WS-LINE-AMOUNT-131-1              
162800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
162900         PERFORM S03-WRITE-W57012                                         
163000       END-IF                                                             
163100                                                                          
163200     WHEN 'FÖRS'                                                          
163300     WHEN 'FRAKT'                                                         
163400     WHEN 'EMB'                                                           
163500       IF SYST-IDSEKVNR = 1                                               
163600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
163700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
163800         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
163900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
164000                 IN-EKH-SUBEL / WS-PRKURS-TW3                             
164100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
164200         PERFORM S04-WRITE-W57013A                                        
164300       END-IF                                                             
164400                                                                          
164500       IF SYST-IDSEKVNR = 2                                               
164600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
164700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
164800         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
164900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
165000                 IN-EKH-SUBEL / WS-PRKURS-TW3                             
165100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
165200         PERFORM S04-WRITE-W57013A                                        
165300       END-IF                                                             
165400                                                                          
165500     END-EVALUATE                                                         
165600     .                                                                    
165700     EJECT                                                                
165800                                                                          
165900 CEBD-SUB-EVENT-102-132 SECTION.                                          
166000     EVALUATE IN-EKH-KDEKNIVA                                             
166100     WHEN 'DET'                                                           
166200       IF IN-EKH-KVANTAL > 0                                              
166300         IF SYST-IDSEKVNR = 1                                             
166400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
166500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
166600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
166700            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
166800            + (IN-EKH-KVANTAL *                                           
166900            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
167000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
167100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
167200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
167300           MOVE SPACE               TO WS-ALLOCATE-REF                    
167400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
167500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
167600           PERFORM S02-WRITE-W57011A                                      
167700         END-IF                                                           
167800                                                                          
167900         IF SYST-IDSEKVNR = 4                                             
168000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
168100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
168200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
168300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
168400            + (IN-EKH-KVANTAL *                                           
168500            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
168600           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
168700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
168800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
168900           MOVE SPACE               TO WS-ALLOCATE-REF                    
169000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
169100           MOVE SPACE               TO R3-LINE-COST-CENTER                
169200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
169300           PERFORM S02-WRITE-W57011A                                      
169400         END-IF                                                           
169500       END-IF                                                             
169600                                                                          
169700       IF IN-EKH-KVANTAL < 0                                              
169800         IF SYST-IDSEKVNR = 2                                             
169900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
170000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
170100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
170200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
170300            + (IN-EKH-KVANTAL *                                           
170400            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
170500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
170600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
170700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
170800           MOVE SPACE               TO WS-ALLOCATE-REF                    
170900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
171000           MOVE SPACE             TO R3-LINE-COST-CENTER                  
171100           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
171200           PERFORM S02-WRITE-W57011A                                      
171300         END-IF                                                           
171400                                                                          
171500         IF SYST-IDSEKVNR = 3                                             
171600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
171700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
171800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
171900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3)            
172000            + (IN-EKH-KVANTAL *                                           
172100            IN-EKH-PRARTNTO / WS-PRKURS-TW3 * WS-MARKUP)                  
172200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
172300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
172400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
172500           MOVE SPACE               TO WS-ALLOCATE-REF                    
172600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
172700           MOVE SPACE             TO R3-LINE-COST-CENTER                  
172800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
172900           PERFORM S02-WRITE-W57011A                                      
173000         END-IF                                                           
173100       END-IF                                                             
173200     END-EVALUATE                                                         
173300     .                                                                    
173400     EJECT                                                                
173500                                                                          
173600 CEBD-SUB-EVENT-102-134 SECTION.                                          
173700     EVALUATE IN-EKH-KDEKNIVA                                             
173800     WHEN 'DET'                                                           
173900       IF SYST-IDSEKVNR = 1                                               
174000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
174100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
174200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
174300         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW   * -1           
174400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
174500                                   WS-102-134-DET-1                       
174600         MOVE SPACE               TO WS-ALLOCATE-DC                       
174700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
174800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
174900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
175000         PERFORM S03-WRITE-W57012                                         
175100*** CALC THE VALUE WITH THE LANDED COST                                   
175200         COMPUTE WS-102-134-DET-2 = WS-102-134-DET-1 * WS-MARKUP          
175300       END-IF                                                             
175400                                                                          
175500       IF SYST-IDSEKVNR = 2                                               
175600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
175700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
175800         MOVE WS-102-134-DET-2    TO R3-LINE-AMOUNT-LC                    
175900         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
176000         MOVE SPACE               TO WS-ALLOCATE-DC                       
176100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
176200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
176300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
176400         PERFORM S03-WRITE-W57012                                         
176500       END-IF                                                             
176600                                                                          
176700       IF SYST-IDSEKVNR = 3                                               
176800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
176900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
177000         MOVE WS-102-134-DET-2    TO R3-LINE-AMOUNT-LC                    
177100         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
177200         MOVE SPACE               TO WS-ALLOCATE-DC                       
177300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
177400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
177500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
177600         PERFORM S03-WRITE-W57012                                         
177700         MOVE ZERO              TO WS-102-134-DET-2                       
177800       END-IF                                                             
177900                                                                          
178000     WHEN 'EMB'                                                           
178100     WHEN 'FÖRS'                                                          
178200     WHEN 'FRAKT'                                                         
178300       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
178400       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
178500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
178600               (IN-EKH-SUBEL / WS-PRKURS-TW) * -1                         
178700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
178800       MOVE SPACE               TO WS-ALLOCATE-DC                         
178900       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
179000       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
179100       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
179200       PERFORM S04-WRITE-W57013A                                          
179300                                                                          
179400     WHEN 'DDI'                                                           
179500       IF IN-EKH-SUBEL > ZERO                                             
179600         IF SYST-IDSEKVNR = 1                                             
179700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
179800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
179900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
180000                   IN-EKH-SUBEL                                           
180100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
180200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
180300           MOVE SPACE               TO WS-ALLOCATE-DC                     
180400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
180500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
180600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
180700           PERFORM S04-WRITE-W57013A                                      
180800         END-IF                                                           
180900       ELSE                                                               
181000         IF SYST-IDSEKVNR = 2                                             
181100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
181200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
181300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
181400                   IN-EKH-SUBEL                                           
181500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
181600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
181700           MOVE SPACE               TO WS-ALLOCATE-DC                     
181800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
181900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
182000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
182100           PERFORM S04-WRITE-W57013A                                      
182200         END-IF                                                           
182300       END-IF                                                             
182400     END-EVALUATE                                                         
182500     .                                                                    
182600     EJECT                                                                
182700                                                                          
182800 CEC-MAIN-EVENT-103 SECTION.                                              
182900     EVALUATE IN-EKH-KDEKSHT                                              
183000     WHEN '102'                                                           
183100          PERFORM CECB-SUB-EVENT-103-102                                  
183200     END-EVALUATE                                                         
183300     .                                                                    
183400     EJECT                                                                
183500                                                                          
183600 CECB-SUB-EVENT-103-102 SECTION.                                          
183700     EVALUATE IN-EKH-KDEKNIVA                                             
183800     WHEN 'DET'                                                           
183900       IF SYST-IDSEKVNR = 1                                               
184000         IF IN-EKH-KVANTAL < 0                                            
184100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
184200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
184300           COMPUTE R3-LINE-AMOUNT-LC =                                    
184400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
184500           IF IN-EKH-KDVALISO = 'TWD'                                     
184600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
184700           END-IF                                                         
184800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
184900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
185000           MOVE SPACE               TO WS-ALLOCATE-REF                    
185100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
185200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
185300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
185400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
185500           PERFORM S04-WRITE-W57013A                                      
185600         END-IF                                                           
185700       END-IF                                                             
185800                                                                          
185900       IF SYST-IDSEKVNR = 2                                               
186000         IF IN-EKH-KVANTAL > 0                                            
186100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
186200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
186300           COMPUTE R3-LINE-AMOUNT-LC =                                    
186400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
186500           IF IN-EKH-KDVALISO = 'TWD'                                     
186600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
186700           END-IF                                                         
186800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
186900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
187000           MOVE SPACE               TO WS-ALLOCATE-REF                    
187100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
187200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
187300           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
187400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
187500           PERFORM S04-WRITE-W57013A                                      
187600         END-IF                                                           
187700       END-IF                                                             
187800                                                                          
187900     WHEN 'DDI'                                                           
188000       IF IN-EKH-SUBEL > ZERO                                             
188100         IF SYST-IDSEKVNR = 1                                             
188200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
188300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
188400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
188500                   IN-EKH-SUBEL                                           
188600           MOVE ZEROES              TO R3-LINE-AMOUNT                     
188610           IF IN-EKH-KDVALISO = 'TWD'                                     
188620             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
188630           END-IF                                                         
188700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
189100           MOVE SPACE               TO R3-LINE-ALLOCATE                   
189200           PERFORM S04-WRITE-W57013A                                      
189300         END-IF                                                           
189400       ELSE                                                               
189500         IF SYST-IDSEKVNR = 2                                             
189600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
189700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
189800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
189900                   IN-EKH-SUBEL                                           
190000           MOVE ZEROES              TO R3-LINE-AMOUNT                     
190010           IF IN-EKH-KDVALISO = 'TWD'                                     
190020             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
190030           END-IF                                                         
190040           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
190050           MOVE SPACE               TO R3-LINE-ALLOCATE                   
190600           PERFORM S04-WRITE-W57013A                                      
190700         END-IF                                                           
190800       END-IF                                                             
190900     END-EVALUATE                                                         
191000     .                                                                    
191100     EJECT                                                                
191200                                                                          
191300 CED-MAIN-EVENT-201 SECTION.                                              
191400     EVALUATE IN-EKH-KDEKSHT                                              
191500     WHEN '201'                                                           
191600          PERFORM CEDA-SUB-EVENT-201-201                                  
191700     WHEN '202'                                                           
191800          PERFORM CEDB-SUB-EVENT-201-202                                  
191900     END-EVALUATE                                                         
192000     .                                                                    
192100     EJECT                                                                
192200                                                                          
192300 CEDA-SUB-EVENT-201-201 SECTION.                                          
192400     EVALUATE IN-EKH-KDEKNIVA                                             
192500     WHEN 'DET'                                                           
192600       IF SYST-IDSEKVNR = 1                                               
192700         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
192800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
192900         COMPUTE R3-LINE-AMOUNT-LC =                                      
193000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
193100         IF R3-LINE-AMOUNT-LC < 1                                         
193200           MOVE 1.0               TO R3-LINE-AMOUNT-LC                    
193300         END-IF                                                           
193400         IF IN-EKH-KDVALISO = 'TWD'                                       
193500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
193600         END-IF                                                           
193700         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
193800         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
193900         PERFORM S03-WRITE-W57012                                         
194000       END-IF                                                             
194100                                                                          
194200       IF SYST-IDSEKVNR = 2                                               
194300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
194400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
194500         COMPUTE R3-LINE-AMOUNT-LC =                                      
194600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
194700         IF R3-LINE-AMOUNT-LC < 1                                         
194800           MOVE 1.0               TO R3-LINE-AMOUNT-LC                    
194900         END-IF                                                           
195000         IF IN-EKH-KDVALISO = 'TWD'                                       
195100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
195200         END-IF                                                           
195300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
195400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
195500         MOVE SPACE               TO WS-ALLOCATE-REF                      
195600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
195700         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
195800         PERFORM S03-WRITE-W57012                                         
195900       END-IF                                                             
196000     END-EVALUATE                                                         
196100     .                                                                    
196200     EJECT                                                                
196300 CEDB-SUB-EVENT-201-202 SECTION.                                          
196400     EVALUATE IN-EKH-KDEKNIVA                                             
196500     WHEN 'DET'                                                           
196600       IF SYST-IDSEKVNR = 1                                               
196700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
196800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
196900         COMPUTE R3-LINE-AMOUNT-LC =                                      
197000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
197100         IF R3-LINE-AMOUNT-LC < 1                                         
197200           MOVE 1.0               TO R3-LINE-AMOUNT-LC                    
197300         END-IF                                                           
197400         IF IN-EKH-KDVALISO = 'TWD'                                       
197500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
197600         END-IF                                                           
197700         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
197800         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
197900         PERFORM S03-WRITE-W57012                                         
198000       END-IF                                                             
198100                                                                          
198200       IF SYST-IDSEKVNR = 2                                               
198300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
198400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
198500         COMPUTE R3-LINE-AMOUNT-LC =                                      
198600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
198700         IF R3-LINE-AMOUNT-LC < 1                                         
198800           MOVE 1.0               TO R3-LINE-AMOUNT-LC                    
198900         END-IF                                                           
199000         IF IN-EKH-KDVALISO = 'TWD'                                       
199100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
199200         END-IF                                                           
199300         MOVE SPACE               TO WS-ALLOCATE-DC                       
199400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
199500         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
199600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
199700         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
199800         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
199900         PERFORM S03-WRITE-W57012                                         
200000       END-IF                                                             
200100     END-EVALUATE                                                         
200200     .                                                                    
200300     EJECT                                                                
200400                                                                          
200500 CEF-MAIN-EVENT-203 SECTION.                                              
200600     EVALUATE IN-EKH-KDEKSHT                                              
200700     WHEN '201'                                                           
200800          PERFORM CEFA-SUB-EVENT-203-201                                  
200900     END-EVALUATE                                                         
201000     .                                                                    
201100     EJECT                                                                
201200                                                                          
201300 CEFA-SUB-EVENT-203-201 SECTION.                                          
201400     EVALUATE IN-EKH-KDEKNIVA                                             
201500     WHEN 'DET'                                                           
201600       IF SYST-IDSEKVNR = 1                                               
201700         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
201800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
201900         COMPUTE R3-LINE-AMOUNT-LC =                                      
202000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
202100         IF IN-EKH-KDVALISO = 'TWD'                                       
202200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
202300         END-IF                                                           
202400         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
202500         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
202600         PERFORM S03-WRITE-W57012                                         
202700       END-IF                                                             
202800                                                                          
202900       IF SYST-IDSEKVNR = 2                                               
203000         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
203100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
203200         COMPUTE R3-LINE-AMOUNT-LC =                                      
203300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
203400         IF IN-EKH-KDVALISO = 'TWD'                                       
203500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
203600         END-IF                                                           
203700         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
203800         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
203900         MOVE SPACE             TO WS-ALLOCATE-REF                        
204000         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
204100         MOVE 0000430877        TO R3-LINE-PA-CUSTOMER                    
204200         PERFORM S03-WRITE-W57012                                         
204300       END-IF                                                             
204400     END-EVALUATE                                                         
204500     .                                                                    
204600     EJECT                                                                
204700                                                                          
204800 CEG-MAIN-EVENT-204 SECTION.                                              
204900     EVALUATE IN-EKH-KDEKSHT                                              
205000     WHEN '201'                                                           
205100          PERFORM CEGA-SUB-EVENT-204-201                                  
205200     WHEN '301'                                                           
205300          PERFORM CEGB-SUB-EVENT-204-301                                  
205400     END-EVALUATE                                                         
205500     .                                                                    
205600     EJECT                                                                
205700                                                                          
205800 CEGA-SUB-EVENT-204-201 SECTION.                                          
205900     EVALUATE IN-EKH-KDEKNIVA                                             
206000     WHEN 'DET'                                                           
206100         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
206200* R-FAKTURA                                                               
206300       IF SYST-IDSEKVNR = 1                                               
206400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
206500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
206600         COMPUTE R3-LINE-AMOUNT-LC =                                      
206700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
206800         IF IN-EKH-KDVALISO = 'TWD'                                       
206900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
207000         END-IF                                                           
207100         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
207200         MOVE 'TW  '              TO R3-LINE-TRADING-PARTNER              
207300         MOVE SPACE               TO WS-ALLOCATE-DC                       
207400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
207500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
207600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
207700         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
207800         PERFORM S03-WRITE-W57012                                         
207900       END-IF                                                             
208000                                                                          
208100       IF SYST-IDSEKVNR = 2                                               
208200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
208300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
208400         COMPUTE R3-LINE-AMOUNT-LC =                                      
208500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
208600         IF IN-EKH-KDVALISO = 'TWD'                                       
208700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
208800         END-IF                                                           
208900         MOVE SPACE               TO WS-ALLOCATE-DC                       
209000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
209100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
209200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
209300         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
209400         PERFORM S03-WRITE-W57012                                         
209500       END-IF                                                             
209600     END-EVALUATE                                                         
209700     .                                                                    
209800     EJECT                                                                
209900 CEGB-SUB-EVENT-204-301 SECTION.                                          
210000     EVALUATE IN-EKH-KDEKNIVA                                             
210100     WHEN 'DET'                                                           
210200         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
210300* R-FAKTURA                                                               
210400       IF SYST-IDSEKVNR = 1                                               
210500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
210600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
210700         COMPUTE R3-LINE-AMOUNT-LC =                                      
210800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
210900         IF IN-EKH-KDVALISO = 'TWD'                                       
211000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
211100         END-IF                                                           
211200         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
211300         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
211400         PERFORM S03-WRITE-W57012                                         
211500       END-IF                                                             
211600                                                                          
211700       IF SYST-IDSEKVNR = 2                                               
211800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
211900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
212000         COMPUTE R3-LINE-AMOUNT-LC =                                      
212100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
212200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
212300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
212400         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
212500         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
212600         MOVE SPACE             TO WS-ALLOCATE-REF                        
212700         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
212800         PERFORM S03-WRITE-W57012                                         
212900       END-IF                                                             
213000                                                                          
213100       IF SYST-IDSEKVNR = 3                                               
213200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
213300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
213400         COMPUTE R3-LINE-AMOUNT-LC =                                      
213500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
213600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
213700         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
213800         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
213900         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
214000         MOVE SPACE             TO WS-ALLOCATE-REF                        
214100         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
214200         PERFORM S03-WRITE-W57012                                         
214300       END-IF                                                             
214400                                                                          
214500     WHEN 'EMB'                                                           
214600     WHEN 'FÖRS'                                                          
214700     WHEN 'FRAKT'                                                         
214800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
214900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
215000       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
215700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
215800               IN-EKH-SUBEL * -1                                          
215900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
216000       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
216100       PERFORM S04-WRITE-W57013A                                          
216200                                                                          
216300     END-EVALUATE                                                         
216400     .                                                                    
216500     EJECT                                                                
216600                                                                          
216700 CEI-MAIN-EVENT-302 SECTION.                                              
216800     EVALUATE IN-EKH-KDEKSHT                                              
216900     WHEN '301'                                                           
217000          PERFORM CEIA-SUB-EVENT-302-301                                  
217100     WHEN '302'                                                           
217200          PERFORM CEIB-SUB-EVENT-302-302                                  
217300     END-EVALUATE                                                         
217400     .                                                                    
217500     EJECT                                                                
217600                                                                          
217700 CEIA-SUB-EVENT-302-301 SECTION.                                          
217800     EVALUATE IN-EKH-KDEKNIVA                                             
217900     WHEN 'DET'                                                           
218000       IF SYST-IDSEKVNR = 1                                               
218100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
218200         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
218300         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
218400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
218500         COMPUTE R3-LINE-AMOUNT-LC =                                      
218600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
218700         IF R3-LINE-AMOUNT-LC < 1                                         
218800           MOVE 1.0               TO R3-LINE-AMOUNT-LC                    
218900         END-IF                                                           
219000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
219100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
219200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
219300         MOVE SPACE               TO WS-ALLOCATE-REF                      
219400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
219500         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
219600         PERFORM S02-WRITE-W57011A                                        
219700       END-IF                                                             
219800                                                                          
219900       IF SYST-IDSEKVNR = 2                                               
220000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
220100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
220200         MOVE SPACE           TO R3-LINE-COST-CENTER                      
220300         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
220400         COMPUTE R3-LINE-AMOUNT-LC =                                      
220500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
220600         IF R3-LINE-AMOUNT-LC < 1                                         
220700           MOVE 1.0               TO R3-LINE-AMOUNT-LC                    
220800         END-IF                                                           
220900         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
221000         MOVE SPACE               TO WS-LINE-TEXT                         
221100         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
221200         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
221300         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
221400         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
221500         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
221600         PERFORM S02-WRITE-W57011A                                        
221700       END-IF                                                             
221800     END-EVALUATE                                                         
221900     .                                                                    
222000     EJECT                                                                
222100                                                                          
222200 CEIB-SUB-EVENT-302-302 SECTION.                                          
222300     EVALUATE IN-EKH-KDEKNIVA                                             
222400     WHEN 'DET'                                                           
222500       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
222600         IF SYST-IDSEKVNR = 1                                             
222700           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
222800           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
222900           IF BET-KDTRADP(3:2) NOT = SPACE                                
223000             MOVE '1'             TO WS-ACCOUNT-4                         
223100           ELSE                                                           
223200             MOVE '3'             TO WS-ACCOUNT-4                         
223300           END-IF                                                         
223400           COMPUTE R3-LINE-AMOUNT-LC  =                                   
223500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
223600           IF IN-EKH-KDVALISO = 'TWD'                                     
223700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
223800           END-IF                                                         
223900           MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                
224000           PERFORM S02-WRITE-W57011A                                      
224100         END-IF                                                           
224200                                                                          
224300         IF SYST-IDSEKVNR = 4                                             
224400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
224500           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
224600           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
224700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
224800           COMPUTE R3-LINE-AMOUNT-LC  =                                   
224900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
225000           IF IN-EKH-KDVALISO = 'TWD'                                     
225100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
225200           END-IF                                                         
225300           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
225400           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
225500           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
225600           MOVE SPACE             TO WS-ALLOCATE-REF                      
225700           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
225800           MOVE 0000430877        TO R3-LINE-PA-CUSTOMER                  
225900           MOVE 'TW  '            TO R3-LINE-TRADING-PARTNER              
226000                                                                          
226100           PERFORM S02-WRITE-W57011A                                      
226200         END-IF                                                           
226300       ELSE                                                               
226400         IF SYST-IDSEKVNR = 2                                             
226500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
226600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
226700           IF BET-KDTRADP(3:2) NOT = SPACE                                
226800             MOVE '1'             TO WS-ACCOUNT-4                         
226900           ELSE                                                           
227000             MOVE '3'             TO WS-ACCOUNT-4                         
227100           END-IF                                                         
227200           COMPUTE R3-LINE-AMOUNT-LC  =                                   
227300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
227400           IF IN-EKH-KDVALISO = 'TWD'                                     
227500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
227600           END-IF                                                         
227700           MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                
227800           PERFORM S02-WRITE-W57011A                                      
227900         END-IF                                                           
228000                                                                          
228100         IF SYST-IDSEKVNR = 3                                             
228200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
228300           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
228400           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
228500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
228600           COMPUTE R3-LINE-AMOUNT-LC  =                                   
228700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
228800           IF IN-EKH-KDVALISO = 'TWD'                                     
228900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
229000           END-IF                                                         
229100           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
229200           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
229300           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
229400           MOVE SPACE             TO WS-ALLOCATE-REF                      
229500           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
229600           MOVE 0000430877        TO R3-LINE-PA-CUSTOMER                  
229700           MOVE 'TW  '            TO R3-LINE-TRADING-PARTNER              
229800           PERFORM S02-WRITE-W57011A                                      
229900         END-IF                                                           
230000       END-IF                                                             
230100     END-EVALUATE                                                         
230200     .                                                                    
230300     EJECT                                                                
230400                                                                          
230500 CEJ-MAIN-EVENT-303 SECTION.                                              
230600     EVALUATE IN-EKH-KDEKSHT                                              
230700     WHEN '3XX'                                                           
230800          PERFORM CEJ301-SUB-EVENT-303-3XX                                
230900     WHEN '301'                                                           
231000          PERFORM CEJ301-SUB-EVENT-303-301                                
231100     WHEN '307'                                                           
231200          PERFORM CEJ307-SUB-EVENT-303-307                                
231300     WHEN '310'                                                           
231400          PERFORM CEJ310-SUB-EVENT-303-310                                
231500     WHEN '311'                                                           
231600          PERFORM CEJ311-SUB-EVENT-303-311                                
231700     WHEN '391'                                                           
231800          PERFORM CEJ301-SUB-EVENT-303-391                                
231900     WHEN '371'                                                           
232000          PERFORM CEJ371-SUB-EVENT-303-371                                
232100     END-EVALUATE                                                         
232200     .                                                                    
232300     EJECT                                                                
232400                                                                          
232500 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
232600     EVALUATE IN-EKH-KDEKNIVA                                             
232700                                                                          
232800     WHEN 'LAND'                                                          
232900       IF SYST-IDSEKVNR = 1                                               
233000         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
233100         MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                          
233200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
233300                 IN-EKH-SUBEL * -1  / WS-PRKURS-TW3                       
233400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
233500         MOVE SPACE           TO WS-ALLOCATE-DC                           
233600         MOVE SPACE           TO WS-ALLOCATE-DISTR                        
233700         MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                        
233800         MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                         
233900         PERFORM S03-WRITE-W57012                                         
234000       END-IF                                                             
234100                                                                          
234200     WHEN 'DDI'                                                           
234300       IF IN-EKH-SUBEL > ZERO                                             
234400         IF SYST-IDSEKVNR = 1                                             
234500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
234600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
234700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
234800                   IN-EKH-SUBEL                                           
234900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
235000           MOVE SPACE               TO WS-ALLOCATE-DC                     
235100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
235200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
235300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
235400           PERFORM S04-WRITE-W57013A                                      
235500         END-IF                                                           
235600       ELSE                                                               
235700         IF SYST-IDSEKVNR = 2                                             
235800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
235900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
236000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
236100                   IN-EKH-SUBEL                                           
236200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
236300           MOVE SPACE               TO WS-ALLOCATE-DC                     
236400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
236500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
236600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
236700           PERFORM S04-WRITE-W57013A                                      
236800         END-IF                                                           
236900       END-IF                                                             
237000     END-EVALUATE                                                         
237100     .                                                                    
237200     EJECT                                                                
237300                                                                          
237400 CEJ301-SUB-EVENT-303-301 SECTION.                                        
237500     EVALUATE IN-EKH-KDEKNIVA                                             
237600     WHEN 'DET'                                                           
237700       IF SYST-IDSEKVNR = 1                                               
237800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
237900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
238000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
238100         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3 * -1            
238200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
238300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
238400         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
238500         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
238600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
238700         MOVE SPACE               TO WS-ALLOCATE-DC                       
238800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
238900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
239000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
239100         PERFORM S03-WRITE-W57012                                         
239200       END-IF                                                             
239300                                                                          
239400       IF SYST-IDSEKVNR = 2                                               
239500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
239600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
239700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
239800         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3 * -1            
239900         * WS-MARKUP                                                      
240000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
240100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
240200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
240300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
240400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
240500         MOVE SPACE               TO WS-ALLOCATE-DC                       
240600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
240700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
240800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
240900         PERFORM S03-WRITE-W57012                                         
241000       END-IF                                                             
241100                                                                          
241200       IF SYST-IDSEKVNR = 3                                               
241300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
241400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
241500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
241600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3 * -1            
241700         * WS-MARKUP                                                      
241800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
241900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
242000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
242100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
242200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
242300         MOVE SPACE               TO WS-ALLOCATE-DC                       
242400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
242500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
242600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
242700         PERFORM S03-WRITE-W57012                                         
242800       END-IF                                                             
242900                                                                          
243000     END-EVALUATE                                                         
243100     .                                                                    
243200     EJECT                                                                
243300                                                                          
243400 CEJ307-SUB-EVENT-303-307 SECTION.                                        
243500     EVALUATE IN-EKH-KDEKNIVA                                             
243600     WHEN 'DET'                                                           
243700       IF SYST-IDSEKVNR = 1                                               
243800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
243900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
244000         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
244100         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3 * -1            
244200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
244300         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
244400         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
244500         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
244600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
244700         MOVE SPACE               TO WS-ALLOCATE-DC                       
244800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
244900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
245000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
245100         PERFORM S03-WRITE-W57012                                         
245200       END-IF                                                             
245300                                                                          
245400       IF SYST-IDSEKVNR = 2                                               
245500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
245600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
245700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
245800         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3 * -1            
245900         * WS-MARKUP                                                      
246000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
246100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
246200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
246300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
246400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
246500         MOVE SPACE               TO WS-ALLOCATE-DC                       
246600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
246700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
246800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
246900         PERFORM S03-WRITE-W57012                                         
247000       END-IF                                                             
247100                                                                          
247200       IF SYST-IDSEKVNR = 3                                               
247300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
247400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
247500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
247600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3 * -1            
247700         * WS-MARKUP                                                      
247800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
247900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
248000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
248100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
248200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
248300         MOVE SPACE               TO WS-ALLOCATE-DC                       
248400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
248500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
248600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
248700         PERFORM S03-WRITE-W57012                                         
248800       END-IF                                                             
248900                                                                          
249000     END-EVALUATE                                                         
249100     .                                                                    
249200     EJECT                                                                
249300                                                                          
249400 CEJ310-SUB-EVENT-303-310 SECTION.                                        
249500     EVALUATE IN-EKH-KDEKNIVA                                             
249600     WHEN 'DET'                                                           
249700       IF SYST-IDSEKVNR = 1                                               
249800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
249900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
250000         COMPUTE R3-LINE-AMOUNT-LC =                                      
250100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
250200         IF IN-EKH-KDVALISO = 'TWD'                                       
250300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
250400         END-IF                                                           
250500         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
250600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
250700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
250800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
250900         MOVE SPACE               TO WS-LINE-TEXT                         
251000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
251100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
251200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
251300         PERFORM S02-WRITE-W57011A                                        
251400       END-IF                                                             
251500                                                                          
251600       IF SYST-IDSEKVNR = 2                                               
251700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
251800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
251900         COMPUTE R3-LINE-AMOUNT-LC =                                      
252000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
252100         IF IN-EKH-KDVALISO = 'TWD'                                       
252200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
252300         END-IF                                                           
252400         MOVE SPACE               TO WS-LINE-TEXT                         
252500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
252600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
252700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
252800         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
252900         MOVE SPACE               TO WS-ALLOCATE-DC                       
253000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
253100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
253200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
253300         PERFORM S02-WRITE-W57011A                                        
253400       END-IF                                                             
253500     END-EVALUATE                                                         
253600     .                                                                    
253700     EJECT                                                                
253800                                                                          
253900 CEJ311-SUB-EVENT-303-311 SECTION.                                        
254000     EVALUATE IN-EKH-KDEKNIVA                                             
254100     WHEN 'DET'                                                           
254200        IF SYST-IDSEKVNR = 1                                              
254300          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
254400          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
254500          COMPUTE R3-LINE-AMOUNT-LC =                                     
254600                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
254700          IF IN-EKH-KDVALISO = 'TWD'                                      
254800            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
254900          END-IF                                                          
255000          MOVE SPACE               TO WS-LINE-TEXT                        
255100          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
255200          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
255300          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
255400          MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                 
255500          MOVE SPACE               TO WS-ALLOCATE-DC                      
255600          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
255700          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
255800          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
255900          PERFORM S02-WRITE-W57011A                                       
256000        END-IF                                                            
256100                                                                          
256200        IF SYST-IDSEKVNR = 2                                              
256300          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
256400          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
256500          COMPUTE R3-LINE-AMOUNT-LC =                                     
256600                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
256700          IF IN-EKH-KDVALISO = 'TWD'                                      
256800            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
256900          END-IF                                                          
257000          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
257100          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
257200          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
257300          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
257400          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
257500          MOVE SPACE               TO WS-LINE-TEXT                        
257600          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
257700          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
257800          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
257900          MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                 
258000          MOVE 'TW  '              TO R3-LINE-TRADING-PARTNER             
258100          PERFORM S02-WRITE-W57011A                                       
258200        END-IF                                                            
258300     END-EVALUATE                                                         
258400     .                                                                    
258500     EJECT                                                                
258600                                                                          
258700 CEJ301-SUB-EVENT-303-391 SECTION.                                        
258800     EVALUATE IN-EKH-KDEKNIVA                                             
258900     WHEN 'DET'                                                           
259000       IF SYST-IDSEKVNR = 1                                               
259100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
259200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
259300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
259400              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
259500         IF IN-EKH-KDVALISO = 'TWD'                                       
259600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
259700         END-IF                                                           
259800         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
259900         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
260000         MOVE SPACE               TO WS-LINE-TEXT                         
260100         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
260200         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
260300         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
260400         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
260500         MOVE 'TW  '              TO R3-LINE-TRADING-PARTNER              
260600         MOVE SPACE               TO WS-ALLOCATE-DC                       
260700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
260800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
260900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
261000         PERFORM S03-WRITE-W57012                                         
261100       END-IF                                                             
261200                                                                          
261300       IF SYST-IDSEKVNR = 2                                               
261400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
261500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
261600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
261700              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
261800         IF IN-EKH-KDVALISO = 'TWD'                                       
261900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
262000         END-IF                                                           
262100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
262200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
262300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
262400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
262500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
262600         MOVE SPACE               TO WS-LINE-TEXT                         
262700         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
262800         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
262900         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
263000         MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                  
263100         PERFORM S03-WRITE-W57012                                         
263200       END-IF                                                             
263300                                                                          
263400     END-EVALUATE                                                         
263500     .                                                                    
263600     EJECT                                                                
263700                                                                          
263800 CEJ371-SUB-EVENT-303-371  SECTION.                                       
263900     EVALUATE IN-EKH-KDEKNIVA                                             
264000     WHEN 'DET'                                                           
264100       IF SYST-IDSEKVNR = 1                                               
264200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
264300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
264400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
264500           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TW3               
264600         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
264700         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
264800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
264900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
265000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
265100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
265200         PERFORM S04-WRITE-W57013A                                        
265300       END-IF                                                             
265400                                                                          
265500     WHEN 'LAND'                                                          
265600       IF SYST-IDSEKVNR = 1                                               
265700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
265800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
265900         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
266000         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
266100               R3-LINE-AMOUNT-LC / WS-PRKURS-TW3                          
266200         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
266300         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
266400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
266500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
266600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
266700         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
266800         PERFORM S02-WRITE-W57011A                                        
266900       END-IF                                                             
267000                                                                          
267100     WHEN 'DDI'                                                           
267200       IF IN-EKH-SUBEL > ZERO                                             
267300         IF SYST-IDSEKVNR = 1                                             
267400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
267500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
267600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
267700                   IN-EKH-SUBEL                                           
267800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
267900           MOVE SPACE               TO WS-ALLOCATE-DC                     
268000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
268100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
268200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
268300           PERFORM S02-WRITE-W57011A                                      
268400         END-IF                                                           
268500       ELSE                                                               
268600         IF SYST-IDSEKVNR = 2                                             
268700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
268800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
268900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
269000                   IN-EKH-SUBEL                                           
269100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
269200           MOVE SPACE               TO WS-ALLOCATE-DC                     
269300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
269400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
269500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
269600           PERFORM S02-WRITE-W57011A                                      
269700         END-IF                                                           
269800       END-IF                                                             
269900     END-EVALUATE                                                         
270000     .                                                                    
270100     EJECT                                                                
270200 CEK-MAIN-EVENT-401 SECTION.                                              
270300     EVALUATE IN-EKH-KDEKNIVA                                             
270400                                                                          
270500* PRISÄNDRING LÖPANDE                                                     
270600     WHEN 'DET'                                                           
270700       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
270800                           IN-EKH-PRARTSTD                                
270900       IF SYST-IDSEKVNR = 1                                               
271000* PRISHÖJNING                                                             
271100         IF WS-BELOPP > 0                                                 
271200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
271300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
271400           COMPUTE R3-LINE-AMOUNT-LC =                                    
271500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
271600           IF IN-EKH-KDVALISO = 'TWD'                                     
271700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
271800           END-IF                                                         
271900           MOVE SPACES              TO R3-LINE-TRADING-PARTNER            
272000           MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                
272100           PERFORM S02-WRITE-W57011A                                      
272200         END-IF                                                           
272300       END-IF                                                             
272400                                                                          
272500       IF SYST-IDSEKVNR = 2                                               
272600* PRISSÄKNING                                                             
272700         IF WS-BELOPP < 0                                                 
272800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
272900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
273000           COMPUTE R3-LINE-AMOUNT-LC =                                    
273100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
273200           IF IN-EKH-KDVALISO = 'TWD'                                     
273300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
273400           END-IF                                                         
273500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
273600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
273700           MOVE SPACE               TO WS-ALLOCATE-REF                    
273800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
273900           MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                
274000           PERFORM S02-WRITE-W57011A                                      
274100         END-IF                                                           
274200       END-IF                                                             
274300                                                                          
274400       IF SYST-IDSEKVNR = 3                                               
274500* PRISSÄNKNING                                                            
274600         IF WS-BELOPP < 0                                                 
274700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
274800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
274900           COMPUTE R3-LINE-AMOUNT-LC =                                    
275000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
275100           IF IN-EKH-KDVALISO = 'TWD'                                     
275200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
275300           END-IF                                                         
275400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
275500           MOVE 'TW  '              TO R3-LINE-TRADING-PARTNER            
275600           MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                
275700           PERFORM S02-WRITE-W57011A                                      
275800         END-IF                                                           
275900       END-IF                                                             
276000                                                                          
276100       IF SYST-IDSEKVNR = 4                                               
276200* PRISHÖJNING                                                             
276300         IF WS-BELOPP > 0                                                 
276400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
276500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
276600           COMPUTE R3-LINE-AMOUNT-LC =                                    
276700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
276800           IF IN-EKH-KDVALISO = 'TWD'                                     
276900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
277000           END-IF                                                         
277100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
277200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
277300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
277400           MOVE SPACE               TO WS-ALLOCATE-REF                    
277500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
277600           MOVE 'TW  '              TO R3-LINE-TRADING-PARTNER            
277700           MOVE 0000430877          TO R3-LINE-PA-CUSTOMER                
277800           PERFORM S02-WRITE-W57011A                                      
277900         END-IF                                                           
278000       END-IF                                                             
278100     END-EVALUATE                                                         
278200     .                                                                    
278300     EJECT                                                                
278400                                                                          
278500 CEL-MAIN-EVENT-402 SECTION.                                              
278600     EVALUATE IN-EKH-KDEKNIVA                                             
278700     WHEN 'DET'                                                           
278800       IF SYST-IDSEKVNR = 1                                               
278900         IF IN-EKH-KVANTAL > 0                                            
279000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
279100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
279200           COMPUTE R3-LINE-AMOUNT-LC =                                    
279300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
279400           IF IN-EKH-KDVALISO = 'TWD'                                     
279500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
279600           END-IF                                                         
279700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
279800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
279900           MOVE SPACE               TO WS-ALLOCATE-REF                    
280000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
280100           PERFORM S02-WRITE-W57011A                                      
280200         END-IF                                                           
280300       END-IF                                                             
280400                                                                          
280500       IF SYST-IDSEKVNR = 2                                               
280600         IF IN-EKH-KVANTAL < 0                                            
280700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
280800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
280900           COMPUTE R3-LINE-AMOUNT-LC =                                    
281000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
281100           IF IN-EKH-KDVALISO = 'TWD'                                     
281200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
281300           END-IF                                                         
281400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
281500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
281600           MOVE SPACE               TO WS-ALLOCATE-REF                    
281700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
281800           PERFORM S02-WRITE-W57011A                                      
281900         END-IF                                                           
282000       END-IF                                                             
282100     END-EVALUATE                                                         
282200     .                                                                    
282300     EJECT                                                                
282400                                                                          
282500 CEM-MAIN-EVENT-403 SECTION.                                              
282600     EVALUATE IN-EKH-KDEKSHT                                              
282700     WHEN '401'                                                           
282800     WHEN '402'                                                           
282900     WHEN '403'                                                           
283000     WHEN '404'                                                           
283100     WHEN '405'                                                           
283200     WHEN '407'                                                           
283300     WHEN '408'                                                           
283400     WHEN '409'                                                           
283500          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
283600     END-EVALUATE                                                         
283700     .                                                                    
283800     EJECT                                                                
283900                                                                          
284000 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
284100     EVALUATE IN-EKH-KDEKNIVA                                             
284200     WHEN 'DET'                                                           
284300       IF IN-EKH-KVANTAL > 0                                              
284400         IF SYST-IDSEKVNR = 1                                             
284500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
284600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
284700           COMPUTE R3-LINE-AMOUNT-LC =                                    
284800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
284900           IF IN-EKH-KDVALISO = 'TWD'                                     
285000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
285100           END-IF                                                         
285200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
285300           PERFORM S02-WRITE-W57011A                                      
285400         END-IF                                                           
285500                                                                          
285600         IF SYST-IDSEKVNR = 4                                             
285700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
285800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
285900           COMPUTE R3-LINE-AMOUNT-LC =                                    
286000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
286100           IF IN-EKH-KDVALISO = 'TWD'                                     
286200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
286300           END-IF                                                         
286400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
286500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
286600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
286700           MOVE SPACE               TO WS-ALLOCATE-REF                    
286800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
286900           PERFORM S02-WRITE-W57011A                                      
287000         END-IF                                                           
287100       END-IF                                                             
287200                                                                          
287300       IF IN-EKH-KVANTAL < 0                                              
287400         IF SYST-IDSEKVNR = 2                                             
287500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
287600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
287700           COMPUTE R3-LINE-AMOUNT-LC =                                    
287800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
287900           IF IN-EKH-KDVALISO = 'TWD'                                     
288000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
288100           END-IF                                                         
288200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
288300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
288400           MOVE SPACE               TO WS-ALLOCATE-REF                    
288500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
288600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
288700           PERFORM S02-WRITE-W57011A                                      
288800         END-IF                                                           
288900                                                                          
289000         IF SYST-IDSEKVNR = 3                                             
289100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
289200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
289300           COMPUTE R3-LINE-AMOUNT-LC =                                    
289400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
289500           IF IN-EKH-KDVALISO = 'TWD'                                     
289600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
289700           END-IF                                                         
289800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
289900           PERFORM S02-WRITE-W57011A                                      
290000         END-IF                                                           
290100       END-IF                                                             
290200     END-EVALUATE                                                         
290300     .                                                                    
290400     EJECT                                                                
290500                                                                          
290600 CEN-MAIN-EVENT-404 SECTION.                                              
290700     EVALUATE IN-EKH-KDEKNIVA                                             
290800     WHEN 'DET'                                                           
290900       IF SYST-IDSEKVNR = 1                                               
291000* KONTO EJ MANUELLT REGISTRERAT                                           
291100         IF IN-EKH-IDKONTO = 0                                            
291200           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
291300           IF DIST18-SCRAP-NDC-SC                                         
291400           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
291600             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
291700             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
291800             COMPUTE R3-LINE-AMOUNT-LC =                                  
291900                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
292000             IF IN-EKH-KDVALISO = 'TWD'                                   
292100               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
292200             END-IF                                                       
292300             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
292400             PERFORM S02-WRITE-W57011A                                    
292500           END-IF                                                         
292600         END-IF                                                           
292700       END-IF                                                             
292800                                                                          
292900       IF SYST-IDSEKVNR = 2                                               
293000* KONTO MANUELLT REGISTRERAT                                              
293100         IF IN-EKH-IDKONTO > 0                                            
293200           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
293300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
293400           COMPUTE R3-LINE-AMOUNT-LC =                                    
293500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
293600           IF IN-EKH-KDVALISO = 'TWD'                                     
293700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
293800           END-IF                                                         
293900           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
294000           PERFORM S02-WRITE-W57011A                                      
294100         END-IF                                                           
294200       END-IF                                                             
294300                                                                          
294400       IF SYST-IDSEKVNR = 3                                               
294500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
294600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
294700         COMPUTE R3-LINE-AMOUNT-LC =                                      
294800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
294900         IF IN-EKH-KDVALISO = 'TWD'                                       
295000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
295100         END-IF                                                           
295200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
295300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
295400         MOVE SPACE               TO WS-ALLOCATE-REF                      
295500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
295600         PERFORM S02-WRITE-W57011A                                        
295700       END-IF                                                             
295800                                                                          
295810       IF SYST-IDSEKVNR = 4                                               
295820* KONTO EJ MANUELLT REGISTRERAT                                           
295830         IF IN-EKH-IDKONTO = 0                                            
295840           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
295870           IF DIST18-SCRAP-NDC-QUAL                                       
295880             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
295890             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
295891             COMPUTE R3-LINE-AMOUNT-LC =                                  
295892                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
295893             IF IN-EKH-KDVALISO = 'TWD'                                   
295894               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
295895             END-IF                                                       
295896             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
295897             PERFORM S02-WRITE-W57011A                                    
295898           END-IF                                                         
295899         END-IF                                                           
295900       END-IF                                                             
295901                                                                          
295910     END-EVALUATE                                                         
296000     .                                                                    
296100     EJECT                                                                
296200                                                                          
296300 CF-BUILD-COMMON-210-PART SECTION.                                        
296400     MOVE SPACE              TO R3-LINE-R3                                
296500     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
296600                                R3-LINE-DUE-DATE                          
296700                                R3-LINE-AMOUNT                            
296800                                R3-LINE-AMOUNT-LC                         
296900                                R3-LINE-TAX-AMOUNT                        
297000                                R3-LINE-TAX-AMOUNT-LC                     
297100                                R3-LINE-NUMBER-OF-DAYS                    
297200                                R3-LINE-QUANTITY                          
297300                                R3-LINE-SAMNR                             
297400     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
297500     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
297600     MOVE 'TW01'             TO R3-LINE-COMPANY-CODE                      
297700     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
297800     IF SYST-KDPOST = '31'                                                
297900       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
298000     ELSE                                                                 
298100       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
298200     END-IF                                                               
298300     .                                                                    
298400     EJECT                                                                
298500                                                                          
298600 CG-SCHEDULE-LINE-AP SECTION.                                             
298700     MOVE NEJ                     TO WS-HEADER-SW                         
298800     MOVE JA                      TO WS-LINE-SW                           
298900     EVALUATE IN-EKH-KDEKHHT                                              
299000     WHEN '102'                                                           
299100       IF IN-EKH-KDEKSHT = '130'                                          
299200       OR IN-EKH-KDEKSHT = '134'                                          
299300         IF IN-EKH-KDEKSHT = '130'                                        
299400           PERFORM CGA-MAIN-EVENT-102-130                                 
299500         ELSE                                                             
299600           PERFORM CGA-MAIN-EVENT-102-134                                 
299700         END-IF                                                           
299800       ELSE                                                               
299900         IF IN-EKH-KDEKSHT = '120'                                        
300000         OR IN-EKH-KDEKSHT = '124'                                        
300100         OR IN-EKH-KDEKSHT = '125'                                        
300200           IF IN-EKH-KDEKSHT = '125'                                      
300300             PERFORM CGA-MAIN-EVENT-102-125                               
300400           ELSE                                                           
300500             PERFORM CGA-MAIN-EVENT-102-12X                               
300600           END-IF                                                         
300700         ELSE                                                             
300800           PERFORM CGA-MAIN-EVENT-102                                     
300900         END-IF                                                           
301000       END-IF                                                             
301100     WHEN '103'                                                           
301200         PERFORM CGA-MAIN-EVENT-103                                       
301300     WHEN '303'                                                           
301400       IF IN-EKH-KDEKSHT = '371'                                          
301500         PERFORM S81-GET-CURRENCY-RATE                                    
301600         PERFORM CGA-MAIN-EVENT-303-371                                   
301700       ELSE                                                               
301710         IF IN-EKH-KDEKSHT = '3XX'                                        
301720           PERFORM S81-GET-CURRENCY-RATE                                  
301800           PERFORM CGA-MAIN-EVENT-303-3XX                                 
301801         ELSE                                                             
301810           PERFORM CGA-MAIN-EVENT-303                                     
301820         END-IF                                                           
301900       END-IF                                                             
302000     END-EVALUATE                                                         
302100     .                                                                    
302200     EJECT                                                                
302300                                                                          
302400 CGA-MAIN-EVENT-102     SECTION.                                          
302500     EVALUATE IN-EKH-KDEKNIVA                                             
302600     WHEN 'SUM'                                                           
302700       IF IN-EKH-SUBEL > ZERO                                             
302800         IF SYST-IDSEKVNR = 1                                             
302900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
303000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
303100            IN-EKH-SUBEL                                                  
303200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
303300           PERFORM S10-VATCODE                                            
303400           IF IN-EKH-SUVAT = ZERO                                         
303500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
303600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
303700           ELSE                                                           
303800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
303900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
304000           END-IF                                                         
304100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
304200                                                                          
304300           PERFORM S04-WRITE-W57013A                                      
304400         END-IF                                                           
304500       END-IF                                                             
304600                                                                          
304700       IF IN-EKH-SUBEL < ZERO                                             
304800         IF SYST-IDSEKVNR = 2                                             
304900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
305000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
305100           IN-EKH-SUBEL                                                   
305200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
305300           PERFORM S10-VATCODE                                            
305400           IF IN-EKH-SUVAT = ZERO                                         
305500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
305600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
305700           ELSE                                                           
305800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
305900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
306000           END-IF                                                         
306100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
306200                                                                          
306300           PERFORM S04-WRITE-W57013A                                      
306400         END-IF                                                           
306500       END-IF                                                             
306600     END-EVALUATE                                                         
306700     .                                                                    
306800     EJECT                                                                
306900                                                                          
307000 CGA-MAIN-EVENT-102-12X SECTION.                                          
307100     EVALUATE IN-EKH-KDEKNIVA                                             
307200     WHEN 'SUM'                                                           
307300       IF IN-EKH-SUBEL > ZERO                                             
307400         IF SYST-IDSEKVNR = 1                                             
307500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
307600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
307700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
307800                   R3-LINE-AMOUNT    / WS-PRKURS-TW  * -1                 
307900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
308000           MOVE '  '               TO R3-LINE-TAX-CODE                    
308100           IF IN-EKH-SUVAT = ZERO                                         
308200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
308300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
308400           ELSE                                                           
308500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
308600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
308700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TW  * -1           
308800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
308900           END-IF                                                         
309000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
309100                                                                          
309200           PERFORM S04-WRITE-W57013A                                      
309300         END-IF                                                           
309400       END-IF                                                             
309500                                                                          
309600       IF IN-EKH-SUBEL < ZERO                                             
309700         IF SYST-IDSEKVNR = 2                                             
309800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
309900           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
310000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
310100                   R3-LINE-AMOUNT    / WS-PRKURS-TW  * -1                 
310200           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
310300           MOVE '  '               TO R3-LINE-TAX-CODE                    
310400           IF IN-EKH-SUVAT = ZERO                                         
310500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
310600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
310700           ELSE                                                           
310800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
310900             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
311000                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TW  * -1           
311100             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
311200           END-IF                                                         
311300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
311400           MOVE SPACE           TO R3-LINE-COST-CENTER                    
311500                                                                          
311600           PERFORM S04-WRITE-W57013A                                      
311700         END-IF                                                           
311800       END-IF                                                             
311900     END-EVALUATE                                                         
312000     .                                                                    
312100     EJECT                                                                
312200                                                                          
312300                                                                          
312400 CGA-MAIN-EVENT-102-125 SECTION.                                          
312500     EVALUATE IN-EKH-KDEKNIVA                                             
312600     WHEN 'SUM'                                                           
312700       IF IN-EKH-SUBEL > ZERO                                             
312800         IF SYST-IDSEKVNR = 1                                             
312900           MOVE ZERO TO SPAR-SUMMA-102-125                                
313000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
313100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
313200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
313300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
313400                   R3-LINE-AMOUNT    / WS-PRKURS-TW  * -1                 
313500           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
313600           MOVE '  '               TO R3-LINE-TAX-CODE                    
313700           IF IN-EKH-SUVAT = ZERO                                         
313800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
313900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
314000           ELSE                                                           
314100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
314200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
314300               R3-LINE-TAX-AMOUNT / WS-PRKURS-TW  * -1                    
314400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
314500           END-IF                                                         
314600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
314700           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
314800                                                                          
314900           PERFORM S04-WRITE-W57013A                                      
315000         END-IF                                                           
315100       END-IF                                                             
315200                                                                          
315300       IF IN-EKH-SUBEL < ZERO                                             
315400         IF SYST-IDSEKVNR = 2                                             
315500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
315600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
315700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
315800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
315900                   R3-LINE-AMOUNT    / WS-PRKURS-TW  * -1                 
316000           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
316100           MOVE '  '               TO R3-LINE-TAX-CODE                    
316200           IF IN-EKH-SUVAT = ZERO                                         
316300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
316400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
316500           ELSE                                                           
316600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
316700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
316800               R3-LINE-TAX-AMOUNT / WS-PRKURS-TW * -1                     
316900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
317000           END-IF                                                         
317100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
317200           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
317300                                                                          
317400           PERFORM S04-WRITE-W57013A                                      
317500         END-IF                                                           
317600       END-IF                                                             
317700     END-EVALUATE                                                         
317800     .                                                                    
317900     EJECT                                                                
318000 CGA-MAIN-EVENT-102-130 SECTION.                                          
318100     EVALUATE IN-EKH-KDEKNIVA                                             
318200     WHEN 'SUM'                                                           
318300       IF IN-EKH-SUBEL > ZERO                                             
318400         IF SYST-IDSEKVNR = 1                                             
318500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
318600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
318700           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
318800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
318900                   R3-LINE-AMOUNT    / WS-PRKURS-TW  * -1                 
319000           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
319100           MOVE '  '     TO R3-LINE-TAX-CODE                              
319200           IF IN-EKH-SUVAT = ZERO                                         
319300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
319400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
319500           ELSE                                                           
319600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
319700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
319800                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TW  * -1           
319900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
320000           END-IF                                                         
320100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
320200                                                                          
320300           PERFORM S04-WRITE-W57013A                                      
320400         END-IF                                                           
320500       END-IF                                                             
320600                                                                          
320700       IF IN-EKH-SUBEL < ZERO                                             
320800         IF SYST-IDSEKVNR = 2                                             
320900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
321000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
321100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
321200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
321300                   R3-LINE-AMOUNT    / WS-PRKURS-TW                       
321400           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
321500           MOVE '  '     TO R3-LINE-TAX-CODE                              
321600           IF IN-EKH-SUVAT = ZERO                                         
321700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
321800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
321900           ELSE                                                           
322000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
322100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
322200                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TW                 
322300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
322400           END-IF                                                         
322500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
322600                                                                          
322700           PERFORM S04-WRITE-W57013A                                      
322800         END-IF                                                           
322900       END-IF                                                             
323000     END-EVALUATE                                                         
323100     .                                                                    
323200     EJECT                                                                
323300                                                                          
323400 CGA-MAIN-EVENT-102-134 SECTION.                                          
323500     EVALUATE IN-EKH-KDEKNIVA                                             
323600     WHEN 'SUM'                                                           
323700       IF IN-EKH-SUBEL > ZERO                                             
323800         IF SYST-IDSEKVNR = 1                                             
323900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
324000           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
324100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
324200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
324300                   R3-LINE-AMOUNT    / WS-PRKURS-TW  * -1                 
324400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
324500           MOVE '  '     TO R3-LINE-TAX-CODE                              
324600           IF IN-EKH-SUVAT = ZERO                                         
324700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
324800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
324900           ELSE                                                           
325000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
325100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
325200                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TW  * -1           
325300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
325400           END-IF                                                         
325500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
325600                                                                          
325700           PERFORM S04-WRITE-W57013A                                      
325800         END-IF                                                           
325900       END-IF                                                             
326000                                                                          
326100       IF IN-EKH-SUBEL < ZERO                                             
326200         IF SYST-IDSEKVNR = 2                                             
326300           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
326400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
326500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
326600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
326700                   R3-LINE-AMOUNT    / WS-PRKURS-TW                       
326800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
326900           MOVE '  '     TO R3-LINE-TAX-CODE                              
327000           IF IN-EKH-SUVAT = ZERO                                         
327100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
327200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
327300           ELSE                                                           
327400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
327500             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
327600                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TW                 
327700             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
327800           END-IF                                                         
327900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
328000                                                                          
328100           PERFORM S04-WRITE-W57013A                                      
328200         END-IF                                                           
328300       END-IF                                                             
328400     END-EVALUATE                                                         
328500     .                                                                    
328600     EJECT                                                                
328700                                                                          
328800 CGA-MAIN-EVENT-103     SECTION.                                          
328900     EVALUATE IN-EKH-KDEKNIVA                                             
329000     WHEN 'SUM'                                                           
329100       IF IN-EKH-SUBEL > ZERO                                             
329200         IF SYST-IDSEKVNR = 1                                             
329300           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
329400           PERFORM S10-VATCODE                                            
329500           IF IN-EKH-SUVAT = ZERO                                         
329600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
329700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
329800           ELSE                                                           
329900             IF IN-EKH-KDVALISO = 'TWD'                                   
330000               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
330100                                      R3-LINE-TAX-AMOUNT-LC               
330200             ELSE                                                         
330300               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
330400                                      R3-LINE-TAX-AMOUNT-LC               
330500               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
330600                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
330700             END-IF                                                       
330800           END-IF                                                         
330900**** CALCULATE NEW SUM WITH VAT                                           
331000           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
331100                   IN-EKH-SUVAT                                           
331200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
331300           IF IN-EKH-KDVALISO = 'TWD'                                     
331400             MOVE R3-LINE-AMOUNT TO R3-LINE-AMOUNT-LC                     
331500           ELSE                                                           
331600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
331700                     R3-LINE-AMOUNT * WS-PRKURS                           
331800           END-IF                                                         
331900           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
332000                                                                          
332100           PERFORM S04-WRITE-W57013A                                      
332200         END-IF                                                           
332300       END-IF                                                             
332400                                                                          
332500       IF IN-EKH-SUBEL < ZERO                                             
332600         IF SYST-IDSEKVNR = 2                                             
332700           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
332800           IF IN-EKH-SUVAT = ZERO                                         
332900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
333000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
333100           ELSE                                                           
333200             IF IN-EKH-KDVALISO = 'TWD'                                   
333300               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
333400                                      R3-LINE-TAX-AMOUNT-LC               
333500             ELSE                                                         
333600               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
333700                                      R3-LINE-TAX-AMOUNT-LC               
333800               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
333900                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
334000             END-IF                                                       
334100           END-IF                                                         
334200           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
334300                   IN-EKH-SUVAT                                           
334400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
334500           IF IN-EKH-KDVALISO = 'TWD'                                     
334600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
334700           ELSE                                                           
334800             COMPUTE R3-LINE-AMOUNT ROUNDED =                             
334900                   R3-LINE-AMOUNT-LC / WS-PRKURS                          
335000           END-IF                                                         
335100           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
335200                                                                          
335300           PERFORM S04-WRITE-W57013A                                      
335400         END-IF                                                           
335500       END-IF                                                             
335600                                                                          
335700     END-EVALUATE                                                         
335800     .                                                                    
335900     EJECT                                                                
336000                                                                          
336100 CGA-MAIN-EVENT-303 SECTION.                                              
336200     EVALUATE IN-EKH-KDEKNIVA                                             
336300     WHEN 'SUM'                                                           
336400       IF SYST-IDSEKVNR = 1                                               
336500         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
336600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
336700          IN-EKH-SUBEL / WS-PRKURS-TW                                     
336800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
336900         MOVE '  '     TO R3-LINE-TAX-CODE                                
337000         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
337100         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
337200                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-TW                     
337300         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
337400                                                                          
337500         PERFORM S04-WRITE-W57013A                                        
337600       END-IF                                                             
337700     END-EVALUATE                                                         
337800     .                                                                    
337900     EJECT                                                                
338000                                                                          
338010 CGA-MAIN-EVENT-303-3XX SECTION.                                          
338020     EVALUATE IN-EKH-KDEKNIVA                                             
338030     WHEN 'SUM'                                                           
338040       IF SYST-IDSEKVNR = 1                                               
338050         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
338060         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
338070          IN-EKH-SUBEL / WS-PRKURS-TW3                                    
338080         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
338090         MOVE '  '     TO R3-LINE-TAX-CODE                                
338091         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
338092         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
338093                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-TW3                    
338094         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
338095                                                                          
338096         PERFORM S04-WRITE-W57013A                                        
338097       END-IF                                                             
338098     END-EVALUATE                                                         
338099     .                                                                    
338100     EJECT                                                                
338101                                                                          
338110 CGA-MAIN-EVENT-303-371 SECTION.                                          
338200     EVALUATE IN-EKH-KDEKNIVA                                             
338300     WHEN 'SUM'                                                           
338400       IF SYST-IDSEKVNR = 1                                               
338500         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
338600         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
338700         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
338800         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
338900               R3-LINE-AMOUNT-LC / WS-PRKURS-TW3                          
339000         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
339100         PERFORM S10-VATCODE                                              
339200         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
339300         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
339400                 R3-LINE-TAX-AMOUNT-LC * WS-PRKURS-TW2                    
339500         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
339600                                                                          
339700         PERFORM S04-WRITE-W57013A                                        
339800       END-IF                                                             
339900     END-EVALUATE                                                         
340000     .                                                                    
340100     EJECT                                                                
340200 CH-BUILD-COMMON-310-PART SECTION.                                        
340300     MOVE SPACE              TO R3-LINE-R3                                
340400     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
340500                                R3-LINE-DUE-DATE                          
340600                                R3-LINE-AMOUNT                            
340700                                R3-LINE-AMOUNT-LC                         
340800                                R3-LINE-TAX-AMOUNT                        
340900                                R3-LINE-TAX-AMOUNT-LC                     
341000                                R3-LINE-NUMBER-OF-DAYS                    
341100                                R3-LINE-QUANTITY                          
341200                                R3-LINE-SAMNR                             
341300     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
341400     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
341500     MOVE 'TW01'             TO R3-LINE-COMPANY-CODE                      
341600     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
341700     IF SYST-KDPOST = '31'                                                
341800       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
341900     ELSE                                                                 
342000       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
342100     END-IF                                                               
342200     .                                                                    
342300     EJECT                                                                
342400                                                                          
342500 CI-SCHEDULE-LINE-AR SECTION.                                             
342600     MOVE NEJ                     TO WS-HEADER-SW                         
342700     MOVE JA                      TO WS-LINE-SW                           
342800     EVALUATE IN-EKH-KDEKHHT                                              
342900     WHEN '204'                                                           
343000         PERFORM CIA-MAIN-EVENT-204                                       
343100     END-EVALUATE                                                         
343200     .                                                                    
343300     EJECT                                                                
343400                                                                          
343500 CIA-MAIN-EVENT-204     SECTION.                                          
343600     EVALUATE IN-EKH-KDEKNIVA                                             
343700     WHEN 'SUM'                                                           
343800       IF IN-EKH-SUBEL > ZERO                                             
343900         IF SYST-IDSEKVNR = 1                                             
344000           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
344100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
344200           IF IN-EKH-KDVALISO = 'TWD'                                     
344300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
344400           END-IF                                                         
344500           PERFORM S10-VATCODE                                            
344600           IF IN-EKH-SUVAT = ZERO                                         
344700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
344800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
344900           ELSE                                                           
345000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
345100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
345200           END-IF                                                         
345300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
345400                                                                          
345500           PERFORM S04-WRITE-W57013A                                      
345600         END-IF                                                           
345700       END-IF                                                             
345800                                                                          
345900     END-EVALUATE                                                         
346000     .                                                                    
346100     EJECT                                                                
346200                                                                          
346300 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
346400     MOVE ZERO             TO LOGG-W57073                                 
346500     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
346600     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
346700     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
346800     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
346900     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
347000     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
347100     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
347200     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
347300     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
347400     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
347500     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
347600     MOVE 'TW01'           TO LOGG-KDTRADP                                
347700                                                                          
347800****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
347900     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
348000     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
348100     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
348200     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
348300     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
348400     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
348500     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
348600     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
348700     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
348800     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
348900     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
349000     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
349100     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
349200     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
349300     .                                                                    
349400     EJECT                                                                
349500                                                                          
349600 Z-FINI SECTION.                                                          
349700     CLOSE W57066                                                         
349800           W57018                                                         
349900           W57011A                                                        
350000           W57012A                                                        
350100           W57013A                                                        
350200           W57015                                                         
350300           W5701N                                                         
350400           W51310A                                                        
350500                                                                          
350600     MOVE 'S' TO POSTSUM-OPKOD                                            
350700     CALL POSTSUM USING POSTSUM-PARM                                      
350800     .                                                                    
350900     EJECT                                                                
351000                                                                          
351100 S01-READ-W57066  SECTION.                                                
351200     READ W57066 INTO IN-AREA                                             
351300     AT END                                                               
351400        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
351500        SET END-OF-W57066 TO TRUE                                         
351600                                                                          
351700     NOT AT END                                                           
351800        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
351900        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
352000        MOVE 'W57018D1'   TO POSTSUM-DDNAMN2                              
352100        CALL POSTSUM USING POSTSUM-PARM                                   
352200     END-READ                                                             
352300     .                                                                    
352400                                                                          
352500 S02-WRITE-W57011A SECTION.                                               
352600     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
352700     MOVE SPACE                 TO 71LINE-POST                            
352800     IF WS-LINE-SW = JA                                                   
352900       IF IN-EKH-KDSORT = 'SW'                                            
353000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
353100         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
353200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
353300       ELSE                                                               
353400         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
353500         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
353600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
353700       END-IF                                                             
353800       WRITE 71LINE-POST        FROM R3-LINE-R3                           
353900       PERFORM S20-CREATE-WRITE-LOG                                       
354000     ELSE                                                                 
354100       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
354200     END-IF                                                               
354300                                                                          
354400     IF WS-LINE-SW = JA                                                   
354500       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
354600     ELSE                                                                 
354700       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
354800     END-IF                                                               
354900     MOVE 'W57011A'             TO POSTSUM-FDNAMN                         
355000     MOVE 'W57018D2'            TO POSTSUM-DDNAMN2                        
355100     CALL POSTSUM USING POSTSUM-PARM                                      
355200     .                                                                    
355300                                                                          
355400 S002-WRITE-W57011A-HEAD SECTION.                                         
355500     MOVE SPACE                 TO 71LINE-POST                            
355600     IF IN-EKH-KDSORT = 'SW'                                              
355700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
355800       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
355900       MOVE WS-TEXT           TO R3-LINE-TEXT                             
356000     ELSE                                                                 
356100       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
356200       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
356300       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
356400     END-IF                                                               
356500     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
356600                                                                          
356700     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
356800     MOVE 'W57011A'             TO POSTSUM-FDNAMN                         
356900     MOVE 'W57018D2'            TO POSTSUM-DDNAMN2                        
357000     CALL POSTSUM USING POSTSUM-PARM                                      
357100     .                                                                    
357200                                                                          
357300 S03-WRITE-W57012 SECTION.                                                
357400     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
357500     IF IN-EKH-KDSORT = 'SW'                                              
357600       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
357700       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
357800       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
357900     ELSE                                                                 
358000       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
358100       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
358200       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
358300     END-IF                                                               
358400     WRITE 72LINE-POST        FROM R3-LINE-R3                             
358500                                                                          
358600     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
358700     MOVE 'W57012A'           TO POSTSUM-FDNAMN                           
358800     MOVE 'W57018D3'          TO POSTSUM-DDNAMN2                          
358900     CALL POSTSUM USING POSTSUM-PARM                                      
359000                                                                          
359100     PERFORM S20-CREATE-WRITE-LOG                                         
359200     .                                                                    
359300                                                                          
359400 S04-WRITE-W57013A SECTION.                                               
359500     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
359600     MOVE SPACE                 TO 73LINE-POST                            
359700     IF WS-LINE-SW = JA                                                   
359800       IF IN-EKH-KDSORT = 'SW'                                            
359900         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
360000         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
360100         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
360200       ELSE                                                               
360300         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
360400         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
360500         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
360600       END-IF                                                             
360700       WRITE 73LINE-POST        FROM R3-LINE-R3                           
360800       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
360900     ELSE                                                                 
361000       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
361100       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
361200     END-IF                                                               
361300                                                                          
361400     MOVE 'W57013A'             TO POSTSUM-FDNAMN                         
361500     MOVE 'W57018D4'            TO POSTSUM-DDNAMN2                        
361600     CALL POSTSUM USING POSTSUM-PARM                                      
361700                                                                          
361800     IF WS-LINE-SW = JA                                                   
361900       PERFORM S20-CREATE-WRITE-LOG                                       
362000     END-IF                                                               
362100     .                                                                    
362200                                                                          
362300 S004-WRITE-W57013A-HEAD SECTION.                                         
362400     MOVE SPACE                 TO 73LINE-POST                            
362500     IF IN-EKH-KDSORT = 'SW'                                              
362600       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
362700       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
362800       MOVE WS-TEXT           TO R3-LINE-TEXT                             
362900     ELSE                                                                 
363000       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
363100       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
363200       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
363300     END-IF                                                               
363400     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
363500                                                                          
363600     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
363700     MOVE 'W57013A'             TO POSTSUM-FDNAMN                         
363800     MOVE 'W57018D4'            TO POSTSUM-DDNAMN2                        
363900     CALL POSTSUM USING POSTSUM-PARM                                      
364000     .                                                                    
364100                                                                          
364200 S10-VATCODE SECTION.                                                     
364300     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
364400     PERFORM IMS-GU-WDB601                                                
364500     IF DCS-KDDC = SPACE                                                  
364600       MOVE NEJ              TO WDB6-A-SW                                 
364700     ELSE                                                                 
364800       MOVE JA               TO WDB6-A-SW                                 
364900     END-IF                                                               
365000                                                                          
365100     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
365200     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
365300     IF IN-EKH-SUVAT = ZERO                                               
365400       MOVE '  '     TO R3-LINE-TAX-CODE                                  
365500     END-IF                                                               
365600     IF IN-EKH-BEVAT = 'XX'                                               
365700       MOVE '  '     TO R3-LINE-TAX-CODE                                  
365800     END-IF                                                               
365900     .                                                                    
366000     EJECT                                                                
366100                                                                          
366200 S20-CREATE-WRITE-LOG SECTION.                                            
366300     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
366400     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
366500     IF SYST-IDPTYP = '610'                                               
366600       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
366700     ELSE                                                                 
366800       MOVE ZERO                      TO WS-IDLEVNR                       
366900       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
367000                          FOR CHARACTERS BEFORE INITIAL SPACE             
367100       IF WS-IDLEVNR   > ZERO                                             
367200          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
367300                                      TO LOGG-IDKONTO                     
367400       END-IF                                                             
367500     END-IF                                                               
367600     IF R3-LINE-COST-CENTER NOT = SPACE                                   
367700       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
367800     END-IF                                                               
367900     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
368000     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
368100     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
368200     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
368300     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
368400                                                                          
368500     PERFORM S21-WRITE-W57015                                             
368600     PERFORM S22-WRITE-W57018                                             
368700                                                                          
368800     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
368900       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
369000       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
369100       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
369200                                                                          
369300       PERFORM S21-WRITE-W57015                                           
369400     END-IF                                                               
369500     .                                                                    
369600     EJECT                                                                
369700                                                                          
369800 S21-WRITE-W57015 SECTION.                                                
369900     IF DCS-IDDC NOT = LOGG-IDDC                                          
370000        MOVE LOGG-IDDC TO W-IDDC-B6                                       
370100        PERFORM IMS-GU-WDB601                                             
370200     END-IF                                                               
370300     IF DCS-KDDC = SPACE                                                  
370400       MOVE NEJ              TO WDB6-A-SW                                 
370500     ELSE                                                                 
370600       MOVE JA               TO WDB6-A-SW                                 
370700     END-IF                                                               
370800                                                                          
370900     IF  WDB6-A-FINNS                                                     
371000     AND DCS-DDC                                                          
371100       MOVE 'N'       TO LOGG-FLLSBOK                                     
371200     END-IF                                                               
371300     WRITE LOGG-POST FROM LOGG-W57073                                     
371400                                                                          
371500     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
371600     MOVE 'W57015'    TO POSTSUM-FDNAMN                                   
371700     MOVE 'W57018D5'  TO POSTSUM-DDNAMN2                                  
371800     CALL POSTSUM USING POSTSUM-PARM                                      
371900     .                                                                    
372000                                                                          
372100 S22-WRITE-W57018 SECTION.                                                
372200     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
372300     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
372400     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
372500                                                                          
372600     IF R3-LINE-AMOUNT-SIGN = '+'                                         
372700       IF AVST-SUBEL < +0                                                 
372800         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
372900       END-IF                                                             
373000       IF AVST-KVANTAL < +0                                               
373100         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
373200       END-IF                                                             
373300     ELSE                                                                 
373400       IF AVST-SUBEL > +0                                                 
373500         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
373600       END-IF                                                             
373700       IF AVST-KVANTAL > +0                                               
373800         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
373900       END-IF                                                             
374000     END-IF                                                               
374100                                                                          
374200     IF DCS-IDDC NOT = AVST-IDDC                                          
374300        MOVE AVST-IDDC  TO W-IDDC-B6                                      
374400        PERFORM IMS-GU-WDB601                                             
374500     END-IF                                                               
374600     IF DCS-KDDC = SPACE                                                  
374700       MOVE NEJ              TO WDB6-A-SW                                 
374800     ELSE                                                                 
374900       MOVE JA               TO WDB6-A-SW                                 
375000     END-IF                                                               
375100                                                                          
375200     IF  WDB6-A-FINNS                                                     
375300     AND DCS-DDC                                                          
375400       MOVE 'N'                 TO AVST-FLLSBOK                           
375500     END-IF                                                               
375600                                                                          
375700     IF AVST-IDKONTO(1:4) = '1454'                                        
375800       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
375900       WRITE AVST-POST FROM AVST-W57070                                   
376000                                                                          
376100       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
376200       MOVE 'W57018'            TO POSTSUM-FDNAMN                         
376300       MOVE 'W57018D6'          TO POSTSUM-DDNAMN2                        
376400       CALL POSTSUM USING POSTSUM-PARM                                    
376500     END-IF                                                               
376600     .                                                                    
376700     EJECT                                                                
376800                                                                          
376900 S30-READ-DATABASE-B2-B1 SECTION.                                         
377000     IF IN-EKH-IDLEVNR = '1441'                                           
377100       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
377200     ELSE                                                                 
377300       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
377400       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
377500       PERFORM IMS-GU-WDB201                                              
377600       IF SEGMENT-SAKNAS                                                  
377700         MOVE 'TW99999'       TO W-WDB1-IDPARTNR                          
377800       ELSE                                                               
377900         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
378000       END-IF                                                             
378100     END-IF                                                               
378200     MOVE WC-IDFTG-TW       TO W-WDB1-IDFTG                               
378300     PERFORM IMS-GU-WDB101                                                
378400     IF SEGMENT-SAKNAS                                                    
378500       DISPLAY 'BETALARUPPG. SAKNAS '                                     
378600       DISPLAY IN-EKH-IDVERGL                                             
378700       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
378800       DISPLAY GMT-IDPARTNR                                               
378900                                                                          
379000       MOVE SPACE         TO BET-KDTRADP                                  
379100       MOVE ZERO          TO BET-IDPARTNR                                 
379200       MOVE '????'        TO WS-KDBETVIL                                  
379300       MOVE '???'         TO WS-KDVALISO-WDB1                             
379400     ELSE                                                                 
379500       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
379600     END-IF                                                               
379700     MOVE 'TWD'           TO WS-KDVALISO-WDB1                             
379800                                                                          
379900     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
380000     MOVE ZERO TO TALLY                                                   
380100     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
380200                 FOR CHARACTERS BEFORE INITIAL SPACE                      
380300     IF TALLY = ZERO                                                      
380400       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
380500     ELSE                                                                 
380600       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
380700                                TO W-BET-IDPARTNR-NUM                     
380800     END-IF                                                               
380900     .                                                                    
381000     EJECT                                                                
381100                                                                          
381200 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
381300     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
381400     PERFORM IMS-GU-WDB601                                                
381500     IF DCS-KDDC = SPACE                                                  
381600       MOVE NEJ              TO WDB6-A-SW                                 
381700     ELSE                                                                 
381800       MOVE JA               TO WDB6-A-SW                                 
381900     END-IF                                                               
382000                                                                          
382100     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
382200       IF IN-EKH-KDEKSHT NOT = '406'                                      
382300         IF IN-EKH-FLDCET = NEJ                                           
382400           PERFORM S42-SKAPA-RW2-INV-POSTER                               
382500         END-IF                                                           
382600       END-IF                                                             
382700     END-IF                                                               
382800                                                                          
382900     IF IN-EKH-KDEKNIVA = 'DET'                                           
383000       IF  IN-EKH-KDEKHHT = '204'                                         
383100       AND (IN-EKH-KDEKSHT = '201')                                       
383200         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
383300       END-IF                                                             
383400                                                                          
383500       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
383600       AND (WDB6-A-FINNS                                                  
383700       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
383800       OR   DCS-DDC OR DCS-NDC-PF))                                       
383900         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
384000       END-IF                                                             
384100                                                                          
384200       IF IN-FIL-IDPGM = 'W4183000'                                       
384300       AND (WDB6-A-FINNS                                                  
384400       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
384500       OR   DCS-DDC OR DCS-NDC-PF))                                       
384600         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
384700       END-IF                                                             
384800     END-IF                                                               
384900     .                                                                    
385000     EJECT                                                                
385100                                                                          
385200 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
385300     MOVE 'RW2'              TO RW2-IDPTYP                                
385400     MOVE 'RW2'              TO WS-IDPTYP                                 
385500     MOVE ZERO               TO RW2-IDDISTR                               
385600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
385700       MOVE WC-CDC-SE        TO RW2-IDDC                                  
385800     ELSE                                                                 
385900       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
386000     END-IF                                                               
386100     IF IN-EKH-KVANTAL < +0                                               
386200       MOVE '0422'           TO RW2-KDWRTYP                               
386300     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
386400     ELSE                                                                 
386500       MOVE '0421'           TO RW2-KDWRTYP                               
386600       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
386700     END-IF                                                               
386800                                                                          
386900     IF RW2-SUARTSTD NOT = +0                                             
387000       PERFORM S70-WRITE-W51320                                           
387100     END-IF                                                               
387200     .                                                                    
387300     EJECT                                                                
387400                                                                          
387500 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
387600     MOVE '0110'             TO RW1-KDWRTYP                               
387700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
387800       MOVE WC-CDC-SE        TO RW1-IDDC                                  
387900     ELSE                                                                 
388000       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
388100     END-IF                                                               
388200     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
388300     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
388400     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
388500                                                                          
388600     IF RW1-SUARTSTD NOT = +0                                             
388700       MOVE 'RW1' TO WS-IDPTYP                                            
388800       PERFORM S70-WRITE-W51320                                           
388900     END-IF                                                               
389000     .                                                                    
389100     EJECT                                                                
389200                                                                          
389300 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
389400     MOVE '0110'             TO RW1-KDWRTYP                               
389500     IF DCS-KDDC = SPACE OR DCS-DDC                                       
389600       MOVE WC-CDC-SE        TO RW1-IDDC                                  
389700     ELSE                                                                 
389800       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
389900     END-IF                                                               
390000     IF IN-EKH-KDANMORS = '30'                                            
390100       MOVE ZERO             TO RW1-SUARTSTD                              
390200     ELSE                                                                 
390300      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
390400     END-IF                                                               
390500     IF IN-EKH-KDANMORS = '30' OR '80'                                    
390600       MOVE ZERO             TO RW1-SUARTSJK                              
390700     ELSE                                                                 
390800      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
390900     END-IF                                                               
391000     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
391100                                                                          
391200     IF RW1-SUARTSTD NOT = +0                                             
391300       MOVE 'RW1' TO WS-IDPTYP                                            
391400       PERFORM S70-WRITE-W51320                                           
391500     END-IF                                                               
391600     .                                                                    
391700     EJECT                                                                
391800                                                                          
391900 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
392000     MOVE '0110'             TO RW1-KDWRTYP                               
392100     IF DCS-KDDC = SPACE OR DCS-DDC                                       
392200       MOVE WC-CDC-SE        TO RW1-IDDC                                  
392300     ELSE                                                                 
392400       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
392500     END-IF                                                               
392600     IF IN-EKH-KDEKSHT = '310'                                            
392700*** SKROTNING KDANMORS  13 O 23                                           
392800       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
392900     ELSE                                                                 
393000      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
393100     END-IF                                                               
393200                                                                          
393300     MOVE ZERO               TO RW1-SUARTSJK                              
393400                                RW1-SUARTFSG                              
393500     IF RW1-SUARTSTD NOT = +0                                             
393600       MOVE 'RW1' TO WS-IDPTYP                                            
393700       PERFORM S70-WRITE-W51320                                           
393800     END-IF                                                               
393900     .                                                                    
394000     EJECT                                                                
394100                                                                          
394200 S60-WRITE-W5701N SECTION.                                                
394300     WRITE SAPUT-POST  FROM IN-AREA                                       
394400                                                                          
394500     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
394600     MOVE 'W5701N'            TO POSTSUM-FDNAMN                           
394700     MOVE 'W57018D7'          TO POSTSUM-DDNAMN2                          
394800     CALL POSTSUM USING POSTSUM-PARM                                      
394900     .                                                                    
395000     EJECT                                                                
395100                                                                          
395200 S70-WRITE-W51320 SECTION.                                                
395300     IF WS-IDPTYP  = 'RW2'                                                
395400       IF DCS-KDDC = SPACE OR DCS-DDC                                     
395500         MOVE WC-CDC-SE        TO INV-IDDC                                
395600       ELSE                                                               
395700         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
395800       END-IF                                                             
395900       IF IN-EKH-KVANTAL < +0                                             
396000         MOVE '003'            TO INV-IDPTYP                              
396100       COMPUTE INV-SUARTSTD =                                             
396200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
396300       ELSE                                                               
396400         MOVE '002'            TO INV-IDPTYP                              
396500         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
396600       END-IF                                                             
396700       MOVE SPACE TO WS-IDPTYP                                            
396800       MOVE 0                  TO INV-ADLAGOMR                            
396900       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
397000       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
397100     END-IF                                                               
397200     IF WS-IDPTYP  = 'RW1'                                                
397300       IF DCS-KDDC = SPACE OR DCS-DDC                                     
397400         MOVE WC-CDC-SE        TO INV-IDDC                                
397500       ELSE                                                               
397600         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
397700       END-IF                                                             
397800       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
397900       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
398000       MOVE 0                  TO INV-ADLAGOMR                            
398100       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
398200       MOVE '001'              TO INV-IDPTYP                              
398300       MOVE SPACE              TO WS-IDPTYP                               
398400     END-IF                                                               
398500     WRITE INV-POST  FROM INV-W51310                                      
398600                                                                          
398700     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
398800     MOVE 'W51310A'           TO POSTSUM-FDNAMN                           
398900     MOVE 'W57018D8'          TO POSTSUM-DDNAMN2                          
399000     CALL POSTSUM USING POSTSUM-PARM                                      
399100     .                                                                    
399200     EJECT                                                                
399300                                                                          
399400 S13-GET-LANDING-COST SECTION.                                            
399500     MOVE '64'                   TO W-IDDC-B6                             
399600     PERFORM IMS-GU-WDB601                                                
399700     IF SEGMENT-FINNS                                                     
399800       PERFORM IMS-GNP-WDB617                                             
399900       IF SEGMENT-FINNS                                                   
400000         IF PROC-TILANDCO > IN-EKH-DAVERDAT                               
400100           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
400200         ELSE                                                             
400300           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
400400         END-IF                                                           
400500       END-IF                                                             
400600     END-IF                                                               
400700                                                                          
400800*** GET IMPORT DUTY. USE 19.2% AS DEFAULT IF NOT PRESENT FOR              
400900*** THE GIVEN FUNCTION GROUP                                              
401000     PERFORM IMS-GU-WDB622                                                
401100     IF SEGMENT-FINNS                                                     
401200       IF FGAD-TILANDCO >  IN-EKH-DAVERDAT                                
401300         MOVE FGAD-RELANDCO-FG-TO TO WS-MARKUP                            
401400       ELSE                                                               
401500         MOVE FGAD-RELANDCO-FG-FROM TO WS-MARKUP                          
401600       END-IF                                                             
401700     ELSE                                                                 
401800       MOVE 0.192                 TO WS-MARKUP                            
401900     END-IF                                                               
402000     .                                                                    
402100     EJECT                                                                
402200 S80-GET-CURRENCY-RATE SECTION.                                           
402300     MOVE +0                  TO W-ANT                                    
402400     IF (IN-EKH-KDEKHHT = '102'                                           
402500     AND IN-EKH-KDEKSHT = '124')                                          
402600     OR (IN-EKH-KDEKHHT = '102'                                           
402700     AND IN-EKH-KDEKSHT = '134')                                          
402800       INSPECT IN-EKH-IDFAKT-EXP TALLYING W-ANT FOR CHARACTERS            
402900             BEFORE INITIAL ' '                                           
403000       MOVE IN-EKH-IDFAKT-EXP(1:W-ANT) TO W-IDFAKT                        
403100     ELSE                                                                 
403200       INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS               
403300             BEFORE INITIAL ' '                                           
403400       MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                           
403500     END-IF                                                               
403600                                                                          
403700     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
403800     PERFORM IMS-GU-WDL601                                                
403900     IF SEGMENT-SAKNAS                                                    
404000       CONTINUE                                                           
404100     ELSE                                                                 
404200       PERFORM IMS-GNP-WDL611                                             
404300       IF SEGMENT-SAKNAS                                                  
404400         CONTINUE                                                         
404500       ELSE                                                               
404600         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
404700                                   - INL-DAINLEV                          
404800         MOVE WS-FAKTURA-DATUM2   TO WS-FAKTURA-DATUM                     
404900         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
405000         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
405100         MOVE W-DATE-AAMM         TO CURR-TIAAMM                          
405200         MOVE WS-KDVALISO-TW      TO CURR-KDVALISO-ROW                    
405300         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
405400         IF CURR-KDSVAR = ' '                                             
405500           MOVE CURR-PRKURS-NEW   TO WS-PRKURS-TW3                        
405600         ELSE                                                             
405700           MOVE +1                TO WS-PRKURS-TW3                        
405800         END-IF                                                           
405900       END-IF                                                             
406000     END-IF                                                               
406100     .                                                                    
406200     EJECT                                                                
406300                                                                          
406400 S81-GET-CURRENCY-RATE SECTION.                                           
406500     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
406600     MOVE 'TWD'               TO CURR-KDVALISO-ROW                        
406700     IF IN-FIL-IDPGM = 'W4183300'                                         
406800       IF IN-EKH-DAAVIDAT > ZERO                                          
406900         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
407000         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
407100       ELSE                                                               
407200         MOVE WS-TIAA            TO WS-TIAA-CR                            
407300         MOVE WS-TIMM            TO WS-TIMM-CR                            
407400       END-IF                                                             
407500     ELSE                                                                 
407600       MOVE WS-TIAA              TO WS-TIAA-CR                            
407700       MOVE WS-TIMM              TO WS-TIMM-CR                            
407800     END-IF                                                               
407900     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
408000     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
408100     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
408200     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
408300     IF CURR-KDSVAR = ' '                                                 
408400       IF IN-EKH-IDDISTR > ZERO                                           
408500         MOVE CURR-PRKURS-NEW TO WS-PRKURS-TW3                            
408600       ELSE                                                               
408700         IF WS-PRKURS = ZERO                                              
408800           MOVE 1           TO WS-PRKURS-TW3                              
408900         END-IF                                                           
409000       END-IF                                                             
409100     ELSE                                                                 
409200       MOVE 1               TO WS-PRKURS-TW3                              
409300     END-IF                                                               
409400     .                                                                    
409500     EJECT                                                                
409600* --- IMS SECTIONS ---                                                    
409700                                                                          
409800 IMS-GU-WDH521 SECTION.                                                   
409900     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
410000          DELIMITED BY SIZE INTO SSA1                                     
410100     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
410200          DELIMITED BY SIZE INTO SSA2                                     
410300     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
410400          DELIMITED BY SIZE INTO SSA3                                     
410500     MOVE '  '              TO GODK-STATUSKODER                           
410600     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
410700                                                   SSA2                   
410800                                                   SSA3                   
410900     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
411000                                                                          
411100     PERFORM IMS-STATUS-CONTROL                                           
411200     .                                                                    
411300                                                                          
411400 IMS-GNP-WDH531 SECTION.                                                  
411500     MOVE 'WDH531  '        TO SSA1                                       
411600     MOVE '  GE'            TO GODK-STATUSKODER                           
411700     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
411800     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
411900                               WS-STATUS                                  
412000     PERFORM IMS-STATUS-CONTROL                                           
412100     .                                                                    
412200     EJECT                                                                
412300                                                                          
412400 IMS-GU-WDB201 SECTION.                                                   
412500     STRING 'WDB201  (IDGMT    =' W-IDGMT-KEY ')'                         
412600          DELIMITED BY SIZE INTO SSA1                                     
412700     MOVE '  GE'                 TO GODK-STATUSKODER                      
412800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
412900      MOVE WDB2-STATUS-CODE      TO STATUS-WS                             
413000     PERFORM IMS-STATUS-CONTROL                                           
413100     .                                                                    
413200     EJECT                                                                
413300                                                                          
413400 IMS-GU-WDB101 SECTION.                                                   
413500     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
413600          DELIMITED BY SIZE INTO SSA1                                     
413700     MOVE '  GE'               TO GODK-STATUSKODER                        
413800     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
413900     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
414000     PERFORM IMS-STATUS-CONTROL                                           
414100     .                                                                    
414200     EJECT                                                                
414300                                                                          
414400 IMS-GU-WDB601    SECTION.                                                
414500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
414600          DELIMITED BY SIZE INTO SSA1                                     
414700     MOVE '  GE' TO GODK-STATUSKODER                                      
414800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
414900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
415000     PERFORM IMS-STATUS-CONTROL                                           
415100     IF SEGMENT-SAKNAS                                                    
415200        MOVE SPACE TO DCS-KDDC                                            
415300     END-IF                                                               
415400     .                                                                    
415500     EJECT                                                                
415600                                                                          
415700 IMS-GNP-WDB617 SECTION.                                                  
415800     MOVE 'WDB617   ' TO SSA1                                             
415900     MOVE '  GE'        TO GODK-STATUSKODER                               
416000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
416100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
416200     PERFORM IMS-STATUS-CONTROL                                           
416300     .                                                                    
416400     SKIP3                                                                
416500                                                                          
416600 IMS-GU-WDB622 SECTION.                                                   
416700                                                                          
416800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
416900          DELIMITED BY SIZE INTO SSA1                                     
417000     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
417100          DELIMITED BY SIZE INTO SSA2                                     
417200     STRING 'WDB622  (IDFKNGRP =' W-IDFKNGRP-X ')'                        
417300          DELIMITED BY SIZE INTO SSA3                                     
417400     MOVE '  GE' TO GODK-STATUSKODER                                      
417500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB622 SSA1 SSA2 SSA3          
417600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
417700     PERFORM IMS-STATUS-CONTROL                                           
417800     .                                                                    
417900     SKIP3                                                                
418000 IMS-GU-WDL601   SECTION.                                                 
418100     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
418200          DELIMITED BY SIZE INTO SSA1                                     
418300     MOVE '  GE' TO GODK-STATUSKODER                                      
418400     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
418500     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
418600     PERFORM IMS-STATUS-CONTROL                                           
418700     .                                                                    
418800     SKIP3                                                                
418900                                                                          
419000 IMS-GNP-WDL611   SECTION.                                                
419100     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
419200          DELIMITED BY SIZE INTO SSA1                                     
419300     MOVE '  GE' TO GODK-STATUSKODER                                      
419400     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
419500     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
419600     PERFORM IMS-STATUS-CONTROL                                           
419700     .                                                                    
419800     SKIP3                                                                
419900                                                                          
420000 IMS-GU-WDK601   SECTION.                                                 
420100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
420200          DELIMITED BY SIZE INTO SSA1                                     
420300     MOVE '  GE' TO GODK-STATUSKODER                                      
420400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K601 SSA1                 
420500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
420600     PERFORM IMS-STATUS-CONTROL                                           
420700     .                                                                    
420800     SKIP3                                                                
420900 IMS-STATUS-CONTROL SECTION.                                              
421000     SET STATUS-IX TO 1                                                   
421100     SEARCH GODK-STATUS                                                   
421200       AT END                                                             
421300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
421400           DELIMITED BY SIZE INTO FELTEXT                                 
421500         DISPLAY FELTEXT                                                  
421600         CALL FELLOG                                                      
421700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
421800         CONTINUE                                                         
421900     END-SEARCH                                                           
422000     .                                                                    
