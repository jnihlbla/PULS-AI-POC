000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5704800.                                                
000300 AUTHOR.         MAMATHA SHETTY.                                          
000400 DATE-WRITTEN.   20230202.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
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
004700     SELECT W57066                     ASSIGN TO W57048D1.                
004800                                                                          
004900*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
005000     SELECT W57041A                    ASSIGN TO W57048D2.                
005100                                                                          
005200*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005300     SELECT W57042A                    ASSIGN TO W57048D3.                
005400                                                                          
005500*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005600     SELECT W57043A                    ASSIGN TO W57048D4.                
005700                                                                          
005800*          --- LOGG TILL ON-DEMAND                                        
005900     SELECT W57043                     ASSIGN TO W57048D5.                
006000                                                                          
006100*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006200     SELECT W57048                     ASSIGN TO W57048D6.                
006300                                                                          
006400*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006500     SELECT W5704N                     ASSIGN TO W57048D7.                
006600                                                                          
006700*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006800     SELECT W51340                     ASSIGN TO W57048D8.                
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
008100 FD  W57041A                                                              
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400*01  71INIT-POST -COPY R3INIT20               -L.                         
008500*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008600*01  71LINE-POST -COPY R3LINE20               -L.                         
008700                                                                          
008800 FD  W57042A                                                              
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100*01  72LINE-POST -COPY R3LINE20               -L.                         
009200                                                                          
009300 FD  W57043A                                                              
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009700*01  73LINE-POST -COPY R3LINE20               -L.                         
009800                                                                          
009900 FD  W57043                                                               
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200*01  LOGG-POST   -COPY W57073                 -L.                         
010300                                                                          
010400 FD  W57048                                                               
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700*01  AVST-POST   -COPY W57070                 -L.                         
010800                                                                          
010900 FD  W5704N                                                               
011000     RECORDING       F                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300 01  SAPUT-POST.                                                          
011400*    03  -COPY WDR801        -L.                                          
011500     03 FILLER                   PIC X(6).                                
011600                                                                          
011700 FD  W51340                                                               
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012100                                                                          
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400*    -- CHECKED BY WY2000                                                 
012500 77  IDPGM                        PIC X(8)    VALUE 'W5704800'.           
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
014200 77  WS-SPAR-CMD                  PIC X(3) VALUE SPACE.                   
014300 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
014400 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
014500 77  WS-SPAR-IDVERGL              PIC X(10) VALUE SPACE.                  
014600 77  WS-SPAR-DAVERDAT             PIC 9(8) VALUE ZERO.                    
014700 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
014800 77  SPAR-LINE-ORDER              PIC X(12).                              
014900 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
015000 77  WS-RED-IDKST                 PIC X(10).                              
015100 77  WS-IDPTYP                    PIC X(3).                               
015200 77  WS-FAKTURA-DATUM             PIC X(16).                              
015300 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
015400 77  SPAR-SUMMA                 PIC S9(13)V99  COMP-3 VALUE ZERO.         
015500 77  SPAR-PRDMTRL               PIC S9(13)V99  COMP-3 VALUE ZERO.         
015600 77  SPAR-PROVRPAL              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015700 77  SPAR-PRDIRLON              PIC S9(13)V99  COMP-3 VALUE ZERO.         
015800 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
015900 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
016000 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
016100 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
016200 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
016300 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
016400 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
016500 77  WS-MARKUP1                   PIC 9V9(3)  VALUE ZERO.                 
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
019600 01  WS-KDVALISO-TH               PIC X(3) VALUE 'THB'.                   
019700 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
019800 01  WS-PRKURS-TH                 PIC S9(6)V9(5) COMP-3.                  
019900 01  WS-PRKURS-TH2                PIC S9(6)V9(5) COMP-3.                  
020000 01  WS-PRKURS-TH3                PIC S9(6)V9(5) COMP-3.                  
020100 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
020200 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
020300                                                                          
020400 01  WS-ALLOCATE.                                                         
020500     03  WS-ALLOCATE-DC           PIC X(2).                               
020600     03  WS-ALLOCATE-DISTR        PIC X(5).                               
020700     03  WS-ALLOCATE-REF          PIC X(8)    VALUE SPACE.                
020800     03  FILLER                   PIC X(3)    VALUE SPACE.                
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
028400 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57048'.             
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
034100     03  W-IDDC-B6-X.                                                     
034200         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
034300                                                                          
034400     03  W-IDARTNR-X.                                                     
034500         05 W-IDARTNR             PIC S9(9) COMP-3.                       
034600                                                                          
034700     03  W-IDFAKT-X.                                                      
034800         05 W-IDFAKT              PIC S9(7) COMP-3.                       
034900                                                                          
035000     03  W-IDLEVNR-X.                                                     
035100         05  W-IDLEVNR            PIC X(5)    VALUE SPACE.                
035200                                                                          
035300     03  W-KDSEGKEY-X.                                                    
035400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
035500                                                                          
035600     03  W-KDPRODSL-X.                                                    
035700         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
035800     EJECT                                                                
035900                                                                          
036000*    --- STATUS-KOD FRÅN IMS                                              
036100 01  STATUS-WS                    PIC XX.                                 
036200     88  SEGMENT-FINNS                        VALUE '  '.                 
036300     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
036400                                                                          
036500 01  GODK-STATUSKODER.                                                    
036600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036700                                                                          
036800 01  SSA1                         PIC X(128).                             
036900 01  SSA2                         PIC X(64).                              
037000 01  SSA3                         PIC X(64).                              
037100     EJECT                                                                
037200                                                                          
037300*    --- IMS FUNKTIONSKODER                                               
037400*01  -COPY W0003                                                          
037500     EJECT                                                                
037600                                                                          
037700*    ---  DLI INPUT-OUTPUT AREA                                           
037800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
037900 01  DLI-IO-WDH501.                                                       
038000*    03  -COPY WDH501                                                     
038100     EJECT                                                                
038200                                                                          
038300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
038400 01  DLI-IO-WDH511.                                                       
038500*    03  -COPY WDH511                                                     
038600     EJECT                                                                
038700                                                                          
038800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
038900 01  DLI-IO-WDH521.                                                       
039000*    03  -COPY WDH521                                                     
039100     EJECT                                                                
039200                                                                          
039300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
039400 01  DLI-IO-WDH531.                                                       
039500*    03  -COPY WDH531                                                     
039600     EJECT                                                                
039700                                                                          
039800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
039900 01  DLI-IO-WDB101.                                                       
040000*    03  -COPY WDB101                                                     
040100     EJECT                                                                
040200                                                                          
040300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
040400 01  DLI-IO-WDB201.                                                       
040500*    03  -COPY WDB201                                                     
040600     EJECT                                                                
040700                                                                          
040800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040900 01   DLI-IO-AREA-B601.                                                   
041000*     03  -COPY WDB601                                                    
041100     EJECT                                                                
041200 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
041300 01   DLI-IO-WDB617.                                                      
041400*     03  -COPY WDB617                                                    
041500     EJECT                                                                
041600 01  FILLER               PIC X(16)   VALUE 'WDB621 AREA'.                
041700 01   DLI-IO-WDB621.                                                      
041800*     03  -COPY WDB621                                                    
041900     EJECT                                                                
042000 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
042100 01   DLI-IO-AREA-L601.                                                   
042200*     03  -COPY WDL601                                                    
042300     EJECT                                                                
042400 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
042500 01   DLI-IO-AREA-L611.                                                   
042600*     03  -COPY WDL611                                                    
042700     EJECT                                                                
042800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6C1'.                
042900     SKIP3                                                                
043000     EJECT                                                                
043100 LINKAGE SECTION.                                                         
043200*01  -COPY W0008  -PRE WDH5-                                              
043300     05  FILLER                  PIC X.                                   
043400                                                                          
043500*01  -COPY W0008  -PRE WDB2-                                              
043600     05  FILLER                  PIC X.                                   
043700                                                                          
043800*01  -COPY W0008  -PRE WDB1-                                              
043900     05  FILLER                  PIC X.                                   
044000                                                                          
044100*01  -COPY W0008  -PRE WDG2-                                              
044200     05  FILLER                  PIC X.                                   
044300                                                                          
044400*01  -COPY W0008  -PRE WDB6-                                              
044500     05  FILLER                  PIC X.                                   
044600                                                                          
044700*01  -COPY W0008  -PRE WDL6-                                              
044800     05  FILLER                  PIC X.                                   
044900                                                                          
045000                                                                          
045100     EJECT                                                                
045200                                                                          
045300 PROCEDURE DIVISION  USING WDH5-PCB WDB2-PCB WDB1-PCB                     
045400                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
045500 MAIN SECTION.                                                            
045600     ENTRY 'DLITCBL' USING WDH5-PCB WDB2-PCB WDB1-PCB                     
045700                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
045800                                                                          
045900     PERFORM A-INIT                                                       
046000                                                                          
046100     PERFORM S01-READ-W57066                                              
046200     PERFORM UNTIL END-OF-W57066                                          
046300*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
046400       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
046500       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
046600       AND WS-NEW-MONTH > 01                                              
046700         PERFORM S60-WRITE-W5704N                                         
046800       ELSE                                                               
046900         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
047000         PERFORM S30-READ-DATABASE-B2-B1                                  
047100         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
047200           PERFORM C-EXECUTE                                              
047300         END-IF                                                           
047400       END-IF                                                             
047500       PERFORM S01-READ-W57066                                            
047600     END-PERFORM                                                          
047700                                                                          
047800     PERFORM Z-FINI                                                       
047900                                                                          
048000     MOVE ZERO TO RETURN-CODE                                             
048100     GOBACK                                                               
048200     .                                                                    
048300     EJECT                                                                
048400                                                                          
048500 A-INIT SECTION.                                                          
048600     OPEN INPUT  W57066                                                   
048700                                                                          
048800     OPEN OUTPUT W57048                                                   
048900                 W57041A                                                  
049000                 W57042A                                                  
049100                 W57043A                                                  
049200                 W57043                                                   
049300                 W5704N                                                   
049400                 W51340                                                   
049500                                                                          
049600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
049700     MOVE 20               TO RW1-DAVVREG(1:2)                            
049800     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
049900                              RW1-DAVVREG(3:2)                            
050000                              W-DATE-AAMM(1:2)                            
050100                              WS-TIAA                                     
050200     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
050300                              W-DATE-AAMM(3:2)                            
050400                              WS-TIMM                                     
050500                              WS-NEW-MONTH                                
050600     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
050700     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
050800     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
050900                                                                          
051000*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
051100*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
051200     IF WS-NEW-MONTH = 12                                                 
051300       MOVE 1              TO WS-NEW-MONTH                                
051400     ELSE                                                                 
051500       ADD 1               TO WS-NEW-MONTH                                
051600*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
051700***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
051800***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
051900***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
052000***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
052100       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
052200       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
052300         ADD 1             TO WS-NEW-MONTH                                
052400         ADD 1             TO WS-TIMM                                     
052500         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
052600       END-IF                                                             
052700     END-IF                                                               
052800                                                                          
052900     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
053000                                                                          
053100     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
053200                                                                          
053300     ACCEPT DAGENS-KLOCKA FROM TIME                                       
053400     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
053500                                                                          
053600     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
053700     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
053800     MOVE 'M'                   TO CURR-KDVALTYP                          
053900                                                                          
054000     MOVE WS-KDVALISO-TH        TO CURR-KDVALISO-ROW                      
054100     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
054200     IF CURR-KDSVAR = ' '                                                 
054300       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-TH                           
054400     ELSE                                                                 
054500       MOVE 1                   TO WS-PRKURS-TH                           
054600     END-IF                                                               
054700     COMPUTE WS-PRKURS-TH2 ROUNDED = 1 / WS-PRKURS-TH                     
054800     MOVE WS-PRKURS-TH          TO WS-PRKURS-TH3                          
054900     .                                                                    
055000     EJECT                                                                
055100                                                                          
055200 C-EXECUTE SECTION.                                                       
055300     MOVE WC-IDFTG-TH           TO W-IDFTG                                
055400     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
055500     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
055600     IF IN-EKH-KDEKNIVA = 'TDET'                                          
055700       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
055800     END-IF                                                               
055900     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
056000     PERFORM IMS-GU-WDH521                                                
056100     PERFORM IMS-GNP-WDH531                                               
056200                                                                          
056300     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
056400     IF IN-EKH-IDDISTR > ZERO                                             
056500       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
056600     ELSE                                                                 
056700       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
056800     END-IF                                                               
056900     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
057000                                                                          
057100* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
057200* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
057300* BOKFÖRINGEN I SAP SKER DOCK ALLTID I TH, DÄRFÖR BYTET HÄR:              
057400*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
057500*      MOVE 'THB'               TO WS-KDVALISO                            
057600*    END-IF                                                               
057700                                                                          
057800     IF IN-EKH-IDVERGL = WS-SPAR-IDVERGL                                  
057900     AND (IN-EKH-DAVERDAT = WS-SPAR-DAVERDAT)                             
058000       IF  (IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                              
058100       AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                              
058200       OR (IN-EKH-KDEKHHT = '303')                                        
058300         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
058400         IF IN-EKH-KDEKHHT = '103'                                        
058500           IF IN-EKH-CMD = WS-SPAR-CMD                                    
058600             MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                         
058700           ELSE                                                           
058800             IF WS-LOP = 9                                                
058900               MOVE ZERO  TO WS-LOP                                       
059000             ELSE                                                         
059100               ADD +1     TO WS-LOP                                       
059200             END-IF                                                       
059300             MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                         
059400             MOVE IN-EKH-CMD TO WS-SPAR-CMD                               
059500           END-IF                                                         
059600         END-IF                                                           
059700       ELSE                                                               
059800         MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                           
059900         MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                           
060000         IF WS-LOP = 9                                                    
060100           MOVE ZERO  TO WS-LOP                                           
060200         ELSE                                                             
060300           ADD +1     TO WS-LOP                                           
060400         END-IF                                                           
060500         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
060600       END-IF                                                             
060700     ELSE                                                                 
060800       MOVE IN-EKH-IDVERGL  TO WS-SPAR-IDVERGL                            
060900       MOVE IN-EKH-KDEKHHT  TO WS-SPAR-KDEKHHT                            
061000       MOVE IN-EKH-KDEKSHT  TO WS-SPAR-KDEKSHT                            
061100       MOVE IN-EKH-DAVERDAT TO WS-SPAR-DAVERDAT                           
061200       MOVE IN-EKH-CMD      TO WS-SPAR-CMD                                
061300       IF WS-LOP = 9                                                      
061400         MOVE ZERO  TO WS-LOP                                             
061500       ELSE                                                               
061600         ADD +1     TO WS-LOP                                             
061700       END-IF                                                             
061800       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
061900     END-IF                                                               
062000* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
062100     IF SYST-IDPTYP = '210'                                               
062200       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
062300     ELSE                                                                 
062400       IF SYST-IDPTYP = '310'                                             
062500         PERFORM CC-CREATE-WRITE-HEADER-AR                                
062600       ELSE                                                               
062700* TEST OM BRYTNING PÅ VERIFIKATION                                        
062800         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
062900         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
063000         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
063100         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
063200           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
063300           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
063400           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
063500           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
063600           IF (IN-EKH-KDEKHHT = '102'                                     
063700           AND IN-EKH-KDEKSHT = '121')                                    
063800           OR (IN-EKH-KDEKHHT = '102'                                     
063900           AND IN-EKH-KDEKSHT = '122')                                    
064000           OR (IN-EKH-KDEKHHT = '102'                                     
064100           AND IN-EKH-KDEKSHT = '131')                                    
064200           OR (IN-EKH-KDEKHHT = '102'                                     
064300           AND IN-EKH-KDEKSHT = '132')                                    
064400             PERFORM S80-GET-CURRENCY-RATE                                
064500           END-IF                                                         
064600           IF (IN-EKH-KDEKHHT = '303'                                     
064700           AND IN-EKH-KDEKSHT = '301')                                    
064800           OR (IN-EKH-KDEKHHT = '303'                                     
064900           AND IN-EKH-KDEKSHT = '307')                                    
065000           OR (IN-EKH-KDEKHHT = '303'                                     
065100           AND IN-EKH-KDEKSHT = '371')                                    
065200           OR (IN-EKH-KDEKHHT = '303'                                     
065300           AND IN-EKH-KDEKSHT = '3XX')                                    
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
072000     MOVE 'TH01'                  TO R3-HEAD-COMPANY-CODE                 
072100     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
072200     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
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
073400     IF WS-KDVALISO = 'THB'                                               
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
077800     OR (IN-EKH-KDEKHHT = '501')                                          
077900       PERFORM S004-WRITE-W57043A-HEAD                                    
078000     ELSE                                                                 
078100       PERFORM S002-WRITE-W57041A-HEAD                                    
078200     END-IF                                                               
078300     .                                                                    
078400     EJECT                                                                
078500                                                                          
078600 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
078700     MOVE SPACE                   TO R3-HEAD-R3                           
078800     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
078900     MOVE 'TH01'                  TO R3-HEAD-COMPANY-CODE                 
079000                                     R3-HEAD-CONTROL-AREA                 
079100     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
079200     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
079300     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
079400     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
079500     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
079600     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
079700       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
079800     ELSE                                                                 
079900       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
080000     END-IF                                                               
080100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
080200     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
080300     IF (IN-EKH-KDEKHHT = '103'                                           
080400     AND IN-EKH-KDEKSHT = '102')                                          
080500       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
080600       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
080700     ELSE                                                                 
080800       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
080900       MOVE WS-PRKURS-TH2         TO R3-HEAD-EXCHANGE-RATE                
081000       MOVE 'THB'                 TO CURR-KDVALISO-ROW                    
081100       IF IN-FIL-IDPGM = 'W4183300'                                       
081200         IF IN-EKH-DAAVIDAT > ZERO                                        
081300           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
081400           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
081500         ELSE                                                             
081600           MOVE WS-TIAA              TO WS-TIAA-CR                        
081700           MOVE WS-TIMM              TO WS-TIMM-CR                        
081800         END-IF                                                           
081900       ELSE                                                               
082000         MOVE WS-TIAA                TO WS-TIAA-CR                        
082100         MOVE WS-TIMM                TO WS-TIMM-CR                        
082200       END-IF                                                             
082300       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
082400       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
082500       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
082600       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
082700       IF CURR-KDSVAR = ' '                                               
082800         IF IN-EKH-IDDISTR > ZERO                                         
082900           MOVE CURR-PRKURS-NEW TO WS-PRKURS-TH                           
083000         ELSE                                                             
083100           IF WS-PRKURS = ZERO                                            
083200             MOVE 1             TO WS-PRKURS-TH                           
083300           END-IF                                                         
083400         END-IF                                                           
083500       ELSE                                                               
083600         MOVE 1                 TO WS-PRKURS-TH                           
083700       END-IF                                                             
083800       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-TH *                     
083900                                       CURR-REVALUTA-TO                   
084000       END-COMPUTE                                                        
084100       IF CURR-REVALUTA-TO = +1                                           
084200         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
084300       END-IF                                                             
084400       IF CURR-REVALUTA-TO = +10                                          
084500         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
084600       END-IF                                                             
084700       IF CURR-REVALUTA-TO = +100                                         
084800         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
084900       END-IF                                                             
085000     END-IF                                                               
085100     IF (IN-EKH-KDEKHHT = '102'                                           
085200     AND IN-EKH-KDEKSHT = '120')                                          
085300     OR (IN-EKH-KDEKHHT = '102'                                           
085400     AND IN-EKH-KDEKSHT = '124')                                          
085500     OR (IN-EKH-KDEKHHT = '102'                                           
085600     AND IN-EKH-KDEKSHT = '125')                                          
085700     OR (IN-EKH-KDEKHHT = '102'                                           
085800     AND IN-EKH-KDEKSHT = '130')                                          
085900     OR (IN-EKH-KDEKHHT = '102'                                           
086000     AND IN-EKH-KDEKSHT = '134')                                          
086100     OR (IN-EKH-KDEKHHT = '303'                                           
086200     AND IN-EKH-KDEKSHT = '301')                                          
086300     OR (IN-EKH-KDEKHHT = '303'                                           
086400     AND IN-EKH-KDEKSHT = '307')                                          
086500     OR (IN-EKH-KDEKHHT = '303'                                           
086600     AND IN-EKH-KDEKSHT = '371')                                          
086700     OR (IN-EKH-KDEKHHT = '303'                                           
086800     AND IN-EKH-KDEKSHT = '3XX')                                          
086900       MOVE 'THB'                 TO R3-HEAD-CURRENCY                     
087000       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
087100     END-IF                                                               
087200     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
087300     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
087400     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
087500     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
087600     MOVE JA                      TO WS-HEADER-SW                         
087700     MOVE NEJ                     TO WS-LINE-SW                           
087800                                                                          
087900* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57043A                       
088000       PERFORM S004-WRITE-W57043A-HEAD                                    
088100     .                                                                    
088200     EJECT                                                                
088300                                                                          
088400 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
088500     MOVE SPACE                   TO R3-HEAD-R3                           
088600     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
088700     MOVE 'TH01'                  TO R3-HEAD-COMPANY-CODE                 
088800                                     R3-HEAD-CONTROL-AREA                 
088900     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
089000     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
089100     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
089200     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
089300     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
089400     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
089500       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
089600     ELSE                                                                 
089700       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
089800     END-IF                                                               
089900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
090000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
090100     IF (IN-EKH-KDEKHHT = '204'                                           
090200     AND IN-EKH-KDEKSHT = '201')                                          
090300     OR (IN-EKH-KDEKHHT = '204'                                           
090400     AND IN-EKH-KDEKSHT = '301')                                          
090500       MOVE 'THB'                 TO R3-HEAD-CURRENCY                     
090600       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
090700     ELSE                                                                 
090800       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
090900       MOVE WS-PRKURS-TH2         TO R3-HEAD-EXCHANGE-RATE                
091000       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
091100       IF IN-FIL-IDPGM = 'W4183300'                                       
091200         IF IN-EKH-DAAVIDAT > ZERO                                        
091300           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
091400           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
091500         ELSE                                                             
091600           MOVE WS-TIAA              TO WS-TIAA-CR                        
091700           MOVE WS-TIMM              TO WS-TIMM-CR                        
091800         END-IF                                                           
091900       ELSE                                                               
092000         MOVE WS-TIAA                TO WS-TIAA-CR                        
092100         MOVE WS-TIMM                TO WS-TIMM-CR                        
092200       END-IF                                                             
092300       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
092400       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
092500       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
092600       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
092700       IF CURR-KDSVAR = ' '                                               
092800         IF IN-EKH-IDDISTR > ZERO                                         
092900           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
093000         ELSE                                                             
093100           IF WS-PRKURS = ZERO                                            
093200             MOVE 1             TO WS-PRKURS                              
093300           END-IF                                                         
093400         END-IF                                                           
093500       ELSE                                                               
093600         MOVE 1                 TO WS-PRKURS                              
093700       END-IF                                                             
093800       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
093900                                       CURR-REVALUTA-TO                   
094000       END-COMPUTE                                                        
094100       IF CURR-REVALUTA-TO = +1                                           
094200         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
094300       END-IF                                                             
094400       IF CURR-REVALUTA-TO = +10                                          
094500         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
094600       END-IF                                                             
094700       IF CURR-REVALUTA-TO = +100                                         
094800         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
094900       END-IF                                                             
095000     END-IF                                                               
095100     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
095200     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
095300     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
095400     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
095500     MOVE JA                      TO WS-HEADER-SW                         
095600     MOVE NEJ                     TO WS-LINE-SW                           
095700                                                                          
095800* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57043A                       
095900       PERFORM S004-WRITE-W57043A-HEAD                                    
096000     .                                                                    
096100     EJECT                                                                
096200                                                                          
096300 CD-BUILD-COMMON-610-PART SECTION.                                        
096400     MOVE SPACE               TO R3-LINE-R3                               
096500     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
096600                                 R3-LINE-DUE-DATE                         
096700                                 R3-LINE-AMOUNT                           
096800                                 R3-LINE-AMOUNT-LC                        
096900                                 R3-LINE-TAX-AMOUNT                       
097000                                 R3-LINE-TAX-AMOUNT-LC                    
097100                                 R3-LINE-NUMBER-OF-DAYS                   
097200                                 R3-LINE-QUANTITY                         
097300                                 R3-LINE-SAMNR                            
097400     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
097500     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
097600     MOVE 'TH01'              TO R3-LINE-COMPANY-CODE                     
097700     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
097800     IF SYST-KDPOST = '50'                                                
097900       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
098000     ELSE                                                                 
098100       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
098200     END-IF                                                               
098300     IF SYST-IDPRCTR NOT = SPACE                                          
098400       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
098500       IF WS-PRCTR-PRODSL = '??'                                          
098600         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
098700         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
098800       END-IF                                                             
098900       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
099000     END-IF                                                               
099100     .                                                                    
099200     EJECT                                                                
099300                                                                          
099400 CE-SCHEDULE-LINE-GL SECTION.                                             
099500     MOVE NEJ                     TO WS-HEADER-SW                         
099600     MOVE JA                      TO WS-LINE-SW                           
099700     EVALUATE IN-EKH-KDEKHHT                                              
099800     WHEN '102'                                                           
099900          PERFORM CEB-MAIN-EVENT-102                                      
100000     WHEN '103'                                                           
100100          PERFORM CEC-MAIN-EVENT-103                                      
100200     WHEN '201'                                                           
100300          PERFORM CED-MAIN-EVENT-201                                      
100400     WHEN '203'                                                           
100500          PERFORM CEF-MAIN-EVENT-203                                      
100600     WHEN '204'                                                           
100700          PERFORM CEG-MAIN-EVENT-204                                      
100800     WHEN '302'                                                           
100900          PERFORM CEI-MAIN-EVENT-302                                      
101000     WHEN '303'                                                           
101100          PERFORM CEJ-MAIN-EVENT-303                                      
101200     WHEN '401'                                                           
101300          PERFORM CEK-MAIN-EVENT-401                                      
101400     WHEN '402'                                                           
101500          PERFORM CEL-MAIN-EVENT-402                                      
101600     WHEN '403'                                                           
101700          PERFORM CEM-MAIN-EVENT-403                                      
101800     WHEN '404'                                                           
101900          PERFORM CEN-MAIN-EVENT-404                                      
102000     WHEN '501'                                                           
102100          PERFORM CEQ-MAIN-EVENT-501                                      
102200     WHEN '502'                                                           
102300          PERFORM CER-MAIN-EVENT-502                                      
102400     WHEN '503'                                                           
102500          PERFORM CES-MAIN-EVENT-503                                      
102600     END-EVALUATE                                                         
102700     .                                                                    
102800     EJECT                                                                
102900                                                                          
103000 CEB-MAIN-EVENT-102 SECTION.                                              
103100     EVALUATE IN-EKH-KDEKSHT                                              
103200     WHEN '102'                                                           
103300          PERFORM CEBB-SUB-EVENT-102-102                                  
103400     WHEN '120'                                                           
103500          PERFORM CEBD-SUB-EVENT-102-120                                  
103600     WHEN '121'                                                           
103700          PERFORM CEBD-SUB-EVENT-102-121                                  
103800     WHEN '122'                                                           
103900          PERFORM CEBD-SUB-EVENT-102-122                                  
104000     WHEN '123'                                                           
104100          PERFORM CEBD-SUB-EVENT-102-123                                  
104200     WHEN '124'                                                           
104300          PERFORM CEBD-SUB-EVENT-102-124                                  
104400     WHEN '125'                                                           
104500          PERFORM CEBD-SUB-EVENT-102-125                                  
104600     WHEN '130'                                                           
104700          PERFORM CEBD-SUB-EVENT-102-130                                  
104800     WHEN '131'                                                           
104900          PERFORM CEBD-SUB-EVENT-102-131                                  
105000     WHEN '132'                                                           
105100          PERFORM CEBD-SUB-EVENT-102-132                                  
105200     WHEN '134'                                                           
105300          PERFORM CEBD-SUB-EVENT-102-134                                  
105400     END-EVALUATE                                                         
105500     .                                                                    
105600     EJECT                                                                
105700                                                                          
105800 CEBD-SUB-EVENT-102-130 SECTION.                                          
105900     EVALUATE IN-EKH-KDEKNIVA                                             
106000     WHEN 'DET'                                                           
106100       IF SYST-IDSEKVNR = 1                                               
106200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
106300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
106400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
106500         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH  * -1            
106600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
106700         PERFORM S03-WRITE-W57022                                         
106800       END-IF                                                             
106900                                                                          
107000     WHEN 'FÖRS'                                                          
107100     WHEN 'FRAKT'                                                         
107200       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
107300       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
107400       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
107500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
107600               IN-EKH-SUBEL / WS-PRKURS-TH  * -1                          
107700       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
107800       PERFORM S04-WRITE-W57043A                                          
107900                                                                          
108000     WHEN 'EMB'                                                           
108100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
108200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
108300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
108400               IN-EKH-SUBEL / WS-PRKURS-TH  * -1                          
108500       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
108600       PERFORM S04-WRITE-W57043A                                          
108700                                                                          
108800     WHEN 'DDI'                                                           
108900       IF IN-EKH-SUBEL > ZERO                                             
109000         IF SYST-IDSEKVNR = 1                                             
109100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
109200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
109300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
109400                   IN-EKH-SUBEL                                           
109500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
109600           PERFORM S04-WRITE-W57043A                                      
109700         END-IF                                                           
109800       ELSE                                                               
109900         IF SYST-IDSEKVNR = 2                                             
110000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
110100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
110200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
110300                   IN-EKH-SUBEL                                           
110400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
110500           PERFORM S04-WRITE-W57043A                                      
110600         END-IF                                                           
110700       END-IF                                                             
110800                                                                          
110900     END-EVALUATE                                                         
111000     .                                                                    
111100     EJECT                                                                
111200                                                                          
111300 CEBD-SUB-EVENT-102-131 SECTION.                                          
111400     EVALUATE IN-EKH-KDEKNIVA                                             
111500     WHEN 'DET'                                                           
111600       PERFORM S13-GET-LANDING-COST                                       
111700                                                                          
111800       IF SYST-IDSEKVNR = 1                                               
111900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
112000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
112100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
112200            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
112300         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-1                   
112400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
112500         PERFORM S03-WRITE-W57022                                         
112600       END-IF                                                             
112700                                                                          
112800       IF SYST-IDSEKVNR = 2                                               
112900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
113000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
113100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
113200            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
113300            + IN-EKH-KVANTAL *                                            
113400            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
113500         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-2                   
113600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
113700         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
113800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
113900         MOVE SPACE               TO WS-ALLOCATE-REF                      
114000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
114100         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
114200         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
114300         PERFORM S03-WRITE-W57022                                         
114400       END-IF                                                             
114500                                                                          
114600       IF SYST-IDSEKVNR = 3                                               
114700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
114800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
114900         COMPUTE R3-LINE-AMOUNT-LC =                                      
115000                 WS-LINE-AMOUNT-131-2 - WS-LINE-AMOUNT-131-1              
115100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
115200         PERFORM S03-WRITE-W57022                                         
115300       END-IF                                                             
115400                                                                          
115500     WHEN 'FÖRS'                                                          
115600     WHEN 'FRAKT'                                                         
115700       IF SYST-IDSEKVNR = 1                                               
115800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
115900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
116000         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
116100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
116200                 IN-EKH-SUBEL / WS-PRKURS-TH3                             
116300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
116400         PERFORM S04-WRITE-W57043A                                        
116500       END-IF                                                             
116600                                                                          
116700       IF SYST-IDSEKVNR = 2                                               
116800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
116900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
117000         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
117100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
117200                 IN-EKH-SUBEL / WS-PRKURS-TH3                             
117300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
117400         PERFORM S04-WRITE-W57043A                                        
117500       END-IF                                                             
117600                                                                          
117700     WHEN 'EMB'                                                           
117800       IF SYST-IDSEKVNR = 1                                               
117900         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
118000         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
118100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
118200                 IN-EKH-SUBEL / WS-PRKURS-TH3                             
118300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
118400         PERFORM S04-WRITE-W57043A                                        
118500       END-IF                                                             
118600                                                                          
118700       IF SYST-IDSEKVNR = 2                                               
118800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
118900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
119000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
119100                 IN-EKH-SUBEL / WS-PRKURS-TH3                             
119200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
119300         PERFORM S04-WRITE-W57043A                                        
119400       END-IF                                                             
119500     END-EVALUATE                                                         
119600     .                                                                    
119700     EJECT                                                                
119800                                                                          
119900 CEBD-SUB-EVENT-102-132 SECTION.                                          
120000     EVALUATE IN-EKH-KDEKNIVA                                             
120100     WHEN 'DET'                                                           
120200       PERFORM S13-GET-LANDING-COST                                       
120300                                                                          
120400       IF IN-EKH-KVANTAL > 0                                              
120500         IF SYST-IDSEKVNR = 1                                             
120600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
120700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
120800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
120900            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
121000            + IN-EKH-KVANTAL *                                            
121100            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
121200           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
121300           MOVE SPACE               TO WS-ALLOCATE-DC                     
121400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
121500           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
121600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
121700           MOVE SPACES              TO R3-LINE-ORDER                      
121800           PERFORM S02-WRITE-W57041A                                      
121900         END-IF                                                           
122000                                                                          
122100         IF SYST-IDSEKVNR = 4                                             
122200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
122300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
122400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
122500            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
122600            + IN-EKH-KVANTAL *                                            
122700            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
122800           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
122900           MOVE SPACE               TO WS-ALLOCATE-DC                     
123000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
123100           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
123200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
123300           MOVE SPACE               TO R3-LINE-COST-CENTER                
123400           MOVE SPACES              TO R3-LINE-ORDER                      
123500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
123600           PERFORM S02-WRITE-W57041A                                      
123700         END-IF                                                           
123800       END-IF                                                             
123900                                                                          
124000       IF IN-EKH-KVANTAL < 0                                              
124100         IF SYST-IDSEKVNR = 2                                             
124200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
124300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
124400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
124500            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
124600            + IN-EKH-KVANTAL *                                            
124700            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
124800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
124900           MOVE SPACE               TO WS-ALLOCATE-DC                     
125000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
125100           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
125200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
125300           MOVE SPACE             TO R3-LINE-COST-CENTER                  
125400           MOVE SPACES              TO R3-LINE-ORDER                      
125500           PERFORM S02-WRITE-W57041A                                      
125600         END-IF                                                           
125700                                                                          
125800         IF SYST-IDSEKVNR = 3                                             
125900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
126000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
126100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
126200            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
126300            + IN-EKH-KVANTAL *                                            
126400            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
126500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
126600           MOVE SPACE               TO WS-ALLOCATE-DC                     
126700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
126800           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
126900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
127000           MOVE SPACE             TO R3-LINE-COST-CENTER                  
127100           MOVE SPACES              TO R3-LINE-ORDER                      
127200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
127300           PERFORM S02-WRITE-W57041A                                      
127400         END-IF                                                           
127500       END-IF                                                             
127600     END-EVALUATE                                                         
127700     .                                                                    
127800     EJECT                                                                
127900                                                                          
128000 CEBD-SUB-EVENT-102-134 SECTION.                                          
128100     EVALUATE IN-EKH-KDEKNIVA                                             
128200     WHEN 'DET'                                                           
128300       IF SYST-IDSEKVNR = 1                                               
128400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
128500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
128600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
128700         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH   * -1           
128800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
128900         MOVE SPACE               TO WS-ALLOCATE-DC                       
129000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
129100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
129200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
129300         PERFORM S03-WRITE-W57022                                         
129400       END-IF                                                             
129500                                                                          
129600     WHEN 'EMB'                                                           
129700     WHEN 'FÖRS'                                                          
129800     WHEN 'FRAKT'                                                         
129900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
130000       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
130100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
130200               (IN-EKH-SUBEL / WS-PRKURS-TH) * -1                         
130300       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
130400       MOVE SPACE               TO WS-ALLOCATE-DC                         
130500       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
130600       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
130700       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
130800       PERFORM S04-WRITE-W57043A                                          
130900                                                                          
131000     WHEN 'DDI'                                                           
131100       IF IN-EKH-SUBEL > ZERO                                             
131200         IF SYST-IDSEKVNR = 1                                             
131300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
131400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
131500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
131600                   IN-EKH-SUBEL                                           
131700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
131800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
131900           MOVE SPACE               TO WS-ALLOCATE-DC                     
132000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
132100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
132200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
132300           PERFORM S04-WRITE-W57043A                                      
132400         END-IF                                                           
132500       ELSE                                                               
132600         IF SYST-IDSEKVNR = 2                                             
132700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
132800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
132900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
133000                   IN-EKH-SUBEL                                           
133100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
133200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
133300           MOVE SPACE               TO WS-ALLOCATE-DC                     
133400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
133500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
133600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
133700           PERFORM S04-WRITE-W57043A                                      
133800         END-IF                                                           
133900       END-IF                                                             
134000     END-EVALUATE                                                         
134100     .                                                                    
134200     EJECT                                                                
134300                                                                          
134400 CEBB-SUB-EVENT-102-102 SECTION.                                          
134500     EVALUATE IN-EKH-KDEKNIVA                                             
134600     WHEN 'DET'                                                           
134700       IF SYST-IDSEKVNR = 1                                               
134800         IF IN-EKH-KVANTAL > 0                                            
134900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
135000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
135100           COMPUTE R3-LINE-AMOUNT-LC =                                    
135200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
135300           IF IN-EKH-KDVALISO = 'THB'                                     
135400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
135500           END-IF                                                         
135600           MOVE SPACE               TO WS-ALLOCATE-DC                     
135700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
135800           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
135900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
136000           PERFORM S02-WRITE-W57041A                                      
136100         END-IF                                                           
136200       END-IF                                                             
136300                                                                          
136400       IF SYST-IDSEKVNR = 2                                               
136500         IF IN-EKH-KVANTAL < 0                                            
136600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
136700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
136800           COMPUTE R3-LINE-AMOUNT-LC =                                    
136900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
137000           IF IN-EKH-KDVALISO = 'THB'                                     
137100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
137200           END-IF                                                         
137300           MOVE SPACE               TO WS-ALLOCATE-DC                     
137400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
137500           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
137600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
137700           PERFORM S02-WRITE-W57041A                                      
137800         END-IF                                                           
137900       END-IF                                                             
138000                                                                          
138100       IF SYST-IDSEKVNR = 3                                               
138200         IF IN-EKH-KVANTAL < 0                                            
138300           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
138400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
138500           COMPUTE R3-LINE-AMOUNT-LC =                                    
138600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
138700           IF IN-EKH-KDVALISO = 'THB'                                     
138800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
138900           END-IF                                                         
139000           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
139100           PERFORM S02-WRITE-W57041A                                      
139200         END-IF                                                           
139300       END-IF                                                             
139400                                                                          
139500       IF SYST-IDSEKVNR = 4                                               
139600         IF IN-EKH-KVANTAL > 0                                            
139700           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
139800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
139900           COMPUTE R3-LINE-AMOUNT-LC =                                    
140000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
140100           IF IN-EKH-KDVALISO = 'THB'                                     
140200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
140300           END-IF                                                         
140400           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
140500           PERFORM S02-WRITE-W57041A                                      
140600         END-IF                                                           
140700       END-IF                                                             
140800                                                                          
140900     END-EVALUATE                                                         
141000     .                                                                    
141100     EJECT                                                                
141200                                                                          
141300 CEBD-SUB-EVENT-102-120 SECTION.                                          
141400     EVALUATE IN-EKH-KDEKNIVA                                             
141500     WHEN 'DET'                                                           
141600       IF SYST-IDSEKVNR = 1                                               
141700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
141800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
141900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
142000          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH * -1            
142100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
142200         PERFORM S03-WRITE-W57022                                         
142300       END-IF                                                             
142400                                                                          
142500     WHEN 'FÖRS'                                                          
142600     WHEN 'FRAKT'                                                         
142700     WHEN 'EMB'                                                           
142800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
142900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
143000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
143100               IN-EKH-SUBEL / WS-PRKURS-TH   * -1                         
143200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
143300       PERFORM S04-WRITE-W57043A                                          
143400                                                                          
143500     WHEN 'DDI'                                                           
143600       IF IN-EKH-SUBEL > ZERO                                             
143700         IF SYST-IDSEKVNR = 1                                             
143800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
143900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
144000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
144100                   IN-EKH-SUBEL                                           
144200           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
144300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
144400           PERFORM S04-WRITE-W57043A                                      
144500         END-IF                                                           
144600       ELSE                                                               
144700         IF SYST-IDSEKVNR = 2                                             
144800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
144900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
145000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
145100                   IN-EKH-SUBEL                                           
145200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
145300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
145400           PERFORM S04-WRITE-W57043A                                      
145500         END-IF                                                           
145600       END-IF                                                             
145700     END-EVALUATE                                                         
145800     .                                                                    
145900     EJECT                                                                
146000                                                                          
146100 CEBD-SUB-EVENT-102-121 SECTION.                                          
146200     EVALUATE IN-EKH-KDEKNIVA                                             
146300     WHEN 'DET'                                                           
146400       PERFORM S13-GET-LANDING-COST                                       
146500                                                                          
146600       IF SYST-IDSEKVNR = 1                                               
146700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
146800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
146900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
147000            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
147100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
147200         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-1                      
147300         MOVE SPACE               TO WS-ALLOCATE-DC                       
147400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
147500         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
147600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
147700         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
147800         PERFORM S03-WRITE-W57022                                         
147900       END-IF                                                             
148000                                                                          
148100       IF SYST-IDSEKVNR = 2                                               
148200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
148300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
148400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
148500            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
148600            + IN-EKH-KVANTAL *                                            
148700            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
148800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
148900         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-2                      
149000         PERFORM S03-WRITE-W57022                                         
149100       END-IF                                                             
149200                                                                          
149300       IF SYST-IDSEKVNR = 3                                               
149400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
149500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
149600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
149700            WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
149800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
149900         MOVE SPACE               TO WS-ALLOCATE-DC                       
150000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
150100         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
150200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
150300         PERFORM S03-WRITE-W57022                                         
150400       END-IF                                                             
150500                                                                          
150600     WHEN 'FÖRS'                                                          
150700     WHEN 'FRAKT'                                                         
150800     WHEN 'EMB'                                                           
150900       IF SYST-IDSEKVNR = 1                                               
151000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
151100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
151200         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
151300                 IN-EKH-SUBEL / WS-PRKURS-TH3                             
151400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
151500         PERFORM S04-WRITE-W57043A                                        
151600       END-IF                                                             
151700                                                                          
151800       IF SYST-IDSEKVNR = 2                                               
151900         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
152000         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
152100         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
152200                 IN-EKH-SUBEL / WS-PRKURS-TH3                             
152300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
152400         MOVE SPACE               TO WS-ALLOCATE-DC                       
152500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
152600         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
152700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
152800         PERFORM S04-WRITE-W57043A                                        
152900       END-IF                                                             
153000                                                                          
153100     END-EVALUATE                                                         
153200                                                                          
153300     .                                                                    
153400     EJECT                                                                
153500                                                                          
153600 CEBD-SUB-EVENT-102-122 SECTION.                                          
153700     EVALUATE IN-EKH-KDEKNIVA                                             
153800     WHEN 'DET'                                                           
153900       PERFORM S13-GET-LANDING-COST                                       
154000                                                                          
154100                                                                          
154200       IF IN-EKH-KVANTAL > 0                                              
154300         IF SYST-IDSEKVNR = 1                                             
154400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
154500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
154600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
154700            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
154800            + IN-EKH-KVANTAL *                                            
154900            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
155000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
155100           MOVE SPACE               TO WS-ALLOCATE-DC                     
155200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
155300           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
155400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
155500           MOVE SPACES              TO R3-LINE-ORDER                      
155600           PERFORM S02-WRITE-W57041A                                      
155700         END-IF                                                           
155800                                                                          
155900         IF SYST-IDSEKVNR = 4                                             
156000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
156100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
156200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
156300            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
156400            + IN-EKH-KVANTAL *                                            
156500            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
156600           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
156700           MOVE SPACE               TO WS-ALLOCATE-DC                     
156800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
156900           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
157000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
157100           MOVE SPACE               TO R3-LINE-COST-CENTER                
157200           MOVE SPACES              TO R3-LINE-ORDER                      
157300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
157400           PERFORM S02-WRITE-W57041A                                      
157500         END-IF                                                           
157600       END-IF                                                             
157700                                                                          
157800       IF IN-EKH-KVANTAL < 0                                              
157900         IF SYST-IDSEKVNR = 2                                             
158000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
158100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
158200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
158300            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
158400            + IN-EKH-KVANTAL *                                            
158500            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
158600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
158700           MOVE SPACE               TO WS-ALLOCATE-DC                     
158800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
158900           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
159000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
159100           MOVE SPACE             TO R3-LINE-COST-CENTER                  
159200           MOVE SPACES              TO R3-LINE-ORDER                      
159300           PERFORM S02-WRITE-W57041A                                      
159400         END-IF                                                           
159500                                                                          
159600         IF SYST-IDSEKVNR = 3                                             
159700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
159800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
159900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
160000            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
160100            + IN-EKH-KVANTAL *                                            
160200            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
160300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
160400           MOVE SPACE               TO WS-ALLOCATE-DC                     
160500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
160600           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
160700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
160800           MOVE SPACE             TO R3-LINE-COST-CENTER                  
160900           MOVE SPACES              TO R3-LINE-ORDER                      
161000           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
161100           PERFORM S02-WRITE-W57041A                                      
161200         END-IF                                                           
161300       END-IF                                                             
161400     END-EVALUATE                                                         
161500     .                                                                    
161600     EJECT                                                                
161700                                                                          
161800 CEBD-SUB-EVENT-102-123 SECTION.                                          
161900     EVALUATE IN-EKH-KDEKNIVA                                             
162000     WHEN 'DET'                                                           
162100       IF SYST-IDSEKVNR = 1                                               
162200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
162300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
162400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
162500              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
162600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
162700         MOVE SPACE               TO WS-ALLOCATE-DC                       
162800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
162900         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
163000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
163100         PERFORM S02-WRITE-W57041A                                        
163200       END-IF                                                             
163300                                                                          
163400       IF SYST-IDSEKVNR = 2                                               
163500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
163600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
163700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
163800             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
163900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
164000         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
164100         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
164200         PERFORM S02-WRITE-W57041A                                        
164300       END-IF                                                             
164400     END-EVALUATE                                                         
164500     .                                                                    
164600     EJECT                                                                
164700                                                                          
164800 CEBD-SUB-EVENT-102-124 SECTION.                                          
164900     EVALUATE IN-EKH-KDEKNIVA                                             
165000     WHEN 'DET'                                                           
165100       IF SYST-IDSEKVNR = 1                                               
165200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
165300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
165400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
165500         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH   * -1           
165600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
165700         MOVE SPACE               TO WS-ALLOCATE-DC                       
165800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
165900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
166000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
166100         PERFORM S03-WRITE-W57022                                         
166200       END-IF                                                             
166300                                                                          
166400     WHEN 'EMB'                                                           
166500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
166600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
166700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
166800               (IN-EKH-SUBEL / WS-PRKURS-TH) * -1                         
166900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
167000       MOVE SPACE               TO WS-ALLOCATE-DC                         
167100       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
167200       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
167300       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
167400       PERFORM S04-WRITE-W57043A                                          
167500                                                                          
167600     WHEN 'DDI'                                                           
167700       IF IN-EKH-SUBEL > ZERO                                             
167800         IF SYST-IDSEKVNR = 1                                             
167900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
168000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
168100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
168200                   IN-EKH-SUBEL                                           
168300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
168400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
168500           MOVE SPACE               TO WS-ALLOCATE-DC                     
168600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
168700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
168800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
168900           PERFORM S04-WRITE-W57043A                                      
169000         END-IF                                                           
169100       ELSE                                                               
169200         IF SYST-IDSEKVNR = 2                                             
169300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
169400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
169500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
169600                   IN-EKH-SUBEL                                           
169700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
169800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
169900           MOVE SPACE               TO WS-ALLOCATE-DC                     
170000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
170100           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
170200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
170300           PERFORM S04-WRITE-W57043A                                      
170400         END-IF                                                           
170500       END-IF                                                             
170600     END-EVALUATE                                                         
170700     .                                                                    
170800     EJECT                                                                
170900                                                                          
171000 CEBD-SUB-EVENT-102-125 SECTION.                                          
171100     EVALUATE IN-EKH-KDEKNIVA                                             
171200     WHEN 'DET'                                                           
171300       PERFORM S13-GET-LANDING-COST                                       
171400                                                                          
171500       IF SYST-IDSEKVNR = 1                                               
171600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
171700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
171800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
171900            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3              
172000            + IN-EKH-KVANTAL *                                            
172100            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
172200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
172300         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
172400         PERFORM S03-WRITE-W57022                                         
172500       END-IF                                                             
172600                                                                          
172700       IF SYST-IDSEKVNR = 2                                               
172800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
172900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
173000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
173100            IN-EKH-KVANTAL *                                              
173200            IN-EKH-PRARTNTO / WS-PRKURS-TH3 * WS-MARKUP1                  
173300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
173400         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
173500         PERFORM S03-WRITE-W57022                                         
173600       END-IF                                                             
173700                                                                          
173800     WHEN 'FÖRS'                                                          
173900     WHEN 'FRAKT'                                                         
174000     WHEN 'EMB'                                                           
174100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
174200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
174300       MOVE SPACE             TO R3-LINE-COST-CENTER                      
174400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
174500          IN-EKH-SUBEL / WS-PRKURS-TH   * -1                              
174600       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
174700       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
174800       PERFORM S04-WRITE-W57043A                                          
174900                                                                          
175000     WHEN 'DDI'                                                           
175100       IF IN-EKH-SUBEL > ZERO                                             
175200         IF SYST-IDSEKVNR = 1                                             
175300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
175400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
175500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
175600                   IN-EKH-SUBEL                                           
175700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
175800           ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                      
175900           PERFORM S04-WRITE-W57043A                                      
176000         END-IF                                                           
176100       ELSE                                                               
176200         IF IN-EKH-SUBEL < ZERO                                           
176300           IF SYST-IDSEKVNR = 2                                           
176400             MOVE SYST-IDKONTO  TO WS-R3-ACCOUNT-10                       
176500             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
176600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
176700                     IN-EKH-SUBEL                                         
176800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
176900             SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125              
177000             PERFORM S04-WRITE-W57043A                                    
177100           END-IF                                                         
177200         END-IF                                                           
177300       END-IF                                                             
177400                                                                          
177500     END-EVALUATE                                                         
177600     .                                                                    
177700     EJECT                                                                
177800                                                                          
177900 CEC-MAIN-EVENT-103 SECTION.                                              
178000     EVALUATE IN-EKH-KDEKSHT                                              
178100     WHEN '102'                                                           
178200          PERFORM CECB-SUB-EVENT-103-102                                  
178300     END-EVALUATE                                                         
178400     .                                                                    
178500     EJECT                                                                
178600                                                                          
178700 CGA-MAIN-EVENT-303-371 SECTION.                                          
178800     EVALUATE IN-EKH-KDEKNIVA                                             
178900     WHEN 'SUM'                                                           
179000       IF SYST-IDSEKVNR = 1                                               
179100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
179200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
179300         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
179400         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
179500               R3-LINE-AMOUNT-LC / WS-PRKURS-TH3                          
179600         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
179700         PERFORM S10-VATCODE                                              
179800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
179900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
180000                 R3-LINE-TAX-AMOUNT-LC * WS-PRKURS-TH2                    
180100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
180200                                                                          
180300         PERFORM S04-WRITE-W57043A                                        
180400       END-IF                                                             
180500     END-EVALUATE                                                         
180600     .                                                                    
180700     EJECT                                                                
180800 CECB-SUB-EVENT-103-102 SECTION.                                          
180900     EVALUATE IN-EKH-KDEKNIVA                                             
181000     WHEN 'DET'                                                           
181100       IF SYST-IDSEKVNR = 1                                               
181200         IF IN-EKH-KVANTAL < 0                                            
181300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
181400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
181500           COMPUTE R3-LINE-AMOUNT-LC =                                    
181600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
181700           IF IN-EKH-KDVALISO = 'THB'                                     
181800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
181900           END-IF                                                         
182000           MOVE SPACE               TO WS-ALLOCATE-DC                     
182100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
182200           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
182300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
182400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
182500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
182600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
182700           PERFORM S04-WRITE-W57043A                                      
182800         END-IF                                                           
182900       END-IF                                                             
183000                                                                          
183100       IF SYST-IDSEKVNR = 2                                               
183200         IF IN-EKH-KVANTAL > 0                                            
183300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
183400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
183500           COMPUTE R3-LINE-AMOUNT-LC =                                    
183600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
183700           IF IN-EKH-KDVALISO = 'THB'                                     
183800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
183900           END-IF                                                         
184000           MOVE SPACE               TO WS-ALLOCATE-DC                     
184100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
184200           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
184300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
184400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
184500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
184600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
184700           PERFORM S04-WRITE-W57043A                                      
184800         END-IF                                                           
184900       END-IF                                                             
185000                                                                          
185100     WHEN 'DDI'                                                           
185200       IF IN-EKH-SUBEL > ZERO                                             
185300         IF SYST-IDSEKVNR = 1                                             
185400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
185500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
185600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
185700                   IN-EKH-SUBEL                                           
185800           MOVE ZEROES              TO R3-LINE-AMOUNT                     
185900           IF IN-EKH-KDVALISO = 'THB'                                     
186000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
186100           END-IF                                                         
186200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
186300           MOVE SPACE               TO R3-LINE-ALLOCATE                   
186400           PERFORM S04-WRITE-W57043A                                      
186500         END-IF                                                           
186600       ELSE                                                               
186700         IF SYST-IDSEKVNR = 2                                             
186800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
186900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
187000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
187100                   IN-EKH-SUBEL                                           
187200           MOVE ZEROES              TO R3-LINE-AMOUNT                     
187300           IF IN-EKH-KDVALISO = 'THB'                                     
187400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
187500           END-IF                                                         
187600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
187700           MOVE SPACE               TO R3-LINE-ALLOCATE                   
187800           PERFORM S04-WRITE-W57043A                                      
187900         END-IF                                                           
188000       END-IF                                                             
188100                                                                          
188200     END-EVALUATE                                                         
188300     .                                                                    
188400     EJECT                                                                
188500                                                                          
188600 CED-MAIN-EVENT-201 SECTION.                                              
188700     EVALUATE IN-EKH-KDEKSHT                                              
188800     WHEN '201'                                                           
188900          PERFORM CEDA-SUB-EVENT-201-201                                  
189000     WHEN '202'                                                           
189100          PERFORM CEDB-SUB-EVENT-201-202                                  
189200     END-EVALUATE                                                         
189300     .                                                                    
189400     EJECT                                                                
189500                                                                          
189600 CEDA-SUB-EVENT-201-201 SECTION.                                          
189700     EVALUATE IN-EKH-KDEKNIVA                                             
189800     WHEN 'DET'                                                           
189900       IF SYST-IDSEKVNR = 1                                               
190000         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
190100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
190200         COMPUTE R3-LINE-AMOUNT-LC =                                      
190300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
190400         IF IN-EKH-KDVALISO = 'THB'                                       
190500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
190600         END-IF                                                           
190700         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
190800         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
190900         PERFORM S03-WRITE-W57022                                         
191000       END-IF                                                             
191100                                                                          
191200       IF SYST-IDSEKVNR = 2                                               
191300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
191400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
191500         COMPUTE R3-LINE-AMOUNT-LC =                                      
191600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
191700         IF IN-EKH-KDVALISO = 'THB'                                       
191800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
191900         END-IF                                                           
192000         MOVE SPACE               TO WS-ALLOCATE-DC                       
192100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
192200         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
192300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
192400         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
192500         PERFORM S03-WRITE-W57022                                         
192600       END-IF                                                             
192700     END-EVALUATE                                                         
192800     .                                                                    
192900     EJECT                                                                
193000                                                                          
193100 CEDB-SUB-EVENT-201-202 SECTION.                                          
193200     EVALUATE IN-EKH-KDEKNIVA                                             
193300     WHEN 'DET'                                                           
193400       IF SYST-IDSEKVNR = 1                                               
193500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
193600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
193700         COMPUTE R3-LINE-AMOUNT-LC =                                      
193800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
193900         IF IN-EKH-KDVALISO = 'THB'                                       
194000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
194100         END-IF                                                           
194200         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
194300         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
194400         PERFORM S03-WRITE-W57022                                         
194500       END-IF                                                             
194600                                                                          
194700       IF SYST-IDSEKVNR = 2                                               
194800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
194900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
195000         COMPUTE R3-LINE-AMOUNT-LC =                                      
195100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
195200         IF IN-EKH-KDVALISO = 'THB'                                       
195300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
195400         END-IF                                                           
195500         MOVE SPACE               TO WS-ALLOCATE-DC                       
195600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
195700         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
195800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
195900         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
196000         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
196100         PERFORM S03-WRITE-W57022                                         
196200       END-IF                                                             
196300     END-EVALUATE                                                         
196400     .                                                                    
196500     EJECT                                                                
196600                                                                          
196700 CEF-MAIN-EVENT-203 SECTION.                                              
196800     EVALUATE IN-EKH-KDEKSHT                                              
196900     WHEN '201'                                                           
197000          PERFORM CEFA-SUB-EVENT-203-201                                  
197100     END-EVALUATE                                                         
197200     .                                                                    
197300     EJECT                                                                
197400                                                                          
197500 CEFA-SUB-EVENT-203-201 SECTION.                                          
197600     EVALUATE IN-EKH-KDEKNIVA                                             
197700     WHEN 'DET'                                                           
197800       IF SYST-IDSEKVNR = 1                                               
197900         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
198000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
198100         COMPUTE R3-LINE-AMOUNT-LC =                                      
198200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
198300         IF IN-EKH-KDVALISO = 'THB'                                       
198400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
198500         END-IF                                                           
198600         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
198700         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
198800         PERFORM S03-WRITE-W57022                                         
198900       END-IF                                                             
199000                                                                          
199100       IF SYST-IDSEKVNR = 2                                               
199200         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
199300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
199400         COMPUTE R3-LINE-AMOUNT-LC =                                      
199500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
199600         IF IN-EKH-KDVALISO = 'THB'                                       
199700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
199800         END-IF                                                           
199900         MOVE SPACE             TO WS-ALLOCATE-DC                         
200000         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
200100         MOVE IN-EKH-IDVERGL    TO WS-ALLOCATE-REF                        
200200         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
200300         MOVE 0000368288        TO R3-LINE-PA-CUSTOMER                    
200400         PERFORM S03-WRITE-W57022                                         
200500       END-IF                                                             
200600     END-EVALUATE                                                         
200700     .                                                                    
200800     EJECT                                                                
200900                                                                          
201000 CEG-MAIN-EVENT-204 SECTION.                                              
201100     EVALUATE IN-EKH-KDEKSHT                                              
201200     WHEN '201'                                                           
201300          PERFORM CEGA-SUB-EVENT-204-201                                  
201400     WHEN '301'                                                           
201500          PERFORM CEGB-SUB-EVENT-204-301                                  
201600     END-EVALUATE                                                         
201700     .                                                                    
201800     EJECT                                                                
201900                                                                          
202000 CEGA-SUB-EVENT-204-201 SECTION.                                          
202100     EVALUATE IN-EKH-KDEKNIVA                                             
202200     WHEN 'DET'                                                           
202300         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
202400* R-FAKTURA                                                               
202500       IF SYST-IDSEKVNR = 1                                               
202600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
202700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
202800         COMPUTE R3-LINE-AMOUNT-LC =                                      
202900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
203000         IF IN-EKH-KDVALISO = 'THB'                                       
203100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
203200         END-IF                                                           
203300         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
203400         MOVE 'TH  '              TO R3-LINE-TRADING-PARTNER              
203500         MOVE SPACE               TO WS-ALLOCATE-DC                       
203600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
203700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
203800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
203900         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
204000         PERFORM S03-WRITE-W57022                                         
204100       END-IF                                                             
204200                                                                          
204300       IF SYST-IDSEKVNR = 2                                               
204400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
204500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
204600         COMPUTE R3-LINE-AMOUNT-LC =                                      
204700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
204800         IF IN-EKH-KDVALISO = 'THB'                                       
204900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
205000         END-IF                                                           
205100         MOVE SPACE               TO WS-ALLOCATE-DC                       
205200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
205300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
205400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
205500         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
205600         PERFORM S03-WRITE-W57022                                         
205700       END-IF                                                             
205800     END-EVALUATE                                                         
205900     .                                                                    
206000     EJECT                                                                
206100 CEGB-SUB-EVENT-204-301 SECTION.                                          
206200     EVALUATE IN-EKH-KDEKNIVA                                             
206300     WHEN 'DET'                                                           
206400         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
206500* R-FAKTURA                                                               
206600       IF SYST-IDSEKVNR = 1                                               
206700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
206800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
206900         COMPUTE R3-LINE-AMOUNT-LC =                                      
207000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
207100         IF IN-EKH-KDVALISO = 'THB'                                       
207200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
207300         END-IF                                                           
207400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
207500         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
207600         PERFORM S03-WRITE-W57022                                         
207700       END-IF                                                             
207800                                                                          
207900       IF SYST-IDSEKVNR = 2                                               
208000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
208100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
208200         COMPUTE R3-LINE-AMOUNT-LC =                                      
208300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
208400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
208500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
208600         MOVE SPACE             TO WS-ALLOCATE-DC                         
208700         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
208800         MOVE IN-EKH-IDVERGL    TO WS-ALLOCATE-REF                        
208900         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
209000         PERFORM S03-WRITE-W57022                                         
209100       END-IF                                                             
209200                                                                          
209300       IF SYST-IDSEKVNR = 3                                               
209400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
209500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
209600         COMPUTE R3-LINE-AMOUNT-LC =                                      
209700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
209800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
209900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
210000         MOVE SPACE             TO WS-ALLOCATE-DC                         
210100         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
210200         MOVE IN-EKH-IDVERGL    TO WS-ALLOCATE-REF                        
210300         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
210400         PERFORM S03-WRITE-W57022                                         
210500       END-IF                                                             
210600                                                                          
210700     WHEN 'EMB'                                                           
210800     WHEN 'FÖRS'                                                          
210900     WHEN 'FRAKT'                                                         
211000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
211100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
211200       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
211300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
211400               IN-EKH-SUBEL * -1                                          
211500       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
211600       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
211700       PERFORM S04-WRITE-W57043A                                          
211800                                                                          
211900     END-EVALUATE                                                         
212000     .                                                                    
212100     EJECT                                                                
212200                                                                          
212300 CEI-MAIN-EVENT-302 SECTION.                                              
212400     EVALUATE IN-EKH-KDEKSHT                                              
212500     WHEN '301'                                                           
212600          PERFORM CEIA-SUB-EVENT-302-301                                  
212700     WHEN '302'                                                           
212800          PERFORM CEIB-SUB-EVENT-302-302                                  
212900     END-EVALUATE                                                         
213000     .                                                                    
213100     EJECT                                                                
213200                                                                          
213300 CEIA-SUB-EVENT-302-301 SECTION.                                          
213400     EVALUATE IN-EKH-KDEKNIVA                                             
213500     WHEN 'DET'                                                           
213600       IF SYST-IDSEKVNR = 1                                               
213700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
213800         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
213900         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
214000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
214100         COMPUTE R3-LINE-AMOUNT-LC =                                      
214200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
214300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
214400         MOVE SPACE               TO WS-ALLOCATE-DC                       
214500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
214600         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
214700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
214800         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
214900         PERFORM S02-WRITE-W57041A                                        
215000       END-IF                                                             
215100                                                                          
215200       IF SYST-IDSEKVNR = 2                                               
215300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
215400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
215500         MOVE SPACE           TO R3-LINE-COST-CENTER                      
215600         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
215700         COMPUTE R3-LINE-AMOUNT-LC =                                      
215800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
215900         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
216000         MOVE SPACE               TO WS-LINE-TEXT                         
216100         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
216200         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
216300         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
216400         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
216500         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
216600         PERFORM S02-WRITE-W57041A                                        
216700       END-IF                                                             
216800     END-EVALUATE                                                         
216900     .                                                                    
217000     EJECT                                                                
217100                                                                          
217200 CEIB-SUB-EVENT-302-302 SECTION.                                          
217300     EVALUATE IN-EKH-KDEKNIVA                                             
217400     WHEN 'DET'                                                           
217500       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
217600         IF SYST-IDSEKVNR = 1                                             
217700           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
217800           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
217900           IF BET-KDTRADP(3:2) NOT = SPACE                                
218000             MOVE '1'             TO WS-ACCOUNT-4                         
218100           ELSE                                                           
218200             MOVE '3'             TO WS-ACCOUNT-4                         
218300           END-IF                                                         
218400           COMPUTE R3-LINE-AMOUNT-LC  =                                   
218500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
218600           IF IN-EKH-KDVALISO = 'THB'                                     
218700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
218800           END-IF                                                         
218900           MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                
219000           PERFORM S02-WRITE-W57041A                                      
219100         END-IF                                                           
219200                                                                          
219300         IF SYST-IDSEKVNR = 4                                             
219400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
219500           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
219600           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
219700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
219800           COMPUTE R3-LINE-AMOUNT-LC  =                                   
219900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
220000           IF IN-EKH-KDVALISO = 'THB'                                     
220100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
220200           END-IF                                                         
220300           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
220400           MOVE SPACE             TO WS-ALLOCATE-DC                       
220500           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
220600           MOVE IN-EKH-IDVERGL    TO WS-ALLOCATE-REF                      
220700           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
220800           MOVE 0000368288        TO R3-LINE-PA-CUSTOMER                  
220900           MOVE 'TH  '            TO R3-LINE-TRADING-PARTNER              
221000                                                                          
221100           PERFORM S02-WRITE-W57041A                                      
221200         END-IF                                                           
221300       ELSE                                                               
221400         IF SYST-IDSEKVNR = 2                                             
221500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
221600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
221700           IF BET-KDTRADP(3:2) NOT = SPACE                                
221800             MOVE '1'             TO WS-ACCOUNT-4                         
221900           ELSE                                                           
222000             MOVE '3'             TO WS-ACCOUNT-4                         
222100           END-IF                                                         
222200           COMPUTE R3-LINE-AMOUNT-LC  =                                   
222300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
222400           IF IN-EKH-KDVALISO = 'THB'                                     
222500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
222600           END-IF                                                         
222700           MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                
222800           PERFORM S02-WRITE-W57041A                                      
222900         END-IF                                                           
223000                                                                          
223100         IF SYST-IDSEKVNR = 3                                             
223200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
223300           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
223400           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
223500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
223600           COMPUTE R3-LINE-AMOUNT-LC  =                                   
223700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
223800           IF IN-EKH-KDVALISO = 'THB'                                     
223900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
224000           END-IF                                                         
224100           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
224200           MOVE SPACE             TO WS-ALLOCATE-DC                       
224300           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
224400           MOVE IN-EKH-IDVERGL    TO WS-ALLOCATE-REF                      
224500           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
224600           MOVE 0000368288        TO R3-LINE-PA-CUSTOMER                  
224700           MOVE 'TH  '            TO R3-LINE-TRADING-PARTNER              
224800           PERFORM S02-WRITE-W57041A                                      
224900         END-IF                                                           
225000       END-IF                                                             
225100     END-EVALUATE                                                         
225200     .                                                                    
225300     EJECT                                                                
225400                                                                          
225500 CEJ-MAIN-EVENT-303 SECTION.                                              
225600     EVALUATE IN-EKH-KDEKSHT                                              
225700     WHEN '3XX'                                                           
225800          PERFORM CEJ301-SUB-EVENT-303-3XX                                
225900     WHEN '301'                                                           
226000          PERFORM CEJ301-SUB-EVENT-303-301                                
226100     WHEN '307'                                                           
226200          PERFORM CEJ307-SUB-EVENT-303-307                                
226300     WHEN '310'                                                           
226400          PERFORM CEJ310-SUB-EVENT-303-310                                
226500     WHEN '311'                                                           
226600          PERFORM CEJ311-SUB-EVENT-303-311                                
226700     WHEN '391'                                                           
226800          PERFORM CEJ301-SUB-EVENT-303-391                                
226900     WHEN '371'                                                           
227000          PERFORM CEJ371-SUB-EVENT-303-371                                
227100     END-EVALUATE                                                         
227200     .                                                                    
227300     EJECT                                                                
227400                                                                          
227500 CEJ371-SUB-EVENT-303-371  SECTION.                                       
227600     EVALUATE IN-EKH-KDEKNIVA                                             
227700     WHEN 'DET'                                                           
227800       IF SYST-IDSEKVNR = 1                                               
227900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
228000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
228100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
228200           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3               
228300         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
228400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
228500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
228600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
228700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
228800         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
228900         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
229000         PERFORM S04-WRITE-W57043A                                        
229100       END-IF                                                             
229200                                                                          
229300     WHEN 'LAND'                                                          
229400       IF SYST-IDSEKVNR = 1                                               
229500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
229600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
229700         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
229800         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
229900               R3-LINE-AMOUNT-LC / WS-PRKURS-TH3                          
230000         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
230100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
230200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
230300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
230400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
230500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
230600         PERFORM S02-WRITE-W57041A                                        
230700       END-IF                                                             
230800                                                                          
230900     WHEN 'DDI'                                                           
231000       IF IN-EKH-SUBEL > ZERO                                             
231100         IF SYST-IDSEKVNR = 1                                             
231200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
231300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
231400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
231500                   IN-EKH-SUBEL                                           
231600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
231700           MOVE SPACE               TO WS-ALLOCATE-DC                     
231800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
231900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
232000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
232100           PERFORM S02-WRITE-W57041A                                      
232200         END-IF                                                           
232300       ELSE                                                               
232400         IF SYST-IDSEKVNR = 2                                             
232500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
232600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
232700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
232800                   IN-EKH-SUBEL                                           
232900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
233000           MOVE SPACE               TO WS-ALLOCATE-DC                     
233100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
233200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
233300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
233400           PERFORM S02-WRITE-W57041A                                      
233500         END-IF                                                           
233600       END-IF                                                             
233700     END-EVALUATE                                                         
233800     .                                                                    
233900     EJECT                                                                
234000 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
234100     EVALUATE IN-EKH-KDEKNIVA                                             
234200                                                                          
234300     WHEN 'LAND'                                                          
234400       IF SYST-IDSEKVNR = 1                                               
234500         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
234600         MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                          
234700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
234800                 IN-EKH-SUBEL * -1  / WS-PRKURS-TH3                       
234900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
235000         MOVE SPACE           TO WS-ALLOCATE-DC                           
235100         MOVE SPACE           TO WS-ALLOCATE-DISTR                        
235200         MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                        
235300         MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                         
235400         PERFORM S03-WRITE-W57022                                         
235500       END-IF                                                             
235600                                                                          
235700     WHEN 'DDI'                                                           
235800       IF IN-EKH-SUBEL > ZERO                                             
235900         IF SYST-IDSEKVNR = 1                                             
236000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
236100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
236200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
236300                   IN-EKH-SUBEL                                           
236400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
236500           MOVE SPACE               TO WS-ALLOCATE-DC                     
236600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
236700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
236800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
236900           PERFORM S04-WRITE-W57043A                                      
237000         END-IF                                                           
237100       ELSE                                                               
237200         IF SYST-IDSEKVNR = 2                                             
237300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
237400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
237500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
237600                   IN-EKH-SUBEL                                           
237700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
237800           MOVE SPACE               TO WS-ALLOCATE-DC                     
237900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
238000           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
238100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
238200           PERFORM S04-WRITE-W57043A                                      
238300         END-IF                                                           
238400       END-IF                                                             
238500     END-EVALUATE                                                         
238600     .                                                                    
238700     EJECT                                                                
238800                                                                          
238900 CEJ301-SUB-EVENT-303-301 SECTION.                                        
239000     EVALUATE IN-EKH-KDEKNIVA                                             
239100     WHEN 'DET'                                                           
239200       IF SYST-IDSEKVNR = 1                                               
239300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
239400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
239500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
239600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3 * -1            
239700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
239800         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
239900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
240000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
240100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
240200         MOVE SPACE               TO WS-ALLOCATE-DC                       
240300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
240400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
240500         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
240600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
240700         PERFORM S03-WRITE-W57022                                         
240800       END-IF                                                             
240900                                                                          
241000     END-EVALUATE                                                         
241100     .                                                                    
241200     EJECT                                                                
241300                                                                          
241400 CEJ307-SUB-EVENT-303-307 SECTION.                                        
241500     EVALUATE IN-EKH-KDEKNIVA                                             
241600     WHEN 'DET'                                                           
241700       IF SYST-IDSEKVNR = 1                                               
241800         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
241900         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
242000         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
242100         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-TH3 * -1            
242200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
242300         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
242400         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
242500         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
242600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
242700         MOVE SPACE               TO WS-ALLOCATE-DC                       
242800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
242900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
243000         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
243100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
243200         PERFORM S03-WRITE-W57022                                         
243300       END-IF                                                             
243400                                                                          
243500     END-EVALUATE                                                         
243600     .                                                                    
243700     EJECT                                                                
243800                                                                          
243900 CEJ310-SUB-EVENT-303-310 SECTION.                                        
244000     EVALUATE IN-EKH-KDEKNIVA                                             
244100     WHEN 'DET'                                                           
244200       IF SYST-IDSEKVNR = 1                                               
244300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
244400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
244500         COMPUTE R3-LINE-AMOUNT-LC =                                      
244600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
244700         IF IN-EKH-KDVALISO = 'THB'                                       
244800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
244900         END-IF                                                           
245000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
245100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
245200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
245300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
245400         MOVE SPACE               TO WS-LINE-TEXT                         
245500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
245600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
245700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
245800         PERFORM S02-WRITE-W57041A                                        
245900       END-IF                                                             
246000                                                                          
246100       IF SYST-IDSEKVNR = 2                                               
246200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
246300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
246400         COMPUTE R3-LINE-AMOUNT-LC =                                      
246500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
246600         IF IN-EKH-KDVALISO = 'THB'                                       
246700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
246800         END-IF                                                           
246900         MOVE SPACE               TO WS-LINE-TEXT                         
247000         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
247100         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
247200         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
247300         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
247400         MOVE SPACE               TO WS-ALLOCATE-DC                       
247500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
247600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
247700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
247800         PERFORM S02-WRITE-W57041A                                        
247900       END-IF                                                             
248000     END-EVALUATE                                                         
248100     .                                                                    
248200     EJECT                                                                
248300                                                                          
248400 CEJ311-SUB-EVENT-303-311 SECTION.                                        
248500     EVALUATE IN-EKH-KDEKNIVA                                             
248600     WHEN 'DET'                                                           
248700        IF SYST-IDSEKVNR = 1                                              
248800          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
248900          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
249000          COMPUTE R3-LINE-AMOUNT-LC =                                     
249100                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
249200          IF IN-EKH-KDVALISO = 'THB'                                      
249300            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
249400          END-IF                                                          
249500          MOVE SPACE               TO WS-LINE-TEXT                        
249600          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
249700          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
249800          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
249900          MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                 
250000          MOVE SPACE               TO WS-ALLOCATE-DC                      
250100          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
250200          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
250300          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
250400          PERFORM S02-WRITE-W57041A                                       
250500        END-IF                                                            
250600                                                                          
250700        IF SYST-IDSEKVNR = 2                                              
250800          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
250900          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
251000          COMPUTE R3-LINE-AMOUNT-LC =                                     
251100                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
251200          IF IN-EKH-KDVALISO = 'THB'                                      
251300            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
251400          END-IF                                                          
251500          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
251600          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
251700          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
251800          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
251900          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
252000          MOVE SPACE               TO WS-LINE-TEXT                        
252100          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
252200          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
252300          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
252400          MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                 
252500          MOVE 'TH  '              TO R3-LINE-TRADING-PARTNER             
252600          PERFORM S02-WRITE-W57041A                                       
252700        END-IF                                                            
252800     END-EVALUATE                                                         
252900     .                                                                    
253000     EJECT                                                                
253100                                                                          
253200 CEJ301-SUB-EVENT-303-391 SECTION.                                        
253300     EVALUATE IN-EKH-KDEKNIVA                                             
253400     WHEN 'DET'                                                           
253500       IF SYST-IDSEKVNR = 1                                               
253600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
253700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
253800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
253900              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
254000         IF IN-EKH-KDVALISO = 'THB'                                       
254100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
254200         END-IF                                                           
254300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
254400         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
254500         MOVE SPACE               TO WS-LINE-TEXT                         
254600         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
254700         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
254800         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
254900         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
255000         MOVE 'TH  '              TO R3-LINE-TRADING-PARTNER              
255100         MOVE SPACE               TO WS-ALLOCATE-DC                       
255200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
255300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
255400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
255500         PERFORM S03-WRITE-W57022                                         
255600       END-IF                                                             
255700                                                                          
255800       IF SYST-IDSEKVNR = 2                                               
255900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
256000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
256100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
256200              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
256300         IF IN-EKH-KDVALISO = 'THB'                                       
256400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
256500         END-IF                                                           
256600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
256700         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
256800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
256900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
257000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
257100         MOVE SPACE               TO WS-LINE-TEXT                         
257200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
257300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
257400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
257500         MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                  
257600         PERFORM S03-WRITE-W57022                                         
257700       END-IF                                                             
257800                                                                          
257900     END-EVALUATE                                                         
258000     .                                                                    
258100     EJECT                                                                
258200                                                                          
258300 CEK-MAIN-EVENT-401 SECTION.                                              
258400     EVALUATE IN-EKH-KDEKNIVA                                             
258500                                                                          
258600* PRISÄNDRING LÖPANDE                                                     
258700     WHEN 'DET'                                                           
258800       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
258900                           IN-EKH-PRARTSTD                                
259000       IF SYST-IDSEKVNR = 1                                               
259100* PRISHÖJNING                                                             
259200         IF WS-BELOPP > 0                                                 
259300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
259400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
259500           COMPUTE R3-LINE-AMOUNT-LC =                                    
259600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
259700           IF IN-EKH-KDVALISO = 'THB'                                     
259800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
259900           END-IF                                                         
260000           MOVE SPACES              TO R3-LINE-TRADING-PARTNER            
260100           MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                
260200           PERFORM S02-WRITE-W57041A                                      
260300         END-IF                                                           
260400       END-IF                                                             
260500                                                                          
260600       IF SYST-IDSEKVNR = 2                                               
260700* PRISSÄKNING                                                             
260800         IF WS-BELOPP < 0                                                 
260900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
261000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
261100           COMPUTE R3-LINE-AMOUNT-LC =                                    
261200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
261300           IF IN-EKH-KDVALISO = 'THB'                                     
261400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
261500           END-IF                                                         
261600           MOVE SPACE               TO WS-ALLOCATE-DC                     
261700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
261800           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
261900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
262000           MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                
262100           PERFORM S02-WRITE-W57041A                                      
262200         END-IF                                                           
262300       END-IF                                                             
262400                                                                          
262500       IF SYST-IDSEKVNR = 3                                               
262600* PRISSÄNKNING                                                            
262700         IF WS-BELOPP < 0                                                 
262800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
262900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
263000           COMPUTE R3-LINE-AMOUNT-LC =                                    
263100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
263200           IF IN-EKH-KDVALISO = 'THB'                                     
263300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
263400           END-IF                                                         
263500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
263600           MOVE 'TH  '              TO R3-LINE-TRADING-PARTNER            
263700           MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                
263800           PERFORM S02-WRITE-W57041A                                      
263900         END-IF                                                           
264000       END-IF                                                             
264100                                                                          
264200       IF SYST-IDSEKVNR = 4                                               
264300* PRISHÖJNING                                                             
264400         IF WS-BELOPP > 0                                                 
264500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
264600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
264700           COMPUTE R3-LINE-AMOUNT-LC =                                    
264800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
264900           IF IN-EKH-KDVALISO = 'THB'                                     
265000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
265100           END-IF                                                         
265200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
265300           MOVE SPACE               TO WS-ALLOCATE-DC                     
265400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
265500           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
265600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
265700           MOVE 'TH  '              TO R3-LINE-TRADING-PARTNER            
265800           MOVE 0000368288          TO R3-LINE-PA-CUSTOMER                
265900           PERFORM S02-WRITE-W57041A                                      
266000         END-IF                                                           
266100       END-IF                                                             
266200     END-EVALUATE                                                         
266300     .                                                                    
266400     EJECT                                                                
266500                                                                          
266600 CEL-MAIN-EVENT-402 SECTION.                                              
266700     EVALUATE IN-EKH-KDEKNIVA                                             
266800     WHEN 'DET'                                                           
266900       IF SYST-IDSEKVNR = 1                                               
267000         IF IN-EKH-KVANTAL > 0                                            
267100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
267200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
267300           COMPUTE R3-LINE-AMOUNT-LC =                                    
267400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
267500           IF IN-EKH-KDVALISO = 'THB'                                     
267600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
267700           END-IF                                                         
267800           MOVE SPACE               TO WS-ALLOCATE-DC                     
267900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
268000           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
268100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
268200           PERFORM S02-WRITE-W57041A                                      
268300         END-IF                                                           
268400       END-IF                                                             
268500                                                                          
268600       IF SYST-IDSEKVNR = 2                                               
268700         IF IN-EKH-KVANTAL < 0                                            
268800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
268900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
269000           COMPUTE R3-LINE-AMOUNT-LC =                                    
269100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
269200           IF IN-EKH-KDVALISO = 'THB'                                     
269300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
269400           END-IF                                                         
269500           MOVE SPACE               TO WS-ALLOCATE-DC                     
269600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
269700           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
269800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
269900           PERFORM S02-WRITE-W57041A                                      
270000         END-IF                                                           
270100       END-IF                                                             
270200     END-EVALUATE                                                         
270300     .                                                                    
270400     EJECT                                                                
270500                                                                          
270600 CEM-MAIN-EVENT-403 SECTION.                                              
270700     EVALUATE IN-EKH-KDEKSHT                                              
270800     WHEN '401'                                                           
270900     WHEN '402'                                                           
271000     WHEN '403'                                                           
271100     WHEN '404'                                                           
271200     WHEN '405'                                                           
271300     WHEN '407'                                                           
271400     WHEN '408'                                                           
271500     WHEN '409'                                                           
271600          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
271700     END-EVALUATE                                                         
271800     .                                                                    
271900     EJECT                                                                
272000                                                                          
272100 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
272200     EVALUATE IN-EKH-KDEKNIVA                                             
272300     WHEN 'DET'                                                           
272400       IF IN-EKH-KVANTAL > 0                                              
272500         IF SYST-IDSEKVNR = 1                                             
272600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
272700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
272800           COMPUTE R3-LINE-AMOUNT-LC =                                    
272900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
273000           IF IN-EKH-KDVALISO = 'THB'                                     
273100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
273200           END-IF                                                         
273300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
273400           PERFORM S02-WRITE-W57041A                                      
273500         END-IF                                                           
273600                                                                          
273700         IF SYST-IDSEKVNR = 4                                             
273800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
273900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
274000           COMPUTE R3-LINE-AMOUNT-LC =                                    
274100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
274200           IF IN-EKH-KDVALISO = 'THB'                                     
274300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
274400           END-IF                                                         
274500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
274600           MOVE SPACE               TO WS-ALLOCATE-DC                     
274700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
274800           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
274900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
275000           PERFORM S02-WRITE-W57041A                                      
275100         END-IF                                                           
275200       END-IF                                                             
275300                                                                          
275400       IF IN-EKH-KVANTAL < 0                                              
275500         IF SYST-IDSEKVNR = 2                                             
275600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
275700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
275800           COMPUTE R3-LINE-AMOUNT-LC =                                    
275900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
276000           IF IN-EKH-KDVALISO = 'THB'                                     
276100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
276200           END-IF                                                         
276300           MOVE SPACE               TO WS-ALLOCATE-DC                     
276400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
276500           MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                    
276600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
276700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
276800           PERFORM S02-WRITE-W57041A                                      
276900         END-IF                                                           
277000                                                                          
277100         IF SYST-IDSEKVNR = 3                                             
277200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
277300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
277400           COMPUTE R3-LINE-AMOUNT-LC =                                    
277500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
277600           IF IN-EKH-KDVALISO = 'THB'                                     
277700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
277800           END-IF                                                         
277900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
278000           PERFORM S02-WRITE-W57041A                                      
278100         END-IF                                                           
278200       END-IF                                                             
278300     END-EVALUATE                                                         
278400     .                                                                    
278500     EJECT                                                                
278600                                                                          
278700 CEN-MAIN-EVENT-404 SECTION.                                              
278800     EVALUATE IN-EKH-KDEKNIVA                                             
278900     WHEN 'DET'                                                           
279000       IF SYST-IDSEKVNR = 1                                               
279100* KONTO EJ MANUELLT REGISTRERAT                                           
279200         IF IN-EKH-IDKONTO = 0                                            
279300           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
279400           IF DIST18-SCRAP-NDC-SC                                         
279500           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
279600           OR DIST18-SCRAP-NDC-QUAL                                       
279700             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
279800             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
279900             COMPUTE R3-LINE-AMOUNT-LC =                                  
280000                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
280100             IF IN-EKH-KDVALISO = 'THB'                                   
280200               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
280300             END-IF                                                       
280400             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
280500             PERFORM S02-WRITE-W57041A                                    
280600           END-IF                                                         
280700         END-IF                                                           
280800       END-IF                                                             
280900                                                                          
281000       IF SYST-IDSEKVNR = 2                                               
281100* KONTO MANUELLT REGISTRERAT                                              
281200         IF IN-EKH-IDKONTO > 0                                            
281300           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
281400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
281500           COMPUTE R3-LINE-AMOUNT-LC =                                    
281600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
281700           IF IN-EKH-KDVALISO = 'THB'                                     
281800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
281900           END-IF                                                         
282000           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
282100           PERFORM S02-WRITE-W57041A                                      
282200         END-IF                                                           
282300       END-IF                                                             
282400                                                                          
282500       IF SYST-IDSEKVNR = 3                                               
282600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
282700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
282800         COMPUTE R3-LINE-AMOUNT-LC =                                      
282900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
283000         IF IN-EKH-KDVALISO = 'THB'                                       
283100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
283200         END-IF                                                           
283300         MOVE SPACE               TO WS-ALLOCATE-DC                       
283400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
283500         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
283600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
283700         PERFORM S02-WRITE-W57041A                                        
283800       END-IF                                                             
283900                                                                          
284000     END-EVALUATE                                                         
284100     .                                                                    
284200     EJECT                                                                
284300                                                                          
284400 CF-BUILD-COMMON-210-PART SECTION.                                        
284500     MOVE SPACE              TO R3-LINE-R3                                
284600     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
284700                                R3-LINE-DUE-DATE                          
284800                                R3-LINE-AMOUNT                            
284900                                R3-LINE-AMOUNT-LC                         
285000                                R3-LINE-TAX-AMOUNT                        
285100                                R3-LINE-TAX-AMOUNT-LC                     
285200                                R3-LINE-NUMBER-OF-DAYS                    
285300                                R3-LINE-QUANTITY                          
285400                                R3-LINE-SAMNR                             
285500     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
285600     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
285700     MOVE 'TH01'             TO R3-LINE-COMPANY-CODE                      
285800     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
285900     IF SYST-KDPOST = '31'                                                
286000       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
286100     ELSE                                                                 
286200       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
286300     END-IF                                                               
286400     .                                                                    
286500     EJECT                                                                
286600                                                                          
286700 CG-SCHEDULE-LINE-AP SECTION.                                             
286800     MOVE NEJ                     TO WS-HEADER-SW                         
286900     MOVE JA                      TO WS-LINE-SW                           
287000     EVALUATE IN-EKH-KDEKHHT                                              
287100     WHEN '102'                                                           
287200       IF IN-EKH-KDEKSHT = '130'                                          
287300       OR IN-EKH-KDEKSHT = '134'                                          
287400         IF IN-EKH-KDEKSHT = '130'                                        
287500           PERFORM CGA-MAIN-EVENT-102-130                                 
287600         ELSE                                                             
287700           PERFORM CGA-MAIN-EVENT-102-134                                 
287800         END-IF                                                           
287900       ELSE                                                               
288000         IF IN-EKH-KDEKSHT = '120'                                        
288100         OR IN-EKH-KDEKSHT = '124'                                        
288200         OR IN-EKH-KDEKSHT = '125'                                        
288300           IF IN-EKH-KDEKSHT = '125'                                      
288400             PERFORM CGA-MAIN-EVENT-102-125                               
288500           ELSE                                                           
288600             PERFORM CGA-MAIN-EVENT-102-12X                               
288700           END-IF                                                         
288800         ELSE                                                             
288900           PERFORM CGA-MAIN-EVENT-102                                     
289000         END-IF                                                           
289100       END-IF                                                             
289200     WHEN '103'                                                           
289300         PERFORM CGA-MAIN-EVENT-103                                       
289400     WHEN '303'                                                           
289500       IF IN-EKH-KDEKSHT = '371'                                          
289600         PERFORM S81-GET-CURRENCY-RATE                                    
289700         PERFORM CGA-MAIN-EVENT-303-371                                   
289800       ELSE                                                               
289900         IF IN-EKH-KDEKSHT = '3XX'                                        
290000           PERFORM S81-GET-CURRENCY-RATE                                  
290100           PERFORM CGA-MAIN-EVENT-303-3XX                                 
290200         ELSE                                                             
290300           PERFORM CGA-MAIN-EVENT-303                                     
290400         END-IF                                                           
290500       END-IF                                                             
290600     END-EVALUATE                                                         
290700     .                                                                    
290800     EJECT                                                                
290900                                                                          
291000 CGA-MAIN-EVENT-102     SECTION.                                          
291100     EVALUATE IN-EKH-KDEKNIVA                                             
291200     WHEN 'SUM'                                                           
291300       IF IN-EKH-SUBEL > ZERO                                             
291400         IF SYST-IDSEKVNR = 1                                             
291500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
291600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
291700            IN-EKH-SUBEL                                                  
291800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
291900           PERFORM S10-VATCODE                                            
292000           IF IN-EKH-SUVAT = ZERO                                         
292100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
292200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
292300           ELSE                                                           
292400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
292500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
292600           END-IF                                                         
292700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
292800                                                                          
292900           PERFORM S04-WRITE-W57043A                                      
293000         END-IF                                                           
293100       END-IF                                                             
293200                                                                          
293300       IF IN-EKH-SUBEL < ZERO                                             
293400         IF SYST-IDSEKVNR = 2                                             
293500           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
293600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
293700           IN-EKH-SUBEL                                                   
293800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
293900           PERFORM S10-VATCODE                                            
294000           IF IN-EKH-SUVAT = ZERO                                         
294100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
294200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
294300           ELSE                                                           
294400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
294500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
294600           END-IF                                                         
294700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
294800                                                                          
294900           PERFORM S04-WRITE-W57043A                                      
295000         END-IF                                                           
295100       END-IF                                                             
295200     END-EVALUATE                                                         
295300     .                                                                    
295400     EJECT                                                                
295500                                                                          
295600 CGA-MAIN-EVENT-102-12X SECTION.                                          
295700     EVALUATE IN-EKH-KDEKNIVA                                             
295800     WHEN 'SUM'                                                           
295900       IF IN-EKH-SUBEL > ZERO                                             
296000         IF SYST-IDSEKVNR = 1                                             
296100           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
296200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
296300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
296400                   R3-LINE-AMOUNT    / WS-PRKURS-TH  * -1                 
296500           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
296600           MOVE '  '               TO R3-LINE-TAX-CODE                    
296700           IF IN-EKH-SUVAT = ZERO                                         
296800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
296900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
297000           ELSE                                                           
297100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
297200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
297300                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TH  * -1           
297400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
297500           END-IF                                                         
297600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
297700                                                                          
297800           PERFORM S04-WRITE-W57043A                                      
297900         END-IF                                                           
298000       END-IF                                                             
298100                                                                          
298200       IF IN-EKH-SUBEL < ZERO                                             
298300         IF SYST-IDSEKVNR = 2                                             
298400           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
298500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
298600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
298700                   R3-LINE-AMOUNT    / WS-PRKURS-TH  * -1                 
298800           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
298900           MOVE '  '               TO R3-LINE-TAX-CODE                    
299000           IF IN-EKH-SUVAT = ZERO                                         
299100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
299200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
299300           ELSE                                                           
299400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
299500             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
299600                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TH  * -1           
299700             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
299800           END-IF                                                         
299900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
300000           MOVE SPACE           TO R3-LINE-COST-CENTER                    
300100                                                                          
300200           PERFORM S04-WRITE-W57043A                                      
300300         END-IF                                                           
300400       END-IF                                                             
300500     END-EVALUATE                                                         
300600     .                                                                    
300700     EJECT                                                                
300800                                                                          
300900                                                                          
301000 CGA-MAIN-EVENT-102-125 SECTION.                                          
301100     EVALUATE IN-EKH-KDEKNIVA                                             
301200     WHEN 'SUM'                                                           
301300       IF IN-EKH-SUBEL > ZERO                                             
301400         IF SYST-IDSEKVNR = 1                                             
301500           MOVE ZERO TO SPAR-SUMMA-102-125                                
301600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
301700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
301800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
301900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
302000                   R3-LINE-AMOUNT    / WS-PRKURS-TH  * -1                 
302100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
302200           MOVE '  '               TO R3-LINE-TAX-CODE                    
302300           IF IN-EKH-SUVAT = ZERO                                         
302400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
302500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
302600           ELSE                                                           
302700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
302800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
302900               R3-LINE-TAX-AMOUNT / WS-PRKURS-TH  * -1                    
303000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
303100           END-IF                                                         
303200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
303300           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
303400                                                                          
303500           PERFORM S04-WRITE-W57043A                                      
303600         END-IF                                                           
303700       END-IF                                                             
303800                                                                          
303900       IF IN-EKH-SUBEL < ZERO                                             
304000         IF SYST-IDSEKVNR = 2                                             
304100           MOVE ZERO TO SPAR-SUMMA-102-125                                
304200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
304300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
304400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
304500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
304600                   R3-LINE-AMOUNT    / WS-PRKURS-TH  * -1                 
304700           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
304800           MOVE '  '               TO R3-LINE-TAX-CODE                    
304900           IF IN-EKH-SUVAT = ZERO                                         
305000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
305100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
305200           ELSE                                                           
305300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
305400             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
305500               R3-LINE-TAX-AMOUNT / WS-PRKURS-TH * -1                     
305600             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
305700           END-IF                                                         
305800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
305900           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
306000                                                                          
306100           PERFORM S04-WRITE-W57043A                                      
306200         END-IF                                                           
306300       END-IF                                                             
306400     END-EVALUATE                                                         
306500     .                                                                    
306600     EJECT                                                                
306700 CGA-MAIN-EVENT-102-130 SECTION.                                          
306800     EVALUATE IN-EKH-KDEKNIVA                                             
306900     WHEN 'SUM'                                                           
307000       IF IN-EKH-SUBEL > ZERO                                             
307100         IF SYST-IDSEKVNR = 1                                             
307200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
307300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
307400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
307500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
307600                   R3-LINE-AMOUNT    / WS-PRKURS-TH  * -1                 
307700           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
307800           MOVE '  '     TO R3-LINE-TAX-CODE                              
307900           IF IN-EKH-SUVAT = ZERO                                         
308000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
308100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
308200           ELSE                                                           
308300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
308400             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
308500                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TH  * -1           
308600             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
308700           END-IF                                                         
308800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
308900                                                                          
309000           PERFORM S04-WRITE-W57043A                                      
309100         END-IF                                                           
309200       END-IF                                                             
309300                                                                          
309400       IF IN-EKH-SUBEL < ZERO                                             
309500         IF SYST-IDSEKVNR = 2                                             
309600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
309700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
309800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
309900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
310000                   R3-LINE-AMOUNT    / WS-PRKURS-TH                       
310100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
310200           MOVE '  '     TO R3-LINE-TAX-CODE                              
310300           IF IN-EKH-SUVAT = ZERO                                         
310400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
310500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
310600           ELSE                                                           
310700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
310800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
310900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TH                 
311000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
311100           END-IF                                                         
311200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
311300                                                                          
311400           PERFORM S04-WRITE-W57043A                                      
311500         END-IF                                                           
311600       END-IF                                                             
311700     END-EVALUATE                                                         
311800     .                                                                    
311900     EJECT                                                                
312000                                                                          
312100 CGA-MAIN-EVENT-102-134 SECTION.                                          
312200     EVALUATE IN-EKH-KDEKNIVA                                             
312300     WHEN 'SUM'                                                           
312400       IF IN-EKH-SUBEL > ZERO                                             
312500         IF SYST-IDSEKVNR = 1                                             
312600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
312700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
312800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
312900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
313000                   R3-LINE-AMOUNT    / WS-PRKURS-TH  * -1                 
313100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
313200           MOVE '  '     TO R3-LINE-TAX-CODE                              
313300           IF IN-EKH-SUVAT = ZERO                                         
313400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
313500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
313600           ELSE                                                           
313700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
313800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
313900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TH  * -1           
314000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
314100           END-IF                                                         
314200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
314300                                                                          
314400           PERFORM S04-WRITE-W57043A                                      
314500         END-IF                                                           
314600       END-IF                                                             
314700                                                                          
314800       IF IN-EKH-SUBEL < ZERO                                             
314900         IF SYST-IDSEKVNR = 2                                             
315000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
315100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
315200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
315300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
315400                   R3-LINE-AMOUNT    / WS-PRKURS-TH                       
315500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
315600           MOVE '  '     TO R3-LINE-TAX-CODE                              
315700           IF IN-EKH-SUVAT = ZERO                                         
315800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
315900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
316000           ELSE                                                           
316100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
316200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
316300                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-TH                 
316400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
316500           END-IF                                                         
316600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
316700                                                                          
316800           PERFORM S04-WRITE-W57043A                                      
316900         END-IF                                                           
317000       END-IF                                                             
317100     END-EVALUATE                                                         
317200     .                                                                    
317300     EJECT                                                                
317400                                                                          
317500 CGA-MAIN-EVENT-103     SECTION.                                          
317600     EVALUATE IN-EKH-KDEKNIVA                                             
317700     WHEN 'SUM'                                                           
317800       IF IN-EKH-SUBEL > ZERO                                             
317900         IF SYST-IDSEKVNR = 1                                             
318000           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
318100           PERFORM S10-VATCODE                                            
318200           IF IN-EKH-KDEKSHT = '102'                                      
318300             MOVE '12'             TO R3-LINE-TAX-CODE                    
318400             COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.07           
318500           END-IF                                                         
318600           IF IN-EKH-SUVAT = ZERO                                         
318700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
318800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
318900           ELSE                                                           
319000             IF IN-EKH-KDVALISO = 'THB'                                   
319100               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
319200                                      R3-LINE-TAX-AMOUNT-LC               
319300             ELSE                                                         
319400               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
319500                                      R3-LINE-TAX-AMOUNT-LC               
319600               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
319700                       R3-LINE-TAX-AMOUNT * WS-PRKURS                     
319800             END-IF                                                       
319900           END-IF                                                         
320000**** CALCULATE NEW SUM WITH VAT                                           
320100           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
320200                   IN-EKH-SUVAT                                           
320300           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
320400           IF IN-EKH-KDVALISO = 'THB'                                     
320500             MOVE R3-LINE-AMOUNT TO R3-LINE-AMOUNT-LC                     
320600           ELSE                                                           
320700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
320800                     R3-LINE-AMOUNT * WS-PRKURS                           
320900           END-IF                                                         
321000           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
321100                                                                          
321200           PERFORM S04-WRITE-W57043A                                      
321300         END-IF                                                           
321400       END-IF                                                             
321500                                                                          
321600       IF IN-EKH-SUBEL < ZERO                                             
321700         IF SYST-IDSEKVNR = 2                                             
321800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
321900           PERFORM S10-VATCODE                                            
322000           IF IN-EKH-KDEKSHT = '102'                                      
322100             MOVE '12'             TO R3-LINE-TAX-CODE                    
322200             COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.07           
322300           END-IF                                                         
322400           IF IN-EKH-SUVAT = ZERO                                         
322500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
322600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
322700           ELSE                                                           
322800             IF IN-EKH-KDVALISO = 'THB'                                   
322900               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
323000                                      R3-LINE-TAX-AMOUNT-LC               
323100             ELSE                                                         
323200               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
323300                                      R3-LINE-TAX-AMOUNT-LC               
323400               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
323500                       R3-LINE-TAX-AMOUNT * WS-PRKURS                     
323600             END-IF                                                       
323700           END-IF                                                         
323800           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
323900                   IN-EKH-SUVAT                                           
324000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
324100           IF IN-EKH-KDVALISO = 'THB'                                     
324200             MOVE R3-LINE-AMOUNT TO R3-LINE-AMOUNT-LC                     
324300           ELSE                                                           
324400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
324500                     R3-LINE-AMOUNT * WS-PRKURS                           
324600           END-IF                                                         
324700           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
324800                                                                          
324900           PERFORM S04-WRITE-W57043A                                      
325000         END-IF                                                           
325100       END-IF                                                             
325200     END-EVALUATE                                                         
325300     .                                                                    
325400     EJECT                                                                
325500                                                                          
325600 CGA-MAIN-EVENT-303 SECTION.                                              
325700     EVALUATE IN-EKH-KDEKNIVA                                             
325800     WHEN 'SUM'                                                           
325900       IF SYST-IDSEKVNR = 1                                               
326000         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
326100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
326200          IN-EKH-SUBEL / WS-PRKURS-TH                                     
326300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
326400         MOVE '  '     TO R3-LINE-TAX-CODE                                
326500         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
326600         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
326700                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-TH                     
326800         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
326900                                                                          
327000         PERFORM S04-WRITE-W57043A                                        
327100       END-IF                                                             
327200     END-EVALUATE                                                         
327300     .                                                                    
327400     EJECT                                                                
327500                                                                          
327600 CGA-MAIN-EVENT-303-3XX SECTION.                                          
327700     EVALUATE IN-EKH-KDEKNIVA                                             
327800     WHEN 'SUM'                                                           
327900       IF SYST-IDSEKVNR = 1                                               
328000         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
328100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
328200          IN-EKH-SUBEL / WS-PRKURS-TH3                                    
328300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
328400         MOVE '  '     TO R3-LINE-TAX-CODE                                
328500         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
328600         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
328700                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-TH3                    
328800         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
328900                                                                          
329000         PERFORM S04-WRITE-W57043A                                        
329100       END-IF                                                             
329200     END-EVALUATE                                                         
329300     .                                                                    
329400     EJECT                                                                
329500                                                                          
329600 CH-BUILD-COMMON-310-PART SECTION.                                        
329700     MOVE SPACE              TO R3-LINE-R3                                
329800     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
329900                                R3-LINE-DUE-DATE                          
330000                                R3-LINE-AMOUNT                            
330100                                R3-LINE-AMOUNT-LC                         
330200                                R3-LINE-TAX-AMOUNT                        
330300                                R3-LINE-TAX-AMOUNT-LC                     
330400                                R3-LINE-NUMBER-OF-DAYS                    
330500                                R3-LINE-QUANTITY                          
330600                                R3-LINE-SAMNR                             
330700     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
330800     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
330900     MOVE 'TH01'             TO R3-LINE-COMPANY-CODE                      
331000     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
331100     IF SYST-KDPOST = '31'                                                
331200       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
331300     ELSE                                                                 
331400       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
331500     END-IF                                                               
331600     .                                                                    
331700     EJECT                                                                
331800                                                                          
331900 CI-SCHEDULE-LINE-AR SECTION.                                             
332000     MOVE NEJ                     TO WS-HEADER-SW                         
332100     MOVE JA                      TO WS-LINE-SW                           
332200     EVALUATE IN-EKH-KDEKHHT                                              
332300     WHEN '204'                                                           
332400         PERFORM CIA-MAIN-EVENT-204                                       
332500     END-EVALUATE                                                         
332600     .                                                                    
332700     EJECT                                                                
332800                                                                          
332900 CIA-MAIN-EVENT-204     SECTION.                                          
333000     EVALUATE IN-EKH-KDEKNIVA                                             
333100     WHEN 'SUM'                                                           
333200       IF IN-EKH-SUBEL > ZERO                                             
333300         IF SYST-IDSEKVNR = 1                                             
333400           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
333500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
333600           IF IN-EKH-KDVALISO = 'THB'                                     
333700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
333800           END-IF                                                         
333900           PERFORM S10-VATCODE                                            
334000           IF IN-EKH-SUVAT = ZERO                                         
334100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
334200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
334300           ELSE                                                           
334400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
334500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
334600           END-IF                                                         
334700           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
334800                                                                          
334900           PERFORM S04-WRITE-W57043A                                      
335000         END-IF                                                           
335100       END-IF                                                             
335200                                                                          
335300     END-EVALUATE                                                         
335400     .                                                                    
335500     EJECT                                                                
335600                                                                          
335700 CEQ-MAIN-EVENT-501 SECTION.                                              
335800     EVALUATE IN-EKH-KDEKNIVA                                             
335900     WHEN 'DET'                                                           
336000       IF SYST-IDSEKVNR = 1                                               
336100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
336200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
336300         COMPUTE R3-LINE-AMOUNT-LC =                                      
336400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
336500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
336600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
336700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
336800         MOVE SPACE               TO WS-ALLOCATE-REF                      
336900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
337000         PERFORM S04-WRITE-W57043A                                        
337100       END-IF                                                             
337200                                                                          
337300       IF SYST-IDSEKVNR = 2                                               
337400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
337500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
337600         COMPUTE R3-LINE-AMOUNT-LC =                                      
337700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
337800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
337900         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
338000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
338100         MOVE SPACE               TO WS-ALLOCATE-REF                      
338200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
338300         PERFORM S04-WRITE-W57043A                                        
338400       END-IF                                                             
338500     END-EVALUATE                                                         
338600     .                                                                    
338700     EJECT                                                                
338800                                                                          
338900 CER-MAIN-EVENT-502 SECTION.                                              
339000     EVALUATE IN-EKH-KDEKSHT                                              
339100     WHEN '501'                                                           
339200          PERFORM CERA-SUB-EVENT-502-501                                  
339300     WHEN '502'                                                           
339400          PERFORM CERB-SUB-EVENT-502-502                                  
339500     WHEN '503'                                                           
339600          PERFORM CERB-SUB-EVENT-502-503                                  
339700     END-EVALUATE                                                         
339800     .                                                                    
339900     EJECT                                                                
340000                                                                          
340100 CERA-SUB-EVENT-502-501 SECTION.                                          
340200     EVALUATE IN-EKH-KDEKNIVA                                             
340300     WHEN 'DET'                                                           
340400       IF SYST-IDSEKVNR = 1                                               
340500* HÄR BOKAS FÖRLORAT KOLLI                                                
340600         IF IN-EKH-KVANTAL < 0                                            
340700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
340800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
340900           COMPUTE R3-LINE-AMOUNT-LC =                                    
341000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
341100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
341200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
341300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
341400           MOVE SPACE               TO WS-ALLOCATE-REF                    
341500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
341600           PERFORM S02-WRITE-W57041A                                      
341700         END-IF                                                           
341800       END-IF                                                             
341900                                                                          
342000       IF SYST-IDSEKVNR = 2                                               
342100* HÄR BOKAS FÖRLORAT KOLLI                                                
342200         IF IN-EKH-KVANTAL < 0                                            
342300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
342400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
342500           COMPUTE R3-LINE-AMOUNT-LC =                                    
342600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
342700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
342800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
342900           PERFORM S02-WRITE-W57041A                                      
343000         END-IF                                                           
343100       END-IF                                                             
343200                                                                          
343300       IF SYST-IDSEKVNR = 3                                               
343400* HÄR BOKAS ÅTERFUNNET KOLLI                                              
343500         IF IN-EKH-KVANTAL > 0                                            
343600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
343700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
343800           COMPUTE R3-LINE-AMOUNT-LC =                                    
343900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
344000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
344100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
344200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
344300           MOVE SPACE               TO WS-ALLOCATE-REF                    
344400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
344500           PERFORM S02-WRITE-W57041A                                      
344600         END-IF                                                           
344700       END-IF                                                             
344800                                                                          
344900       IF SYST-IDSEKVNR = 4                                               
345000* HÄR BOKAS ÅTERFUNNET KOLLI                                              
345100         IF IN-EKH-KVANTAL > 0                                            
345200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
345300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
345400           COMPUTE R3-LINE-AMOUNT-LC =                                    
345500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
345600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
345700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
345800           PERFORM S02-WRITE-W57041A                                      
345900         END-IF                                                           
346000       END-IF                                                             
346100     END-EVALUATE                                                         
346200     .                                                                    
346300     EJECT                                                                
346400                                                                          
346500 CERB-SUB-EVENT-502-502     SECTION.                                      
346600     EVALUATE IN-EKH-KDEKNIVA                                             
346700     WHEN 'DET'                                                           
346800       IF SYST-IDSEKVNR = 1                                               
346900* HÄR BOKAS ÖVERLEVERNAS                                                  
347000         IF IN-EKH-KVANTAL > 0                                            
347100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
347200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
347300           COMPUTE R3-LINE-AMOUNT-LC =                                    
347400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
347500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
347600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
347700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
347800           MOVE SPACE               TO WS-ALLOCATE-REF                    
347900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
348000           PERFORM S02-WRITE-W57041A                                      
348100         END-IF                                                           
348200       END-IF                                                             
348300                                                                          
348400       IF SYST-IDSEKVNR = 2                                               
348500* HÄR BOKAS ÖVERLEVERNAS                                                  
348600         IF IN-EKH-KVANTAL > 0                                            
348700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
348800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
348900           COMPUTE R3-LINE-AMOUNT-LC =                                    
349000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
349100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
349200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
349300           MOVE SPACE               TO WS-ALLOCATE-REF                    
349400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
349500           PERFORM S02-WRITE-W57041A                                      
349600         END-IF                                                           
349700       END-IF                                                             
349800     END-EVALUATE                                                         
349900     .                                                                    
350000     EJECT                                                                
350100                                                                          
350200 CERB-SUB-EVENT-502-503     SECTION.                                      
350300     EVALUATE IN-EKH-KDEKNIVA                                             
350400     WHEN 'DET'                                                           
350500       IF SYST-IDSEKVNR = 1                                               
350600* HÄR BOKAS UNDERLEVERNAS                                                 
350700         IF IN-EKH-KVANTAL < 0                                            
350800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
350900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
351000           COMPUTE R3-LINE-AMOUNT-LC =                                    
351100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
351200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
351300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
351400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
351500           MOVE SPACE               TO WS-ALLOCATE-REF                    
351600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
351700           PERFORM S02-WRITE-W57041A                                      
351800         END-IF                                                           
351900       END-IF                                                             
352000                                                                          
352100       IF SYST-IDSEKVNR = 2                                               
352200* HÄR BOKAS UNDERLEVERNAS                                                 
352300         IF IN-EKH-KVANTAL < 0                                            
352400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
352500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
352600           COMPUTE R3-LINE-AMOUNT-LC =                                    
352700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
352800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
352900           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
353000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
353100           MOVE SPACE               TO WS-ALLOCATE-REF                    
353200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
353300           PERFORM S02-WRITE-W57041A                                      
353400         END-IF                                                           
353500       END-IF                                                             
353600     END-EVALUATE                                                         
353700     .                                                                    
353800     EJECT                                                                
353900                                                                          
354000 CES-MAIN-EVENT-503 SECTION.                                              
354100     EVALUATE IN-EKH-KDEKSHT                                              
354200     WHEN '501'                                                           
354300          PERFORM CESA-SUB-EVENT-503-501                                  
354400     WHEN '502'                                                           
354500          PERFORM CESB-SUB-EVENT-503-502                                  
354600     WHEN '503'                                                           
354700          PERFORM CESB-SUB-EVENT-503-503                                  
354800     WHEN '504'                                                           
354900          PERFORM CESC-SUB-EVENT-503-504                                  
355000     END-EVALUATE                                                         
355100     .                                                                    
355200     EJECT                                                                
355300                                                                          
355400 CESA-SUB-EVENT-503-501 SECTION.                                          
355500     EVALUATE IN-EKH-KDEKNIVA                                             
355600     WHEN 'DET'                                                           
355700       IF SYST-IDSEKVNR = 1                                               
355800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
355900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
356000         COMPUTE R3-LINE-AMOUNT-LC =                                      
356100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
356200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
356300         PERFORM S02-WRITE-W57041A                                        
356400       END-IF                                                             
356500                                                                          
356600       IF SYST-IDSEKVNR = 2                                               
356700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
356800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
356900         COMPUTE R3-LINE-AMOUNT-LC =                                      
357000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
357100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
357200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
357300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
357400         MOVE SPACE               TO WS-ALLOCATE-REF                      
357500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
357600         PERFORM S02-WRITE-W57041A                                        
357700       END-IF                                                             
357800     END-EVALUATE                                                         
357900     .                                                                    
358000     EJECT                                                                
358100                                                                          
358200 CESB-SUB-EVENT-503-502     SECTION.                                      
358300     EVALUATE IN-EKH-KDEKNIVA                                             
358400     WHEN 'DET'                                                           
358500       IF SYST-IDSEKVNR = 1                                               
358600* HÄR BOKAS ÖVERLEVERANS                                                  
358700         IF IN-EKH-KVANTAL > 0                                            
358800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
358900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
359000           COMPUTE R3-LINE-AMOUNT-LC =                                    
359100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
359200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
359300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
359400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
359500           MOVE SPACE               TO WS-ALLOCATE-REF                    
359600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
359700           PERFORM S02-WRITE-W57041A                                      
359800         END-IF                                                           
359900       END-IF                                                             
360000                                                                          
360100       IF SYST-IDSEKVNR = 2                                               
360200* HÄR BOKAS ÖVERLEVERANS                                                  
360300         IF IN-EKH-KVANTAL > 0                                            
360400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
360500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
360600           COMPUTE R3-LINE-AMOUNT-LC =                                    
360700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
360800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
360900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
361000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
361100           MOVE SPACE               TO WS-ALLOCATE-REF                    
361200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
361300           PERFORM S02-WRITE-W57041A                                      
361400         END-IF                                                           
361500       END-IF                                                             
361600     END-EVALUATE                                                         
361700     .                                                                    
361800     EJECT                                                                
361900                                                                          
362000 CESB-SUB-EVENT-503-503     SECTION.                                      
362100     EVALUATE IN-EKH-KDEKNIVA                                             
362200     WHEN 'DET'                                                           
362300       IF SYST-IDSEKVNR = 1                                               
362400* HÄR BOKAS UNDERLEVERANS                                                 
362500         IF IN-EKH-KVANTAL < 0                                            
362600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
362700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
362800           COMPUTE R3-LINE-AMOUNT-LC =                                    
362900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
363000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
363100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
363200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
363300           MOVE SPACE               TO WS-ALLOCATE-REF                    
363400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
363500           PERFORM S02-WRITE-W57041A                                      
363600         END-IF                                                           
363700       END-IF                                                             
363800                                                                          
363900       IF SYST-IDSEKVNR = 2                                               
364000* HÄR BOKAS UNDERLEVERANS                                                 
364100         IF IN-EKH-KVANTAL < 0                                            
364200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
364300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
364400           COMPUTE R3-LINE-AMOUNT-LC =                                    
364500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
364600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
364700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
364800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
364900           MOVE SPACE               TO WS-ALLOCATE-REF                    
365000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
365100           PERFORM S02-WRITE-W57041A                                      
365200         END-IF                                                           
365300       END-IF                                                             
365400     END-EVALUATE                                                         
365500     .                                                                    
365600     EJECT                                                                
365700                                                                          
365800 CESC-SUB-EVENT-503-504 SECTION.                                          
365900     EVALUATE IN-EKH-KDEKNIVA                                             
366000     WHEN 'DET'                                                           
366100       IF SYST-IDSEKVNR = 1                                               
366200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
366300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
366400         COMPUTE R3-LINE-AMOUNT-LC =                                      
366500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
366600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
366700         PERFORM S02-WRITE-W57041A                                        
366800       END-IF                                                             
366900                                                                          
367000       IF SYST-IDSEKVNR = 2                                               
367100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
367200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
367300         COMPUTE R3-LINE-AMOUNT-LC =                                      
367400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
367500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
367600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
367700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
367800         MOVE SPACE               TO WS-ALLOCATE-REF                      
367900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
368000         PERFORM S02-WRITE-W57041A                                        
368100       END-IF                                                             
368200     END-EVALUATE                                                         
368300     .                                                                    
368400     EJECT                                                                
368500 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
368600     MOVE ZERO             TO LOGG-W57073                                 
368700     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
368800     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
368900     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
369000     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
369100     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
369200     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
369300     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
369400     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
369500     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
369600     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
369700     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
369800     MOVE 'TH01'           TO LOGG-KDTRADP                                
369900                                                                          
370000****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
370100     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
370200     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
370300     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
370400     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
370500     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
370600     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
370700     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
370800     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
370900     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
371000     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
371100     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
371200     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
371300     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
371400     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
371500     .                                                                    
371600     EJECT                                                                
371700                                                                          
371800 Z-FINI SECTION.                                                          
371900     CLOSE W57066                                                         
372000           W57048                                                         
372100           W57041A                                                        
372200           W57042A                                                        
372300           W57043A                                                        
372400           W57043                                                         
372500           W5704N                                                         
372600           W51340                                                         
372700                                                                          
372800     MOVE 'S' TO POSTSUM-OPKOD                                            
372900     CALL POSTSUM USING POSTSUM-PARM                                      
373000     .                                                                    
373100     EJECT                                                                
373200                                                                          
373300 S01-READ-W57066  SECTION.                                                
373400     READ W57066 INTO IN-AREA                                             
373500     AT END                                                               
373600        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
373700        SET END-OF-W57066 TO TRUE                                         
373800                                                                          
373900     NOT AT END                                                           
374000        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
374100        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
374200        MOVE 'W57048D1'   TO POSTSUM-DDNAMN2                              
374300        CALL POSTSUM USING POSTSUM-PARM                                   
374400     END-READ                                                             
374500     .                                                                    
374600                                                                          
374700 S02-WRITE-W57041A SECTION.                                               
374800     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
374900     MOVE SPACE                 TO 71LINE-POST                            
375000     IF WS-LINE-SW = JA                                                   
375100       IF IN-EKH-KDSORT = 'SW'                                            
375200         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
375300         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
375400         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
375500       ELSE                                                               
375600         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
375700         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
375800         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
375900       END-IF                                                             
376000       WRITE 71LINE-POST        FROM R3-LINE-R3                           
376100       PERFORM S20-CREATE-WRITE-LOG                                       
376200     ELSE                                                                 
376300       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
376400     END-IF                                                               
376500                                                                          
376600     IF WS-LINE-SW = JA                                                   
376700       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
376800     ELSE                                                                 
376900       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
377000     END-IF                                                               
377100     MOVE 'W57041A'             TO POSTSUM-FDNAMN                         
377200     MOVE 'W57048D2'            TO POSTSUM-DDNAMN2                        
377300     CALL POSTSUM USING POSTSUM-PARM                                      
377400     .                                                                    
377500                                                                          
377600 S002-WRITE-W57041A-HEAD SECTION.                                         
377700     MOVE SPACE                 TO 71LINE-POST                            
377800     IF IN-EKH-KDSORT = 'SW'                                              
377900       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
378000       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
378100       MOVE WS-TEXT           TO R3-LINE-TEXT                             
378200     ELSE                                                                 
378300       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
378400       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
378500       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
378600     END-IF                                                               
378700     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
378800                                                                          
378900     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
379000     MOVE 'W57041A'             TO POSTSUM-FDNAMN                         
379100     MOVE 'W57048D2'            TO POSTSUM-DDNAMN2                        
379200     CALL POSTSUM USING POSTSUM-PARM                                      
379300     .                                                                    
379400                                                                          
379500 S03-WRITE-W57022 SECTION.                                                
379600     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
379700     IF IN-EKH-KDSORT = 'SW'                                              
379800       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
379900       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
380000       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
380100     ELSE                                                                 
380200       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
380300       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
380400       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
380500     END-IF                                                               
380600     WRITE 72LINE-POST        FROM R3-LINE-R3                             
380700                                                                          
380800     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
380900     MOVE 'W57042A'           TO POSTSUM-FDNAMN                           
381000     MOVE 'W57048D3'          TO POSTSUM-DDNAMN2                          
381100     CALL POSTSUM USING POSTSUM-PARM                                      
381200                                                                          
381300     PERFORM S20-CREATE-WRITE-LOG                                         
381400     .                                                                    
381500                                                                          
381600 S04-WRITE-W57043A SECTION.                                               
381700     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
381800     MOVE SPACE                 TO 73LINE-POST                            
381900     IF WS-LINE-SW = JA                                                   
382000       IF IN-EKH-KDSORT = 'SW'                                            
382100         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
382200         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
382300         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
382400       ELSE                                                               
382500         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
382600         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
382700         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
382800       END-IF                                                             
382900       WRITE 73LINE-POST        FROM R3-LINE-R3                           
383000       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
383100     ELSE                                                                 
383200       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
383300       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
383400     END-IF                                                               
383500                                                                          
383600     MOVE 'W57043A'             TO POSTSUM-FDNAMN                         
383700     MOVE 'W57048D4'            TO POSTSUM-DDNAMN2                        
383800     CALL POSTSUM USING POSTSUM-PARM                                      
383900                                                                          
384000     IF WS-LINE-SW = JA                                                   
384100       PERFORM S20-CREATE-WRITE-LOG                                       
384200     END-IF                                                               
384300     .                                                                    
384400                                                                          
384500 S004-WRITE-W57043A-HEAD SECTION.                                         
384600     MOVE SPACE                 TO 73LINE-POST                            
384700     IF IN-EKH-KDSORT = 'SW'                                              
384800       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
384900       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
385000       MOVE WS-TEXT           TO R3-LINE-TEXT                             
385100     ELSE                                                                 
385200       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
385300       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
385400       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
385500     END-IF                                                               
385600     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
385700                                                                          
385800     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
385900     MOVE 'W57043A'             TO POSTSUM-FDNAMN                         
386000     MOVE 'W57048D4'            TO POSTSUM-DDNAMN2                        
386100     CALL POSTSUM USING POSTSUM-PARM                                      
386200     .                                                                    
386300                                                                          
386400 S10-VATCODE SECTION.                                                     
386500     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
386600     PERFORM IMS-GU-WDB601                                                
386700     IF DCS-KDDC = SPACE                                                  
386800       MOVE NEJ              TO WDB6-A-SW                                 
386900     ELSE                                                                 
387000       MOVE JA               TO WDB6-A-SW                                 
387100     END-IF                                                               
387200                                                                          
387300     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
387400     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
387500     IF IN-EKH-SUVAT = ZERO                                               
387600       MOVE '  '     TO R3-LINE-TAX-CODE                                  
387700     END-IF                                                               
387800     IF IN-EKH-BEVAT = 'XX'                                               
387900       MOVE '  '     TO R3-LINE-TAX-CODE                                  
388000     END-IF                                                               
388100     .                                                                    
388200     EJECT                                                                
388300                                                                          
388400 S20-CREATE-WRITE-LOG SECTION.                                            
388500     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
388600     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
388700     IF SYST-IDPTYP = '610'                                               
388800       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
388900     ELSE                                                                 
389000       MOVE ZERO                      TO WS-IDLEVNR                       
389100       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
389200                          FOR CHARACTERS BEFORE INITIAL SPACE             
389300       IF WS-IDLEVNR   > ZERO                                             
389400          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
389500                                      TO LOGG-IDKONTO                     
389600       END-IF                                                             
389700     END-IF                                                               
389800     IF R3-LINE-COST-CENTER NOT = SPACE                                   
389900       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
390000     END-IF                                                               
390100     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
390200     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
390300     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
390400     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
390500     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
390600                                                                          
390700     PERFORM S21-WRITE-W57043                                             
390800     PERFORM S22-WRITE-W57048                                             
390900                                                                          
391000     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
391100       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
391200       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
391300       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
391400                                                                          
391500       PERFORM S21-WRITE-W57043                                           
391600     END-IF                                                               
391700     .                                                                    
391800     EJECT                                                                
391900                                                                          
392000 S21-WRITE-W57043 SECTION.                                                
392100     IF DCS-IDDC NOT = LOGG-IDDC                                          
392200        MOVE LOGG-IDDC TO W-IDDC-B6                                       
392300        PERFORM IMS-GU-WDB601                                             
392400     END-IF                                                               
392500     IF DCS-KDDC = SPACE                                                  
392600       MOVE NEJ              TO WDB6-A-SW                                 
392700     ELSE                                                                 
392800       MOVE JA               TO WDB6-A-SW                                 
392900     END-IF                                                               
393000                                                                          
393100     IF  WDB6-A-FINNS                                                     
393200     AND DCS-DDC                                                          
393300       MOVE 'N'       TO LOGG-FLLSBOK                                     
393400     END-IF                                                               
393500     WRITE LOGG-POST FROM LOGG-W57073                                     
393600                                                                          
393700     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
393800     MOVE 'W57043'    TO POSTSUM-FDNAMN                                   
393900     MOVE 'W57048D5'  TO POSTSUM-DDNAMN2                                  
394000     CALL POSTSUM USING POSTSUM-PARM                                      
394100     .                                                                    
394200                                                                          
394300 S22-WRITE-W57048 SECTION.                                                
394400     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
394500     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
394600     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
394700                                                                          
394800     IF R3-LINE-AMOUNT-SIGN = '+'                                         
394900       IF AVST-SUBEL < +0                                                 
395000         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
395100       END-IF                                                             
395200       IF AVST-KVANTAL < +0                                               
395300         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
395400       END-IF                                                             
395500     ELSE                                                                 
395600       IF AVST-SUBEL > +0                                                 
395700         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
395800       END-IF                                                             
395900       IF AVST-KVANTAL > +0                                               
396000         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
396100       END-IF                                                             
396200     END-IF                                                               
396300                                                                          
396400     IF DCS-IDDC NOT = AVST-IDDC                                          
396500        MOVE AVST-IDDC  TO W-IDDC-B6                                      
396600        PERFORM IMS-GU-WDB601                                             
396700     END-IF                                                               
396800     IF DCS-KDDC = SPACE                                                  
396900       MOVE NEJ              TO WDB6-A-SW                                 
397000     ELSE                                                                 
397100       MOVE JA               TO WDB6-A-SW                                 
397200     END-IF                                                               
397300                                                                          
397400     IF  WDB6-A-FINNS                                                     
397500     AND DCS-DDC                                                          
397600       MOVE 'N'                 TO AVST-FLLSBOK                           
397700     END-IF                                                               
397800                                                                          
397900     IF AVST-IDKONTO(1:4) = '1454'                                        
398000       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
398100       WRITE AVST-POST FROM AVST-W57070                                   
398200                                                                          
398300       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
398400       MOVE 'W57048'            TO POSTSUM-FDNAMN                         
398500       MOVE 'W57048D6'          TO POSTSUM-DDNAMN2                        
398600       CALL POSTSUM USING POSTSUM-PARM                                    
398700     END-IF                                                               
398800     .                                                                    
398900     EJECT                                                                
399000                                                                          
399100 S30-READ-DATABASE-B2-B1 SECTION.                                         
399200     IF IN-EKH-IDLEVNR = '1441'                                           
399300       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
399400     ELSE                                                                 
399500       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
399600       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
399700       PERFORM IMS-GU-WDB201                                              
399800       IF SEGMENT-SAKNAS                                                  
399900         MOVE 'TH99999'       TO W-WDB1-IDPARTNR                          
400000       ELSE                                                               
400100         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
400200       END-IF                                                             
400300     END-IF                                                               
400400     MOVE WC-IDFTG-TH       TO W-WDB1-IDFTG                               
400500     PERFORM IMS-GU-WDB101                                                
400600     IF SEGMENT-SAKNAS                                                    
400700       DISPLAY 'BETALARUPPG. SAKNAS '                                     
400800       DISPLAY IN-EKH-IDVERGL                                             
400900       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
401000       DISPLAY GMT-IDPARTNR                                               
401100                                                                          
401200       MOVE SPACE         TO BET-KDTRADP                                  
401300       MOVE ZERO          TO BET-IDPARTNR                                 
401400       MOVE '????'        TO WS-KDBETVIL                                  
401500       MOVE '???'         TO WS-KDVALISO-WDB1                             
401600     ELSE                                                                 
401700       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
401800     END-IF                                                               
401900     MOVE 'THB'           TO WS-KDVALISO-WDB1                             
402000                                                                          
402100     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
402200     MOVE ZERO TO TALLY                                                   
402300     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
402400                 FOR CHARACTERS BEFORE INITIAL SPACE                      
402500     IF TALLY = ZERO                                                      
402600       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
402700     ELSE                                                                 
402800       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
402900                                TO W-BET-IDPARTNR-NUM                     
403000     END-IF                                                               
403100     .                                                                    
403200     EJECT                                                                
403300                                                                          
403400 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
403500     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
403600     PERFORM IMS-GU-WDB601                                                
403700     IF DCS-KDDC = SPACE                                                  
403800       MOVE NEJ              TO WDB6-A-SW                                 
403900     ELSE                                                                 
404000       MOVE JA               TO WDB6-A-SW                                 
404100     END-IF                                                               
404200                                                                          
404300     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
404400       IF IN-EKH-KDEKSHT NOT = '406'                                      
404500         IF IN-EKH-FLDCET = NEJ                                           
404600           PERFORM S42-SKAPA-RW2-INV-POSTER                               
404700         END-IF                                                           
404800       END-IF                                                             
404900     END-IF                                                               
405000                                                                          
405100     IF IN-EKH-KDEKNIVA = 'DET'                                           
405200       IF  IN-EKH-KDEKHHT = '204'                                         
405300       AND (IN-EKH-KDEKSHT = '201')                                       
405400         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
405500       END-IF                                                             
405600                                                                          
405700       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
405800       AND (WDB6-A-FINNS                                                  
405900       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
406000       OR   DCS-DDC OR DCS-NDC-PF))                                       
406100         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
406200       END-IF                                                             
406300                                                                          
406400       IF IN-FIL-IDPGM = 'W4183000'                                       
406500       AND (WDB6-A-FINNS                                                  
406600       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
406700       OR   DCS-DDC OR DCS-NDC-PF))                                       
406800         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
406900       END-IF                                                             
407000     END-IF                                                               
407100     .                                                                    
407200     EJECT                                                                
407300                                                                          
407400 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
407500     MOVE 'RW2'              TO RW2-IDPTYP                                
407600     MOVE 'RW2'              TO WS-IDPTYP                                 
407700     MOVE ZERO               TO RW2-IDDISTR                               
407800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
407900       MOVE WC-CDC-SE        TO RW2-IDDC                                  
408000     ELSE                                                                 
408100       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
408200     END-IF                                                               
408300     IF IN-EKH-KVANTAL < +0                                               
408400       MOVE '0422'           TO RW2-KDWRTYP                               
408500     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
408600     ELSE                                                                 
408700       MOVE '0421'           TO RW2-KDWRTYP                               
408800       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
408900     END-IF                                                               
409000                                                                          
409100     IF RW2-SUARTSTD NOT = +0                                             
409200       PERFORM S70-WRITE-W51340                                           
409300     END-IF                                                               
409400     .                                                                    
409500     EJECT                                                                
409600                                                                          
409700 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
409800     MOVE '0110'             TO RW1-KDWRTYP                               
409900     IF DCS-KDDC = SPACE OR DCS-DDC                                       
410000       MOVE WC-CDC-SE        TO RW1-IDDC                                  
410100     ELSE                                                                 
410200       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
410300     END-IF                                                               
410400     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
410500     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
410600     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
410700                                                                          
410800     IF RW1-SUARTSTD NOT = +0                                             
410900       MOVE 'RW1' TO WS-IDPTYP                                            
411000       PERFORM S70-WRITE-W51340                                           
411100     END-IF                                                               
411200     .                                                                    
411300     EJECT                                                                
411400                                                                          
411500 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
411600     MOVE '0110'             TO RW1-KDWRTYP                               
411700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
411800       MOVE WC-CDC-SE        TO RW1-IDDC                                  
411900     ELSE                                                                 
412000       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
412100     END-IF                                                               
412200     IF IN-EKH-KDANMORS = '30'                                            
412300       MOVE ZERO             TO RW1-SUARTSTD                              
412400     ELSE                                                                 
412500      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
412600     END-IF                                                               
412700     IF IN-EKH-KDANMORS = '30' OR '80'                                    
412800       MOVE ZERO             TO RW1-SUARTSJK                              
412900     ELSE                                                                 
413000      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
413100     END-IF                                                               
413200     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
413300                                                                          
413400     IF RW1-SUARTSTD NOT = +0                                             
413500       MOVE 'RW1' TO WS-IDPTYP                                            
413600       PERFORM S70-WRITE-W51340                                           
413700     END-IF                                                               
413800     .                                                                    
413900     EJECT                                                                
414000                                                                          
414100 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
414200     MOVE '0110'             TO RW1-KDWRTYP                               
414300     IF DCS-KDDC = SPACE OR DCS-DDC                                       
414400       MOVE WC-CDC-SE        TO RW1-IDDC                                  
414500     ELSE                                                                 
414600       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
414700     END-IF                                                               
414800     IF IN-EKH-KDEKSHT = '310'                                            
414900*** SKROTNING KDANMORS  13 O 23                                           
415000       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
415100     ELSE                                                                 
415200      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
415300     END-IF                                                               
415400                                                                          
415500     MOVE ZERO               TO RW1-SUARTSJK                              
415600                                RW1-SUARTFSG                              
415700     IF RW1-SUARTSTD NOT = +0                                             
415800       MOVE 'RW1' TO WS-IDPTYP                                            
415900       PERFORM S70-WRITE-W51340                                           
416000     END-IF                                                               
416100     .                                                                    
416200     EJECT                                                                
416300                                                                          
416400 S60-WRITE-W5704N SECTION.                                                
416500     WRITE SAPUT-POST  FROM IN-AREA                                       
416600                                                                          
416700     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
416800     MOVE 'W5704N'            TO POSTSUM-FDNAMN                           
416900     MOVE 'W57048D7'          TO POSTSUM-DDNAMN2                          
417000     CALL POSTSUM USING POSTSUM-PARM                                      
417100     .                                                                    
417200     EJECT                                                                
417300                                                                          
417400 S70-WRITE-W51340 SECTION.                                                
417500     IF WS-IDPTYP  = 'RW2'                                                
417600       IF DCS-KDDC = SPACE OR DCS-DDC                                     
417700         MOVE WC-CDC-SE        TO INV-IDDC                                
417800       ELSE                                                               
417900         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
418000       END-IF                                                             
418100       IF IN-EKH-KVANTAL < +0                                             
418200         MOVE '003'            TO INV-IDPTYP                              
418300       COMPUTE INV-SUARTSTD =                                             
418400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
418500       ELSE                                                               
418600         MOVE '002'            TO INV-IDPTYP                              
418700         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
418800       END-IF                                                             
418900       MOVE SPACE TO WS-IDPTYP                                            
419000       MOVE 0                  TO INV-ADLAGOMR                            
419100       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
419200       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
419300     END-IF                                                               
419400     IF WS-IDPTYP  = 'RW1'                                                
419500       IF DCS-KDDC = SPACE OR DCS-DDC                                     
419600         MOVE WC-CDC-SE        TO INV-IDDC                                
419700       ELSE                                                               
419800         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
419900       END-IF                                                             
420000       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
420100       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
420200       MOVE 0                  TO INV-ADLAGOMR                            
420300       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
420400       MOVE '001'              TO INV-IDPTYP                              
420500       MOVE SPACE              TO WS-IDPTYP                               
420600     END-IF                                                               
420700     WRITE INV-POST  FROM INV-W51310                                      
420800                                                                          
420900     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
421000     MOVE 'W51340'            TO POSTSUM-FDNAMN                           
421100     MOVE 'W57048D8'          TO POSTSUM-DDNAMN2                          
421200     CALL POSTSUM USING POSTSUM-PARM                                      
421300     .                                                                    
421400     EJECT                                                                
421500                                                                          
421600 S13-GET-LANDING-COST SECTION.                                            
421700     MOVE '63'                   TO W-IDDC-B6                             
421800     MOVE IN-EKH-KDPRODSL        TO W-KDPRODSL                            
421900*** GET LCF/IMPORT EXPENSE                                                
422000     PERFORM IMS-GU-WDB617                                                
422100     IF SEGMENT-FINNS                                                     
422200       IF PROC-TILANDCO > IN-EKH-DAVERDAT                                 
422300         MOVE PROC-RELANDCO-TO   TO WS-MARKUP1                            
422400       ELSE                                                               
422500         MOVE PROC-RELANDCO-FROM TO WS-MARKUP1                            
422600       END-IF                                                             
422700     END-IF                                                               
422800     .                                                                    
422900     EJECT                                                                
423000 S80-GET-CURRENCY-RATE SECTION.                                           
423100     MOVE +0                  TO W-ANT                                    
423200     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
423300             BEFORE INITIAL ' '                                           
423400     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
423500     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
423600     PERFORM IMS-GU-WDL601                                                
423700     IF SEGMENT-SAKNAS                                                    
423800       CONTINUE                                                           
423900     ELSE                                                                 
424000       PERFORM IMS-GNP-WDL611                                             
424100       IF SEGMENT-SAKNAS                                                  
424200         CONTINUE                                                         
424300       ELSE                                                               
424400         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
424500                                   - INL-DAINLEV                          
424600         MOVE WS-FAKTURA-DATUM2   TO WS-FAKTURA-DATUM                     
424700         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
424800         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
424900         MOVE W-DATE-AAMM         TO CURR-TIAAMM                          
425000         MOVE WS-KDVALISO-TH      TO CURR-KDVALISO-ROW                    
425100         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
425200         IF CURR-KDSVAR = ' '                                             
425300           MOVE CURR-PRKURS-NEW   TO WS-PRKURS-TH3                        
425400         ELSE                                                             
425500           MOVE +1                TO WS-PRKURS-TH3                        
425600         END-IF                                                           
425700       END-IF                                                             
425800     END-IF                                                               
425900     .                                                                    
426000     EJECT                                                                
426100                                                                          
426200 S81-GET-CURRENCY-RATE SECTION.                                           
426300     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
426400     MOVE 'THB'               TO CURR-KDVALISO-ROW                        
426500     IF IN-FIL-IDPGM = 'W4183300'                                         
426600       IF IN-EKH-DAAVIDAT > ZERO                                          
426700         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
426800         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
426900       ELSE                                                               
427000         MOVE WS-TIAA            TO WS-TIAA-CR                            
427100         MOVE WS-TIMM            TO WS-TIMM-CR                            
427200       END-IF                                                             
427300     ELSE                                                                 
427400       MOVE WS-TIAA              TO WS-TIAA-CR                            
427500       MOVE WS-TIMM              TO WS-TIMM-CR                            
427600     END-IF                                                               
427700     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
427800     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
427900     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
428000     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
428100     IF CURR-KDSVAR = ' '                                                 
428200       IF IN-EKH-IDDISTR > ZERO                                           
428300         MOVE CURR-PRKURS-NEW TO WS-PRKURS-TH3                            
428400       ELSE                                                               
428500         IF WS-PRKURS = ZERO                                              
428600           MOVE 1           TO WS-PRKURS-TH3                              
428700         END-IF                                                           
428800       END-IF                                                             
428900     ELSE                                                                 
429000       MOVE 1               TO WS-PRKURS-TH3                              
429100     END-IF                                                               
429200     .                                                                    
429300     EJECT                                                                
429400* --- IMS SECTIONS ---                                                    
429500                                                                          
429600 IMS-GU-WDH521 SECTION.                                                   
429700     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
429800          DELIMITED BY SIZE INTO SSA1                                     
429900     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
430000          DELIMITED BY SIZE INTO SSA2                                     
430100     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
430200          DELIMITED BY SIZE INTO SSA3                                     
430300     MOVE '  '              TO GODK-STATUSKODER                           
430400     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
430500                                                   SSA2                   
430600                                                   SSA3                   
430700     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
430800                                                                          
430900     PERFORM IMS-STATUS-CONTROL                                           
431000     .                                                                    
431100                                                                          
431200 IMS-GNP-WDH531 SECTION.                                                  
431300     MOVE 'WDH531  '        TO SSA1                                       
431400     MOVE '  GE'            TO GODK-STATUSKODER                           
431500     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
431600     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
431700                               WS-STATUS                                  
431800     PERFORM IMS-STATUS-CONTROL                                           
431900     .                                                                    
432000     EJECT                                                                
432100                                                                          
432200 IMS-GU-WDB201 SECTION.                                                   
432300     STRING 'WDB201  (IDGMT    =' W-IDGMT-KEY ')'                         
432400          DELIMITED BY SIZE INTO SSA1                                     
432500     MOVE '  GE'                 TO GODK-STATUSKODER                      
432600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
432700      MOVE WDB2-STATUS-CODE      TO STATUS-WS                             
432800     PERFORM IMS-STATUS-CONTROL                                           
432900     .                                                                    
433000     EJECT                                                                
433100                                                                          
433200 IMS-GU-WDB101 SECTION.                                                   
433300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
433400          DELIMITED BY SIZE INTO SSA1                                     
433500     MOVE '  GE'               TO GODK-STATUSKODER                        
433600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
433700     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
433800     PERFORM IMS-STATUS-CONTROL                                           
433900     .                                                                    
434000     EJECT                                                                
434100                                                                          
434200 IMS-GU-WDB601    SECTION.                                                
434300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
434400          DELIMITED BY SIZE INTO SSA1                                     
434500     MOVE '  GE' TO GODK-STATUSKODER                                      
434600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
434700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
434800     PERFORM IMS-STATUS-CONTROL                                           
434900     IF SEGMENT-SAKNAS                                                    
435000        MOVE SPACE TO DCS-KDDC                                            
435100     END-IF                                                               
435200     .                                                                    
435300     EJECT                                                                
435400                                                                          
435500 IMS-GU-WDB617 SECTION.                                                   
435600                                                                          
435700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
435800          DELIMITED BY SIZE INTO SSA1                                     
435900     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
436000          DELIMITED BY SIZE INTO SSA2                                     
436100     MOVE '  GE' TO GODK-STATUSKODER                                      
436200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB617 SSA1 SSA2               
436300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
436400     PERFORM IMS-STATUS-CONTROL                                           
436500     .                                                                    
436600     SKIP3                                                                
436700 IMS-GU-WDB621 SECTION.                                                   
436800                                                                          
436900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
437000          DELIMITED BY SIZE INTO SSA1                                     
437100     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
437200          DELIMITED BY SIZE INTO SSA2                                     
437300     STRING 'WDB621  (KDPRODSL =' W-KDPRODSL-X ')'                        
437400          DELIMITED BY SIZE INTO SSA3                                     
437500     MOVE '  GE' TO GODK-STATUSKODER                                      
437600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB621 SSA1 SSA2 SSA3          
437700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
437800     PERFORM IMS-STATUS-CONTROL                                           
437900     .                                                                    
438000     SKIP3                                                                
438100 IMS-GU-WDL601   SECTION.                                                 
438200     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
438300          DELIMITED BY SIZE INTO SSA1                                     
438400     MOVE '  GE' TO GODK-STATUSKODER                                      
438500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
438600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
438700     PERFORM IMS-STATUS-CONTROL                                           
438800     .                                                                    
438900     SKIP3                                                                
439000                                                                          
439100 IMS-GNP-WDL611   SECTION.                                                
439200     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
439300          DELIMITED BY SIZE INTO SSA1                                     
439400     MOVE '  GE' TO GODK-STATUSKODER                                      
439500     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
439600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
439700     PERFORM IMS-STATUS-CONTROL                                           
439800     .                                                                    
439900     SKIP3                                                                
440000                                                                          
440100 IMS-STATUS-CONTROL SECTION.                                              
440200     SET STATUS-IX TO 1                                                   
440300     SEARCH GODK-STATUS                                                   
440400       AT END                                                             
440500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
440600           DELIMITED BY SIZE INTO FELTEXT                                 
440700         DISPLAY FELTEXT                                                  
440800         CALL FELLOG                                                      
440900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
441000         CONTINUE                                                         
441100     END-SEARCH                                                           
441200     .                                                                    
