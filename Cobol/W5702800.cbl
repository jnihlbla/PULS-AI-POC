000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5702800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   20220829.                                                
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
004700     SELECT W57066                     ASSIGN TO W57028D1.                
004800                                                                          
004900*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
005000     SELECT W57021A                    ASSIGN TO W57028D2.                
005100                                                                          
005200*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005300     SELECT W57022A                    ASSIGN TO W57028D3.                
005400                                                                          
005500*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005600     SELECT W57023A                    ASSIGN TO W57028D4.                
005700                                                                          
005800*          --- LOGG TILL ON-DEMAND                                        
005900     SELECT W57025                     ASSIGN TO W57028D5.                
006000                                                                          
006100*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006200     SELECT W57028                     ASSIGN TO W57028D6.                
006300                                                                          
006400*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006500     SELECT W5702N                     ASSIGN TO W57028D7.                
006600                                                                          
006700*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006800     SELECT W51320                     ASSIGN TO W57028D8.                
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
008100 FD  W57021A                                                              
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS  0.                                                   
008400*01  71INIT-POST -COPY R3INIT20               -L.                         
008500*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008600*01  71LINE-POST -COPY R3LINE20               -L.                         
008700                                                                          
008800 FD  W57022A                                                              
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100*01  72LINE-POST -COPY R3LINE20               -L.                         
009200                                                                          
009300 FD  W57023A                                                              
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009700*01  73LINE-POST -COPY R3LINE20               -L.                         
009800                                                                          
009900 FD  W57025                                                               
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200*01  LOGG-POST   -COPY W57073                 -L.                         
010300                                                                          
010400 FD  W57028                                                               
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700*01  AVST-POST   -COPY W57070                 -L.                         
010800                                                                          
010900 FD  W5702N                                                               
011000     RECORDING       F                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300 01  SAPUT-POST.                                                          
011400*    03  -COPY WDR801        -L.                                          
011500     03 FILLER                   PIC X(6).                                
011600                                                                          
011700 FD  W51320                                                               
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000*01  POST -COPY W51310  -PRE  INV-   -L.                                  
012100                                                                          
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400*    -- CHECKED BY WY2000                                                 
012500 77  IDPGM                        PIC X(8)    VALUE 'W5702800'.           
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
016500 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
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
019600 01  WS-KDVALISO-MY               PIC X(3) VALUE 'MYR'.                   
019700 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
019800 01  WS-PRKURS-MY                 PIC S9(6)V9(5) COMP-3.                  
019900 01  WS-PRKURS-MY2                PIC S9(6)V9(5) COMP-3.                  
020000 01  WS-PRKURS-MY3                PIC S9(6)V9(5) COMP-3.                  
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
028400 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57028'.             
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
035200     EJECT                                                                
035300                                                                          
035400*    --- STATUS-KOD FRÅN IMS                                              
035500 01  STATUS-WS                    PIC XX.                                 
035600     88  SEGMENT-FINNS                        VALUE '  '.                 
035700     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
035800                                                                          
035900 01  GODK-STATUSKODER.                                                    
036000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036100                                                                          
036200 01  SSA1                         PIC X(128).                             
036300 01  SSA2                         PIC X(64).                              
036400 01  SSA3                         PIC X(64).                              
036500     EJECT                                                                
036600                                                                          
036700*    --- IMS FUNKTIONSKODER                                               
036800*01  -COPY W0003                                                          
036900     EJECT                                                                
037000                                                                          
037100*    ---  DLI INPUT-OUTPUT AREA                                           
037200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
037300 01  DLI-IO-WDH501.                                                       
037400*    03  -COPY WDH501                                                     
037500     EJECT                                                                
037600                                                                          
037700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
037800 01  DLI-IO-WDH511.                                                       
037900*    03  -COPY WDH511                                                     
038000     EJECT                                                                
038100                                                                          
038200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
038300 01  DLI-IO-WDH521.                                                       
038400*    03  -COPY WDH521                                                     
038500     EJECT                                                                
038600                                                                          
038700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
038800 01  DLI-IO-WDH531.                                                       
038900*    03  -COPY WDH531                                                     
039000     EJECT                                                                
039100                                                                          
039200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
039300 01  DLI-IO-WDB101.                                                       
039400*    03  -COPY WDB101                                                     
039500     EJECT                                                                
039600                                                                          
039700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
039800 01  DLI-IO-WDB201.                                                       
039900*    03  -COPY WDB201                                                     
040000     EJECT                                                                
040100                                                                          
040200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040300 01   DLI-IO-AREA-B601.                                                   
040400*     03  -COPY WDB601                                                    
040500     EJECT                                                                
040600 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
040700 01   DLI-IO-WDB617.                                                      
040800*     03  -COPY WDB617                                                    
040900     EJECT                                                                
041000 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
041100 01   DLI-IO-AREA-L601.                                                   
041200*     03  -COPY WDL601                                                    
041300     EJECT                                                                
041400 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
041500 01   DLI-IO-AREA-L611.                                                   
041600*     03  -COPY WDL611                                                    
041700     EJECT                                                                
041800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6C1'.                
041900     SKIP3                                                                
042000     EJECT                                                                
042100 LINKAGE SECTION.                                                         
042200*01  -COPY W0008  -PRE WDH5-                                              
042300     05  FILLER                  PIC X.                                   
042400                                                                          
042500*01  -COPY W0008  -PRE WDB2-                                              
042600     05  FILLER                  PIC X.                                   
042700                                                                          
042800*01  -COPY W0008  -PRE WDB1-                                              
042900     05  FILLER                  PIC X.                                   
043000                                                                          
043100*01  -COPY W0008  -PRE WDG2-                                              
043200     05  FILLER                  PIC X.                                   
043300                                                                          
043400*01  -COPY W0008  -PRE WDB6-                                              
043500     05  FILLER                  PIC X.                                   
043600                                                                          
043700*01  -COPY W0008  -PRE WDL6-                                              
043800     05  FILLER                  PIC X.                                   
043900                                                                          
044000                                                                          
044100     EJECT                                                                
044200                                                                          
044300 PROCEDURE DIVISION  USING WDH5-PCB WDB2-PCB WDB1-PCB                     
044400                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
044500 MAIN SECTION.                                                            
044600     ENTRY 'DLITCBL' USING WDH5-PCB WDB2-PCB WDB1-PCB                     
044700                           WDG2-PCB WDB6-PCB WDL6-PCB.                    
044800                                                                          
044900     PERFORM A-INIT                                                       
045000                                                                          
045100     PERFORM S01-READ-W57066                                              
045200     PERFORM UNTIL END-OF-W57066                                          
045300*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
045400       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
045500       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
045600       AND WS-NEW-MONTH > 01                                              
045700         PERFORM S60-WRITE-W5702N                                         
045800       ELSE                                                               
045900         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
046000         PERFORM S30-READ-DATABASE-B2-B1                                  
046100         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
046200           PERFORM C-EXECUTE                                              
046300         END-IF                                                           
046400       END-IF                                                             
046500       PERFORM S01-READ-W57066                                            
046600     END-PERFORM                                                          
046700                                                                          
046800     PERFORM Z-FINI                                                       
046900                                                                          
047000     MOVE ZERO TO RETURN-CODE                                             
047100     GOBACK                                                               
047200     .                                                                    
047300     EJECT                                                                
047400                                                                          
047500 A-INIT SECTION.                                                          
047600     OPEN INPUT  W57066                                                   
047700                                                                          
047800     OPEN OUTPUT W57028                                                   
047900                 W57021A                                                  
048000                 W57022A                                                  
048100                 W57023A                                                  
048200                 W57025                                                   
048300                 W5702N                                                   
048400                 W51320                                                   
048500                                                                          
048600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
048700     MOVE 20               TO RW1-DAVVREG(1:2)                            
048800     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
048900                              RW1-DAVVREG(3:2)                            
049000                              W-DATE-AAMM(1:2)                            
049100                              WS-TIAA                                     
049200     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
049300                              W-DATE-AAMM(3:2)                            
049400                              WS-TIMM                                     
049500                              WS-NEW-MONTH                                
049600     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
049700     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
049800     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
049900                                                                          
050000*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
050100*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
050200     IF WS-NEW-MONTH = 12                                                 
050300       MOVE 1              TO WS-NEW-MONTH                                
050400     ELSE                                                                 
050500       ADD 1               TO WS-NEW-MONTH                                
050600*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
050700***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
050800***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
050900***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
051000***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
051100       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
051200       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
051300         ADD 1             TO WS-NEW-MONTH                                
051400         ADD 1             TO WS-TIMM                                     
051500         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
051600       END-IF                                                             
051700     END-IF                                                               
051800                                                                          
051900     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
052000                                                                          
052100     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
052200                                                                          
052300     ACCEPT DAGENS-KLOCKA FROM TIME                                       
052400     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
052500                                                                          
052600     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
052700     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
052800     MOVE 'M'                   TO CURR-KDVALTYP                          
052900                                                                          
053000     MOVE WS-KDVALISO-MY        TO CURR-KDVALISO-ROW                      
053100     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
053200     IF CURR-KDSVAR = ' '                                                 
053300       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-MY                           
053400     ELSE                                                                 
053500       MOVE 1                   TO WS-PRKURS-MY                           
053600     END-IF                                                               
053700     COMPUTE WS-PRKURS-MY2 ROUNDED = 1 / WS-PRKURS-MY                     
053800     MOVE WS-PRKURS-MY          TO WS-PRKURS-MY3                          
053900     .                                                                    
054000     EJECT                                                                
054100                                                                          
054200 C-EXECUTE SECTION.                                                       
054300     MOVE WC-IDFTG-MY           TO W-IDFTG                                
054400     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
054500     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
054600     IF IN-EKH-KDEKNIVA = 'TDET'                                          
054700       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
054800     END-IF                                                               
054900     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
055000     PERFORM IMS-GU-WDH521                                                
055100     PERFORM IMS-GNP-WDH531                                               
055200                                                                          
055300     PERFORM S13-GET-LANDING-COST                                         
055400                                                                          
055500     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
055600     IF IN-EKH-IDDISTR > ZERO                                             
055700       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
055800     ELSE                                                                 
055900       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
056000     END-IF                                                               
056100     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
056200                                                                          
056300* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
056400* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
056500* BOKFÖRINGEN I SAP SKER DOCK ALLTID I MY, DÄRFÖR BYTET HÄR:              
056600*    IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
056700*      MOVE 'MYR'               TO WS-KDVALISO                            
056800*    END-IF                                                               
056900                                                                          
057000     IF IN-EKH-IDVERGL = WS-SPAR-IDVERGL                                  
057100     AND (IN-EKH-DAVERDAT = WS-SPAR-DAVERDAT)                             
057200       IF  (IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                              
057300       AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                              
057400       OR (IN-EKH-KDEKHHT = '303')                                        
057500         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
057600         IF IN-EKH-KDEKHHT = '103'                                        
057700           IF IN-EKH-CMD = WS-SPAR-CMD                                    
057800             MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                         
057900           ELSE                                                           
058000             IF WS-LOP = 9                                                
058100               MOVE ZERO  TO WS-LOP                                       
058200             ELSE                                                         
058300               ADD +1     TO WS-LOP                                       
058400             END-IF                                                       
058500             MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                         
058600             MOVE IN-EKH-CMD TO WS-SPAR-CMD                               
058700           END-IF                                                         
058800         END-IF                                                           
058900       ELSE                                                               
059000         MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                           
059100         MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                           
059200         IF WS-LOP = 9                                                    
059300           MOVE ZERO  TO WS-LOP                                           
059400         ELSE                                                             
059500           ADD +1     TO WS-LOP                                           
059600         END-IF                                                           
059700         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
059800       END-IF                                                             
059900     ELSE                                                                 
060000       MOVE IN-EKH-IDVERGL  TO WS-SPAR-IDVERGL                            
060100       MOVE IN-EKH-KDEKHHT  TO WS-SPAR-KDEKHHT                            
060200       MOVE IN-EKH-KDEKSHT  TO WS-SPAR-KDEKSHT                            
060300       MOVE IN-EKH-DAVERDAT TO WS-SPAR-DAVERDAT                           
060400       MOVE IN-EKH-CMD      TO WS-SPAR-CMD                                
060500       IF WS-LOP = 9                                                      
060600         MOVE ZERO  TO WS-LOP                                             
060700       ELSE                                                               
060800         ADD +1     TO WS-LOP                                             
060900       END-IF                                                             
061000       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
061100     END-IF                                                               
061200* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
061300     IF SYST-IDPTYP = '210'                                               
061400       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
061500     ELSE                                                                 
061600       IF SYST-IDPTYP = '310'                                             
061700         PERFORM CC-CREATE-WRITE-HEADER-AR                                
061800       ELSE                                                               
061900* TEST OM BRYTNING PÅ VERIFIKATION                                        
062000         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
062100         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
062200         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
062300         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
062400           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
062500           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
062600           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
062700           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
062800           IF (IN-EKH-KDEKHHT = '102'                                     
062900           AND IN-EKH-KDEKSHT = '121')                                    
063000           OR (IN-EKH-KDEKHHT = '102'                                     
063100           AND IN-EKH-KDEKSHT = '122')                                    
063200           OR (IN-EKH-KDEKHHT = '102'                                     
063300           AND IN-EKH-KDEKSHT = '131')                                    
063400           OR (IN-EKH-KDEKHHT = '102'                                     
063500           AND IN-EKH-KDEKSHT = '132')                                    
063600             PERFORM S80-GET-CURRENCY-RATE                                
063700           END-IF                                                         
063800           IF (IN-EKH-KDEKHHT = '303'                                     
063900           AND IN-EKH-KDEKSHT = '301')                                    
064000           OR (IN-EKH-KDEKHHT = '303'                                     
064100           AND IN-EKH-KDEKSHT = '307')                                    
064200           OR (IN-EKH-KDEKHHT = '303'                                     
064300           AND IN-EKH-KDEKSHT = '371')                                    
064400           OR (IN-EKH-KDEKHHT = '303'                                     
064500           AND IN-EKH-KDEKSHT = '3XX')                                    
064600             PERFORM S81-GET-CURRENCY-RATE                                
064700           END-IF                                                         
064800*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
064900*   HEADER-POST TILL HUVUDBOKEN                                           
065000           IF (IN-EKH-KDEKHHT = '102'                                     
065100           AND IN-EKH-KDEKSHT = '120')                                    
065200           OR (IN-EKH-KDEKHHT = '102'                                     
065300           AND IN-EKH-KDEKSHT = '124')                                    
065400           OR (IN-EKH-KDEKHHT = '102'                                     
065500           AND IN-EKH-KDEKSHT = '125')                                    
065600           OR (IN-EKH-KDEKHHT = '102'                                     
065700           AND IN-EKH-KDEKSHT = '130')                                    
065800           OR (IN-EKH-KDEKHHT = '102'                                     
065900           AND IN-EKH-KDEKSHT = '134')                                    
066000           OR (IN-EKH-KDEKHHT = '103'                                     
066100           AND IN-EKH-KDEKSHT = '102')                                    
066200           OR (IN-EKH-KDEKHHT = '103'                                     
066300           AND IN-EKH-KDEKSHT = '106')                                    
066400           OR (IN-EKH-KDEKHHT = '103'                                     
066500           AND IN-EKH-KDEKSHT = '107')                                    
066600           OR (IN-EKH-KDEKHHT = '204'                                     
066700           AND IN-EKH-KDEKSHT = '301')                                    
066800           OR (IN-EKH-KDEKHHT = '303'                                     
066900           AND IN-EKH-KDEKSHT = '301')                                    
067000           OR (IN-EKH-KDEKHHT = '303'                                     
067100           AND IN-EKH-KDEKSHT = '307')                                    
067200           OR (IN-EKH-KDEKHHT = '303'                                     
067300           AND IN-EKH-KDEKSHT = '3XX')                                    
067400           OR (IN-EKH-KDEKHHT = '303'                                     
067500           AND IN-EKH-KDEKSHT = '371')                                    
067600             CONTINUE                                                     
067700           ELSE                                                           
067800             PERFORM CA-CREATE-WRITE-HEADER-GL                            
067900           END-IF                                                         
068000         END-IF                                                           
068100       END-IF                                                             
068200     END-IF                                                               
068300                                                                          
068400**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
068500     MOVE WS-STATUS TO STATUS-WS                                          
068600     PERFORM UNTIL SEGMENT-SAKNAS                                         
068700       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
068800                                                                          
068900* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
069000       IF SYST-IDPTYP = '610'                                             
069100         PERFORM CD-BUILD-COMMON-610-PART                                 
069200         PERFORM CE-SCHEDULE-LINE-GL                                      
069300       ELSE                                                               
069400         IF SYST-IDPTYP = '210'                                           
069500           PERFORM CF-BUILD-COMMON-210-PART                               
069600           PERFORM CG-SCHEDULE-LINE-AP                                    
069700         ELSE                                                             
069800           IF SYST-IDPTYP = '310'                                         
069900             PERFORM CH-BUILD-COMMON-310-PART                             
070000             PERFORM CI-SCHEDULE-LINE-AR                                  
070100           END-IF                                                         
070200         END-IF                                                           
070300       END-IF                                                             
070400       PERFORM IMS-GNP-WDH531                                             
070500     END-PERFORM                                                          
070600     .                                                                    
070700     EJECT                                                                
070800                                                                          
070900 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
071000     MOVE SPACE                   TO R3-HEAD-R3                           
071100     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
071200     MOVE 'MY04'                  TO R3-HEAD-COMPANY-CODE                 
071300     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
071400     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
071500     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
071600     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
071700     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
071800       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
071900     ELSE                                                                 
072000       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
072100     END-IF                                                               
072200     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
072300     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
072400     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
072500     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
072600     IF WS-KDVALISO = 'MYR'                                               
072700       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
072800     ELSE                                                                 
072900       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
073000       MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                     
073100       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
073200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
073300       IF CURR-KDSVAR = ' '                                               
073400         IF IN-EKH-IDDISTR > ZERO                                         
073500           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
073600         ELSE                                                             
073700           MOVE 1               TO WS-PRKURS                              
073800         END-IF                                                           
073900       ELSE                                                               
074000         MOVE 1                 TO WS-PRKURS                              
074100       END-IF                                                             
074200       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
074300                                       CURR-REVALUTA-TO                   
074400       IF CURR-REVALUTA-TO = +1                                           
074500         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
074600       END-IF                                                             
074700       IF CURR-REVALUTA-TO = +10                                          
074800         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
074900       END-IF                                                             
075000       IF CURR-REVALUTA-TO = +100                                         
075100         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
075200       END-IF                                                             
075300     END-IF                                                               
075400     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
075500     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
075600     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
075700     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
075800     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
075900     MOVE JA                      TO WS-HEADER-SW                         
076000     MOVE NEJ                     TO WS-LINE-SW                           
076100                                                                          
076200* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
076300     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
076400     OR (IN-EKH-KDEKHHT = '303'                                           
076500     AND IN-EKH-KDEKSHT = '391')                                          
076600     OR (IN-EKH-KDEKHHT = '102'                                           
076700     AND IN-EKH-KDEKSHT = '121')                                          
076800     OR (IN-EKH-KDEKHHT = '102'                                           
076900     AND IN-EKH-KDEKSHT = '131')                                          
077000       PERFORM S004-WRITE-W57023A-HEAD                                    
077100     ELSE                                                                 
077200       PERFORM S002-WRITE-W57021A-HEAD                                    
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600                                                                          
077700 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
077800     MOVE SPACE                   TO R3-HEAD-R3                           
077900     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
078000     MOVE 'MY04'                  TO R3-HEAD-COMPANY-CODE                 
078100                                     R3-HEAD-CONTROL-AREA                 
078200     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
078300     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
078400     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
078500     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
078600     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
078700     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
078800       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
078900     ELSE                                                                 
079000       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
079100     END-IF                                                               
079200     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
079300     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
079400     IF (IN-EKH-KDEKHHT = '103'                                           
079500     AND IN-EKH-KDEKSHT = '102')                                          
079600       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
079700       MOVE IN-EKH-PRKURS         TO R3-HEAD-EXCHANGE-RATE                
079800     ELSE                                                                 
079900       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
080000       MOVE WS-PRKURS-MY2         TO R3-HEAD-EXCHANGE-RATE                
080100       MOVE 'MYR'                 TO CURR-KDVALISO-ROW                    
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
082000           MOVE CURR-PRKURS-NEW TO WS-PRKURS-MY                           
082100         ELSE                                                             
082200           IF WS-PRKURS = ZERO                                            
082300             MOVE 1             TO WS-PRKURS-MY                           
082400           END-IF                                                         
082500         END-IF                                                           
082600       ELSE                                                               
082700         MOVE 1                 TO WS-PRKURS-MY                           
082800       END-IF                                                             
082900       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-MY *                     
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
085700     AND IN-EKH-KDEKSHT = '371')                                          
085800     OR (IN-EKH-KDEKHHT = '303'                                           
085900     AND IN-EKH-KDEKSHT = '3XX')                                          
086000       MOVE 'MYR'                 TO R3-HEAD-CURRENCY                     
086100       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
086200     END-IF                                                               
086300     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
086400     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
086500     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
086600     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
086700     MOVE JA                      TO WS-HEADER-SW                         
086800     MOVE NEJ                     TO WS-LINE-SW                           
086900                                                                          
087000* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57023A                       
087100       PERFORM S004-WRITE-W57023A-HEAD                                    
087200     .                                                                    
087300     EJECT                                                                
087400                                                                          
087500 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
087600     MOVE SPACE                   TO R3-HEAD-R3                           
087700     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
087800     MOVE 'MY04'                  TO R3-HEAD-COMPANY-CODE                 
087900                                     R3-HEAD-CONTROL-AREA                 
088000     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
088100     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
088200     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
088300     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
088400     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
088500     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
088600       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
088700     ELSE                                                                 
088800       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
088900     END-IF                                                               
089000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
089100     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
089200     IF (IN-EKH-KDEKHHT = '204'                                           
089300     AND IN-EKH-KDEKSHT = '201')                                          
089400     OR (IN-EKH-KDEKHHT = '204'                                           
089500     AND IN-EKH-KDEKSHT = '301')                                          
089600       MOVE 'MYR'                 TO R3-HEAD-CURRENCY                     
089700       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
089800     ELSE                                                                 
089900       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
090000       MOVE WS-PRKURS-MY2         TO R3-HEAD-EXCHANGE-RATE                
090100       MOVE 'SEK'                 TO CURR-KDVALISO-ROW                    
090200       IF IN-FIL-IDPGM = 'W4183300'                                       
090300         IF IN-EKH-DAAVIDAT > ZERO                                        
090400           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
090500           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
090600         ELSE                                                             
090700           MOVE WS-TIAA              TO WS-TIAA-CR                        
090800           MOVE WS-TIMM              TO WS-TIMM-CR                        
090900         END-IF                                                           
091000       ELSE                                                               
091100         MOVE WS-TIAA                TO WS-TIAA-CR                        
091200         MOVE WS-TIMM                TO WS-TIMM-CR                        
091300       END-IF                                                             
091400       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
091500       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
091600       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
091700       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
091800       IF CURR-KDSVAR = ' '                                               
091900         IF IN-EKH-IDDISTR > ZERO                                         
092000           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
092100         ELSE                                                             
092200           IF WS-PRKURS = ZERO                                            
092300             MOVE 1             TO WS-PRKURS                              
092400           END-IF                                                         
092500         END-IF                                                           
092600       ELSE                                                               
092700         MOVE 1                 TO WS-PRKURS                              
092800       END-IF                                                             
092900       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
093000                                       CURR-REVALUTA-TO                   
093100       END-COMPUTE                                                        
093200       IF CURR-REVALUTA-TO = +1                                           
093300         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
093400       END-IF                                                             
093500       IF CURR-REVALUTA-TO = +10                                          
093600         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
093700       END-IF                                                             
093800       IF CURR-REVALUTA-TO = +100                                         
093900         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
094000       END-IF                                                             
094100     END-IF                                                               
094200     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
094300     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
094400     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
094500     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
094600     MOVE JA                      TO WS-HEADER-SW                         
094700     MOVE NEJ                     TO WS-LINE-SW                           
094800                                                                          
094900* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57023A                       
095000       PERFORM S004-WRITE-W57023A-HEAD                                    
095100     .                                                                    
095200     EJECT                                                                
095300                                                                          
095400 CD-BUILD-COMMON-610-PART SECTION.                                        
095500     MOVE SPACE               TO R3-LINE-R3                               
095600     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
095700                                 R3-LINE-DUE-DATE                         
095800                                 R3-LINE-AMOUNT                           
095900                                 R3-LINE-AMOUNT-LC                        
096000                                 R3-LINE-TAX-AMOUNT                       
096100                                 R3-LINE-TAX-AMOUNT-LC                    
096200                                 R3-LINE-NUMBER-OF-DAYS                   
096300                                 R3-LINE-QUANTITY                         
096400                                 R3-LINE-SAMNR                            
096500     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
096600     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
096700     MOVE 'MY04'              TO R3-LINE-COMPANY-CODE                     
096800     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
096900     IF SYST-KDPOST = '50'                                                
097000       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
097100     ELSE                                                                 
097200       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
097300     END-IF                                                               
097400     IF SYST-IDPRCTR NOT = SPACE                                          
097500       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
097600       IF WS-PRCTR-PRODSL = '??'                                          
097700         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
097800         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
097900       END-IF                                                             
098000       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
098100     END-IF                                                               
098200     .                                                                    
098300     EJECT                                                                
098400                                                                          
098500 CE-SCHEDULE-LINE-GL SECTION.                                             
098600     MOVE NEJ                     TO WS-HEADER-SW                         
098700     MOVE JA                      TO WS-LINE-SW                           
098800     EVALUATE IN-EKH-KDEKHHT                                              
098900     WHEN '102'                                                           
099000          PERFORM CEB-MAIN-EVENT-102                                      
099100     WHEN '103'                                                           
099200          PERFORM CEC-MAIN-EVENT-103                                      
099300     WHEN '201'                                                           
099400          PERFORM CED-MAIN-EVENT-201                                      
099500     WHEN '203'                                                           
099600          PERFORM CEF-MAIN-EVENT-203                                      
099700     WHEN '204'                                                           
099800          PERFORM CEG-MAIN-EVENT-204                                      
099900     WHEN '302'                                                           
100000          PERFORM CEI-MAIN-EVENT-302                                      
100100     WHEN '303'                                                           
100200          PERFORM CEJ-MAIN-EVENT-303                                      
100300     WHEN '401'                                                           
100400          PERFORM CEK-MAIN-EVENT-401                                      
100500     WHEN '402'                                                           
100600          PERFORM CEL-MAIN-EVENT-402                                      
100700     WHEN '403'                                                           
100800          PERFORM CEM-MAIN-EVENT-403                                      
100900     WHEN '404'                                                           
101000          PERFORM CEN-MAIN-EVENT-404                                      
101100     END-EVALUATE                                                         
101200     .                                                                    
101300     EJECT                                                                
101400                                                                          
101500 CEB-MAIN-EVENT-102 SECTION.                                              
101600     EVALUATE IN-EKH-KDEKSHT                                              
101700     WHEN '102'                                                           
101800          PERFORM CEBB-SUB-EVENT-102-102                                  
101900     WHEN '120'                                                           
102000          PERFORM CEBD-SUB-EVENT-102-120                                  
102100     WHEN '121'                                                           
102200          PERFORM CEBD-SUB-EVENT-102-121                                  
102300     WHEN '122'                                                           
102400          PERFORM CEBD-SUB-EVENT-102-122                                  
102500     WHEN '123'                                                           
102600          PERFORM CEBD-SUB-EVENT-102-123                                  
102700     WHEN '124'                                                           
102800          PERFORM CEBD-SUB-EVENT-102-124                                  
102900     WHEN '125'                                                           
103000          PERFORM CEBD-SUB-EVENT-102-125                                  
103100     WHEN '130'                                                           
103200          PERFORM CEBD-SUB-EVENT-102-130                                  
103300     WHEN '131'                                                           
103400          PERFORM CEBD-SUB-EVENT-102-131                                  
103500     WHEN '132'                                                           
103600          PERFORM CEBD-SUB-EVENT-102-132                                  
103700     WHEN '134'                                                           
103800          PERFORM CEBD-SUB-EVENT-102-134                                  
103900     END-EVALUATE                                                         
104000     .                                                                    
104100     EJECT                                                                
104200                                                                          
104300 CEBD-SUB-EVENT-102-130 SECTION.                                          
104400     EVALUATE IN-EKH-KDEKNIVA                                             
104500     WHEN 'DET'                                                           
104600       IF SYST-IDSEKVNR = 1                                               
104700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
104800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
104900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
105000         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY  * -1            
105100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
105200         PERFORM S03-WRITE-W57022                                         
105300       END-IF                                                             
105400                                                                          
105500     WHEN 'FÖRS'                                                          
105600     WHEN 'FRAKT'                                                         
105700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
105800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
105900       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
106000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
106100               IN-EKH-SUBEL / WS-PRKURS-MY  * -1                          
106200       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
106300       PERFORM S04-WRITE-W57023A                                          
106400                                                                          
106500     WHEN 'EMB'                                                           
106600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
106700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
106800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
106900               IN-EKH-SUBEL / WS-PRKURS-MY  * -1                          
107000       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
107100       PERFORM S04-WRITE-W57023A                                          
107200                                                                          
107300     WHEN 'DDI'                                                           
107400       IF IN-EKH-SUBEL > ZERO                                             
107500         IF SYST-IDSEKVNR = 1                                             
107600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
107700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
107800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
107900                   IN-EKH-SUBEL                                           
108000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
108100           PERFORM S04-WRITE-W57023A                                      
108200         END-IF                                                           
108300       ELSE                                                               
108400         IF SYST-IDSEKVNR = 2                                             
108500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
108600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
108700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
108800                   IN-EKH-SUBEL                                           
108900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
109000           PERFORM S04-WRITE-W57023A                                      
109100         END-IF                                                           
109200       END-IF                                                             
109300                                                                          
109400     END-EVALUATE                                                         
109500     .                                                                    
109600     EJECT                                                                
109700                                                                          
109800 CEBD-SUB-EVENT-102-131 SECTION.                                          
109900     EVALUATE IN-EKH-KDEKNIVA                                             
110000     WHEN 'DET'                                                           
110100       IF SYST-IDSEKVNR = 1                                               
110200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
110300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
110400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
110500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
110600         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-1                   
110700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
110800         PERFORM S03-WRITE-W57022                                         
110900       END-IF                                                             
111000                                                                          
111100       IF SYST-IDSEKVNR = 2                                               
111200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
111300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
111400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
111500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3) +          
111600            (IN-EKH-KVANTAL *                                             
111700             IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                 
111800         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-131-2                   
111900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
112000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
112100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
112200         MOVE SPACE               TO WS-ALLOCATE-REF                      
112300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
112400         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
112500         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
112600         PERFORM S03-WRITE-W57022                                         
112700       END-IF                                                             
112800                                                                          
112900       IF SYST-IDSEKVNR = 3                                               
113000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
113100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
113200         COMPUTE R3-LINE-AMOUNT-LC =                                      
113300                 WS-LINE-AMOUNT-131-2 - WS-LINE-AMOUNT-131-1              
113400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
113500         PERFORM S03-WRITE-W57022                                         
113600       END-IF                                                             
113700                                                                          
113800     WHEN 'FÖRS'                                                          
113900     WHEN 'FRAKT'                                                         
114000       IF SYST-IDSEKVNR = 1                                               
114100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
114200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
114300         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
114400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
114500                 IN-EKH-SUBEL / WS-PRKURS-MY3                             
114600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
114700         PERFORM S04-WRITE-W57023A                                        
114800       END-IF                                                             
114900                                                                          
115000       IF SYST-IDSEKVNR = 2                                               
115100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
115200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
115300         MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                    
115400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
115500                 IN-EKH-SUBEL / WS-PRKURS-MY3                             
115600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
115700         PERFORM S04-WRITE-W57023A                                        
115800       END-IF                                                             
115900                                                                          
116000     WHEN 'EMB'                                                           
116100       IF SYST-IDSEKVNR = 1                                               
116200         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
116300         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
116400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
116500                 IN-EKH-SUBEL / WS-PRKURS-MY3                             
116600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
116700         PERFORM S04-WRITE-W57023A                                        
116800       END-IF                                                             
116900                                                                          
117000       IF SYST-IDSEKVNR = 2                                               
117100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
117200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
117300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
117400                 IN-EKH-SUBEL / WS-PRKURS-MY3                             
117500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
117600         PERFORM S04-WRITE-W57023A                                        
117700       END-IF                                                             
117800     END-EVALUATE                                                         
117900     .                                                                    
118000     EJECT                                                                
118100                                                                          
118200 CEBD-SUB-EVENT-102-132 SECTION.                                          
118300     EVALUATE IN-EKH-KDEKNIVA                                             
118400     WHEN 'DET'                                                           
118500       IF IN-EKH-KVANTAL > 0                                              
118600         IF SYST-IDSEKVNR = 1                                             
118700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
118800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
118900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
119000            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
119100            + (IN-EKH-KVANTAL *                                           
119200            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
119300           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
119400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
119500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
119600           MOVE SPACE               TO WS-ALLOCATE-REF                    
119700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
119800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
119900           PERFORM S02-WRITE-W57021A                                      
120000         END-IF                                                           
120100                                                                          
120200         IF SYST-IDSEKVNR = 4                                             
120300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
120400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
120500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
120600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
120700            + (IN-EKH-KVANTAL *                                           
120800            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
120900           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
121000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
121100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
121200           MOVE SPACE               TO WS-ALLOCATE-REF                    
121300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
121400           MOVE SPACE               TO R3-LINE-COST-CENTER                
121500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
121600           PERFORM S02-WRITE-W57021A                                      
121700         END-IF                                                           
121800       END-IF                                                             
121900                                                                          
122000       IF IN-EKH-KVANTAL < 0                                              
122100         IF SYST-IDSEKVNR = 2                                             
122200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
122300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
122400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
122500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
122600            + (IN-EKH-KVANTAL *                                           
122700            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
122800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
122900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
123000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
123100           MOVE SPACE               TO WS-ALLOCATE-REF                    
123200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
123300           MOVE SPACE             TO R3-LINE-COST-CENTER                  
123400           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
123500           PERFORM S02-WRITE-W57021A                                      
123600         END-IF                                                           
123700                                                                          
123800         IF SYST-IDSEKVNR = 3                                             
123900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
124000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
124100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
124200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
124300            + (IN-EKH-KVANTAL *                                           
124400            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
124500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
124600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
124700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
124800           MOVE SPACE               TO WS-ALLOCATE-REF                    
124900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
125000           MOVE SPACE             TO R3-LINE-COST-CENTER                  
125100           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
125200           PERFORM S02-WRITE-W57021A                                      
125300         END-IF                                                           
125400       END-IF                                                             
125500     END-EVALUATE                                                         
125600     .                                                                    
125700     EJECT                                                                
125800                                                                          
125900 CEBD-SUB-EVENT-102-134 SECTION.                                          
126000     EVALUATE IN-EKH-KDEKNIVA                                             
126100     WHEN 'DET'                                                           
126200       IF SYST-IDSEKVNR = 1                                               
126300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
126400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
126500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
126600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY   * -1           
126700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
126800         MOVE SPACE               TO WS-ALLOCATE-DC                       
126900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
127000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
127100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
127200         PERFORM S03-WRITE-W57022                                         
127300       END-IF                                                             
127400                                                                          
127500     WHEN 'EMB'                                                           
127600     WHEN 'FÖRS'                                                          
127700     WHEN 'FRAKT'                                                         
127800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
127900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
128000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
128100               (IN-EKH-SUBEL / WS-PRKURS-MY) * -1                         
128200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
128300       MOVE SPACE               TO WS-ALLOCATE-DC                         
128400       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
128500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
128600       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
128700       PERFORM S04-WRITE-W57023A                                          
128800                                                                          
128900     WHEN 'DDI'                                                           
129000       IF IN-EKH-SUBEL > ZERO                                             
129100         IF SYST-IDSEKVNR = 1                                             
129200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
129300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
129400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
129500                   IN-EKH-SUBEL                                           
129600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
129700           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
129800           MOVE SPACE               TO WS-ALLOCATE-DC                     
129900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
130000           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
130100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
130200           PERFORM S04-WRITE-W57023A                                      
130300         END-IF                                                           
130400       ELSE                                                               
130500         IF SYST-IDSEKVNR = 2                                             
130600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
130700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
130800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
130900                   IN-EKH-SUBEL                                           
131000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
131100           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
131200           MOVE SPACE               TO WS-ALLOCATE-DC                     
131300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
131400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
131500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
131600           PERFORM S04-WRITE-W57023A                                      
131700         END-IF                                                           
131800       END-IF                                                             
131900     END-EVALUATE                                                         
132000     .                                                                    
132100     EJECT                                                                
132200                                                                          
132300 CEBB-SUB-EVENT-102-102 SECTION.                                          
132400     EVALUATE IN-EKH-KDEKNIVA                                             
132500     WHEN 'DET'                                                           
132600       IF SYST-IDSEKVNR = 1                                               
132700         IF IN-EKH-KVANTAL > 0                                            
132800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
132900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
133000           COMPUTE R3-LINE-AMOUNT-LC =                                    
133100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
133200           IF IN-EKH-KDVALISO = 'MYR'                                     
133300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
133400           END-IF                                                         
133500           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
133600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
133700           MOVE SPACE               TO WS-ALLOCATE-REF                    
133800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
133900           PERFORM S02-WRITE-W57021A                                      
134000         END-IF                                                           
134100       END-IF                                                             
134200                                                                          
134300       IF SYST-IDSEKVNR = 2                                               
134400         IF IN-EKH-KVANTAL < 0                                            
134500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
134600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
134700           COMPUTE R3-LINE-AMOUNT-LC =                                    
134800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
134900           IF IN-EKH-KDVALISO = 'MYR'                                     
135000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
135100           END-IF                                                         
135200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
135300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
135400           MOVE SPACE               TO WS-ALLOCATE-REF                    
135500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
135600           PERFORM S02-WRITE-W57021A                                      
135700         END-IF                                                           
135800       END-IF                                                             
135900                                                                          
136000       IF SYST-IDSEKVNR = 3                                               
136100         IF IN-EKH-KVANTAL < 0                                            
136200           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
136300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
136400           COMPUTE R3-LINE-AMOUNT-LC =                                    
136500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
136600           IF IN-EKH-KDVALISO = 'MYR'                                     
136700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
136800           END-IF                                                         
136900           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
137000           PERFORM S02-WRITE-W57021A                                      
137100         END-IF                                                           
137200       END-IF                                                             
137300                                                                          
137400       IF SYST-IDSEKVNR = 4                                               
137500         IF IN-EKH-KVANTAL > 0                                            
137600           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
137700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
137800           COMPUTE R3-LINE-AMOUNT-LC =                                    
137900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
138000           IF IN-EKH-KDVALISO = 'MYR'                                     
138100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
138200           END-IF                                                         
138300           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
138400           PERFORM S02-WRITE-W57021A                                      
138500         END-IF                                                           
138600       END-IF                                                             
138700                                                                          
138800     END-EVALUATE                                                         
138900     .                                                                    
139000     EJECT                                                                
139100                                                                          
139200 CEBD-SUB-EVENT-102-120 SECTION.                                          
139300     EVALUATE IN-EKH-KDEKNIVA                                             
139400     WHEN 'DET'                                                           
139500       IF SYST-IDSEKVNR = 1                                               
139600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
139700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
139800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
139900          IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY * -1            
140000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
140100         PERFORM S03-WRITE-W57022                                         
140200       END-IF                                                             
140300                                                                          
140400     WHEN 'FÖRS'                                                          
140500     WHEN 'FRAKT'                                                         
140600     WHEN 'EMB'                                                           
140700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
140800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
140900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
141000               IN-EKH-SUBEL / WS-PRKURS-MY   * -1                         
141100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
141200       PERFORM S04-WRITE-W57023A                                          
141300                                                                          
141400     WHEN 'DDI'                                                           
141500       IF IN-EKH-SUBEL > ZERO                                             
141600         IF SYST-IDSEKVNR = 1                                             
141700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
141800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
141900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
142000                   IN-EKH-SUBEL                                           
142100           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
142200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
142300           PERFORM S04-WRITE-W57023A                                      
142400         END-IF                                                           
142500       ELSE                                                               
142600         IF SYST-IDSEKVNR = 2                                             
142700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
142800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
142900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
143000                   IN-EKH-SUBEL                                           
143100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
143200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
143300           PERFORM S04-WRITE-W57023A                                      
143400         END-IF                                                           
143500       END-IF                                                             
143600     END-EVALUATE                                                         
143700     .                                                                    
143800     EJECT                                                                
143900                                                                          
144000 CEBD-SUB-EVENT-102-121 SECTION.                                          
144100     EVALUATE IN-EKH-KDEKNIVA                                             
144200     WHEN 'DET'                                                           
144300       IF SYST-IDSEKVNR = 1                                               
144400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
144500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
144600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
144700             IN-EKH-KVANTAL *  IN-EKH-PRARTNTO / WS-PRKURS-MY3            
144800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
144900         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-1                      
145000         PERFORM S03-WRITE-W57022                                         
145100       END-IF                                                             
145200                                                                          
145300       IF SYST-IDSEKVNR = 2                                               
145400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
145500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
145600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
145700            IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3              
145800            + IN-EKH-KVANTAL *                                            
145900            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP                   
146000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
146100         MOVE R3-LINE-AMOUNT TO WS-LINE-AMOUNT-121-2                      
146200         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
146300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
146400         MOVE SPACE               TO WS-ALLOCATE-REF                      
146500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
146600         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
146700         PERFORM S03-WRITE-W57022                                         
146800       END-IF                                                             
146900                                                                          
147000       IF SYST-IDSEKVNR = 3                                               
147100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
147200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
147300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
147400            WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1                   
147500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
147600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
147700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
147800         MOVE SPACE               TO WS-ALLOCATE-REF                      
147900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
148000         PERFORM S03-WRITE-W57022                                         
148100       END-IF                                                             
148200                                                                          
148300     WHEN 'FÖRS'                                                          
148400     WHEN 'FRAKT'                                                         
148500     WHEN 'EMB'                                                           
148600       IF SYST-IDSEKVNR = 1                                               
148700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
148800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
148900         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
149000                 IN-EKH-SUBEL / WS-PRKURS-MY3                             
149100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
149200         PERFORM S04-WRITE-W57023A                                        
149300       END-IF                                                             
149400                                                                          
149500       IF SYST-IDSEKVNR = 2                                               
149600         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
149700         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
149800         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
149900                 IN-EKH-SUBEL / WS-PRKURS-MY3                             
150000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
150100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
150200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
150300         MOVE SPACE               TO WS-ALLOCATE-REF                      
150400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
150500         PERFORM S04-WRITE-W57023A                                        
150600       END-IF                                                             
150700                                                                          
150800     END-EVALUATE                                                         
150900                                                                          
151000     .                                                                    
151100     EJECT                                                                
151200                                                                          
151300 CEBD-SUB-EVENT-102-122 SECTION.                                          
151400     EVALUATE IN-EKH-KDEKNIVA                                             
151500     WHEN 'DET'                                                           
151600       IF IN-EKH-KVANTAL > 0                                              
151700         IF SYST-IDSEKVNR = 1                                             
151800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
151900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
152000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
152100            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
152200            + (IN-EKH-KVANTAL *                                           
152300            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
152400           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
152500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
152600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
152700           MOVE SPACE               TO WS-ALLOCATE-REF                    
152800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
152900           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
153000           PERFORM S02-WRITE-W57021A                                      
153100         END-IF                                                           
153200                                                                          
153300         IF SYST-IDSEKVNR = 4                                             
153400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
153500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
153600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
153700            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
153800            + (IN-EKH-KVANTAL *                                           
153900            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
154000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
154100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
154200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
154300           MOVE SPACE               TO WS-ALLOCATE-REF                    
154400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
154500           MOVE SPACE               TO R3-LINE-COST-CENTER                
154600           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
154700           PERFORM S02-WRITE-W57021A                                      
154800         END-IF                                                           
154900       END-IF                                                             
155000                                                                          
155100       IF IN-EKH-KVANTAL < 0                                              
155200         IF SYST-IDSEKVNR = 2                                             
155300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
155400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
155500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
155600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
155700            + (IN-EKH-KVANTAL *                                           
155800            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
155900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
156000           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
156100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
156200           MOVE SPACE               TO WS-ALLOCATE-REF                    
156300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
156400           MOVE SPACE             TO R3-LINE-COST-CENTER                  
156500           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
156600           PERFORM S02-WRITE-W57021A                                      
156700         END-IF                                                           
156800                                                                          
156900         IF SYST-IDSEKVNR = 3                                             
157000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
157100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
157200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
157300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3)            
157400            + (IN-EKH-KVANTAL *                                           
157500            IN-EKH-PRARTNTO / WS-PRKURS-MY3 * WS-MARKUP)                  
157600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
157700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
157800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
157900           MOVE SPACE               TO WS-ALLOCATE-REF                    
158000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
158100           MOVE SPACE             TO R3-LINE-COST-CENTER                  
158200           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
158300           PERFORM S02-WRITE-W57021A                                      
158400         END-IF                                                           
158500       END-IF                                                             
158600     END-EVALUATE                                                         
158700     .                                                                    
158800     EJECT                                                                
158900                                                                          
159000 CEBD-SUB-EVENT-102-123 SECTION.                                          
159100     EVALUATE IN-EKH-KDEKNIVA                                             
159200     WHEN 'DET'                                                           
159300       IF SYST-IDSEKVNR = 1                                               
159400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
159500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
159600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
159700              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
159800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
159900         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
160000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
160100         MOVE SPACE               TO WS-ALLOCATE-REF                      
160200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
160300         PERFORM S02-WRITE-W57021A                                        
160400       END-IF                                                             
160500                                                                          
160600       IF SYST-IDSEKVNR = 2                                               
160700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
160800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
160900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
161000             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
161100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
161200         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
161300         PERFORM S02-WRITE-W57021A                                        
161400       END-IF                                                             
161500     END-EVALUATE                                                         
161600     .                                                                    
161700     EJECT                                                                
161800                                                                          
161900 CEBD-SUB-EVENT-102-124 SECTION.                                          
162000     EVALUATE IN-EKH-KDEKNIVA                                             
162100     WHEN 'DET'                                                           
162200       IF SYST-IDSEKVNR = 1                                               
162300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
162400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
162500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
162600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY   * -1           
162700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
162800         MOVE SPACE               TO WS-ALLOCATE-DC                       
162900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
163000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
163100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
163200         PERFORM S03-WRITE-W57022                                         
163300       END-IF                                                             
163400                                                                          
163500     WHEN 'EMB'                                                           
163600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
163700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
163800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
163900               (IN-EKH-SUBEL / WS-PRKURS-MY) * -1                         
164000       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
164100       MOVE SPACE               TO WS-ALLOCATE-DC                         
164200       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
164300       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
164400       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
164500       PERFORM S04-WRITE-W57023A                                          
164600                                                                          
164700     WHEN 'DDI'                                                           
164800       IF IN-EKH-SUBEL > ZERO                                             
164900         IF SYST-IDSEKVNR = 1                                             
165000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
165100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
165200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
165300                   IN-EKH-SUBEL                                           
165400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
165500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
165600           MOVE SPACE               TO WS-ALLOCATE-DC                     
165700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
165800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
165900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
166000           PERFORM S04-WRITE-W57023A                                      
166100         END-IF                                                           
166200       ELSE                                                               
166300         IF SYST-IDSEKVNR = 2                                             
166400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
166500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
166600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
166700                   IN-EKH-SUBEL                                           
166800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
166900           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
167000           MOVE SPACE               TO WS-ALLOCATE-DC                     
167100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
167200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
167300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
167400           PERFORM S04-WRITE-W57023A                                      
167500         END-IF                                                           
167600       END-IF                                                             
167700     END-EVALUATE                                                         
167800     .                                                                    
167900     EJECT                                                                
168000                                                                          
168100 CEBD-SUB-EVENT-102-125 SECTION.                                          
168200     EVALUATE IN-EKH-KDEKNIVA                                             
168300     WHEN 'DET'                                                           
168400       IF SYST-IDSEKVNR = 1                                               
168500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
168600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
168700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
168800         (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY) +              
168900         (IN-EKH-KVANTAL *                                                
169000          IN-EKH-PRARTNTO / WS-PRKURS-MY * WS-MARKUP)                     
169100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
169200         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
169300         PERFORM S03-WRITE-W57022                                         
169400       END-IF                                                             
169500                                                                          
169600       IF SYST-IDSEKVNR = 2                                               
169700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
169800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
169900         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
170000            (IN-EKH-KVANTAL *                                             
170100             IN-EKH-PRARTNTO / WS-PRKURS-MY * WS-MARKUP)                  
170200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
170300         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
170400         PERFORM S03-WRITE-W57022                                         
170500       END-IF                                                             
170600                                                                          
170700     WHEN 'FÖRS'                                                          
170800     WHEN 'FRAKT'                                                         
170900     WHEN 'EMB'                                                           
171000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
171100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
171200       MOVE SPACE             TO R3-LINE-COST-CENTER                      
171300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
171400          IN-EKH-SUBEL / WS-PRKURS-MY   * -1                              
171500       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
171600       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
171700       PERFORM S04-WRITE-W57023A                                          
171800                                                                          
171900     WHEN 'DDI'                                                           
172000       IF SPAR-SUMMA-102-125 < ZERO                                       
172100         IF SYST-IDSEKVNR = 1                                             
172200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
172300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
172400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
172500                   SPAR-SUMMA-102-125                                     
172600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
172700           PERFORM S04-WRITE-W57023A                                      
172800         END-IF                                                           
172900       END-IF                                                             
173000       IF SPAR-SUMMA-102-125 > ZERO                                       
173100         IF SYST-IDSEKVNR = 2                                             
173200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
173300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
173400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
173500                   SPAR-SUMMA-102-125                                     
173600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
173700           PERFORM S04-WRITE-W57023A                                      
173800         END-IF                                                           
173900       END-IF                                                             
174000                                                                          
174100     END-EVALUATE                                                         
174200     .                                                                    
174300     EJECT                                                                
174400                                                                          
174500 CEC-MAIN-EVENT-103 SECTION.                                              
174600     EVALUATE IN-EKH-KDEKSHT                                              
174700     WHEN '102'                                                           
174800          PERFORM CECB-SUB-EVENT-103-102                                  
174900     END-EVALUATE                                                         
175000     .                                                                    
175100     EJECT                                                                
175200                                                                          
175300 CGA-MAIN-EVENT-303-371 SECTION.                                          
175400     EVALUATE IN-EKH-KDEKNIVA                                             
175500     WHEN 'SUM'                                                           
175600       IF SYST-IDSEKVNR = 1                                               
175700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
175800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
175900         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
176000         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
176100               R3-LINE-AMOUNT-LC / WS-PRKURS-MY3                          
176200         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
176300         PERFORM S10-VATCODE                                              
176400         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
176500         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
176600                 R3-LINE-TAX-AMOUNT-LC * WS-PRKURS-MY2                    
176700         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
176800                                                                          
176900         PERFORM S04-WRITE-W57023A                                        
177000       END-IF                                                             
177100     END-EVALUATE                                                         
177200     .                                                                    
177300     EJECT                                                                
177400 CECB-SUB-EVENT-103-102 SECTION.                                          
177500     EVALUATE IN-EKH-KDEKNIVA                                             
177600     WHEN 'DET'                                                           
177700       IF SYST-IDSEKVNR = 1                                               
177800         IF IN-EKH-KVANTAL < 0                                            
177900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
178000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
178100           COMPUTE R3-LINE-AMOUNT-LC =                                    
178200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
178300           IF IN-EKH-KDVALISO = 'MYR'                                     
178400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
178500           END-IF                                                         
178600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
178700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
178800           MOVE SPACE               TO WS-ALLOCATE-REF                    
178900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
179000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
179100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
179200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
179300           PERFORM S04-WRITE-W57023A                                      
179400         END-IF                                                           
179500       END-IF                                                             
179600                                                                          
179700       IF SYST-IDSEKVNR = 2                                               
179800         IF IN-EKH-KVANTAL > 0                                            
179900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
180000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
180100           COMPUTE R3-LINE-AMOUNT-LC =                                    
180200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
180300           IF IN-EKH-KDVALISO = 'MYR'                                     
180400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
180500           END-IF                                                         
180600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
180700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
180800           MOVE SPACE               TO WS-ALLOCATE-REF                    
180900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
181000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
181100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
181200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
181300           PERFORM S04-WRITE-W57023A                                      
181400         END-IF                                                           
181500       END-IF                                                             
181600                                                                          
181700     WHEN 'DDI'                                                           
181800       IF IN-EKH-SUBEL > ZERO                                             
181900         IF SYST-IDSEKVNR = 1                                             
182000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
182100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
182200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
182300                   IN-EKH-SUBEL                                           
182400           MOVE ZEROES              TO R3-LINE-AMOUNT                     
182500           IF IN-EKH-KDVALISO = 'MYR'                                     
182600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
182700           END-IF                                                         
182800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
182900           MOVE SPACE               TO R3-LINE-ALLOCATE                   
183000           PERFORM S04-WRITE-W57023A                                      
183100         END-IF                                                           
183200       ELSE                                                               
183300         IF SYST-IDSEKVNR = 2                                             
183400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
183500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
183600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
183700                   IN-EKH-SUBEL                                           
183800           MOVE ZEROES              TO R3-LINE-AMOUNT                     
183900           IF IN-EKH-KDVALISO = 'MYR'                                     
184000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
184100           END-IF                                                         
184200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
184300           MOVE SPACE               TO R3-LINE-ALLOCATE                   
184400           PERFORM S04-WRITE-W57023A                                      
184500         END-IF                                                           
184600       END-IF                                                             
184700     END-EVALUATE                                                         
184800     .                                                                    
184900     EJECT                                                                
185000                                                                          
185100 CED-MAIN-EVENT-201 SECTION.                                              
185200     EVALUATE IN-EKH-KDEKSHT                                              
185300     WHEN '201'                                                           
185400          PERFORM CEDA-SUB-EVENT-201-201                                  
185500     END-EVALUATE                                                         
185600     .                                                                    
185700     EJECT                                                                
185800                                                                          
185900 CEDA-SUB-EVENT-201-201 SECTION.                                          
186000     EVALUATE IN-EKH-KDEKNIVA                                             
186100     WHEN 'DET'                                                           
186200       IF SYST-IDSEKVNR = 1                                               
186300         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
186400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
186500         COMPUTE R3-LINE-AMOUNT-LC =                                      
186600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
186700         IF IN-EKH-KDVALISO = 'MYR'                                       
186800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
186900         END-IF                                                           
187000         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
187100         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
187200         PERFORM S03-WRITE-W57022                                         
187300       END-IF                                                             
187400                                                                          
187500       IF SYST-IDSEKVNR = 2                                               
187600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
187700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
187800         COMPUTE R3-LINE-AMOUNT-LC =                                      
187900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
188000         IF IN-EKH-KDVALISO = 'MYR'                                       
188100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
188200         END-IF                                                           
188300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
188400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
188500         MOVE SPACE               TO WS-ALLOCATE-REF                      
188600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
188700         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
188800         PERFORM S03-WRITE-W57022                                         
188900       END-IF                                                             
189000     END-EVALUATE                                                         
189100     .                                                                    
189200     EJECT                                                                
189300                                                                          
189400 CEF-MAIN-EVENT-203 SECTION.                                              
189500     EVALUATE IN-EKH-KDEKSHT                                              
189600     WHEN '201'                                                           
189700          PERFORM CEFA-SUB-EVENT-203-201                                  
189800     END-EVALUATE                                                         
189900     .                                                                    
190000     EJECT                                                                
190100                                                                          
190200 CEFA-SUB-EVENT-203-201 SECTION.                                          
190300     EVALUATE IN-EKH-KDEKNIVA                                             
190400     WHEN 'DET'                                                           
190500       IF SYST-IDSEKVNR = 1                                               
190600         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
190700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
190800         COMPUTE R3-LINE-AMOUNT-LC =                                      
190900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
191000         IF IN-EKH-KDVALISO = 'MYR'                                       
191100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
191200         END-IF                                                           
191300         MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                  
191400         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
191500         PERFORM S03-WRITE-W57022                                         
191600       END-IF                                                             
191700                                                                          
191800       IF SYST-IDSEKVNR = 2                                               
191900         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
192000         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
192100         COMPUTE R3-LINE-AMOUNT-LC =                                      
192200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
192300         IF IN-EKH-KDVALISO = 'MYR'                                       
192400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
192500         END-IF                                                           
192600         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
192700         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
192800         MOVE SPACE             TO WS-ALLOCATE-REF                        
192900         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
193000         MOVE 0000407554        TO R3-LINE-PA-CUSTOMER                    
193100         PERFORM S03-WRITE-W57022                                         
193200       END-IF                                                             
193300     END-EVALUATE                                                         
193400     .                                                                    
193500     EJECT                                                                
193600                                                                          
193700 CEG-MAIN-EVENT-204 SECTION.                                              
193800     EVALUATE IN-EKH-KDEKSHT                                              
193900     WHEN '201'                                                           
194000          PERFORM CEGA-SUB-EVENT-204-201                                  
194100     WHEN '301'                                                           
194200          PERFORM CEGB-SUB-EVENT-204-301                                  
194300     END-EVALUATE                                                         
194400     .                                                                    
194500     EJECT                                                                
194600                                                                          
194700 CEGA-SUB-EVENT-204-201 SECTION.                                          
194800     EVALUATE IN-EKH-KDEKNIVA                                             
194900     WHEN 'DET'                                                           
195000         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
195100* R-FAKTURA                                                               
195200       IF SYST-IDSEKVNR = 1                                               
195300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
195400         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
195500         COMPUTE R3-LINE-AMOUNT-LC =                                      
195600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
195700         IF IN-EKH-KDVALISO = 'MYR'                                       
195800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
195900         END-IF                                                           
196000         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
196100         MOVE 'MY  '              TO R3-LINE-TRADING-PARTNER              
196200         MOVE SPACE               TO WS-ALLOCATE-DC                       
196300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
196400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
196500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
196600         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
196700         PERFORM S03-WRITE-W57022                                         
196800       END-IF                                                             
196900                                                                          
197000       IF SYST-IDSEKVNR = 2                                               
197100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
197200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
197300         COMPUTE R3-LINE-AMOUNT-LC =                                      
197400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
197500         IF IN-EKH-KDVALISO = 'MYR'                                       
197600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
197700         END-IF                                                           
197800         MOVE SPACE               TO WS-ALLOCATE-DC                       
197900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
198000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
198100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
198200         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
198300         PERFORM S03-WRITE-W57022                                         
198400       END-IF                                                             
198500     END-EVALUATE                                                         
198600     .                                                                    
198700     EJECT                                                                
198800 CEGB-SUB-EVENT-204-301 SECTION.                                          
198900     EVALUATE IN-EKH-KDEKNIVA                                             
199000     WHEN 'DET'                                                           
199100         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
199200* R-FAKTURA                                                               
199300       IF SYST-IDSEKVNR = 1                                               
199400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
199500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
199600         COMPUTE R3-LINE-AMOUNT-LC =                                      
199700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
199800         IF IN-EKH-KDVALISO = 'MYR'                                       
199900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
200000         END-IF                                                           
200100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
200200         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
200300         PERFORM S03-WRITE-W57022                                         
200400       END-IF                                                             
200500                                                                          
200600       IF SYST-IDSEKVNR = 2                                               
200700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
200800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
200900         COMPUTE R3-LINE-AMOUNT-LC =                                      
201000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
201100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
201200         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
201300         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
201400         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
201500         MOVE SPACE             TO WS-ALLOCATE-REF                        
201600         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
201700         PERFORM S03-WRITE-W57022                                         
201800       END-IF                                                             
201900                                                                          
202000       IF SYST-IDSEKVNR = 3                                               
202100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
202200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
202300         COMPUTE R3-LINE-AMOUNT-LC =                                      
202400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
202500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
202600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
202700         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
202800         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
202900         MOVE SPACE             TO WS-ALLOCATE-REF                        
203000         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
203100         PERFORM S03-WRITE-W57022                                         
203200       END-IF                                                             
203300                                                                          
203400     WHEN 'EMB'                                                           
203500     WHEN 'FÖRS'                                                          
203600     WHEN 'FRAKT'                                                         
203700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
203800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
203900       MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                    
204000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
204100               IN-EKH-SUBEL * -1                                          
204200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
204300       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
204400       PERFORM S04-WRITE-W57023A                                          
204500                                                                          
204600     END-EVALUATE                                                         
204700     .                                                                    
204800     EJECT                                                                
204900                                                                          
205000 CEI-MAIN-EVENT-302 SECTION.                                              
205100     EVALUATE IN-EKH-KDEKSHT                                              
205200     WHEN '301'                                                           
205300          PERFORM CEIA-SUB-EVENT-302-301                                  
205400     WHEN '302'                                                           
205500          PERFORM CEIB-SUB-EVENT-302-302                                  
205600     END-EVALUATE                                                         
205700     .                                                                    
205800     EJECT                                                                
205900                                                                          
206000 CEIA-SUB-EVENT-302-301 SECTION.                                          
206100     EVALUATE IN-EKH-KDEKNIVA                                             
206200     WHEN 'DET'                                                           
206300       IF SYST-IDSEKVNR = 1                                               
206400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
206500         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
206600         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
206700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
206800         COMPUTE R3-LINE-AMOUNT-LC =                                      
206900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
207000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
207100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
207200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
207300         MOVE SPACE               TO WS-ALLOCATE-REF                      
207400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
207500         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
207600         PERFORM S02-WRITE-W57021A                                        
207700       END-IF                                                             
207800                                                                          
207900       IF SYST-IDSEKVNR = 2                                               
208000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
208100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
208200         MOVE SPACE           TO R3-LINE-COST-CENTER                      
208300         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
208400         COMPUTE R3-LINE-AMOUNT-LC =                                      
208500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
208600         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
208700         MOVE SPACE               TO WS-LINE-TEXT                         
208800         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
208900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
209000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
209100         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
209200         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
209300         PERFORM S02-WRITE-W57021A                                        
209400       END-IF                                                             
209500     END-EVALUATE                                                         
209600     .                                                                    
209700     EJECT                                                                
209800                                                                          
209900 CEIB-SUB-EVENT-302-302 SECTION.                                          
210000     EVALUATE IN-EKH-KDEKNIVA                                             
210100     WHEN 'DET'                                                           
210200       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
210300         IF SYST-IDSEKVNR = 1                                             
210400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
210500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
210600           IF BET-KDTRADP(3:2) NOT = SPACE                                
210700             MOVE '1'             TO WS-ACCOUNT-4                         
210800           ELSE                                                           
210900             MOVE '3'             TO WS-ACCOUNT-4                         
211000           END-IF                                                         
211100           COMPUTE R3-LINE-AMOUNT-LC  =                                   
211200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
211300           IF IN-EKH-KDVALISO = 'MYR'                                     
211400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
211500           END-IF                                                         
211600           MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                
211700           PERFORM S02-WRITE-W57021A                                      
211800         END-IF                                                           
211900                                                                          
212000         IF SYST-IDSEKVNR = 4                                             
212100           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
212200           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
212300           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
212400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
212500           COMPUTE R3-LINE-AMOUNT-LC  =                                   
212600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
212700           IF IN-EKH-KDVALISO = 'MYR'                                     
212800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
212900           END-IF                                                         
213000           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
213100           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
213200           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
213300           MOVE SPACE             TO WS-ALLOCATE-REF                      
213400           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
213500           MOVE 0000407554        TO R3-LINE-PA-CUSTOMER                  
213600           MOVE 'MY  '            TO R3-LINE-TRADING-PARTNER              
213700                                                                          
213800           PERFORM S02-WRITE-W57021A                                      
213900         END-IF                                                           
214000       ELSE                                                               
214100         IF SYST-IDSEKVNR = 2                                             
214200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
214300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
214400           IF BET-KDTRADP(3:2) NOT = SPACE                                
214500             MOVE '1'             TO WS-ACCOUNT-4                         
214600           ELSE                                                           
214700             MOVE '3'             TO WS-ACCOUNT-4                         
214800           END-IF                                                         
214900           COMPUTE R3-LINE-AMOUNT-LC  =                                   
215000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
215100           IF IN-EKH-KDVALISO = 'MYR'                                     
215200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
215300           END-IF                                                         
215400           MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                
215500           PERFORM S02-WRITE-W57021A                                      
215600         END-IF                                                           
215700                                                                          
215800         IF SYST-IDSEKVNR = 3                                             
215900           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
216000           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
216100           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
216200           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
216300           COMPUTE R3-LINE-AMOUNT-LC  =                                   
216400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
216500           IF IN-EKH-KDVALISO = 'MYR'                                     
216600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
216700           END-IF                                                         
216800           MOVE SYST-IDKST        TO R3-LINE-COST-CENTER                  
216900           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
217000           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
217100           MOVE SPACE             TO WS-ALLOCATE-REF                      
217200           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
217300           MOVE 0000407554        TO R3-LINE-PA-CUSTOMER                  
217400           MOVE 'MY  '            TO R3-LINE-TRADING-PARTNER              
217500           PERFORM S02-WRITE-W57021A                                      
217600         END-IF                                                           
217700       END-IF                                                             
217800     END-EVALUATE                                                         
217900     .                                                                    
218000     EJECT                                                                
218100                                                                          
218200 CEJ-MAIN-EVENT-303 SECTION.                                              
218300     EVALUATE IN-EKH-KDEKSHT                                              
218400     WHEN '3XX'                                                           
218500          PERFORM CEJ301-SUB-EVENT-303-3XX                                
218600     WHEN '301'                                                           
218700          PERFORM CEJ301-SUB-EVENT-303-301                                
218800     WHEN '307'                                                           
218900          PERFORM CEJ307-SUB-EVENT-303-307                                
219000     WHEN '310'                                                           
219100          PERFORM CEJ310-SUB-EVENT-303-310                                
219200     WHEN '311'                                                           
219300          PERFORM CEJ311-SUB-EVENT-303-311                                
219400     WHEN '391'                                                           
219500          PERFORM CEJ301-SUB-EVENT-303-391                                
219600     WHEN '371'                                                           
219700          PERFORM CEJ371-SUB-EVENT-303-371                                
219800     END-EVALUATE                                                         
219900     .                                                                    
220000     EJECT                                                                
220100                                                                          
220200 CEJ371-SUB-EVENT-303-371  SECTION.                                       
220300     EVALUATE IN-EKH-KDEKNIVA                                             
220400     WHEN 'DET'                                                           
220500       IF SYST-IDSEKVNR = 1                                               
220600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
220700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
220800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
220900           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3               
221000         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
221100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
221200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
221300         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
221400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
221500         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
221600         PERFORM S04-WRITE-W57023A                                        
221700       END-IF                                                             
221800                                                                          
221900     WHEN 'LAND'                                                          
222000       IF SYST-IDSEKVNR = 1                                               
222100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
222200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
222300         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
222400         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
222500               R3-LINE-AMOUNT-LC / WS-PRKURS-MY3                          
222600         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
222700         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
222800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
222900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
223000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
223100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
223200         PERFORM S02-WRITE-W57021A                                        
223300       END-IF                                                             
223400                                                                          
223500     WHEN 'DDI'                                                           
223600       IF IN-EKH-SUBEL > ZERO                                             
223700         IF SYST-IDSEKVNR = 1                                             
223800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
223900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
224000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
224100                   IN-EKH-SUBEL                                           
224200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
224300           MOVE SPACE               TO WS-ALLOCATE-DC                     
224400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
224500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
224600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
224700           PERFORM S02-WRITE-W57021A                                      
224800         END-IF                                                           
224900       ELSE                                                               
225000         IF SYST-IDSEKVNR = 2                                             
225100           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
225200           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
225300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
225400                   IN-EKH-SUBEL                                           
225500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
225600           MOVE SPACE               TO WS-ALLOCATE-DC                     
225700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
225800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
225900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
226000           PERFORM S02-WRITE-W57021A                                      
226100         END-IF                                                           
226200       END-IF                                                             
226300     END-EVALUATE                                                         
226400     .                                                                    
226500     EJECT                                                                
226600 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
226700     EVALUATE IN-EKH-KDEKNIVA                                             
226800                                                                          
226900     WHEN 'LAND'                                                          
227000       IF SYST-IDSEKVNR = 1                                               
227100         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
227200         MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                          
227300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
227400                 IN-EKH-SUBEL * -1  / WS-PRKURS-MY3                       
227500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
227600         MOVE SPACE           TO WS-ALLOCATE-DC                           
227700         MOVE SPACE           TO WS-ALLOCATE-DISTR                        
227800         MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                        
227900         MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                         
228000         PERFORM S03-WRITE-W57022                                         
228100       END-IF                                                             
228200                                                                          
228300     WHEN 'DDI'                                                           
228400       IF IN-EKH-SUBEL > ZERO                                             
228500         IF SYST-IDSEKVNR = 1                                             
228600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
228700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
228800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
228900                   IN-EKH-SUBEL                                           
229000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
229100           MOVE SPACE               TO WS-ALLOCATE-DC                     
229200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
229300           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
229400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
229500           PERFORM S04-WRITE-W57023A                                      
229600         END-IF                                                           
229700       ELSE                                                               
229800         IF SYST-IDSEKVNR = 2                                             
229900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
230000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
230100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
230200                   IN-EKH-SUBEL                                           
230300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
230400           MOVE SPACE               TO WS-ALLOCATE-DC                     
230500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
230600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
230700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
230800           PERFORM S04-WRITE-W57023A                                      
230900         END-IF                                                           
231000       END-IF                                                             
231100     END-EVALUATE                                                         
231200     .                                                                    
231300     EJECT                                                                
231400                                                                          
231500 CEJ301-SUB-EVENT-303-301 SECTION.                                        
231600     EVALUATE IN-EKH-KDEKNIVA                                             
231700     WHEN 'DET'                                                           
231800       IF SYST-IDSEKVNR = 1                                               
231900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
232000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
232100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
232200         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3 * -1            
232300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
232400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
232500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
232600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
232700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
232800         MOVE SPACE               TO WS-ALLOCATE-DC                       
232900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
233000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
233100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
233200         PERFORM S03-WRITE-W57022                                         
233300       END-IF                                                             
233400                                                                          
233500     END-EVALUATE                                                         
233600     .                                                                    
233700     EJECT                                                                
233800                                                                          
233900 CEJ307-SUB-EVENT-303-307 SECTION.                                        
234000     EVALUATE IN-EKH-KDEKNIVA                                             
234100     WHEN 'DET'                                                           
234200       IF SYST-IDSEKVNR = 1                                               
234300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
234400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
234500         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
234600         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-MY3 * -1            
234700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
234800         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
234900         MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                   
235000         MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                   
235100         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
235200         MOVE SPACE               TO WS-ALLOCATE-DC                       
235300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
235400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
235500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
235600         PERFORM S03-WRITE-W57022                                         
235700       END-IF                                                             
235800                                                                          
235900     END-EVALUATE                                                         
236000     .                                                                    
236100     EJECT                                                                
236200                                                                          
236300 CEJ310-SUB-EVENT-303-310 SECTION.                                        
236400     EVALUATE IN-EKH-KDEKNIVA                                             
236500     WHEN 'DET'                                                           
236600       IF SYST-IDSEKVNR = 1                                               
236700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
236800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
236900         COMPUTE R3-LINE-AMOUNT-LC =                                      
237000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
237100         IF IN-EKH-KDVALISO = 'MYR'                                       
237200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
237300         END-IF                                                           
237400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
237500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
237600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
237700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
237800         MOVE SPACE               TO WS-LINE-TEXT                         
237900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
238000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
238100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
238200         PERFORM S02-WRITE-W57021A                                        
238300       END-IF                                                             
238400                                                                          
238500       IF SYST-IDSEKVNR = 2                                               
238600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
238700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
238800         COMPUTE R3-LINE-AMOUNT-LC =                                      
238900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
239000         IF IN-EKH-KDVALISO = 'MYR'                                       
239100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
239200         END-IF                                                           
239300         MOVE SPACE               TO WS-LINE-TEXT                         
239400         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
239500         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
239600         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
239700         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
239800         MOVE SPACE               TO WS-ALLOCATE-DC                       
239900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
240000         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
240100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
240200         PERFORM S02-WRITE-W57021A                                        
240300       END-IF                                                             
240400     END-EVALUATE                                                         
240500     .                                                                    
240600     EJECT                                                                
240700                                                                          
240800 CEJ311-SUB-EVENT-303-311 SECTION.                                        
240900     EVALUATE IN-EKH-KDEKNIVA                                             
241000     WHEN 'DET'                                                           
241100        IF SYST-IDSEKVNR = 1                                              
241200          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
241300          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
241400          COMPUTE R3-LINE-AMOUNT-LC =                                     
241500                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
241600          IF IN-EKH-KDVALISO = 'MYR'                                      
241700            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
241800          END-IF                                                          
241900          MOVE SPACE               TO WS-LINE-TEXT                        
242000          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
242100          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
242200          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
242300          MOVE 0000001441          TO R3-LINE-PA-CUSTOMER                 
242400          MOVE SPACE               TO WS-ALLOCATE-DC                      
242500          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
242600          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
242700          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
242800          PERFORM S02-WRITE-W57021A                                       
242900        END-IF                                                            
243000                                                                          
243100        IF SYST-IDSEKVNR = 2                                              
243200          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
243300          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
243400          COMPUTE R3-LINE-AMOUNT-LC =                                     
243500                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
243600          IF IN-EKH-KDVALISO = 'MYR'                                      
243700            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
243800          END-IF                                                          
243900          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
244000          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
244100          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
244200          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
244300          MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                 
244400          MOVE SPACE               TO WS-LINE-TEXT                        
244500          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
244600          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
244700          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
244800          MOVE 0000001441          TO R3-LINE-PA-CUSTOMER                 
244900          PERFORM S02-WRITE-W57021A                                       
245000        END-IF                                                            
245100     END-EVALUATE                                                         
245200     .                                                                    
245300     EJECT                                                                
245400                                                                          
245500 CEJ301-SUB-EVENT-303-391 SECTION.                                        
245600     EVALUATE IN-EKH-KDEKNIVA                                             
245700     WHEN 'DET'                                                           
245800       IF SYST-IDSEKVNR = 1                                               
245900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
246000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
246100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
246200              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
246300         IF IN-EKH-KDVALISO = 'MYR'                                       
246400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
246500         END-IF                                                           
246600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
246700         MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                  
246800         MOVE SPACE               TO WS-LINE-TEXT                         
246900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
247000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
247100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
247200         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
247300         MOVE 'MY  '              TO R3-LINE-TRADING-PARTNER              
247400         MOVE SPACE               TO WS-ALLOCATE-DC                       
247500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
247600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
247700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
247800         PERFORM S03-WRITE-W57022                                         
247900       END-IF                                                             
248000                                                                          
248100       IF SYST-IDSEKVNR = 2                                               
248200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
248300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
248400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
248500              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
248600         IF IN-EKH-KDVALISO = 'MYR'                                       
248700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
248800         END-IF                                                           
248900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
249000         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
249100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
249200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
249300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
249400         MOVE SPACE               TO WS-LINE-TEXT                         
249500         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
249600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
249700         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
249800         MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                  
249900         PERFORM S03-WRITE-W57022                                         
250000       END-IF                                                             
250100                                                                          
250200     END-EVALUATE                                                         
250300     .                                                                    
250400     EJECT                                                                
250500                                                                          
250600 CEK-MAIN-EVENT-401 SECTION.                                              
250700     EVALUATE IN-EKH-KDEKNIVA                                             
250800                                                                          
250900* PRISÄNDRING LÖPANDE                                                     
251000     WHEN 'DET'                                                           
251100       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
251200                           IN-EKH-PRARTSTD                                
251300       IF SYST-IDSEKVNR = 1                                               
251400* PRISHÖJNING                                                             
251500         IF WS-BELOPP > 0                                                 
251600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
251700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
251800           COMPUTE R3-LINE-AMOUNT-LC =                                    
251900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
252000           IF IN-EKH-KDVALISO = 'MYR'                                     
252100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
252200           END-IF                                                         
252300           MOVE SPACES              TO R3-LINE-TRADING-PARTNER            
252400           MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                
252500           PERFORM S02-WRITE-W57021A                                      
252600         END-IF                                                           
252700       END-IF                                                             
252800                                                                          
252900       IF SYST-IDSEKVNR = 2                                               
253000* PRISSÄKNING                                                             
253100         IF WS-BELOPP < 0                                                 
253200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
253300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
253400           COMPUTE R3-LINE-AMOUNT-LC =                                    
253500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
253600           IF IN-EKH-KDVALISO = 'MYR'                                     
253700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
253800           END-IF                                                         
253900           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
254000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
254100           MOVE SPACE               TO WS-ALLOCATE-REF                    
254200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
254300           MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                
254400           PERFORM S02-WRITE-W57021A                                      
254500         END-IF                                                           
254600       END-IF                                                             
254700                                                                          
254800       IF SYST-IDSEKVNR = 3                                               
254900* PRISSÄNKNING                                                            
255000         IF WS-BELOPP < 0                                                 
255100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
255200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
255300           COMPUTE R3-LINE-AMOUNT-LC =                                    
255400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
255500           IF IN-EKH-KDVALISO = 'MYR'                                     
255600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
255700           END-IF                                                         
255800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
255900           MOVE 'MY  '              TO R3-LINE-TRADING-PARTNER            
256000           MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                
256100           PERFORM S02-WRITE-W57021A                                      
256200         END-IF                                                           
256300       END-IF                                                             
256400                                                                          
256500       IF SYST-IDSEKVNR = 4                                               
256600* PRISHÖJNING                                                             
256700         IF WS-BELOPP > 0                                                 
256800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
256900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
257000           COMPUTE R3-LINE-AMOUNT-LC =                                    
257100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
257200           IF IN-EKH-KDVALISO = 'MYR'                                     
257300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
257400           END-IF                                                         
257500           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
257600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
257700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
257800           MOVE SPACE               TO WS-ALLOCATE-REF                    
257900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
258000           MOVE 'MY  '              TO R3-LINE-TRADING-PARTNER            
258100           MOVE 0000407554          TO R3-LINE-PA-CUSTOMER                
258200           PERFORM S02-WRITE-W57021A                                      
258300         END-IF                                                           
258400       END-IF                                                             
258500     END-EVALUATE                                                         
258600     .                                                                    
258700     EJECT                                                                
258800                                                                          
258900 CEL-MAIN-EVENT-402 SECTION.                                              
259000     EVALUATE IN-EKH-KDEKNIVA                                             
259100     WHEN 'DET'                                                           
259200       IF SYST-IDSEKVNR = 1                                               
259300         IF IN-EKH-KVANTAL > 0                                            
259400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
259500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
259600           COMPUTE R3-LINE-AMOUNT-LC =                                    
259700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
259800           IF IN-EKH-KDVALISO = 'MYR'                                     
259900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
260000           END-IF                                                         
260100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
260200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
260300           MOVE SPACE               TO WS-ALLOCATE-REF                    
260400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
260500           PERFORM S02-WRITE-W57021A                                      
260600         END-IF                                                           
260700       END-IF                                                             
260800                                                                          
260900       IF SYST-IDSEKVNR = 2                                               
261000         IF IN-EKH-KVANTAL < 0                                            
261100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
261200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
261300           COMPUTE R3-LINE-AMOUNT-LC =                                    
261400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
261500           IF IN-EKH-KDVALISO = 'MYR'                                     
261600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
261700           END-IF                                                         
261800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
261900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
262000           MOVE SPACE               TO WS-ALLOCATE-REF                    
262100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
262200           PERFORM S02-WRITE-W57021A                                      
262300         END-IF                                                           
262400       END-IF                                                             
262500     END-EVALUATE                                                         
262600     .                                                                    
262700     EJECT                                                                
262800                                                                          
262900 CEM-MAIN-EVENT-403 SECTION.                                              
263000     EVALUATE IN-EKH-KDEKSHT                                              
263100     WHEN '401'                                                           
263200     WHEN '402'                                                           
263300     WHEN '403'                                                           
263400     WHEN '404'                                                           
263500     WHEN '405'                                                           
263600     WHEN '407'                                                           
263700     WHEN '408'                                                           
263800     WHEN '409'                                                           
263900          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
264000     END-EVALUATE                                                         
264100     .                                                                    
264200     EJECT                                                                
264300                                                                          
264400 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
264500     EVALUATE IN-EKH-KDEKNIVA                                             
264600     WHEN 'DET'                                                           
264700       IF IN-EKH-KVANTAL > 0                                              
264800         IF SYST-IDSEKVNR = 1                                             
264900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
265000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
265100           COMPUTE R3-LINE-AMOUNT-LC =                                    
265200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
265300           IF IN-EKH-KDVALISO = 'MYR'                                     
265400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
265500           END-IF                                                         
265600           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
265700           PERFORM S02-WRITE-W57021A                                      
265800         END-IF                                                           
265900                                                                          
266000         IF SYST-IDSEKVNR = 4                                             
266100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
266200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
266300           COMPUTE R3-LINE-AMOUNT-LC =                                    
266400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
266500           IF IN-EKH-KDVALISO = 'MYR'                                     
266600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
266700           END-IF                                                         
266800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
266900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
267000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
267100           MOVE SPACE               TO WS-ALLOCATE-REF                    
267200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
267300           PERFORM S02-WRITE-W57021A                                      
267400         END-IF                                                           
267500       END-IF                                                             
267600                                                                          
267700       IF IN-EKH-KVANTAL < 0                                              
267800         IF SYST-IDSEKVNR = 2                                             
267900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
268000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
268100           COMPUTE R3-LINE-AMOUNT-LC =                                    
268200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
268300           IF IN-EKH-KDVALISO = 'MYR'                                     
268400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
268500           END-IF                                                         
268600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
268700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
268800           MOVE SPACE               TO WS-ALLOCATE-REF                    
268900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
269000           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
269100           PERFORM S02-WRITE-W57021A                                      
269200         END-IF                                                           
269300                                                                          
269400         IF SYST-IDSEKVNR = 3                                             
269500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
269600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
269700           COMPUTE R3-LINE-AMOUNT-LC =                                    
269800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
269900           IF IN-EKH-KDVALISO = 'MYR'                                     
270000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
270100           END-IF                                                         
270200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
270300           PERFORM S02-WRITE-W57021A                                      
270400         END-IF                                                           
270500       END-IF                                                             
270600     END-EVALUATE                                                         
270700     .                                                                    
270800     EJECT                                                                
270900                                                                          
271000 CEN-MAIN-EVENT-404 SECTION.                                              
271100     EVALUATE IN-EKH-KDEKNIVA                                             
271200     WHEN 'DET'                                                           
271300       IF SYST-IDSEKVNR = 1                                               
271400* KONTO EJ MANUELLT REGISTRERAT                                           
271500         IF IN-EKH-IDKONTO = 0                                            
271600           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
271700           IF DIST18-SCRAP-NDC-SC                                         
271800           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
271900           OR DIST18-SCRAP-NDC-QUAL                                       
272000             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
272100             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
272200             COMPUTE R3-LINE-AMOUNT-LC =                                  
272300                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
272400             IF IN-EKH-KDVALISO = 'MYR'                                   
272500               MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                   
272600             END-IF                                                       
272700             MOVE SYST-IDKST          TO R3-LINE-COST-CENTER              
272800             PERFORM S02-WRITE-W57021A                                    
272900           END-IF                                                         
273000         END-IF                                                           
273100       END-IF                                                             
273200                                                                          
273300       IF SYST-IDSEKVNR = 2                                               
273400* KONTO MANUELLT REGISTRERAT                                              
273500         IF IN-EKH-IDKONTO > 0                                            
273600           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
273700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
273800           COMPUTE R3-LINE-AMOUNT-LC =                                    
273900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
274000           IF IN-EKH-KDVALISO = 'MYR'                                     
274100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
274200           END-IF                                                         
274300           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
274400           PERFORM S02-WRITE-W57021A                                      
274500         END-IF                                                           
274600       END-IF                                                             
274700                                                                          
274800       IF SYST-IDSEKVNR = 3                                               
274900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
275000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
275100         COMPUTE R3-LINE-AMOUNT-LC =                                      
275200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
275300         IF IN-EKH-KDVALISO = 'MYR'                                       
275400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
275500         END-IF                                                           
275600         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
275700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
275800         MOVE SPACE               TO WS-ALLOCATE-REF                      
275900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
276000         PERFORM S02-WRITE-W57021A                                        
276100       END-IF                                                             
276200                                                                          
276300     END-EVALUATE                                                         
276400     .                                                                    
276500     EJECT                                                                
276600                                                                          
276700 CF-BUILD-COMMON-210-PART SECTION.                                        
276800     MOVE SPACE              TO R3-LINE-R3                                
276900     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
277000                                R3-LINE-DUE-DATE                          
277100                                R3-LINE-AMOUNT                            
277200                                R3-LINE-AMOUNT-LC                         
277300                                R3-LINE-TAX-AMOUNT                        
277400                                R3-LINE-TAX-AMOUNT-LC                     
277500                                R3-LINE-NUMBER-OF-DAYS                    
277600                                R3-LINE-QUANTITY                          
277700                                R3-LINE-SAMNR                             
277800     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
277900     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
278000     MOVE 'MY04'             TO R3-LINE-COMPANY-CODE                      
278100     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
278200     IF SYST-KDPOST = '31'                                                
278300       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
278400     ELSE                                                                 
278500       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
278600     END-IF                                                               
278700     .                                                                    
278800     EJECT                                                                
278900                                                                          
279000 CG-SCHEDULE-LINE-AP SECTION.                                             
279100     MOVE NEJ                     TO WS-HEADER-SW                         
279200     MOVE JA                      TO WS-LINE-SW                           
279300     EVALUATE IN-EKH-KDEKHHT                                              
279400     WHEN '102'                                                           
279500       IF IN-EKH-KDEKSHT = '130'                                          
279600       OR IN-EKH-KDEKSHT = '134'                                          
279700         IF IN-EKH-KDEKSHT = '130'                                        
279800           PERFORM CGA-MAIN-EVENT-102-130                                 
279900         ELSE                                                             
280000           PERFORM CGA-MAIN-EVENT-102-134                                 
280100         END-IF                                                           
280200       ELSE                                                               
280300         IF IN-EKH-KDEKSHT = '120'                                        
280400         OR IN-EKH-KDEKSHT = '124'                                        
280500         OR IN-EKH-KDEKSHT = '125'                                        
280600           IF IN-EKH-KDEKSHT = '125'                                      
280700             PERFORM CGA-MAIN-EVENT-102-125                               
280800           ELSE                                                           
280900             PERFORM CGA-MAIN-EVENT-102-12X                               
281000           END-IF                                                         
281100         ELSE                                                             
281200           PERFORM CGA-MAIN-EVENT-102                                     
281300         END-IF                                                           
281400       END-IF                                                             
281500     WHEN '103'                                                           
281600         PERFORM CGA-MAIN-EVENT-103                                       
281700     WHEN '303'                                                           
281800       IF IN-EKH-KDEKSHT = '371'                                          
281900         PERFORM S81-GET-CURRENCY-RATE                                    
282000         PERFORM CGA-MAIN-EVENT-303-371                                   
282100       ELSE                                                               
282200         IF IN-EKH-KDEKSHT = '3XX'                                        
282300           PERFORM S81-GET-CURRENCY-RATE                                  
282400           PERFORM CGA-MAIN-EVENT-303-3XX                                 
282500         ELSE                                                             
282600           PERFORM CGA-MAIN-EVENT-303                                     
282700         END-IF                                                           
282800       END-IF                                                             
282900     END-EVALUATE                                                         
283000     .                                                                    
283100     EJECT                                                                
283200                                                                          
283300 CGA-MAIN-EVENT-102     SECTION.                                          
283400     EVALUATE IN-EKH-KDEKNIVA                                             
283500     WHEN 'SUM'                                                           
283600       IF IN-EKH-SUBEL > ZERO                                             
283700         IF SYST-IDSEKVNR = 1                                             
283800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
283900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
284000            IN-EKH-SUBEL                                                  
284100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
284200           PERFORM S10-VATCODE                                            
284300           IF IN-EKH-SUVAT = ZERO                                         
284400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
284500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
284600           ELSE                                                           
284700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
284800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
284900           END-IF                                                         
285000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
285100                                                                          
285200           PERFORM S04-WRITE-W57023A                                      
285300         END-IF                                                           
285400       END-IF                                                             
285500                                                                          
285600       IF IN-EKH-SUBEL < ZERO                                             
285700         IF SYST-IDSEKVNR = 2                                             
285800           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
285900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
286000           IN-EKH-SUBEL                                                   
286100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
286200           PERFORM S10-VATCODE                                            
286300           IF IN-EKH-SUVAT = ZERO                                         
286400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
286500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
286600           ELSE                                                           
286700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
286800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
286900           END-IF                                                         
287000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
287100                                                                          
287200           PERFORM S04-WRITE-W57023A                                      
287300         END-IF                                                           
287400       END-IF                                                             
287500     END-EVALUATE                                                         
287600     .                                                                    
287700     EJECT                                                                
287800                                                                          
287900 CGA-MAIN-EVENT-102-12X SECTION.                                          
288000     EVALUATE IN-EKH-KDEKNIVA                                             
288100     WHEN 'SUM'                                                           
288200       IF IN-EKH-SUBEL > ZERO                                             
288300         IF SYST-IDSEKVNR = 1                                             
288400           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
288500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
288600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
288700                   R3-LINE-AMOUNT    / WS-PRKURS-MY  * -1                 
288800           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
288900           MOVE 'A0'               TO R3-LINE-TAX-CODE                    
289000           IF IN-EKH-SUVAT = ZERO                                         
289100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
289200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
289300           ELSE                                                           
289400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
289500             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
289600                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MY  * -1           
289700             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
289800           END-IF                                                         
289900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
290000                                                                          
290100           PERFORM S04-WRITE-W57023A                                      
290200         END-IF                                                           
290300       END-IF                                                             
290400                                                                          
290500       IF IN-EKH-SUBEL < ZERO                                             
290600         IF SYST-IDSEKVNR = 2                                             
290700           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
290800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
290900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
291000                   R3-LINE-AMOUNT    / WS-PRKURS-MY  * -1                 
291100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
291200           MOVE 'A0'               TO R3-LINE-TAX-CODE                    
291300           IF IN-EKH-SUVAT = ZERO                                         
291400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
291500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
291600           ELSE                                                           
291700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
291800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
291900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MY  * -1           
292000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
292100           END-IF                                                         
292200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
292300           MOVE SPACE           TO R3-LINE-COST-CENTER                    
292400                                                                          
292500           PERFORM S04-WRITE-W57023A                                      
292600         END-IF                                                           
292700       END-IF                                                             
292800     END-EVALUATE                                                         
292900     .                                                                    
293000     EJECT                                                                
293100                                                                          
293200                                                                          
293300 CGA-MAIN-EVENT-102-125 SECTION.                                          
293400     EVALUATE IN-EKH-KDEKNIVA                                             
293500     WHEN 'SUM'                                                           
293600       IF IN-EKH-SUBEL > ZERO                                             
293700         IF SYST-IDSEKVNR = 1                                             
293800           MOVE ZERO TO SPAR-SUMMA-102-125                                
293900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
294000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
294100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
294200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
294300                   R3-LINE-AMOUNT    / WS-PRKURS-MY  * -1                 
294400           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
294500           MOVE 'A0'               TO R3-LINE-TAX-CODE                    
294600           IF IN-EKH-SUVAT = ZERO                                         
294700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
294800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
294900           ELSE                                                           
295000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
295100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
295200               R3-LINE-TAX-AMOUNT / WS-PRKURS-MY  * -1                    
295300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
295400           END-IF                                                         
295500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
295600           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
295700                                                                          
295800           PERFORM S04-WRITE-W57023A                                      
295900         END-IF                                                           
296000       END-IF                                                             
296100                                                                          
296200       IF IN-EKH-SUBEL < ZERO                                             
296300         IF SYST-IDSEKVNR = 2                                             
296400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
296500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
296600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
296700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
296800                   R3-LINE-AMOUNT    / WS-PRKURS-MY  * -1                 
296900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
297000           MOVE 'A0'               TO R3-LINE-TAX-CODE                    
297100           IF IN-EKH-SUVAT = ZERO                                         
297200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
297300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
297400           ELSE                                                           
297500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
297600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
297700               R3-LINE-TAX-AMOUNT / WS-PRKURS-MY * -1                     
297800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
297900           END-IF                                                         
298000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
298100           SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                
298200                                                                          
298300           PERFORM S04-WRITE-W57023A                                      
298400         END-IF                                                           
298500       END-IF                                                             
298600     END-EVALUATE                                                         
298700     .                                                                    
298800     EJECT                                                                
298900 CGA-MAIN-EVENT-102-130 SECTION.                                          
299000     EVALUATE IN-EKH-KDEKNIVA                                             
299100     WHEN 'SUM'                                                           
299200       IF IN-EKH-SUBEL > ZERO                                             
299300         IF SYST-IDSEKVNR = 1                                             
299400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
299500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
299600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
299700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
299800                   R3-LINE-AMOUNT    / WS-PRKURS-MY  * -1                 
299900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
300000           MOVE 'A0'     TO R3-LINE-TAX-CODE                              
300100           IF IN-EKH-SUVAT = ZERO                                         
300200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
300300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
300400           ELSE                                                           
300500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
300600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
300700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MY  * -1           
300800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
300900           END-IF                                                         
301000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
301100                                                                          
301200           PERFORM S04-WRITE-W57023A                                      
301300         END-IF                                                           
301400       END-IF                                                             
301500                                                                          
301600       IF IN-EKH-SUBEL < ZERO                                             
301700         IF SYST-IDSEKVNR = 2                                             
301800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
301900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
302000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
302100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
302200                   R3-LINE-AMOUNT    / WS-PRKURS-MY                       
302300           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
302400           MOVE 'A0'     TO R3-LINE-TAX-CODE                              
302500           IF IN-EKH-SUVAT = ZERO                                         
302600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
302700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
302800           ELSE                                                           
302900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
303000             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
303100                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MY                 
303200             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
303300           END-IF                                                         
303400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
303500                                                                          
303600           PERFORM S04-WRITE-W57023A                                      
303700         END-IF                                                           
303800       END-IF                                                             
303900     END-EVALUATE                                                         
304000     .                                                                    
304100     EJECT                                                                
304200                                                                          
304300 CGA-MAIN-EVENT-102-134 SECTION.                                          
304400     EVALUATE IN-EKH-KDEKNIVA                                             
304500     WHEN 'SUM'                                                           
304600       IF IN-EKH-SUBEL > ZERO                                             
304700         IF SYST-IDSEKVNR = 1                                             
304800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
304900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
305000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
305100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
305200                   R3-LINE-AMOUNT    / WS-PRKURS-MY  * -1                 
305300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
305400           MOVE 'A0'     TO R3-LINE-TAX-CODE                              
305500           IF IN-EKH-SUVAT = ZERO                                         
305600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
305700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
305800           ELSE                                                           
305900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
306000             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
306100                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MY  * -1           
306200             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
306300           END-IF                                                         
306400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
306500                                                                          
306600           PERFORM S04-WRITE-W57023A                                      
306700         END-IF                                                           
306800       END-IF                                                             
306900                                                                          
307000       IF IN-EKH-SUBEL < ZERO                                             
307100         IF SYST-IDSEKVNR = 2                                             
307200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
307300           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
307400           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
307500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
307600                   R3-LINE-AMOUNT    / WS-PRKURS-MY                       
307700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
307800           MOVE 'A0'     TO R3-LINE-TAX-CODE                              
307900           IF IN-EKH-SUVAT = ZERO                                         
308000             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
308100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
308200           ELSE                                                           
308300             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
308400             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
308500                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-MY                 
308600             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
308700           END-IF                                                         
308800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
308900                                                                          
309000           PERFORM S04-WRITE-W57023A                                      
309100         END-IF                                                           
309200       END-IF                                                             
309300     END-EVALUATE                                                         
309400     .                                                                    
309500     EJECT                                                                
309600                                                                          
309700 CGA-MAIN-EVENT-103     SECTION.                                          
309800     EVALUATE IN-EKH-KDEKNIVA                                             
309900     WHEN 'SUM'                                                           
310000       IF IN-EKH-SUBEL > ZERO                                             
310100         IF SYST-IDSEKVNR = 1                                             
310200           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
310300           PERFORM S10-VATCODE                                            
310400           IF IN-EKH-SUVAT = ZERO                                         
310500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
310600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
310700           ELSE                                                           
310800             IF IN-EKH-KDVALISO = 'MYR'                                   
310900               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
311000                                      R3-LINE-TAX-AMOUNT-LC               
311100             ELSE                                                         
311200               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
311300                                      R3-LINE-TAX-AMOUNT-LC               
311400               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
311500                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
311600             END-IF                                                       
311700           END-IF                                                         
311800**** CALCULATE NEW SUM WITH VAT                                           
311900           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
312000                   IN-EKH-SUVAT                                           
312100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
312200           IF IN-EKH-KDVALISO = 'MYR'                                     
312300             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
312400           ELSE                                                           
312500             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
312600                     R3-LINE-AMOUNT * WS-PRKURS                           
312700           END-IF                                                         
312800           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
312900                                                                          
313000           PERFORM S04-WRITE-W57023A                                      
313100         END-IF                                                           
313200       END-IF                                                             
313300                                                                          
313400       IF IN-EKH-SUBEL < ZERO                                             
313500         IF SYST-IDSEKVNR = 2                                             
313600           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
313700           IF IN-EKH-SUVAT = ZERO                                         
313800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
313900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
314000           ELSE                                                           
314100             IF IN-EKH-KDVALISO = 'MYR'                                   
314200               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
314300                                      R3-LINE-TAX-AMOUNT-LC               
314400             ELSE                                                         
314500               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
314600                                      R3-LINE-TAX-AMOUNT-LC               
314700               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
314800                       R3-LINE-TAX-AMOUNT / WS-PRKURS                     
314900             END-IF                                                       
315000           END-IF                                                         
315100           COMPUTE IN-EKH-SUBEL = IN-EKH-SUBEL +                          
315200                   IN-EKH-SUVAT                                           
315300           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
315400           IF IN-EKH-KDVALISO = 'MYR'                                     
315500             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
315600           ELSE                                                           
315700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
315800                     R3-LINE-AMOUNT * WS-PRKURS                           
315900           END-IF                                                         
316000           MOVE 'C030'             TO R3-LINE-PAYTERMS                    
316100                                                                          
316200           PERFORM S04-WRITE-W57023A                                      
316300         END-IF                                                           
316400       END-IF                                                             
316500     END-EVALUATE                                                         
316600     .                                                                    
316700     EJECT                                                                
316800                                                                          
316900 CGA-MAIN-EVENT-303 SECTION.                                              
317000     EVALUATE IN-EKH-KDEKNIVA                                             
317100     WHEN 'SUM'                                                           
317200       IF SYST-IDSEKVNR = 1                                               
317300         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
317400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
317500          IN-EKH-SUBEL / WS-PRKURS-MY                                     
317600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
317700         MOVE '  '     TO R3-LINE-TAX-CODE                                
317800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
317900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
318000                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-MY                     
318100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
318200                                                                          
318300         PERFORM S04-WRITE-W57023A                                        
318400       END-IF                                                             
318500     END-EVALUATE                                                         
318600     .                                                                    
318700     EJECT                                                                
318800                                                                          
318900 CGA-MAIN-EVENT-303-3XX SECTION.                                          
319000     EVALUATE IN-EKH-KDEKNIVA                                             
319100     WHEN 'SUM'                                                           
319200       IF SYST-IDSEKVNR = 1                                               
319300         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
319400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
319500          IN-EKH-SUBEL / WS-PRKURS-MY3                                    
319600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
319700         MOVE '  '     TO R3-LINE-TAX-CODE                                
319800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
319900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
320000                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-MY3                    
320100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
320200                                                                          
320300         PERFORM S04-WRITE-W57023A                                        
320400       END-IF                                                             
320500     END-EVALUATE                                                         
320600     .                                                                    
320700     EJECT                                                                
320800                                                                          
320900 CH-BUILD-COMMON-310-PART SECTION.                                        
321000     MOVE SPACE              TO R3-LINE-R3                                
321100     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
321200                                R3-LINE-DUE-DATE                          
321300                                R3-LINE-AMOUNT                            
321400                                R3-LINE-AMOUNT-LC                         
321500                                R3-LINE-TAX-AMOUNT                        
321600                                R3-LINE-TAX-AMOUNT-LC                     
321700                                R3-LINE-NUMBER-OF-DAYS                    
321800                                R3-LINE-QUANTITY                          
321900                                R3-LINE-SAMNR                             
322000     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
322100     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
322200     MOVE 'MY04'             TO R3-LINE-COMPANY-CODE                      
322300     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
322400     IF SYST-KDPOST = '31'                                                
322500       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
322600     ELSE                                                                 
322700       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
322800     END-IF                                                               
322900     .                                                                    
323000     EJECT                                                                
323100                                                                          
323200 CI-SCHEDULE-LINE-AR SECTION.                                             
323300     MOVE NEJ                     TO WS-HEADER-SW                         
323400     MOVE JA                      TO WS-LINE-SW                           
323500     EVALUATE IN-EKH-KDEKHHT                                              
323600     WHEN '204'                                                           
323700         PERFORM CIA-MAIN-EVENT-204                                       
323800     END-EVALUATE                                                         
323900     .                                                                    
324000     EJECT                                                                
324100                                                                          
324200 CIA-MAIN-EVENT-204     SECTION.                                          
324300     EVALUATE IN-EKH-KDEKNIVA                                             
324400     WHEN 'SUM'                                                           
324500       IF IN-EKH-SUBEL > ZERO                                             
324600         IF SYST-IDSEKVNR = 1                                             
324700           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
324800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
324900           IF IN-EKH-KDVALISO = 'MYR'                                     
325000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
325100           END-IF                                                         
325200           PERFORM S10-VATCODE                                            
325300           IF IN-EKH-SUVAT = ZERO                                         
325400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
325500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
325600           ELSE                                                           
325700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
325800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
325900           END-IF                                                         
326000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
326100                                                                          
326200           PERFORM S04-WRITE-W57023A                                      
326300         END-IF                                                           
326400       END-IF                                                             
326500                                                                          
326600     END-EVALUATE                                                         
326700     .                                                                    
326800     EJECT                                                                
326900                                                                          
327000 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
327100     MOVE ZERO             TO LOGG-W57073                                 
327200     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
327300     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
327400     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
327500     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
327600     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
327700     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
327800     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
327900     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
328000     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
328100     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
328200     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
328300     MOVE 'MY04'           TO LOGG-KDTRADP                                
328400                                                                          
328500****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
328600     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
328700     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
328800     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
328900     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
329000     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
329100     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
329200     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
329300     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
329400     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
329500     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
329600     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
329700     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
329800     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
329900     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
330000     .                                                                    
330100     EJECT                                                                
330200                                                                          
330300 Z-FINI SECTION.                                                          
330400     CLOSE W57066                                                         
330500           W57028                                                         
330600           W57021A                                                        
330700           W57022A                                                        
330800           W57023A                                                        
330900           W57025                                                         
331000           W5702N                                                         
331100           W51320                                                         
331200                                                                          
331300     MOVE 'S' TO POSTSUM-OPKOD                                            
331400     CALL POSTSUM USING POSTSUM-PARM                                      
331500     .                                                                    
331600     EJECT                                                                
331700                                                                          
331800 S01-READ-W57066  SECTION.                                                
331900     READ W57066 INTO IN-AREA                                             
332000     AT END                                                               
332100        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
332200        SET END-OF-W57066 TO TRUE                                         
332300                                                                          
332400     NOT AT END                                                           
332500        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
332600        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
332700        MOVE 'W57028D1'   TO POSTSUM-DDNAMN2                              
332800        CALL POSTSUM USING POSTSUM-PARM                                   
332900     END-READ                                                             
333000     .                                                                    
333100                                                                          
333200 S02-WRITE-W57021A SECTION.                                               
333300     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
333400     MOVE SPACE                 TO 71LINE-POST                            
333500     IF WS-LINE-SW = JA                                                   
333600       IF IN-EKH-KDSORT = 'SW'                                            
333700         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
333800         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
333900         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
334000       ELSE                                                               
334100         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
334200         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
334300         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
334400       END-IF                                                             
334500       WRITE 71LINE-POST        FROM R3-LINE-R3                           
334600       PERFORM S20-CREATE-WRITE-LOG                                       
334700     ELSE                                                                 
334800       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
334900     END-IF                                                               
335000                                                                          
335100     IF WS-LINE-SW = JA                                                   
335200       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
335300     ELSE                                                                 
335400       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
335500     END-IF                                                               
335600     MOVE 'W57021A'             TO POSTSUM-FDNAMN                         
335700     MOVE 'W57028D2'            TO POSTSUM-DDNAMN2                        
335800     CALL POSTSUM USING POSTSUM-PARM                                      
335900     .                                                                    
336000                                                                          
336100 S002-WRITE-W57021A-HEAD SECTION.                                         
336200     MOVE SPACE                 TO 71LINE-POST                            
336300     IF IN-EKH-KDSORT = 'SW'                                              
336400       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
336500       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
336600       MOVE WS-TEXT           TO R3-LINE-TEXT                             
336700     ELSE                                                                 
336800       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
336900       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
337000       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
337100     END-IF                                                               
337200     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
337300                                                                          
337400     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
337500     MOVE 'W57021A'             TO POSTSUM-FDNAMN                         
337600     MOVE 'W57028D2'            TO POSTSUM-DDNAMN2                        
337700     CALL POSTSUM USING POSTSUM-PARM                                      
337800     .                                                                    
337900                                                                          
338000 S03-WRITE-W57022 SECTION.                                                
338100     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
338200     IF IN-EKH-KDSORT = 'SW'                                              
338300       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
338400       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
338500       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
338600     ELSE                                                                 
338700       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
338800       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
338900       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
339000     END-IF                                                               
339100     WRITE 72LINE-POST        FROM R3-LINE-R3                             
339200                                                                          
339300     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
339400     MOVE 'W57022A'           TO POSTSUM-FDNAMN                           
339500     MOVE 'W57028D3'          TO POSTSUM-DDNAMN2                          
339600     CALL POSTSUM USING POSTSUM-PARM                                      
339700                                                                          
339800     PERFORM S20-CREATE-WRITE-LOG                                         
339900     .                                                                    
340000                                                                          
340100 S04-WRITE-W57023A SECTION.                                               
340200     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
340300     MOVE SPACE                 TO 73LINE-POST                            
340400     IF WS-LINE-SW = JA                                                   
340500       IF IN-EKH-KDSORT = 'SW'                                            
340600         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
340700         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
340800         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
340900       ELSE                                                               
341000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
341100         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
341200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
341300       END-IF                                                             
341400       WRITE 73LINE-POST        FROM R3-LINE-R3                           
341500       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
341600     ELSE                                                                 
341700       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
341800       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
341900     END-IF                                                               
342000                                                                          
342100     MOVE 'W57023A'             TO POSTSUM-FDNAMN                         
342200     MOVE 'W57028D4'            TO POSTSUM-DDNAMN2                        
342300     CALL POSTSUM USING POSTSUM-PARM                                      
342400                                                                          
342500     IF WS-LINE-SW = JA                                                   
342600       PERFORM S20-CREATE-WRITE-LOG                                       
342700     END-IF                                                               
342800     .                                                                    
342900                                                                          
343000 S004-WRITE-W57023A-HEAD SECTION.                                         
343100     MOVE SPACE                 TO 73LINE-POST                            
343200     IF IN-EKH-KDSORT = 'SW'                                              
343300       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
343400       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
343500       MOVE WS-TEXT           TO R3-LINE-TEXT                             
343600     ELSE                                                                 
343700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
343800       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
343900       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
344000     END-IF                                                               
344100     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
344200                                                                          
344300     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
344400     MOVE 'W57023A'             TO POSTSUM-FDNAMN                         
344500     MOVE 'W57028D4'            TO POSTSUM-DDNAMN2                        
344600     CALL POSTSUM USING POSTSUM-PARM                                      
344700     .                                                                    
344800                                                                          
344900 S10-VATCODE SECTION.                                                     
345000     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
345100     PERFORM IMS-GU-WDB601                                                
345200     IF DCS-KDDC = SPACE                                                  
345300       MOVE NEJ              TO WDB6-A-SW                                 
345400     ELSE                                                                 
345500       MOVE JA               TO WDB6-A-SW                                 
345600     END-IF                                                               
345700                                                                          
345800     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
345900     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
346000     IF IN-EKH-SUVAT = ZERO                                               
346100       MOVE '  '     TO R3-LINE-TAX-CODE                                  
346200**** HERE WE ADD VAT FOR 10% TO BOOKING                                   
346300**     COMPUTE IN-EKH-SUVAT ROUNDED = IN-EKH-SUBEL * 0.1                  
346400     ELSE                                                                 
346500       MOVE 'A0'     TO R3-LINE-TAX-CODE                                  
346600     END-IF                                                               
346700     IF IN-EKH-BEVAT = 'XX'                                               
346800       MOVE 'A0'     TO R3-LINE-TAX-CODE                                  
346900     END-IF                                                               
347000     .                                                                    
347100     EJECT                                                                
347200                                                                          
347300 S20-CREATE-WRITE-LOG SECTION.                                            
347400     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
347500     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
347600     IF SYST-IDPTYP = '610'                                               
347700       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
347800     ELSE                                                                 
347900       MOVE ZERO                      TO WS-IDLEVNR                       
348000       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
348100                          FOR CHARACTERS BEFORE INITIAL SPACE             
348200       IF WS-IDLEVNR   > ZERO                                             
348300          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
348400                                      TO LOGG-IDKONTO                     
348500       END-IF                                                             
348600     END-IF                                                               
348700     IF R3-LINE-COST-CENTER NOT = SPACE                                   
348800       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
348900     END-IF                                                               
349000     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
349100     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
349200     MOVE R3-LINE-AMOUNT              TO LOGG-SUBEL                       
349300     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
349400     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
349500                                                                          
349600     PERFORM S21-WRITE-W57025                                             
349700     PERFORM S22-WRITE-W57028                                             
349800                                                                          
349900     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
350000       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
350100       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
350200       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
350300                                                                          
350400       PERFORM S21-WRITE-W57025                                           
350500     END-IF                                                               
350600     .                                                                    
350700     EJECT                                                                
350800                                                                          
350900 S21-WRITE-W57025 SECTION.                                                
351000     IF DCS-IDDC NOT = LOGG-IDDC                                          
351100        MOVE LOGG-IDDC TO W-IDDC-B6                                       
351200        PERFORM IMS-GU-WDB601                                             
351300     END-IF                                                               
351400     IF DCS-KDDC = SPACE                                                  
351500       MOVE NEJ              TO WDB6-A-SW                                 
351600     ELSE                                                                 
351700       MOVE JA               TO WDB6-A-SW                                 
351800     END-IF                                                               
351900                                                                          
352000     IF  WDB6-A-FINNS                                                     
352100     AND DCS-DDC                                                          
352200       MOVE 'N'       TO LOGG-FLLSBOK                                     
352300     END-IF                                                               
352400     WRITE LOGG-POST FROM LOGG-W57073                                     
352500                                                                          
352600     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
352700     MOVE 'W57025'    TO POSTSUM-FDNAMN                                   
352800     MOVE 'W57028D5'  TO POSTSUM-DDNAMN2                                  
352900     CALL POSTSUM USING POSTSUM-PARM                                      
353000     .                                                                    
353100                                                                          
353200 S22-WRITE-W57028 SECTION.                                                
353300     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
353400     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
353500     MOVE R3-LINE-AMOUNT        TO AVST-SUBEL                             
353600                                                                          
353700     IF R3-LINE-AMOUNT-SIGN = '+'                                         
353800       IF AVST-SUBEL < +0                                                 
353900         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
354000       END-IF                                                             
354100       IF AVST-KVANTAL < +0                                               
354200         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
354300       END-IF                                                             
354400     ELSE                                                                 
354500       IF AVST-SUBEL > +0                                                 
354600         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
354700       END-IF                                                             
354800       IF AVST-KVANTAL > +0                                               
354900         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
355000       END-IF                                                             
355100     END-IF                                                               
355200                                                                          
355300     IF DCS-IDDC NOT = AVST-IDDC                                          
355400        MOVE AVST-IDDC  TO W-IDDC-B6                                      
355500        PERFORM IMS-GU-WDB601                                             
355600     END-IF                                                               
355700     IF DCS-KDDC = SPACE                                                  
355800       MOVE NEJ              TO WDB6-A-SW                                 
355900     ELSE                                                                 
356000       MOVE JA               TO WDB6-A-SW                                 
356100     END-IF                                                               
356200                                                                          
356300     IF  WDB6-A-FINNS                                                     
356400     AND DCS-DDC                                                          
356500       MOVE 'N'                 TO AVST-FLLSBOK                           
356600     END-IF                                                               
356700                                                                          
356800     IF AVST-IDKONTO(1:4) = '1454'                                        
356900       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
357000       WRITE AVST-POST FROM AVST-W57070                                   
357100                                                                          
357200       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
357300       MOVE 'W57028'            TO POSTSUM-FDNAMN                         
357400       MOVE 'W57028D6'          TO POSTSUM-DDNAMN2                        
357500       CALL POSTSUM USING POSTSUM-PARM                                    
357600     END-IF                                                               
357700     .                                                                    
357800     EJECT                                                                
357900                                                                          
358000 S30-READ-DATABASE-B2-B1 SECTION.                                         
358100     IF IN-EKH-IDLEVNR = '1441'                                           
358200       MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                          
358300     ELSE                                                                 
358400       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
358500       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
358600       PERFORM IMS-GU-WDB201                                              
358700       IF SEGMENT-SAKNAS                                                  
358800         MOVE 'MY99999'       TO W-WDB1-IDPARTNR                          
358900       ELSE                                                               
359000         MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                          
359100       END-IF                                                             
359200     END-IF                                                               
359300     MOVE WC-IDFTG-MY       TO W-WDB1-IDFTG                               
359400     PERFORM IMS-GU-WDB101                                                
359500     IF SEGMENT-SAKNAS                                                    
359600       DISPLAY 'BETALARUPPG. SAKNAS '                                     
359700       DISPLAY IN-EKH-IDVERGL                                             
359800       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
359900       DISPLAY GMT-IDPARTNR                                               
360000                                                                          
360100       MOVE SPACE         TO BET-KDTRADP                                  
360200       MOVE ZERO          TO BET-IDPARTNR                                 
360300       MOVE '????'        TO WS-KDBETVIL                                  
360400       MOVE '???'         TO WS-KDVALISO-WDB1                             
360500     ELSE                                                                 
360600       MOVE BET-KDBETVIL  TO WS-KDBETVIL                                  
360700     END-IF                                                               
360800     MOVE 'MYR'           TO WS-KDVALISO-WDB1                             
360900                                                                          
361000     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
361100     MOVE ZERO TO TALLY                                                   
361200     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
361300                 FOR CHARACTERS BEFORE INITIAL SPACE                      
361400     IF TALLY = ZERO                                                      
361500       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
361600     ELSE                                                                 
361700       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
361800                                TO W-BET-IDPARTNR-NUM                     
361900     END-IF                                                               
362000     .                                                                    
362100     EJECT                                                                
362200                                                                          
362300 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
362400     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
362500     PERFORM IMS-GU-WDB601                                                
362600     IF DCS-KDDC = SPACE                                                  
362700       MOVE NEJ              TO WDB6-A-SW                                 
362800     ELSE                                                                 
362900       MOVE JA               TO WDB6-A-SW                                 
363000     END-IF                                                               
363100                                                                          
363200     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
363300       IF IN-EKH-KDEKSHT NOT = '406'                                      
363400         IF IN-EKH-FLDCET = NEJ                                           
363500           PERFORM S42-SKAPA-RW2-INV-POSTER                               
363600         END-IF                                                           
363700       END-IF                                                             
363800     END-IF                                                               
363900                                                                          
364000     IF IN-EKH-KDEKNIVA = 'DET'                                           
364100       IF  IN-EKH-KDEKHHT = '204'                                         
364200       AND (IN-EKH-KDEKSHT = '201')                                       
364300         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
364400       END-IF                                                             
364500                                                                          
364600       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
364700       AND (WDB6-A-FINNS                                                  
364800       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
364900       OR   DCS-DDC OR DCS-NDC-PF))                                       
365000         PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                              
365100       END-IF                                                             
365200                                                                          
365300       IF IN-FIL-IDPGM = 'W4183000'                                       
365400       AND (WDB6-A-FINNS                                                  
365500       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
365600       OR   DCS-DDC OR DCS-NDC-PF))                                       
365700         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
365800       END-IF                                                             
365900     END-IF                                                               
366000     .                                                                    
366100     EJECT                                                                
366200                                                                          
366300 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
366400     MOVE 'RW2'              TO RW2-IDPTYP                                
366500     MOVE 'RW2'              TO WS-IDPTYP                                 
366600     MOVE ZERO               TO RW2-IDDISTR                               
366700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
366800       MOVE WC-CDC-SE        TO RW2-IDDC                                  
366900     ELSE                                                                 
367000       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
367100     END-IF                                                               
367200     IF IN-EKH-KVANTAL < +0                                               
367300       MOVE '0422'           TO RW2-KDWRTYP                               
367400     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
367500     ELSE                                                                 
367600       MOVE '0421'           TO RW2-KDWRTYP                               
367700       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
367800     END-IF                                                               
367900                                                                          
368000     IF RW2-SUARTSTD NOT = +0                                             
368100       PERFORM S70-WRITE-W51320                                           
368200     END-IF                                                               
368300     .                                                                    
368400     EJECT                                                                
368500                                                                          
368600 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
368700     MOVE '0110'             TO RW1-KDWRTYP                               
368800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
368900       MOVE WC-CDC-SE        TO RW1-IDDC                                  
369000     ELSE                                                                 
369100       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
369200     END-IF                                                               
369300     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
369400     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
369500     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
369600                                                                          
369700     IF RW1-SUARTSTD NOT = +0                                             
369800       MOVE 'RW1' TO WS-IDPTYP                                            
369900       PERFORM S70-WRITE-W51320                                           
370000     END-IF                                                               
370100     .                                                                    
370200     EJECT                                                                
370300                                                                          
370400 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
370500     MOVE '0110'             TO RW1-KDWRTYP                               
370600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
370700       MOVE WC-CDC-SE        TO RW1-IDDC                                  
370800     ELSE                                                                 
370900       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
371000     END-IF                                                               
371100     IF IN-EKH-KDANMORS = '30'                                            
371200       MOVE ZERO             TO RW1-SUARTSTD                              
371300     ELSE                                                                 
371400      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
371500     END-IF                                                               
371600     IF IN-EKH-KDANMORS = '30' OR '80'                                    
371700       MOVE ZERO             TO RW1-SUARTSJK                              
371800     ELSE                                                                 
371900      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
372000     END-IF                                                               
372100     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
372200                                                                          
372300     IF RW1-SUARTSTD NOT = +0                                             
372400       MOVE 'RW1' TO WS-IDPTYP                                            
372500       PERFORM S70-WRITE-W51320                                           
372600     END-IF                                                               
372700     .                                                                    
372800     EJECT                                                                
372900                                                                          
373000 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
373100     MOVE '0110'             TO RW1-KDWRTYP                               
373200     IF DCS-KDDC = SPACE OR DCS-DDC                                       
373300       MOVE WC-CDC-SE        TO RW1-IDDC                                  
373400     ELSE                                                                 
373500       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
373600     END-IF                                                               
373700     IF IN-EKH-KDEKSHT = '310'                                            
373800*** SKROTNING KDANMORS  13 O 23                                           
373900       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
374000     ELSE                                                                 
374100      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
374200     END-IF                                                               
374300                                                                          
374400     MOVE ZERO               TO RW1-SUARTSJK                              
374500                                RW1-SUARTFSG                              
374600     IF RW1-SUARTSTD NOT = +0                                             
374700       MOVE 'RW1' TO WS-IDPTYP                                            
374800       PERFORM S70-WRITE-W51320                                           
374900     END-IF                                                               
375000     .                                                                    
375100     EJECT                                                                
375200                                                                          
375300 S60-WRITE-W5702N SECTION.                                                
375400     WRITE SAPUT-POST  FROM IN-AREA                                       
375500                                                                          
375600     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
375700     MOVE 'W5702N'            TO POSTSUM-FDNAMN                           
375800     MOVE 'W57028D7'          TO POSTSUM-DDNAMN2                          
375900     CALL POSTSUM USING POSTSUM-PARM                                      
376000     .                                                                    
376100     EJECT                                                                
376200                                                                          
376300 S70-WRITE-W51320 SECTION.                                                
376400     IF WS-IDPTYP  = 'RW2'                                                
376500       IF DCS-KDDC = SPACE OR DCS-DDC                                     
376600         MOVE WC-CDC-SE        TO INV-IDDC                                
376700       ELSE                                                               
376800         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
376900       END-IF                                                             
377000       IF IN-EKH-KVANTAL < +0                                             
377100         MOVE '003'            TO INV-IDPTYP                              
377200       COMPUTE INV-SUARTSTD =                                             
377300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
377400       ELSE                                                               
377500         MOVE '002'            TO INV-IDPTYP                              
377600         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
377700       END-IF                                                             
377800       MOVE SPACE TO WS-IDPTYP                                            
377900       MOVE 0                  TO INV-ADLAGOMR                            
378000       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
378100       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
378200     END-IF                                                               
378300     IF WS-IDPTYP  = 'RW1'                                                
378400       IF DCS-KDDC = SPACE OR DCS-DDC                                     
378500         MOVE WC-CDC-SE        TO INV-IDDC                                
378600       ELSE                                                               
378700         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
378800       END-IF                                                             
378900       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
379000       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
379100       MOVE 0                  TO INV-ADLAGOMR                            
379200       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
379300       MOVE '001'              TO INV-IDPTYP                              
379400       MOVE SPACE              TO WS-IDPTYP                               
379500     END-IF                                                               
379600     WRITE INV-POST  FROM INV-W51310                                      
379700                                                                          
379800     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
379900     MOVE 'W51320'            TO POSTSUM-FDNAMN                           
380000     MOVE 'W57028D8'          TO POSTSUM-DDNAMN2                          
380100     CALL POSTSUM USING POSTSUM-PARM                                      
380200     .                                                                    
380300     EJECT                                                                
380400                                                                          
380500 S13-GET-LANDING-COST SECTION.                                            
380600     MOVE '66'                   TO W-IDDC-B6                             
380700     PERFORM IMS-GU-WDB601                                                
380800     IF SEGMENT-FINNS                                                     
380900       PERFORM IMS-GNP-WDB617                                             
381000       IF SEGMENT-FINNS                                                   
381100         IF PROC-TILANDCO > IN-EKH-DAVERDAT                               
381200           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
381300         ELSE                                                             
381400           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
381500         END-IF                                                           
381600       END-IF                                                             
381700     END-IF                                                               
381800     .                                                                    
381900     EJECT                                                                
382000 S80-GET-CURRENCY-RATE SECTION.                                           
382100     MOVE +0                  TO W-ANT                                    
382200     INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS                 
382300             BEFORE INITIAL ' '                                           
382400     MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                             
382500     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
382600     PERFORM IMS-GU-WDL601                                                
382700     IF SEGMENT-SAKNAS                                                    
382800       CONTINUE                                                           
382900     ELSE                                                                 
383000       PERFORM IMS-GNP-WDL611                                             
383100       IF SEGMENT-SAKNAS                                                  
383200         CONTINUE                                                         
383300       ELSE                                                               
383400         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
383500                                   - INL-DAINLEV                          
383600         MOVE WS-FAKTURA-DATUM2   TO WS-FAKTURA-DATUM                     
383700         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
383800         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
383900         MOVE W-DATE-AAMM         TO CURR-TIAAMM                          
384000         MOVE WS-KDVALISO-MY      TO CURR-KDVALISO-ROW                    
384100         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
384200         IF CURR-KDSVAR = ' '                                             
384300           MOVE CURR-PRKURS-NEW   TO WS-PRKURS-MY3                        
384400         ELSE                                                             
384500           MOVE +1                TO WS-PRKURS-MY3                        
384600         END-IF                                                           
384700       END-IF                                                             
384800     END-IF                                                               
384900     .                                                                    
385000     EJECT                                                                
385100                                                                          
385200 S81-GET-CURRENCY-RATE SECTION.                                           
385300     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
385400     MOVE 'MYR'               TO CURR-KDVALISO-ROW                        
385500     IF IN-FIL-IDPGM = 'W4183300'                                         
385600       IF IN-EKH-DAAVIDAT > ZERO                                          
385700         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
385800         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
385900       ELSE                                                               
386000         MOVE WS-TIAA            TO WS-TIAA-CR                            
386100         MOVE WS-TIMM            TO WS-TIMM-CR                            
386200       END-IF                                                             
386300     ELSE                                                                 
386400       MOVE WS-TIAA              TO WS-TIAA-CR                            
386500       MOVE WS-TIMM              TO WS-TIMM-CR                            
386600     END-IF                                                               
386700     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
386800     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
386900     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
387000     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
387100     IF CURR-KDSVAR = ' '                                                 
387200       IF IN-EKH-IDDISTR > ZERO                                           
387300         MOVE CURR-PRKURS-NEW TO WS-PRKURS-MY3                            
387400       ELSE                                                               
387500         IF WS-PRKURS = ZERO                                              
387600           MOVE 1           TO WS-PRKURS-MY3                              
387700         END-IF                                                           
387800       END-IF                                                             
387900     ELSE                                                                 
388000       MOVE 1               TO WS-PRKURS-MY3                              
388100     END-IF                                                               
388200     .                                                                    
388300     EJECT                                                                
388400* --- IMS SECTIONS ---                                                    
388500                                                                          
388600 IMS-GU-WDH521 SECTION.                                                   
388700     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
388800          DELIMITED BY SIZE INTO SSA1                                     
388900     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
389000          DELIMITED BY SIZE INTO SSA2                                     
389100     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
389200          DELIMITED BY SIZE INTO SSA3                                     
389300     MOVE '  '              TO GODK-STATUSKODER                           
389400     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
389500                                                   SSA2                   
389600                                                   SSA3                   
389700     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
389800                                                                          
389900     PERFORM IMS-STATUS-CONTROL                                           
390000     .                                                                    
390100                                                                          
390200 IMS-GNP-WDH531 SECTION.                                                  
390300     MOVE 'WDH531  '        TO SSA1                                       
390400     MOVE '  GE'            TO GODK-STATUSKODER                           
390500     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
390600     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
390700                               WS-STATUS                                  
390800     PERFORM IMS-STATUS-CONTROL                                           
390900     .                                                                    
391000     EJECT                                                                
391100                                                                          
391200 IMS-GU-WDB201 SECTION.                                                   
391300     STRING 'WDB201  (IDGMT    =' W-IDGMT-KEY ')'                         
391400          DELIMITED BY SIZE INTO SSA1                                     
391500     MOVE '  GE'                 TO GODK-STATUSKODER                      
391600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
391700      MOVE WDB2-STATUS-CODE      TO STATUS-WS                             
391800     PERFORM IMS-STATUS-CONTROL                                           
391900     .                                                                    
392000     EJECT                                                                
392100                                                                          
392200 IMS-GU-WDB101 SECTION.                                                   
392300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
392400          DELIMITED BY SIZE INTO SSA1                                     
392500     MOVE '  GE'               TO GODK-STATUSKODER                        
392600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
392700     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
392800     PERFORM IMS-STATUS-CONTROL                                           
392900     .                                                                    
393000     EJECT                                                                
393100                                                                          
393200 IMS-GU-WDB601    SECTION.                                                
393300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
393400          DELIMITED BY SIZE INTO SSA1                                     
393500     MOVE '  GE' TO GODK-STATUSKODER                                      
393600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
393700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
393800     PERFORM IMS-STATUS-CONTROL                                           
393900     IF SEGMENT-SAKNAS                                                    
394000        MOVE SPACE TO DCS-KDDC                                            
394100     END-IF                                                               
394200     .                                                                    
394300     EJECT                                                                
394400                                                                          
394500 IMS-GNP-WDB617 SECTION.                                                  
394600     MOVE 'WDB617   ' TO SSA1                                             
394700     MOVE '  GE'        TO GODK-STATUSKODER                               
394800     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
394900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
395000     PERFORM IMS-STATUS-CONTROL                                           
395100     .                                                                    
395200     SKIP3                                                                
395300                                                                          
395400 IMS-GU-WDL601   SECTION.                                                 
395500     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
395600          DELIMITED BY SIZE INTO SSA1                                     
395700     MOVE '  GE' TO GODK-STATUSKODER                                      
395800     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
395900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
396000     PERFORM IMS-STATUS-CONTROL                                           
396100     .                                                                    
396200     SKIP3                                                                
396300                                                                          
396400 IMS-GNP-WDL611   SECTION.                                                
396500     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
396600          DELIMITED BY SIZE INTO SSA1                                     
396700     MOVE '  GE' TO GODK-STATUSKODER                                      
396800     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
396900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
397000     PERFORM IMS-STATUS-CONTROL                                           
397100     .                                                                    
397200     SKIP3                                                                
397300                                                                          
397400 IMS-STATUS-CONTROL SECTION.                                              
397500     SET STATUS-IX TO 1                                                   
397600     SEARCH GODK-STATUS                                                   
397700       AT END                                                             
397800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
397900           DELIMITED BY SIZE INTO FELTEXT                                 
398000         DISPLAY FELTEXT                                                  
398100         CALL FELLOG                                                      
398200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
398300         CONTINUE                                                         
398400     END-SEARCH                                                           
398500     .                                                                    
