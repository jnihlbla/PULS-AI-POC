000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5706800.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   20120113.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
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
004500     SELECT W57066                     ASSIGN TO W57068D1.                
004600                                                                          
004700*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
004800     SELECT W57071A                    ASSIGN TO W57068D2.                
004900                                                                          
005000*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
005100     SELECT W57072A                    ASSIGN TO W57068D3.                
005200                                                                          
005300*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
005400     SELECT W57073A                    ASSIGN TO W57068D4.                
005500                                                                          
005600*          --- LOGG TILL ON-DEMAND                                        
005700     SELECT W57075                     ASSIGN TO W57068D5.                
005800                                                                          
005900*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
006000     SELECT W57070                     ASSIGN TO W57068D6.                
006100                                                                          
006200*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
006300     SELECT W5706N                     ASSIGN TO W57068D7.                
006400                                                                          
006500*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
006600     SELECT W51310                     ASSIGN TO W57068D8.                
006700     EJECT                                                                
006800                                                                          
006900 DATA DIVISION.                                                           
007000                                                                          
007100 FILE SECTION.                                                            
007200 FD  W57066                                                               
007300     RECORDING       F                                                    
007400     BLOCK CONTAINS  0.                                                   
007500 01  SAP-POST.                                                            
007600*    03  -COPY WDR801        -L.                                          
007700     03 FILLER                   PIC X(6).                                
007800                                                                          
007900 FD  W57071A                                                              
008000     RECORDING       V                                                    
008100     BLOCK CONTAINS  0.                                                   
008200*01  71INIT-POST -COPY R3INIT20               -L.                         
008300*01  71HEAD-POST -COPY R3HEAD20               -L.                         
008400*01  71LINE-POST -COPY R3LINE20               -L.                         
008500                                                                          
008600 FD  W57072A                                                              
008700     RECORDING       F                                                    
008800     BLOCK CONTAINS  0.                                                   
008900*01  72LINE-POST -COPY R3LINE20               -L.                         
009000                                                                          
009100 FD  W57073A                                                              
009200     RECORDING       V                                                    
009300     BLOCK CONTAINS  0.                                                   
009400*01  73HEAD-POST -COPY R3HEAD20               -L.                         
009500*01  73LINE-POST -COPY R3LINE20               -L.                         
009600                                                                          
009700 FD  W57075                                                               
009800     RECORDING       F                                                    
009900     BLOCK CONTAINS  0.                                                   
010000*01  LOGG-POST   -COPY W57073                 -L.                         
010100                                                                          
010200 FD  W57070                                                               
010300     RECORDING       F                                                    
010400     BLOCK CONTAINS  0.                                                   
010500*01  AVST-POST   -COPY W57070                 -L.                         
010600                                                                          
010700 FD  W5706N                                                               
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
012300 77  IDPGM                        PIC X(8)    VALUE 'W5706800'.           
012400 77  JA                           PIC X       VALUE 'J'.                  
012500 77  NEJ                          PIC X       VALUE 'N'.                  
012600 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
012700 77  W57066-EOF-SW                PIC X       VALUE 'N'.                  
012800     88  END-OF-W57066                        VALUE 'J'.                  
012900 77  WS-HEADER-SW                 PIC X       VALUE 'N'.                  
013000 77  WS-LINE-SW                   PIC X       VALUE 'N'.                  
013100 77  WS-STATUS                    PIC XX      VALUE '  '.                 
013200 77  WS-LINE-AMOUNT               PIC S9(13)V99 COMP-3.                   
013300 77  WS-LINE-AMOUNT-121-1         PIC S9(13)V99 COMP-3.                   
013400 77  WS-LINE-AMOUNT-121-2         PIC S9(13)V99 COMP-3.                   
013500 77  WS-BELOPP                    PIC S9(7)V99  COMP-3.                   
013600 77  WS-LOP                       PIC 9       VALUE ZERO.                 
013700 77  WS-SPAR-KDEKHHT              PIC X(3) VALUE SPACE.                   
013800 77  WS-SPAR-KDEKSHT              PIC X(3) VALUE SPACE.                   
013900 77  WS-SPAR-IDVERGL            PIC X(10)            VALUE SPACE.         
014000 77  WS-SPAR-DAVERDAT             PIC 9(8) VALUE ZERO.                    
014100 77  SPAR-SUMMA-102-125         PIC S9(13)V99  COMP-3 VALUE ZERO.         
014200 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
014300 77  SPAR-LINE-ORDER              PIC X(12).                              
014400 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
014500 77  WS-RED-IDKST                 PIC X(10).                              
014600 77  WS-IDPTYP                    PIC X(3).                               
014700 77  WS-FAKTURA-DATUM             PIC X(16).                              
014800 77  WS-FAKTURA-DATUM2            PIC S9(16) COMP-3 VALUE ZERO.           
014900 77  SPAR-SUMMA                   PIC S9(9)V99  COMP-3 VALUE ZERO.        
015000 77  SPAR-PRDMTRL                 PIC S9(9)V99  COMP-3 VALUE ZERO.        
015100 77  SPAR-PROVRPAL                PIC S9(9)V99  COMP-3 VALUE ZERO.        
015200 77  SPAR-PRDIRLON                PIC S9(9)V99  COMP-3 VALUE ZERO.        
015300 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
015400 77  WS-KDTRADP                   PIC X(4)    VALUE SPACES.               
015500 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
015600 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
015700 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
015800 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
015900 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
016000 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
016100 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
016200                                                                          
016300 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
016400       88  WDB6-A-FINNS                       VALUE 'J'.                  
016500       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
016600                                                                          
016700*01  -COPY WWDCKONS                                                       
016800     EJECT                                                                
016900                                                                          
017000 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
017100*01  -COPY WWIDFTG                                                        
017200     EJECT                                                                
017300                                                                          
017400 01  FELTEXT                      PIC X(80).                              
017500 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
017600*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
017700     EJECT                                                                
017800                                                                          
017900*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
018000     EJECT                                                                
018100                                                                          
018200 01  TEST-IDARTNR                 PIC 9(9) COMP-3.                        
018300*01  FILLER  -COPY WWBYT24    -RED TEST-IDARTNR                           
018400     EJECT                                                                
018500                                                                          
018600 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
018700 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
018800     EJECT                                                                
018900 01  WS-IDDISTR-IDKUNDNR.                                                 
019000     03  FILLER                   PIC X(2)    VALUE SPACE.                
019100     03  WS-IDDISTR               PIC 9(4).                               
019200     03  WS-IDKUNDNR              PIC 9(6).                               
019300                                                                          
019400 01  WS-KDBETVIL                  PIC X(4).                               
019500 01  WS-KDVALISO-WDB1             PIC X(3).                               
019600 01  WS-KDVALISO                  PIC X(3).                               
019700 01  WS-KDVALISO-CN               PIC X(3) VALUE 'CNY'.                   
019800 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
019900 01  WS-PRKURS-CN                 PIC S9(6)V9(5) COMP-3.                  
020000 01  WS-PRKURS-CN2                PIC S9(6)V9(5) COMP-3.                  
020100 01  WS-PRKURS-CN3                PIC S9(6)V9(5) COMP-3.                  
020200 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
020300 01  W-ANT                        PIC S9(3)   VALUE ZERO COMP-3.          
020400                                                                          
020500 01  WS-DATE-YYMMDD            PIC 9(06).                                 
020600 01  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                              
020700     03  WS-DATE-YYMM          PIC 9(04).                                 
020800     03  WS-DATE-DD            PIC 9(02).                                 
020900                                                                          
021000 01  W-PRKURS            PIC S9(5)V9(5) VALUE +0   COMP-3.                
021100 01  W-REVALUTA          PIC S9(5)      VALUE +0   COMP-3.                
021200                                                                          
021300 01  WS-ALLOCATE.                                                         
021400     03  WS-ALLOCATE-DC           PIC X(2).                               
021500     03  WS-ALLOCATE-DISTR        PIC X(5).                               
021600     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
021700     03  FILLER                   PIC X(4)    VALUE SPACE.                
021800                                                                          
021900 01  WS-TEXT.                                                             
022000     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
022100     03  WS-TEXT-KDEKHHT          PIC X(3).                               
022200     03  WS-TEXT-KDEKSHT          PIC X(3).                               
022300     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
022400     03  FILLER                   PIC X(7)    VALUE SPACE.                
022500                                                                          
022600 01  WS-LINE-TEXT.                                                        
022700     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
022800     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
022900     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
023000     03  WS-LINE-TEXT-IDKUNDRF    PIC X(10).                              
023100     03  WS-LINE-TEXT-IDLEVNR     PIC X(05).                              
023200     03  WS-LINE-TEXT-IDVERGL     PIC X(10).                              
023300     03  FILLER                   PIC X(17)   VALUE SPACE.                
023400                                                                          
023500 01  WS-PRCTR-PRODSL-DISP         PIC 9(2).                               
023600 01  WS-PRCTR.                                                            
023700     03  WS-PRCTR-PRODSL          PIC X(2).                               
023800     03  FILLER                   PIC X(1).                               
023900     03  FILLER                   PIC X(7).                               
024000                                                                          
024100 01  WS-R3-ACCOUNT.                                                       
024200     03  WS-R3-ACCOUNT-ALFA.                                              
024300         05 FILLER                PIC X(4).                               
024400         05 WS-R3-ACCOUNT-6       PIC X(6).                               
024500     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
024600         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
024700                                                                          
024800 01  WS-ACCOUNT.                                                          
024900     03  FILLER                   PIC X(7).                               
025000     03  WS-ACCOUNT-4             PIC X(1).                               
025100     03  FILLER                   PIC X(2).                               
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
028500     03  W009CIA                  PIC X(8)    VALUE 'W009CIA'.            
028600     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
028700                                                                          
028800*    --- PARAMETRAR TILL ABEND                                            
028900 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
029000 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
029100 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
029200     EJECT                                                                
029300                                                                          
029400*    --- PARAMETRAR TILL DATKORT                                          
029500 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W57068'.             
029600                                                                          
029700 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
029800*01  -COPY WDATKORT                                                       
029900     EJECT                                                                
030000                                                                          
030100*    --- PARAMETRAR TILL POSTSUM                                          
030200*01  -COPY W0005   -PRE  POSTSUM-                                         
030300     EJECT                                                                
030400                                                                          
030500 01  FILLER                       PIC X(16) VALUE 'W510CURR '.            
030600*01  -COPY W510CURR                                                       
030700     EJECT                                                                
030800                                                                          
030900*    --- PARAMETRAR TILL W009CIA                                          
031000*01  -COPY W009CIA                                                        
031100     EJECT                                                                
031200                                                                          
031300 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
031400*01  AREA -COPY WDR801           -PRE IN-                                 
031500*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR801-DATA         
031600         05   IN-EKH-IDSYSMOT     PIC X(6).                               
031700                                                                          
031800     EJECT                                                                
031900 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
032000                                                                          
032100*01  -COPY R3LINE20              -PRE R3-                                 
032200*01  -COPY R3HEAD20              -PRE R3-                                 
032300*01  -COPY R3INIT20              -PRE R3-                                 
032400*01  -COPY W57073                -PRE LOGG-                               
032500*01  -COPY W57070                -PRE AVST-                               
032600*01  -COPY W517RW1               -PRE RW1-                                
032700*01  -COPY W517RW2               -PRE RW2-                                
032800*01  -COPY W51310                -PRE INV-                                
032900     EJECT                                                                
033000                                                                          
033100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033200 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
033300                                                                          
033400 01  NYCKLAR-TILL-DLI.                                                    
033500     03  W-WDH501KY-X.                                                    
033600         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
033700         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
033800     03  W-KDEKSHT-X.                                                     
033900         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
034000     03  W-KDEKNIVA-X.                                                    
034100         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
034200     03  W-WDH531KY-X.                                                    
034300         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
034400         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
034500     03  W-IDRADNR-X.                                                     
034600         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
034700                                                                          
034800     03  W-IDGMT-KEY.                                                     
034900         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
035000         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
035100                                                                          
035200     03  W-WDB101KY-X.                                                    
035300         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
035400         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
035500                                                                          
035600     03  W-WDGXKEY-5121-X.                                                
035700         05  FILLER               PIC X(4)    VALUE '5121'.               
035800         05  FILLER               PIC X(2)    VALUE '60'.                 
035900         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
036000     03  W-WDGXKEY-5122-X.                                                
036100         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
036200         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
036300     03  W-WDGXKEY-5122-MIN-X.                                            
036400         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
036500         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
036600     03  W-WDGXKEY-5122-MAX-X.                                            
036700         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
036800         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
036900                                                                          
037000     03  W-IDDC-B6-X.                                                     
037100         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
037200                                                                          
037300     03  W-IDARTNR-X.                                                     
037400         05 W-IDARTNR             PIC S9(9) COMP-3.                       
037500                                                                          
037600     03  W-IDFAKT-X.                                                      
037700         05 W-IDFAKT              PIC S9(7) COMP-3.                       
037800                                                                          
037900     03  W-WDGX9305-X.                                                    
038000         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
038100         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
038200         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
038300         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
038400     03  W-KDVALISO-X.                                                    
038500         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
038600     03  W-TISTADA9-X.                                                    
038700         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
038800     03  W-WDGX5123-X.                                                    
038900         05  W-IDHTYP            PIC X(4)    VALUE '5123'.                
039000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
039100     03  W-KY5124-X.                                                      
039200         05  W-IDDISTR-5124      PIC S9(5)   VALUE ZERO COMP-3.           
039300         05  W-IDKUNDNR-5124     PIC S9(7)   VALUE ZERO COMP-3.           
039400                                                                          
039500     EJECT                                                                
039600                                                                          
039700*    --- STATUS-KOD FRÅN IMS                                              
039800 01  STATUS-WS                    PIC XX.                                 
039900     88  SEGMENT-FINNS                        VALUE '  '.                 
040000     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
040100                                                                          
040200 01  GODK-STATUSKODER.                                                    
040300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040400                                                                          
040500 01  SSA1                         PIC X(128).                             
040600 01  SSA2                         PIC X(64).                              
040700 01  SSA3                         PIC X(64).                              
040800     EJECT                                                                
040900                                                                          
041000*    --- IMS FUNKTIONSKODER                                               
041100*01  -COPY W0003                                                          
041200     EJECT                                                                
041300                                                                          
041400*    ---  DLI INPUT-OUTPUT AREA                                           
041500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
041600 01  DLI-IO-WDH501.                                                       
041700*    03  -COPY WDH501                                                     
041800     EJECT                                                                
041900                                                                          
042000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
042100 01  DLI-IO-WDH511.                                                       
042200*    03  -COPY WDH511                                                     
042300     EJECT                                                                
042400                                                                          
042500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
042600 01  DLI-IO-WDH521.                                                       
042700*    03  -COPY WDH521                                                     
042800     EJECT                                                                
042900                                                                          
043000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
043100 01  DLI-IO-WDH531.                                                       
043200*    03  -COPY WDH531                                                     
043300     EJECT                                                                
043400                                                                          
043500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
043600 01  DLI-IO-WLBETC01.                                                     
043700*    03  -COPY WDB101                                                     
043800     EJECT                                                                
043900                                                                          
044000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
044100 01  DLI-IO-WLGMTA01.                                                     
044200*    03  -COPY WDB201                                                     
044300     EJECT                                                                
044400                                                                          
044500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
044600 01  DLI-IO-WDGX5121.                                                     
044700*    03  -COPY WDGX5121                                                   
044800     EJECT                                                                
044900                                                                          
045000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
045100 01  DLI-IO-WDGX5122.                                                     
045200*    03  -COPY WDGX5122                                                   
045300     EJECT                                                                
045400                                                                          
045500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
045600 01   DLI-IO-AREA-B601.                                                   
045700*     03  -COPY WDB601                                                    
045800     EJECT                                                                
045900 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
046000 01   DLI-IO-AREA-B617.                                                   
046100*     03  -COPY WDB617                                                    
046200     EJECT                                                                
046300 01  FILLER               PIC X(16)   VALUE 'WDL601 AREA'.                
046400 01   DLI-IO-AREA-L601.                                                   
046500*     03  -COPY WDL601                                                    
046600     EJECT                                                                
046700 01  FILLER               PIC X(16)   VALUE 'WDL611 AREA'.                
046800 01   DLI-IO-AREA-L611.                                                   
046900*     03  -COPY WDL611                                                    
047000     EJECT                                                                
047100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
047200 01  DLI-IO-WDGX9306.                                                     
047300*    03  -COPY WDGX9306                                                   
047400                                                                          
047500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
047600 01  DLI-IO-WDGX9308.                                                     
047700*    03  -COPY WDGX9308                                                   
047800                                                                          
047900 01  DLI-IO-WDGX5124.                                                     
048000*    03  -COPY WDGX5124                                                   
048100     EJECT                                                                
048200                                                                          
048300 LINKAGE SECTION.                                                         
048400*01  -COPY W0008  -PRE WDH5-                                              
048500     05  FILLER                  PIC X.                                   
048600                                                                          
048700*01  -COPY W0008  -PRE GMTA-                                              
048800     05  FILLER                  PIC X.                                   
048900                                                                          
049000*01  -COPY W0008  -PRE BETC-                                              
049100     05  FILLER                  PIC X.                                   
049200                                                                          
049300*01  -COPY W0008  -PRE 5121-                                              
049400     05  FILLER                  PIC X.                                   
049500                                                                          
049600*01  -COPY W0008  -PRE WDB6-                                              
049700     05  FILLER                  PIC X.                                   
049800                                                                          
049900*01  -COPY W0008  -PRE WDL6-                                              
050000     05  FILLER                  PIC X.                                   
050100*01  -COPY W0008  -PRE 9305-                                              
050200     05  FILLER                  PIC X.                                   
050300     EJECT                                                                
050400*01  -COPY W0008  -PRE 5124-                                              
050500     05  FILLER                  PIC X.                                   
050600     EJECT                                                                
050700                                                                          
050800 PROCEDURE DIVISION  USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
050900                           WDB6-PCB WDL6-PCB                              
051000                           9305-PCB 5124-PCB.                             
051100 MAIN SECTION.                                                            
051200     ENTRY 'DLITCBL' USING WDH5-PCB GMTA-PCB BETC-PCB 5121-PCB            
051300                           WDB6-PCB WDL6-PCB                              
051400                           9305-PCB 5124-PCB.                             
051500     PERFORM A-INIT                                                       
051600                                                                          
051700     PERFORM S01-READ-W57066                                              
051800     PERFORM UNTIL END-OF-W57066                                          
051900*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
052000       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
052100       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
052200       AND WS-NEW-MONTH > 01                                              
052300         PERFORM S60-WRITE-W5706N                                         
052400       ELSE                                                               
052500         PERFORM S40-SKAPA-W517-OCH-MON-POSTER                            
052600         PERFORM S30-READ-DATABASE-B2-B1                                  
052700         IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                           
052800           PERFORM C-EXECUTE                                              
052900         END-IF                                                           
053000       END-IF                                                             
053100       PERFORM S01-READ-W57066                                            
053200     END-PERFORM                                                          
053300                                                                          
053400     PERFORM Z-FINI                                                       
053500                                                                          
053600     MOVE ZERO TO RETURN-CODE                                             
053700     GOBACK                                                               
053800     .                                                                    
053900     EJECT                                                                
054000                                                                          
054100 A-INIT SECTION.                                                          
054200     OPEN INPUT  W57066                                                   
054300                                                                          
054400     OPEN OUTPUT W57070                                                   
054500                 W57071A                                                  
054600                 W57072A                                                  
054700                 W57073A                                                  
054800                 W57075                                                   
054900                 W5706N                                                   
055000                 W51310                                                   
055100                                                                          
055200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
055300     MOVE 20               TO RW1-DAVVREG(1:2)                            
055400     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
055500                              RW1-DAVVREG(3:2)                            
055600                              W-DATE-AAMM(1:2)                            
055700                              WS-TIAA                                     
055800     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
055900                              W-DATE-AAMM(3:2)                            
056000                              WS-NEW-MONTH                                
056100                              WS-TIMM                                     
056200     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
056300     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
056400     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
056500                                                                          
056600*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
056700*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
056800     IF WS-NEW-MONTH = 12                                                 
056900       MOVE 1              TO WS-NEW-MONTH                                
057000     ELSE                                                                 
057100       ADD 1               TO WS-NEW-MONTH                                
057200*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
057300***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
057400***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
057500***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
057600***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
057700       IF  FUNCTION CURRENT-DATE(7:2) = 02                                
057800       AND WS-NEW-MONTH = FUNCTION CURRENT-DATE(5:2)                      
057900         ADD 1             TO WS-NEW-MONTH                                
058000         ADD 1             TO WS-TIMM                                     
058100         MOVE WS-NEW-MONTH TO W-DATE-AAMM(3:2)                            
058200       END-IF                                                             
058300     END-IF                                                               
058400                                                                          
058500     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
058600                                                                          
058700     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
058800                                                                          
058900     ACCEPT DAGENS-KLOCKA FROM TIME                                       
059000     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
059100                                                                          
059200**** TA FRAM MÅNADENS FÖRSTA DAG                                          
059300     MOVE WS-DAREGDAT-AAMMDD(1:4)     TO WS-DATE-YYMM                     
059400     MOVE 01                          TO WS-DATE-DD                       
059500                                                                          
059600     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
059700     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
059800     MOVE 'M'                   TO CURR-KDVALTYP                          
059900                                                                          
060000**** DETTA ÄR KURSERNA FÖR ATT TA EMOT SAKER FRÅN VCCS                    
060100     MOVE WS-KDVALISO-CN        TO CURR-KDVALISO-ROW                      
060200     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
060300     IF CURR-KDSVAR = ' '                                                 
060400       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-CN                           
060500     ELSE                                                                 
060600       MOVE 1                   TO WS-PRKURS-CN                           
060700     END-IF                                                               
060800     COMPUTE WS-PRKURS-CN2 ROUNDED = 1 / WS-PRKURS-CN                     
060900     MOVE WS-PRKURS-CN          TO WS-PRKURS-CN3                          
061000     .                                                                    
061100     EJECT                                                                
061200                                                                          
061300 C-EXECUTE SECTION.                                                       
061400     MOVE WC-IDFTG-CN           TO W-IDFTG                                
061500     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
061600     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
061700     IF IN-EKH-KDEKNIVA = 'TDET'                                          
061800       MOVE 'DET'               TO IN-EKH-KDEKNIVA                        
061900     END-IF                                                               
062000     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
062100     PERFORM IMS-GU-WDH521                                                
062200     PERFORM IMS-GNP-WDH531                                               
062300                                                                          
062400     PERFORM S13-GET-LANDING-COST                                         
062500                                                                          
062600     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
062700     IF IN-EKH-IDDISTR > ZERO                                             
062800       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
062900     ELSE                                                                 
063000       MOVE IN-EKH-KDVALISO     TO WS-KDVALISO                            
063100     END-IF                                                               
063200     IF WS-KDVALISO = 'CNY'                                               
063300       MOVE IN-EKH-PRKURS         TO WS-PRKURS                            
063400       MOVE IN-EKH-PRKURS         TO W-PRKURS                             
063500     ELSE                                                                 
063600**** HÄMTA DC HUVUDVALUTA                                                 
063700       MOVE 'CNY'      TO W-KDVALISO-HUV                                  
063800       MOVE WS-KDVALISO      TO W-KDVALISO-ROW                            
063900       PERFORM IMS-GU-WDGX9306                                            
064000       IF SEGMENT-SAKNAS                                                  
064100         MOVE 1               TO W-PRKURS                                 
064200         MOVE 1               TO W-REVALUTA                               
064300       ELSE                                                               
064400         COMPUTE W-TISTADAT-9KOMPL =                                      
064500                 9999999 - WS-DATE-YYMMDD                                 
064600         PERFORM IMS-GNP-WDGX9308                                         
064700         IF SEGMENT-SAKNAS                                                
064800           PERFORM IMS-GNP-WDGX9308-FIRST                                 
064900           IF SEGMENT-SAKNAS                                              
065000             MOVE 1       TO W-PRKURS                                     
065100             MOVE 1       TO W-REVALUTA                                   
065200           ELSE                                                           
065300             MOVE 9308-PRKURS TO W-PRKURS                                 
065400             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
065500           END-IF                                                         
065600         ELSE                                                             
065700           MOVE 9308-PRKURS TO W-PRKURS                                   
065800           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
065900         END-IF                                                           
066000       END-IF                                                             
066100     END-IF                                                               
066200                                                                          
066300     IF IN-EKH-IDVERGL = WS-SPAR-IDVERGL                                  
066400     AND (IN-EKH-DAVERDAT = WS-SPAR-DAVERDAT)                             
066500       IF  (IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                              
066600       AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                              
066700       OR (IN-EKH-KDEKHHT = '303')                                        
066800         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
066900       ELSE                                                               
067000         MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                           
067100         MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                           
067200         IF WS-LOP = 9                                                    
067300           MOVE ZERO  TO WS-LOP                                           
067400         ELSE                                                             
067500           ADD +1     TO WS-LOP                                           
067600         END-IF                                                           
067700         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
067800       END-IF                                                             
067900     ELSE                                                                 
068000       MOVE IN-EKH-IDVERGL  TO WS-SPAR-IDVERGL                            
068100       MOVE IN-EKH-KDEKHHT  TO WS-SPAR-KDEKHHT                            
068200       MOVE IN-EKH-KDEKSHT  TO WS-SPAR-KDEKSHT                            
068300       MOVE IN-EKH-DAVERDAT TO WS-SPAR-DAVERDAT                           
068400       IF WS-LOP = 9                                                      
068500         MOVE ZERO  TO WS-LOP                                             
068600       ELSE                                                               
068700         ADD +1     TO WS-LOP                                             
068800       END-IF                                                             
068900       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
069000     END-IF                                                               
069100* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
069200     IF SYST-IDPTYP = '210'                                               
069300       PERFORM CB-CREATE-WRITE-HEADER-AP                                  
069400     ELSE                                                                 
069500       IF SYST-IDPTYP = '310'                                             
069600         PERFORM CC-CREATE-WRITE-HEADER-AR                                
069700       ELSE                                                               
069800* TEST OM BRYTNING PÅ VERIFIKATION                                        
069900         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
070000         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
070100         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
070200         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
070300           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
070400           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
070500           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
070600           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
070700           IF (IN-EKH-KDEKHHT = '102'                                     
070800           AND IN-EKH-KDEKSHT = '121')                                    
070900           OR (IN-EKH-KDEKHHT = '102'                                     
071000           AND IN-EKH-KDEKSHT = '122')                                    
071100           OR (IN-EKH-KDEKHHT = '102'                                     
071200           AND IN-EKH-KDEKSHT = '131')                                    
071300           OR (IN-EKH-KDEKHHT = '102'                                     
071400           AND IN-EKH-KDEKSHT = '132')                                    
071500             PERFORM S80-GET-CURRENCY-RATE                                
071600           END-IF                                                         
071700           IF (IN-EKH-KDEKHHT = '303'                                     
071800           AND IN-EKH-KDEKSHT = '301')                                    
071900           OR (IN-EKH-KDEKHHT = '303'                                     
072000           AND IN-EKH-KDEKSHT = '307')                                    
072100           OR (IN-EKH-KDEKHHT = '303'                                     
072200           AND IN-EKH-KDEKSHT = '371')                                    
072300           OR (IN-EKH-KDEKHHT = '303'                                     
072400           AND IN-EKH-KDEKSHT = '3XX')                                    
072500             PERFORM S81-GET-CURRENCY-RATE                                
072600           END-IF                                                         
072700*   NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                   
072800*   HEADER-POST TILL HUVUDBOKEN                                           
072900           IF (IN-EKH-KDEKHHT = '102'                                     
073000           AND IN-EKH-KDEKSHT = '120')                                    
073100           OR (IN-EKH-KDEKHHT = '102'                                     
073200           AND IN-EKH-KDEKSHT = '124')                                    
073300           OR (IN-EKH-KDEKHHT = '102'                                     
073400           AND IN-EKH-KDEKSHT = '125')                                    
073500           OR (IN-EKH-KDEKHHT = '102'                                     
073600           AND IN-EKH-KDEKSHT = '130')                                    
073700           OR (IN-EKH-KDEKHHT = '102'                                     
073800           AND IN-EKH-KDEKSHT = '134')                                    
073900           OR (IN-EKH-KDEKHHT = '204'                                     
074000           AND IN-EKH-KDEKSHT = '301')                                    
074100           OR (IN-EKH-KDEKHHT = '303'                                     
074200           AND IN-EKH-KDEKSHT = '301')                                    
074300           OR (IN-EKH-KDEKHHT = '303'                                     
074400           AND IN-EKH-KDEKSHT = '307')                                    
074500           OR (IN-EKH-KDEKHHT = '303'                                     
074600           AND IN-EKH-KDEKSHT = '371')                                    
074700           OR (IN-EKH-KDEKHHT = '303'                                     
074800           AND IN-EKH-KDEKSHT = '377')                                    
074900           OR (IN-EKH-KDEKHHT = '303'                                     
075000           AND IN-EKH-KDEKSHT = '387')                                    
075100           OR (IN-EKH-KDEKHHT = '303'                                     
075200           AND IN-EKH-KDEKSHT = '3XX')                                    
075300             CONTINUE                                                     
075400           ELSE                                                           
075500             PERFORM CA-CREATE-WRITE-HEADER-GL                            
075600           END-IF                                                         
075700         END-IF                                                           
075800       END-IF                                                             
075900     END-IF                                                               
076000                                                                          
076100**** VAR SÄKER PÅ ATT ANVÄNDA RÄTT LÄSNING                                
076200     MOVE WS-STATUS TO STATUS-WS                                          
076300     PERFORM UNTIL SEGMENT-SAKNAS                                         
076400       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
076500                                                                          
076600* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
076700       IF SYST-IDPTYP = '610'                                             
076800         PERFORM CD-BUILD-COMMON-610-PART                                 
076900         PERFORM CE-SCHEDULE-LINE-GL                                      
077000       ELSE                                                               
077100         IF SYST-IDPTYP = '210'                                           
077200           PERFORM CF-BUILD-COMMON-210-PART                               
077300           PERFORM CG-SCHEDULE-LINE-AP                                    
077400         ELSE                                                             
077500           IF SYST-IDPTYP = '310'                                         
077600             PERFORM CH-BUILD-COMMON-310-PART                             
077700             PERFORM CI-SCHEDULE-LINE-AR                                  
077800           END-IF                                                         
077900         END-IF                                                           
078000       END-IF                                                             
078100       PERFORM IMS-GNP-WDH531                                             
078200     END-PERFORM                                                          
078300     .                                                                    
078400     EJECT                                                                
078500                                                                          
078600 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
078700     MOVE SPACE                   TO R3-HEAD-R3                           
078800     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
078900     MOVE 'CN05'                  TO R3-HEAD-COMPANY-CODE                 
079000     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
079100     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
079200     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
079300     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
079400     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
079500       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
079600     ELSE                                                                 
079700       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
079800     END-IF                                                               
079900     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
080000     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
080100     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
080200     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
080300     IF (IN-EKH-KDEKHHT = '102'                                           
080400     AND IN-EKH-KDEKSHT = '107')                                          
080500     OR (IN-EKH-KDEKHHT = '103'                                           
080600     AND IN-EKH-KDEKSHT = '102')                                          
080700       MOVE IN-EKH-KDVALISO       TO R3-HEAD-CURRENCY                     
080800       IF IN-EKH-KDVALISO = 'CNY'                                         
080900         MOVE WS-PRKURS           TO R3-HEAD-EXCHANGE-RATE                
081000       ELSE                                                               
081100**** HÄMTA DC HUVUDVALUTA                                                 
081200         MOVE 'CNY'          TO W-KDVALISO-HUV                            
081300         MOVE WS-KDVALISO    TO W-KDVALISO-ROW                            
081400         MOVE W-KDVALISO-HUV      TO CURR-KDVALISO-HUV                    
081500         MOVE W-KDVALISO-ROW      TO CURR-KDVALISO-ROW                    
081600         MOVE 'M'                 TO CURR-KDVALTYP                        
081700         IF IN-FIL-IDPGM = 'W4183300'                                     
081800           IF IN-EKH-DAAVIDAT > ZERO                                      
081900             MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                      
082000             MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                      
082100           ELSE                                                           
082200             MOVE WS-TIAA            TO WS-TIAA-CR                        
082300             MOVE WS-TIMM            TO WS-TIMM-CR                        
082400           END-IF                                                         
082500         ELSE                                                             
082600           MOVE WS-TIAA              TO WS-TIAA-CR                        
082700           MOVE WS-TIMM              TO WS-TIMM-CR                        
082800         END-IF                                                           
082900         MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                     
083000         MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                     
083100         MOVE W-DATE-AAMM         TO CURR-TIAAMM                          
083200         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
083300         IF CURR-KDSVAR = ' '                                             
083400           MOVE CURR-PRKURS-NEW     TO W-PRKURS                           
083500                                       WS-PRKURS                          
083600           MOVE 1                   TO W-REVALUTA                         
083700         ELSE                                                             
083800           MOVE 1                   TO W-PRKURS                           
083900                                       WS-PRKURS                          
084000           MOVE 1                   TO W-REVALUTA                         
084100         END-IF                                                           
084200                                                                          
084300         COMPUTE R3-HEAD-EXCHANGE-RATE ROUNDED = W-PRKURS *               
084400                                         W-REVALUTA                       
084500         IF W-REVALUTA = +1                                               
084600           MOVE '1    '         TO R3-HEAD-EXCHANGE-FRFACT                
084700         END-IF                                                           
084800         IF W-REVALUTA = +10                                              
084900           MOVE '10   '         TO R3-HEAD-EXCHANGE-FRFACT                
085000         END-IF                                                           
085100         IF W-REVALUTA = +100                                             
085200           MOVE '100  '         TO R3-HEAD-EXCHANGE-FRFACT                
085300         END-IF                                                           
085400       END-IF                                                             
085500     ELSE                                                                 
085600       IF WS-KDVALISO = 'CNY'                                             
085700         MOVE WS-PRKURS           TO R3-HEAD-EXCHANGE-RATE                
085800       ELSE                                                               
085900         MOVE WS-KDVALISO         TO CURR-KDVALISO-ROW                    
086000         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
086100         IF CURR-KDSVAR = ' '                                             
086200           IF IN-EKH-IDDISTR > ZERO                                       
086300             MOVE CURR-PRKURS-NEW TO WS-PRKURS                            
086400           ELSE                                                           
086500             MOVE 1               TO WS-PRKURS                            
086600           END-IF                                                         
086700         ELSE                                                             
086800           MOVE 1                 TO WS-PRKURS                            
086900         END-IF                                                           
087000         COMPUTE R3-HEAD-EXCHANGE-RATE ROUNDED = WS-PRKURS *              
087100                                         CURR-REVALUTA-TO                 
087200         IF CURR-REVALUTA-TO = +1                                         
087300           MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT              
087400         END-IF                                                           
087500         IF CURR-REVALUTA-TO = +10                                        
087600           MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT              
087700         END-IF                                                           
087800         IF CURR-REVALUTA-TO = +100                                       
087900           MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT              
088000         END-IF                                                           
088100       END-IF                                                             
088200     END-IF                                                               
088300     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
088400     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
088500     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
088600     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
088700     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
088800     MOVE JA                      TO WS-HEADER-SW                         
088900     MOVE NEJ                     TO WS-LINE-SW                           
089000                                                                          
089100* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
089200     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
089300     OR (IN-EKH-KDEKHHT = '303'                                           
089400     AND IN-EKH-KDEKSHT = '391')                                          
089500     OR (IN-EKH-KDEKHHT = '102'                                           
089600     AND IN-EKH-KDEKSHT = '121')                                          
089700     OR (IN-EKH-KDEKHHT = '102'                                           
089800     AND IN-EKH-KDEKSHT = '131')                                          
089900     OR (IN-EKH-KDEKHHT = '102'                                           
090000     AND IN-EKH-KDEKSHT = '107')                                          
090100     OR (IN-EKH-KDEKHHT = '103'                                           
090200     AND IN-EKH-KDEKSHT = '102')                                          
090300     OR IN-EKH-KDEKHHT = '501'                                            
090400       PERFORM S004-WRITE-W57073A-HEAD                                    
090500     ELSE                                                                 
090600       PERFORM S002-WRITE-W57071A-HEAD                                    
090700     END-IF                                                               
090800     .                                                                    
090900     EJECT                                                                
091000                                                                          
091100 CB-CREATE-WRITE-HEADER-AP SECTION.                                       
091200     MOVE SPACE                   TO R3-HEAD-R3                           
091300     MOVE '200'                   TO R3-HEAD-RECORD-TYPE                  
091400     MOVE 'CN05'                  TO R3-HEAD-COMPANY-CODE                 
091500                                     R3-HEAD-CONTROL-AREA                 
091600     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
091700     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
091800     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
091900     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
092000     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
092100     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
092200       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
092300     ELSE                                                                 
092400       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
092500     END-IF                                                               
092600     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
092700     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
092800     MOVE 'SEK'                   TO R3-HEAD-CURRENCY                     
092900     MOVE WS-PRKURS-CN2           TO R3-HEAD-EXCHANGE-RATE                
093000     IF (IN-EKH-KDEKHHT = '102'                                           
093100     AND IN-EKH-KDEKSHT = '120')                                          
093200     OR (IN-EKH-KDEKHHT = '102'                                           
093300     AND IN-EKH-KDEKSHT = '124')                                          
093400     OR (IN-EKH-KDEKHHT = '102'                                           
093500     AND IN-EKH-KDEKSHT = '125')                                          
093600     OR (IN-EKH-KDEKHHT = '102'                                           
093700     AND IN-EKH-KDEKSHT = '130')                                          
093800     OR (IN-EKH-KDEKHHT = '102'                                           
093900     AND IN-EKH-KDEKSHT = '134')                                          
094000       MOVE 'CNY'                 TO R3-HEAD-CURRENCY                     
094100       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
094200     ELSE                                                                 
094300       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
094400       MOVE WS-PRKURS-CN2         TO R3-HEAD-EXCHANGE-RATE                
094500       MOVE 'CNY'                 TO CURR-KDVALISO-ROW                    
094600       IF IN-FIL-IDPGM = 'W4183300'                                       
094700         IF IN-EKH-DAAVIDAT > ZERO                                        
094800           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
094900           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
095000         ELSE                                                             
095100           MOVE WS-TIAA         TO WS-TIAA-CR                             
095200           MOVE WS-TIMM         TO WS-TIMM-CR                             
095300         END-IF                                                           
095400       ELSE                                                               
095500         MOVE WS-TIAA           TO WS-TIAA-CR                             
095600         MOVE WS-TIMM           TO WS-TIMM-CR                             
095700       END-IF                                                             
095800       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
095900       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
096000       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
096100       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
096200       IF CURR-KDSVAR = ' '                                               
096300         IF IN-EKH-IDDISTR > ZERO                                         
096400           MOVE CURR-PRKURS-NEW TO WS-PRKURS-CN                           
096500         ELSE                                                             
096600           IF WS-PRKURS = ZERO                                            
096700             MOVE 1             TO WS-PRKURS-CN                           
096800           END-IF                                                         
096900         END-IF                                                           
097000       ELSE                                                               
097100         MOVE 1                 TO WS-PRKURS-CN                           
097200       END-IF                                                             
097300       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-CN *                     
097400                                       CURR-REVALUTA-TO                   
097500       END-COMPUTE                                                        
097600       IF CURR-REVALUTA-TO = +1                                           
097700         MOVE '1    '             TO R3-HEAD-EXCHANGE-FRFACT              
097800       END-IF                                                             
097900       IF CURR-REVALUTA-TO = +10                                          
098000         MOVE '10   '             TO R3-HEAD-EXCHANGE-FRFACT              
098100       END-IF                                                             
098200       IF CURR-REVALUTA-TO = +100                                         
098300         MOVE '100  '             TO R3-HEAD-EXCHANGE-FRFACT              
098400       END-IF                                                             
098500     END-IF                                                               
098600     IF (IN-EKH-KDEKHHT = '102'                                           
098700     AND IN-EKH-KDEKSHT = '120')                                          
098800     OR (IN-EKH-KDEKHHT = '102'                                           
098900     AND IN-EKH-KDEKSHT = '124')                                          
099000     OR (IN-EKH-KDEKHHT = '102'                                           
099100     AND IN-EKH-KDEKSHT = '125')                                          
099200     OR (IN-EKH-KDEKHHT = '102'                                           
099300     AND IN-EKH-KDEKSHT = '130')                                          
099400     OR (IN-EKH-KDEKHHT = '102'                                           
099500     AND IN-EKH-KDEKSHT = '134')                                          
099600     OR (IN-EKH-KDEKHHT = '303'                                           
099700     AND IN-EKH-KDEKSHT = '301')                                          
099800     OR (IN-EKH-KDEKHHT = '303'                                           
099900     AND IN-EKH-KDEKSHT = '307')                                          
100000     OR (IN-EKH-KDEKHHT = '303'                                           
100100     AND IN-EKH-KDEKSHT = '371')                                          
100200     OR (IN-EKH-KDEKHHT = '303'                                           
100300     AND IN-EKH-KDEKSHT = '3XX')                                          
100400       MOVE 'CNY'                 TO R3-HEAD-CURRENCY                     
100500       MOVE 1                     TO R3-HEAD-EXCHANGE-RATE                
100600     END-IF                                                               
100700     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
100800     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
100900     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
101000     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
101100     MOVE JA                      TO WS-HEADER-SW                         
101200     MOVE NEJ                     TO WS-LINE-SW                           
101300                                                                          
101400* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57073A                       
101500     PERFORM S004-WRITE-W57073A-HEAD                                      
101600     .                                                                    
101700     EJECT                                                                
101800                                                                          
101900 CC-CREATE-WRITE-HEADER-AR SECTION.                                       
102000     MOVE SPACE                   TO R3-HEAD-R3                           
102100     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
102200     MOVE 'CN05'                  TO R3-HEAD-COMPANY-CODE                 
102300                                     R3-HEAD-CONTROL-AREA                 
102400     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
102500     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
102600     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
102700     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
102800     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
102900     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
103000       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
103100     ELSE                                                                 
103200       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
103300     END-IF                                                               
103400     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
103500     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
103600     IF (IN-EKH-KDEKHHT = '204'                                           
103700     AND IN-EKH-KDEKSHT = '301')                                          
103800       MOVE 'CNY'                 TO R3-HEAD-CURRENCY                     
103900       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
104000     ELSE                                                                 
104100       MOVE 'SEK'                 TO R3-HEAD-CURRENCY                     
104200       MOVE WS-PRKURS-CN2         TO R3-HEAD-EXCHANGE-RATE                
104300       MOVE 'CNY'                 TO CURR-KDVALISO-ROW                    
104400       IF IN-FIL-IDPGM = 'W4183300'                                       
104500         IF IN-EKH-DAAVIDAT > ZERO                                        
104600           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
104700           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
104800         ELSE                                                             
104900           MOVE WS-TIAA         TO WS-TIAA-CR                             
105000           MOVE WS-TIMM         TO WS-TIMM-CR                             
105100         END-IF                                                           
105200       ELSE                                                               
105300         MOVE WS-TIAA           TO WS-TIAA-CR                             
105400         MOVE WS-TIMM           TO WS-TIMM-CR                             
105500       END-IF                                                             
105600       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
105700       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
105800       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
105900       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
106000       IF CURR-KDSVAR = ' '                                               
106100         IF IN-EKH-IDDISTR > ZERO                                         
106200           MOVE CURR-PRKURS-NEW TO WS-PRKURS-CN                           
106300         ELSE                                                             
106400           IF WS-PRKURS = ZERO                                            
106500             MOVE 1             TO WS-PRKURS-CN                           
106600           END-IF                                                         
106700         END-IF                                                           
106800       ELSE                                                               
106900         MOVE 1                 TO WS-PRKURS-CN                           
107000       END-IF                                                             
107100       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS-CN *                     
107200                                       CURR-REVALUTA-TO                   
107300       END-COMPUTE                                                        
107400       IF CURR-REVALUTA-TO = +1                                           
107500         MOVE '1    '             TO R3-HEAD-EXCHANGE-FRFACT              
107600       END-IF                                                             
107700       IF CURR-REVALUTA-TO = +10                                          
107800         MOVE '10   '             TO R3-HEAD-EXCHANGE-FRFACT              
107900       END-IF                                                             
108000       IF CURR-REVALUTA-TO = +100                                         
108100         MOVE '100  '             TO R3-HEAD-EXCHANGE-FRFACT              
108200       END-IF                                                             
108300     END-IF                                                               
108400     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
108500     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
108600     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
108700     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
108800     MOVE JA                      TO WS-HEADER-SW                         
108900     MOVE NEJ                     TO WS-LINE-SW                           
109000                                                                          
109100* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W57073A                       
109200       PERFORM S004-WRITE-W57073A-HEAD                                    
109300     .                                                                    
109400     EJECT                                                                
109500                                                                          
109600 CD-BUILD-COMMON-610-PART SECTION.                                        
109700     MOVE SPACE               TO R3-LINE-R3                               
109800     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
109900                                 R3-LINE-DUE-DATE                         
110000                                 R3-LINE-AMOUNT                           
110100                                 R3-LINE-AMOUNT-LC                        
110200                                 R3-LINE-TAX-AMOUNT                       
110300                                 R3-LINE-TAX-AMOUNT-LC                    
110400                                 R3-LINE-NUMBER-OF-DAYS                   
110500                                 R3-LINE-QUANTITY                         
110600                                 R3-LINE-SAMNR                            
110700     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
110800     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
110900     MOVE 'CN05'              TO R3-LINE-COMPANY-CODE                     
111000     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
111100     IF SYST-KDPOST = '50'                                                
111200       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
111300     ELSE                                                                 
111400       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
111500     END-IF                                                               
111600     IF SYST-IDPRCTR NOT = SPACE                                          
111700       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
111800       IF WS-PRCTR-PRODSL = '??'                                          
111900         MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP                
112000         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                     
112100       END-IF                                                             
112200       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
112300     END-IF                                                               
112400     .                                                                    
112500     EJECT                                                                
112600                                                                          
112700 CE-SCHEDULE-LINE-GL SECTION.                                             
112800     MOVE NEJ                     TO WS-HEADER-SW                         
112900     MOVE JA                      TO WS-LINE-SW                           
113000     EVALUATE IN-EKH-KDEKHHT                                              
113100     WHEN '102'                                                           
113200          PERFORM CEB-MAIN-EVENT-102                                      
113300     WHEN '103'                                                           
113400          PERFORM CEC-MAIN-EVENT-103                                      
113500     WHEN '201'                                                           
113600          PERFORM CED-MAIN-EVENT-201                                      
113700     WHEN '203'                                                           
113800          PERFORM CEF-MAIN-EVENT-203                                      
113900     WHEN '204'                                                           
114000          PERFORM CEG-MAIN-EVENT-204                                      
114100     WHEN '302'                                                           
114200          PERFORM CEI-MAIN-EVENT-302                                      
114300     WHEN '303'                                                           
114400          PERFORM CEJ-MAIN-EVENT-303                                      
114500     WHEN '401'                                                           
114600          PERFORM CEK-MAIN-EVENT-401                                      
114700     WHEN '402'                                                           
114800          PERFORM CEL-MAIN-EVENT-402                                      
114900     WHEN '403'                                                           
115000          PERFORM CEM-MAIN-EVENT-403                                      
115100     WHEN '404'                                                           
115200          PERFORM CEN-MAIN-EVENT-404                                      
115300     WHEN '501'                                                           
115400          PERFORM CEQ-MAIN-EVENT-501                                      
115500     WHEN '502'                                                           
115600          PERFORM CER-MAIN-EVENT-502                                      
115700     WHEN '503'                                                           
115800          PERFORM CES-MAIN-EVENT-503                                      
115900     END-EVALUATE                                                         
116000     .                                                                    
116100     EJECT                                                                
116200                                                                          
116300 CEB-MAIN-EVENT-102 SECTION.                                              
116400     EVALUATE IN-EKH-KDEKSHT                                              
116500     WHEN '102'                                                           
116600          PERFORM CEBA-SUB-EVENT-102-102                                  
116700     WHEN '103'                                                           
116800          PERFORM CEBB-SUB-EVENT-102-103                                  
116900     WHEN '104'                                                           
117000          PERFORM CEBC-SUB-EVENT-102-104                                  
117100     WHEN '106'                                                           
117200          PERFORM CEBD-SUB-EVENT-102-106                                  
117300     WHEN '107'                                                           
117400          PERFORM CEBD-SUB-EVENT-102-107                                  
117500     WHEN '120'                                                           
117600          PERFORM CEBD-SUB-EVENT-102-120                                  
117700     WHEN '121'                                                           
117800          PERFORM CEBD-SUB-EVENT-102-121                                  
117900     WHEN '122'                                                           
118000          PERFORM CEBD-SUB-EVENT-102-122                                  
118100     WHEN '123'                                                           
118200          PERFORM CEBD-SUB-EVENT-102-123                                  
118300     WHEN '124'                                                           
118400          PERFORM CEBD-SUB-EVENT-102-124                                  
118500     WHEN '125'                                                           
118600          PERFORM CEBD-SUB-EVENT-102-125                                  
118700     WHEN '130'                                                           
118800          PERFORM CEBE-SUB-EVENT-102-130                                  
118900     WHEN '131'                                                           
119000          PERFORM CEBE-SUB-EVENT-102-131                                  
119100     WHEN '132'                                                           
119200          PERFORM CEBE-SUB-EVENT-102-132                                  
119300     WHEN '134'                                                           
119400          PERFORM CEBE-SUB-EVENT-102-134                                  
119500     END-EVALUATE                                                         
119600     .                                                                    
119700     EJECT                                                                
119800                                                                          
119900 CEBA-SUB-EVENT-102-102 SECTION.                                          
120000     EVALUATE IN-EKH-KDEKNIVA                                             
120100     WHEN 'DET'                                                           
120200       IF SYST-IDSEKVNR = 1                                               
120300         IF IN-EKH-IDKUNDRF = 'OBJ'                                       
120400           CONTINUE                                                       
120500         ELSE                                                             
120600           IF IN-EKH-KVANTAL > 0                                          
120700             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
120800             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
120900             COMPUTE R3-LINE-AMOUNT    =                                  
121000                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
121100             MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                   
121200             MOVE SPACE               TO WS-ALLOCATE-DISTR                
121300             MOVE SPACE               TO WS-ALLOCATE-REF                  
121400             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
121500             IF IN-EKH-IDKONTO = 432301                                   
121600               MOVE 'CN  '            TO R3-LINE-TRADING-PARTNER          
121700             END-IF                                                       
121800             PERFORM S02-WRITE-W57071A                                    
121900           END-IF                                                         
122000         END-IF                                                           
122100       END-IF                                                             
122200                                                                          
122300       IF SYST-IDSEKVNR = 2                                               
122400         IF IN-EKH-KVANTAL > 0                                            
122500           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
122600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
122700           COMPUTE R3-LINE-AMOUNT    =                                    
122800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
122900           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
123000           IF WS-RED-IDKST > SPACE                                        
123100             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
123200             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
123300           ELSE                                                           
123400             MOVE SPACE             TO R3-LINE-COST-CENTER                
123500           END-IF                                                         
123600           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
123700           IF IN-EKH-IDKONTO = 432301                                     
123800             MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER          
123900           END-IF                                                         
124000           PERFORM S02-WRITE-W57071A                                      
124100         END-IF                                                           
124200       END-IF                                                             
124300                                                                          
124400       IF SYST-IDSEKVNR = 3                                               
124500          IF IN-EKH-IDKUNDRF = 'OBJ'                                      
124600            CONTINUE                                                      
124700          ELSE                                                            
124800            IF IN-EKH-KVANTAL < 0                                         
124900              MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                
125000              MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                 
125100              COMPUTE R3-LINE-AMOUNT    =                                 
125200                      IN-EKH-KVANTAL * IN-EKH-PRARTSTD                    
125300              MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                  
125400              MOVE SPACE               TO WS-ALLOCATE-DISTR               
125500              MOVE SPACE               TO WS-ALLOCATE-REF                 
125600              MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                
125700              IF IN-EKH-IDKONTO = 432301                                  
125800                MOVE 'CN  '            TO R3-LINE-TRADING-PARTNER         
125900              END-IF                                                      
126000              PERFORM S02-WRITE-W57071A                                   
126100            END-IF                                                        
126200          END-IF                                                          
126300       END-IF                                                             
126400                                                                          
126500       IF SYST-IDSEKVNR = 4                                               
126600         IF IN-EKH-KVANTAL < 0                                            
126700           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
126800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
126900           COMPUTE R3-LINE-AMOUNT    =                                    
127000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
127100           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
127200           IF WS-RED-IDKST > SPACE                                        
127300             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
127400             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
127500           ELSE                                                           
127600             MOVE SPACE             TO R3-LINE-COST-CENTER                
127700           END-IF                                                         
127800           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
127900           IF IN-EKH-IDKONTO = 432301                                     
128000             MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER          
128100           END-IF                                                         
128200           PERFORM S02-WRITE-W57071A                                      
128300         END-IF                                                           
128400       END-IF                                                             
128500                                                                          
128600       IF SYST-IDSEKVNR = 5                                               
128700          IF IN-EKH-IDKUNDRF = 'OBJ'                                      
128800             IF IN-EKH-KVANTAL > 0                                        
128900                MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10              
129000                MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT               
129100                COMPUTE R3-LINE-AMOUNT    =                               
129200                        IN-EKH-KVANTAL * IN-EKH-PRARTSTD                  
129300                MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                
129400                MOVE SPACE               TO WS-ALLOCATE-DISTR             
129500                MOVE SPACE               TO WS-ALLOCATE-REF               
129600                MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE              
129700                IF IN-EKH-IDKONTO = 432301                                
129800                  MOVE 'CN  '          TO R3-LINE-TRADING-PARTNER         
129900                END-IF                                                    
130000                PERFORM S02-WRITE-W57071A                                 
130100             END-IF                                                       
130200          END-IF                                                          
130300       END-IF                                                             
130400                                                                          
130500       IF SYST-IDSEKVNR = 6                                               
130600          IF IN-EKH-IDKUNDRF = 'OBJ'                                      
130700             IF IN-EKH-KVANTAL < 0                                        
130800                MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10              
130900                MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT               
131000                COMPUTE R3-LINE-AMOUNT    =                               
131100                        IN-EKH-KVANTAL * IN-EKH-PRARTSTD                  
131200                MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                
131300                MOVE SPACE               TO WS-ALLOCATE-DISTR             
131400                MOVE SPACE               TO WS-ALLOCATE-REF               
131500                MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE              
131600                IF IN-EKH-IDKONTO = 432301                                
131700                  MOVE 'CN  '          TO R3-LINE-TRADING-PARTNER         
131800                END-IF                                                    
131900                PERFORM S02-WRITE-W57071A                                 
132000             END-IF                                                       
132100         END-IF                                                           
132200       END-IF                                                             
132300     END-EVALUATE                                                         
132400     .                                                                    
132500     EJECT                                                                
132600                                                                          
132700 CEBB-SUB-EVENT-102-103 SECTION.                                          
132800     EVALUATE IN-EKH-KDEKNIVA                                             
132900     WHEN 'DET'                                                           
133000       IF SYST-IDSEKVNR = 1                                               
133100         IF IN-EKH-KVANTAL > 0                                            
133200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
133300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
133400           COMPUTE R3-LINE-AMOUNT    =                                    
133500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
133600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
133700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
133800           MOVE SPACE               TO WS-ALLOCATE-REF                    
133900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
134000           PERFORM S02-WRITE-W57071A                                      
134100         END-IF                                                           
134200       END-IF                                                             
134300                                                                          
134400       IF SYST-IDSEKVNR = 2                                               
134500         IF IN-EKH-KVANTAL > 0                                            
134600           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
134700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
134800           COMPUTE R3-LINE-AMOUNT    =                                    
134900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
135000           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
135100           IF WS-RED-IDKST > SPACE                                        
135200             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
135300             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
135400           ELSE                                                           
135500             MOVE SPACE             TO R3-LINE-COST-CENTER                
135600           END-IF                                                         
135700           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
135800           PERFORM S02-WRITE-W57071A                                      
135900         END-IF                                                           
136000       END-IF                                                             
136100                                                                          
136200       IF SYST-IDSEKVNR = 3                                               
136300         IF IN-EKH-KVANTAL < 0                                            
136400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
136500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
136600           COMPUTE R3-LINE-AMOUNT    =                                    
136700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
136800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
136900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
137000           MOVE SPACE               TO WS-ALLOCATE-REF                    
137100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
137200           PERFORM S02-WRITE-W57071A                                      
137300         END-IF                                                           
137400       END-IF                                                             
137500                                                                          
137600       IF SYST-IDSEKVNR = 4                                               
137700         IF IN-EKH-KVANTAL < 0                                            
137800           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
137900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
138000           COMPUTE R3-LINE-AMOUNT    =                                    
138100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
138200           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
138300           IF WS-RED-IDKST > SPACE                                        
138400             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
138500             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
138600           ELSE                                                           
138700             MOVE SPACE             TO R3-LINE-COST-CENTER                
138800           END-IF                                                         
138900           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
139000           PERFORM S02-WRITE-W57071A                                      
139100         END-IF                                                           
139200       END-IF                                                             
139300     END-EVALUATE                                                         
139400     .                                                                    
139500    EJECT                                                                 
139600                                                                          
139700 CEBC-SUB-EVENT-102-104 SECTION.                                          
139800     EVALUATE IN-EKH-KDEKNIVA                                             
139900     WHEN 'DET'                                                           
140000       IF SYST-IDSEKVNR = 1                                               
140100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
140200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
140300         COMPUTE R3-LINE-AMOUNT-LC =                                      
140400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
140500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
140600         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
140700         PERFORM S02-WRITE-W57071A                                        
140800       END-IF                                                             
140900                                                                          
141000       IF SYST-IDSEKVNR = 2                                               
141100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
141200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
141300         COMPUTE R3-LINE-AMOUNT-LC =                                      
141400                 (IN-EKH-KVANTAL * IN-EKH-PRARTSTD) +                     
141500                 (IN-EKH-KVANTAL * IN-EKH-PRARTSTD * 0)                   
141600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
141700         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
141800         PERFORM S02-WRITE-W57071A                                        
141900       END-IF                                                             
142000                                                                          
142100       IF SYST-IDSEKVNR = 3                                               
142200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
142300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
142400         COMPUTE R3-LINE-AMOUNT-LC =                                      
142500                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD * 0                     
142600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
142700         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
142800         IF R3-LINE-AMOUNT-LC > ZERO                                      
142900           PERFORM S02-WRITE-W57071A                                      
143000         END-IF                                                           
143100       END-IF                                                             
143200     END-EVALUATE                                                         
143300     .                                                                    
143400     EJECT                                                                
143500                                                                          
143600 CEBD-SUB-EVENT-102-106 SECTION.                                          
143700     EVALUATE IN-EKH-KDEKNIVA                                             
143800     WHEN 'DET'                                                           
143900       IF SYST-IDSEKVNR = 1                                               
144000         IF IN-EKH-KVANTAL < 0                                            
144100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
144200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
144300           COMPUTE R3-LINE-AMOUNT-LC =                                    
144400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
144500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
144600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
144700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
144800           MOVE SPACE               TO WS-ALLOCATE-REF                    
144900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
145000           PERFORM S02-WRITE-W57071A                                      
145100         END-IF                                                           
145200       END-IF                                                             
145300                                                                          
145400       IF SYST-IDSEKVNR = 2                                               
145500         IF IN-EKH-KVANTAL < 0                                            
145600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
145700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
145800           COMPUTE R3-LINE-AMOUNT-LC =                                    
145900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
146000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
146100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
146200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
146300           MOVE SPACE               TO WS-ALLOCATE-REF                    
146400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
146500           PERFORM S02-WRITE-W57071A                                      
146600         END-IF                                                           
146700       END-IF                                                             
146800                                                                          
146900       IF SYST-IDSEKVNR = 3                                               
147000         IF IN-EKH-KVANTAL > 0                                            
147100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
147200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
147300           COMPUTE R3-LINE-AMOUNT-LC =                                    
147400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
147500           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
147600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
147700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
147800           MOVE SPACE               TO WS-ALLOCATE-REF                    
147900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
148000           PERFORM S02-WRITE-W57071A                                      
148100         END-IF                                                           
148200       END-IF                                                             
148300                                                                          
148400       IF SYST-IDSEKVNR = 4                                               
148500         IF IN-EKH-KVANTAL > 0                                            
148600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
148700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
148800           COMPUTE R3-LINE-AMOUNT-LC =                                    
148900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
149000           MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                     
149100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
149200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
149300           MOVE SPACE               TO WS-ALLOCATE-REF                    
149400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
149500           PERFORM S02-WRITE-W57071A                                      
149600         END-IF                                                           
149700       END-IF                                                             
149800                                                                          
149900     END-EVALUATE                                                         
150000     .                                                                    
150100     EJECT                                                                
150200                                                                          
150300 CEBD-SUB-EVENT-102-107 SECTION.                                          
150400     EVALUATE IN-EKH-KDEKNIVA                                             
150500     WHEN 'DET'                                                           
150600       IF SYST-IDSEKVNR = 1                                               
150700         IF IN-EKH-KVANTAL < 0                                            
150800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
150900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
151000           COMPUTE R3-LINE-AMOUNT-LC =                                    
151100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
151200           IF IN-EKH-KDVALISO = 'CNY'                                     
151300             COMPUTE R3-LINE-AMOUNT-LC =                                  
151400                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
151500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
151600           ELSE                                                           
151700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
151800                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS          
151900           END-IF                                                         
152000           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
152100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
152200           MOVE SPACE               TO WS-ALLOCATE-REF                    
152300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
152400           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
152500           PERFORM S04-WRITE-W57073A                                      
152600         END-IF                                                           
152700       END-IF                                                             
152800                                                                          
152900       IF SYST-IDSEKVNR = 2                                               
153000         IF IN-EKH-KVANTAL > 0                                            
153100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
153200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
153300           IF IN-EKH-KDVALISO = 'CNY'                                     
153400             COMPUTE R3-LINE-AMOUNT-LC =                                  
153500                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
153600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
153700           ELSE                                                           
153800             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
153900                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS          
154000           END-IF                                                         
154100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
154200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
154300           MOVE SPACE               TO WS-ALLOCATE-REF                    
154400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
154500           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
154600           PERFORM S04-WRITE-W57073A                                      
154700         END-IF                                                           
154800       END-IF                                                             
154900                                                                          
155000     WHEN 'KALK'                                                          
155100       IF SYST-IDSEKVNR = 1                                               
155200         IF IN-EKH-SUBEL > 0                                              
155300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
155400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
155500           IF IN-EKH-KDVALISO = 'CNY'                                     
155600             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
155700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
155800           ELSE                                                           
155900             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
156000                                         W-PRKURS                         
156100           END-IF                                                         
156200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
156300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
156400           MOVE SPACE               TO WS-ALLOCATE-REF                    
156500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
156600           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
156700           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
156800           PERFORM S04-WRITE-W57073A                                      
156900         END-IF                                                           
157000       END-IF                                                             
157100                                                                          
157200       IF SYST-IDSEKVNR = 2                                               
157300         IF IN-EKH-SUBEL < 0                                              
157400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
157500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
157600           IF IN-EKH-KDVALISO = 'CNY'                                     
157700             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
157800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
157900           ELSE                                                           
158000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
158100                                         W-PRKURS                         
158200           END-IF                                                         
158300           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
158400           MOVE SPACE               TO WS-ALLOCATE-DC                     
158500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
158600           MOVE SPACE               TO WS-ALLOCATE-REF                    
158700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
158800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
158900           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
159000           PERFORM S04-WRITE-W57073A                                      
159100         END-IF                                                           
159200       END-IF                                                             
159300                                                                          
159400       IF SYST-IDSEKVNR = 3                                               
159500         IF IN-EKH-SUBEL > 0                                              
159600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
159700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
159800           IF IN-EKH-KDVALISO = 'CNY'                                     
159900             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
160000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
160100           ELSE                                                           
160200             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
160300                                         W-PRKURS                         
160400           END-IF                                                         
160500           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
160600           PERFORM S04-WRITE-W57073A                                      
160700         END-IF                                                           
160800       END-IF                                                             
160900                                                                          
161000       IF SYST-IDSEKVNR = 4                                               
161100         IF IN-EKH-SUBEL < 0                                              
161200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
161300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
161400           IF IN-EKH-KDVALISO = 'CNY'                                     
161500             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
161600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
161700           ELSE                                                           
161800             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
161900                                         W-PRKURS                         
162000           END-IF                                                         
162100           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
162200           PERFORM S04-WRITE-W57073A                                      
162300         END-IF                                                           
162400       END-IF                                                             
162500                                                                          
162600     WHEN 'HEMT'                                                          
162700       IF SYST-IDSEKVNR = 1                                               
162800         IF IN-EKH-SUBEL > 0                                              
162900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
163000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
163100           IF IN-EKH-KDVALISO = 'CNY'                                     
163200             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
163300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
163400           ELSE                                                           
163500             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
163600                                         W-PRKURS                         
163700           END-IF                                                         
163800           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
163900           MOVE SPACE               TO WS-ALLOCATE-DC                     
164000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
164100           MOVE SPACE               TO WS-ALLOCATE-REF                    
164200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
164300           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
164400           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
164500           PERFORM S04-WRITE-W57073A                                      
164600         END-IF                                                           
164700       END-IF                                                             
164800                                                                          
164900       IF SYST-IDSEKVNR = 2                                               
165000         IF IN-EKH-SUBEL < 0                                              
165100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
165200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
165300           IF IN-EKH-KDVALISO = 'CNY'                                     
165400             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
165500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
165600           ELSE                                                           
165700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
165800                                         W-PRKURS                         
165900           END-IF                                                         
166000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
166100           MOVE SPACE               TO WS-ALLOCATE-DC                     
166200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
166300           MOVE SPACE               TO WS-ALLOCATE-REF                    
166400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
166500           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
166600           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
166700           PERFORM S04-WRITE-W57073A                                      
166800         END-IF                                                           
166900       END-IF                                                             
167000                                                                          
167100       IF SYST-IDSEKVNR = 3                                               
167200         IF IN-EKH-SUBEL > 0                                              
167300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
167400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
167500           IF IN-EKH-KDVALISO = 'CNY'                                     
167600             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
167700             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
167800           ELSE                                                           
167900             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
168000                                         W-PRKURS                         
168100           END-IF                                                         
168200           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
168300           PERFORM S04-WRITE-W57073A                                      
168400         END-IF                                                           
168500       END-IF                                                             
168600                                                                          
168700       IF SYST-IDSEKVNR = 4                                               
168800         IF IN-EKH-SUBEL < 0                                              
168900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
169000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
169100           IF IN-EKH-KDVALISO = 'CNY'                                     
169200             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
169300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
169400           ELSE                                                           
169500             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
169600                                         W-PRKURS                         
169700           END-IF                                                         
169800           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
169900           PERFORM S04-WRITE-W57073A                                      
170000         END-IF                                                           
170100       END-IF                                                             
170200                                                                          
170300     WHEN 'ARB'                                                           
170400       IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                                 
170500         MOVE IN-EKH-IDDC-SEND      TO W-IDDC-B6                          
170600         PERFORM IMS-GU-WDB601                                            
170700       END-IF                                                             
170800                                                                          
170900       IF SYST-IDSEKVNR = 1                                               
171000         IF IN-EKH-SUBEL < 0                                              
171100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
171200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
171300           IF IN-EKH-KDVALISO = 'CNY'                                     
171400             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
171500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
171600           ELSE                                                           
171700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
171800                                         W-PRKURS                         
171900           END-IF                                                         
172000           MOVE SPACE               TO R3-LINE-ORDER                      
172100           MOVE SYST-IDKST          TO WS-RED-IDKST                       
172200           IF WS-RED-IDKST > SPACE                                        
172300             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
172400             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
172500           ELSE                                                           
172600             MOVE SPACE             TO R3-LINE-COST-CENTER                
172700           END-IF                                                         
172800           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
172900           PERFORM S04-WRITE-W57073A                                      
173000         END-IF                                                           
173100       END-IF                                                             
173200                                                                          
173300       IF SYST-IDSEKVNR = 2                                               
173400         IF IN-EKH-SUBEL > 0                                              
173500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
173600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
173700           IF IN-EKH-KDVALISO = 'CNY'                                     
173800             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
173900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
174000           ELSE                                                           
174100             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
174200                                         W-PRKURS                         
174300           END-IF                                                         
174400           MOVE SPACE               TO R3-LINE-ORDER                      
174500           MOVE SYST-IDKST          TO WS-RED-IDKST                       
174600           IF WS-RED-IDKST > SPACE                                        
174700             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
174800             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
174900           ELSE                                                           
175000             MOVE SPACE             TO R3-LINE-COST-CENTER                
175100           END-IF                                                         
175200           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
175300           PERFORM S04-WRITE-W57073A                                      
175400         END-IF                                                           
175500       END-IF                                                             
175600                                                                          
175700     WHEN 'TRP'                                                           
175800       IF SYST-IDSEKVNR = 1                                               
175900         IF IN-EKH-SUBEL < 0                                              
176000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
176100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
176200           IF IN-EKH-KDVALISO = 'CNY'                                     
176300             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
176400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
176500           ELSE                                                           
176600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
176700                                         W-PRKURS                         
176800           END-IF                                                         
176900           MOVE SPACE               TO R3-LINE-ORDER                      
177000           MOVE SYST-IDKST          TO WS-RED-IDKST                       
177100           IF WS-RED-IDKST > SPACE                                        
177200             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
177300             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
177400           ELSE                                                           
177500             MOVE SPACE             TO R3-LINE-COST-CENTER                
177600           END-IF                                                         
177700           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
177800           PERFORM S04-WRITE-W57073A                                      
177900         END-IF                                                           
178000       END-IF                                                             
178100                                                                          
178200       IF SYST-IDSEKVNR = 2                                               
178300         IF IN-EKH-SUBEL > 0                                              
178400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
178500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
178600           IF IN-EKH-KDVALISO = 'CNY'                                     
178700             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
178800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
178900           ELSE                                                           
179000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
179100                                         W-PRKURS                         
179200           END-IF                                                         
179300           MOVE SPACE               TO R3-LINE-ORDER                      
179400           MOVE SYST-IDKST          TO WS-RED-IDKST                       
179500           IF WS-RED-IDKST > SPACE                                        
179600             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
179700             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
179800           ELSE                                                           
179900             MOVE SPACE             TO R3-LINE-COST-CENTER                
180000           END-IF                                                         
180100           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
180200           PERFORM S04-WRITE-W57073A                                      
180300         END-IF                                                           
180400       END-IF                                                             
180500                                                                          
180600     WHEN 'MATR'                                                          
180700       IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                                 
180800         MOVE IN-EKH-IDDC-SEND      TO W-IDDC-B6                          
180900         PERFORM IMS-GU-WDB601                                            
181000       END-IF                                                             
181100                                                                          
181200       IF SYST-IDSEKVNR = 1                                               
181300         IF IN-EKH-SUBEL < 0                                              
181400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
181500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
181600           IF IN-EKH-KDVALISO = 'CNY'                                     
181700             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
181800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
181900           ELSE                                                           
182000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
182100                                         W-PRKURS                         
182200           END-IF                                                         
182300           MOVE SPACE               TO R3-LINE-ORDER                      
182400           MOVE SYST-IDKST          TO WS-RED-IDKST                       
182500           IF WS-RED-IDKST > SPACE                                        
182600             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
182700             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
182800           ELSE                                                           
182900             MOVE SPACE             TO R3-LINE-COST-CENTER                
183000           END-IF                                                         
183100           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
183200           PERFORM S04-WRITE-W57073A                                      
183300         END-IF                                                           
183400       END-IF                                                             
183500                                                                          
183600       IF SYST-IDSEKVNR = 2                                               
183700         IF IN-EKH-SUBEL > 0                                              
183800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
183900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
184000           IF IN-EKH-KDVALISO = 'CNY'                                     
184100             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
184200             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
184300           ELSE                                                           
184400             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
184500                                         W-PRKURS                         
184600           END-IF                                                         
184700           MOVE SPACE               TO R3-LINE-ORDER                      
184800           MOVE SYST-IDKST          TO WS-RED-IDKST                       
184900           IF WS-RED-IDKST > SPACE                                        
185000             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
185100             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
185200           ELSE                                                           
185300             MOVE SPACE             TO R3-LINE-COST-CENTER                
185400           END-IF                                                         
185500           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
185600           PERFORM S04-WRITE-W57073A                                      
185700         END-IF                                                           
185800       END-IF                                                             
185900                                                                          
186000     WHEN 'SUM'                                                           
186100       IF IN-EKH-SUBEL < ZERO                                             
186200         IF SYST-IDSEKVNR = 1                                             
186300           MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                    
186400           MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                     
186500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
186600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
186700                   R3-LINE-AMOUNT    * W-PRKURS                           
186800           MOVE ZERO               TO R3-LINE-TAX-AMOUNT                  
186900                                      R3-LINE-TAX-AMOUNT-LC               
187000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
187100                                                                          
187200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
187300           MOVE IN-EKH-IDLEVNR      TO WS-LINE-TEXT-IDLEVNR               
187400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
187500           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
187600                                                                          
187700           PERFORM S04-WRITE-W57073A                                      
187800         END-IF                                                           
187900       END-IF                                                             
188000                                                                          
188100       IF IN-EKH-SUBEL > ZERO                                             
188200         IF SYST-IDSEKVNR = 2                                             
188300           MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                    
188400           MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                     
188500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
188600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
188700                   R3-LINE-AMOUNT    * W-PRKURS                           
188800           MOVE ZERO               TO R3-LINE-TAX-AMOUNT                  
188900                                      R3-LINE-TAX-AMOUNT-LC               
189000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
189100                                                                          
189200           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
189300           MOVE IN-EKH-IDLEVNR      TO WS-LINE-TEXT-IDLEVNR               
189400           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
189500           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
189600                                                                          
189700           PERFORM S04-WRITE-W57073A                                      
189800         END-IF                                                           
189900       END-IF                                                             
190000                                                                          
190100     WHEN 'DDI'                                                           
190200       IF IN-EKH-SUBEL < ZERO                                             
190300         IF SYST-IDSEKVNR = 1                                             
190400           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
190500           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
190600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
190700                   IN-EKH-SUBEL                                           
190800           IF IN-EKH-KDVALISO = 'CNY'                                     
190900             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
191000           ELSE                                                           
191100             MOVE ZEROES              TO R3-LINE-AMOUNT                   
191200           END-IF                                                         
191300           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
191400           MOVE SPACE               TO R3-LINE-ALLOCATE                   
191500           PERFORM S04-WRITE-W57073A                                      
191600         END-IF                                                           
191700       ELSE                                                               
191800         IF SYST-IDSEKVNR = 2                                             
191900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
192000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
192100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
192200                   IN-EKH-SUBEL                                           
192300           IF IN-EKH-KDVALISO = 'CNY'                                     
192400             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
192500           ELSE                                                           
192600             MOVE ZEROES              TO R3-LINE-AMOUNT                   
192700           END-IF                                                         
192800           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
192900           MOVE SPACE               TO R3-LINE-ALLOCATE                   
193000           PERFORM S04-WRITE-W57073A                                      
193100         END-IF                                                           
193200       END-IF                                                             
193300     END-EVALUATE                                                         
193400     .                                                                    
193500     EJECT                                                                
193600                                                                          
193700 CEBD-SUB-EVENT-102-120 SECTION.                                          
193800     EVALUATE IN-EKH-KDEKNIVA                                             
193900     WHEN 'DET'                                                           
194000       IF SYST-IDSEKVNR = 1                                               
194100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
194200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
194300         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
194400         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN * -1             
194500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
194600         PERFORM S03-WRITE-W57072                                         
194700       END-IF                                                             
194800                                                                          
194900     WHEN 'FÖRS'                                                          
195000     WHEN 'FRAKT'                                                         
195100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
195200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
195300       MOVE SYST-IDKST          TO WS-RED-IDKST                           
195400       IF WS-RED-IDKST > SPACE                                            
195500         MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)               
195600         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
195700       ELSE                                                               
195800         MOVE SPACE             TO R3-LINE-COST-CENTER                    
195900       END-IF                                                             
196000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
196100               IN-EKH-SUBEL / WS-PRKURS-CN  * -1                          
196200       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
196300       PERFORM S04-WRITE-W57073A                                          
196400                                                                          
196500     WHEN 'EMB'                                                           
196600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
196700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
196800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
196900               IN-EKH-SUBEL / WS-PRKURS-CN  * -1                          
197000       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
197100       PERFORM S04-WRITE-W57073A                                          
197200                                                                          
197300     WHEN 'DDI'                                                           
197400       IF IN-EKH-SUBEL > ZERO                                             
197500         IF SYST-IDSEKVNR = 1                                             
197600           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
197700           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
197800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
197900                   IN-EKH-SUBEL                                           
198000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
198100           PERFORM S04-WRITE-W57073A                                      
198200         END-IF                                                           
198300       ELSE                                                               
198400         IF SYST-IDSEKVNR = 2                                             
198500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
198600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
198700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
198800                   IN-EKH-SUBEL                                           
198900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
199000           PERFORM S04-WRITE-W57073A                                      
199100         END-IF                                                           
199200       END-IF                                                             
199300                                                                          
199400     END-EVALUATE                                                         
199500     .                                                                    
199600     EJECT                                                                
199700                                                                          
199800 CEBD-SUB-EVENT-102-121 SECTION.                                          
199900     EVALUATE IN-EKH-KDEKNIVA                                             
200000     WHEN 'DET'                                                           
200100       IF SYST-IDSEKVNR = 1                                               
200200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
200300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
200400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
200500             IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3             
200600         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-1                   
200700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
200800         PERFORM S03-WRITE-W57072                                         
200900       END-IF                                                             
201000                                                                          
201100       IF SYST-IDSEKVNR = 2                                               
201200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
201300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
201400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
201500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
201600            (IN-EKH-KVANTAL *                                             
201700             IN-EKH-PRARTNTO / WS-PRKURS-CN3  * WS-MARKUP)                
201800         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-2                   
201900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
202000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
202100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
202200         MOVE SPACE               TO WS-ALLOCATE-REF                      
202300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
202400         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
202500         IF WS-RED-IDKST > SPACE                                          
202600           MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)             
202700           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
202800         ELSE                                                             
202900           MOVE SPACE             TO R3-LINE-COST-CENTER                  
203000         END-IF                                                           
203100         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
203200         PERFORM S03-WRITE-W57072                                         
203300       END-IF                                                             
203400                                                                          
203500       IF SYST-IDSEKVNR = 3                                               
203600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
203700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
203800         COMPUTE R3-LINE-AMOUNT-LC =                                      
203900                 WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1              
204000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
204100         PERFORM S03-WRITE-W57072                                         
204200       END-IF                                                             
204300                                                                          
204400     WHEN 'FÖRS'                                                          
204500     WHEN 'FRAKT'                                                         
204600       IF SYST-IDSEKVNR = 1                                               
204700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
204800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
204900         MOVE SYST-IDKST        TO WS-RED-IDKST                           
205000         IF WS-RED-IDKST > SPACE                                          
205100           MOVE 'HB'            TO R3-LINE-COST-CENTER(1:2)               
205200           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
205300         ELSE                                                             
205400           MOVE SPACE           TO R3-LINE-COST-CENTER                    
205500         END-IF                                                           
205600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
205700                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
205800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
205900         PERFORM S04-WRITE-W57073A                                        
206000       END-IF                                                             
206100                                                                          
206200       IF SYST-IDSEKVNR = 2                                               
206300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
206400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
206500         MOVE SYST-IDKST        TO WS-RED-IDKST                           
206600         IF WS-RED-IDKST > SPACE                                          
206700           MOVE 'HB'            TO R3-LINE-COST-CENTER(1:2)               
206800           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
206900         ELSE                                                             
207000           MOVE SPACE           TO R3-LINE-COST-CENTER                    
207100         END-IF                                                           
207200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
207300                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
207400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
207500         PERFORM S04-WRITE-W57073A                                        
207600       END-IF                                                             
207700                                                                          
207800     WHEN 'EMB'                                                           
207900       IF SYST-IDSEKVNR = 1                                               
208000         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
208100         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
208200         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
208300                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
208400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
208500         PERFORM S04-WRITE-W57073A                                        
208600       END-IF                                                             
208700                                                                          
208800       IF SYST-IDSEKVNR = 2                                               
208900         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
209000         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
209100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
209200                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
209300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
209400         PERFORM S04-WRITE-W57073A                                        
209500       END-IF                                                             
209600     END-EVALUATE                                                         
209700     .                                                                    
209800     EJECT                                                                
209900                                                                          
210000 CEBD-SUB-EVENT-102-122 SECTION.                                          
210100     EVALUATE IN-EKH-KDEKNIVA                                             
210200     WHEN 'DET'                                                           
210300       IF SYST-IDSEKVNR = 1                                               
210400         IF IN-EKH-KVANTAL > 0                                            
210500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
210600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
210700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
210800            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
210900            (IN-EKH-KVANTAL *                                             
211000             IN-EKH-PRARTNTO / WS-PRKURS-CN3  * WS-MARKUP)                
211100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
211200           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
211300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
211400           MOVE SPACE               TO WS-ALLOCATE-REF                    
211500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
211600           PERFORM S02-WRITE-W57071A                                      
211700         END-IF                                                           
211800       END-IF                                                             
211900                                                                          
212000       IF SYST-IDSEKVNR = 2                                               
212100         IF IN-EKH-KVANTAL < 0                                            
212200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
212300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
212400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
212500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
212600            (IN-EKH-KVANTAL *                                             
212700             IN-EKH-PRARTNTO / WS-PRKURS-CN3  * WS-MARKUP)                
212800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
212900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
213000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
213100           MOVE SPACE               TO WS-ALLOCATE-REF                    
213200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
213300           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
213400           IF WS-RED-IDKST > SPACE                                        
213500             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
213600             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
213700           ELSE                                                           
213800             MOVE SPACE             TO R3-LINE-COST-CENTER                
213900           END-IF                                                         
214000           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
214100           PERFORM S02-WRITE-W57071A                                      
214200         END-IF                                                           
214300       END-IF                                                             
214400                                                                          
214500       IF SYST-IDSEKVNR = 3                                               
214600         IF IN-EKH-KVANTAL > 0                                            
214700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
214800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
214900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
215000            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
215100            (IN-EKH-KVANTAL *                                             
215200             IN-EKH-PRARTNTO / WS-PRKURS-CN3 * WS-MARKUP)                 
215300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
215400           PERFORM S02-WRITE-W57071A                                      
215500         END-IF                                                           
215600       END-IF                                                             
215700                                                                          
215800       IF SYST-IDSEKVNR = 4                                               
215900         IF IN-EKH-KVANTAL < 0                                            
216000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
216100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
216200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
216300            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
216400            (IN-EKH-KVANTAL *                                             
216500             IN-EKH-PRARTNTO / WS-PRKURS-CN3 * WS-MARKUP)                 
216600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
216700           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
216800           IF WS-RED-IDKST > SPACE                                        
216900             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
217000             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
217100           ELSE                                                           
217200             MOVE SPACE             TO R3-LINE-COST-CENTER                
217300           END-IF                                                         
217400           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
217500           PERFORM S02-WRITE-W57071A                                      
217600         END-IF                                                           
217700       END-IF                                                             
217800     END-EVALUATE                                                         
217900     .                                                                    
218000     EJECT                                                                
218100                                                                          
218200 CEBD-SUB-EVENT-102-123 SECTION.                                          
218300     EVALUATE IN-EKH-KDEKNIVA                                             
218400     WHEN 'DET'                                                           
218500       IF SYST-IDSEKVNR = 1                                               
218600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
218700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
218800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
218900              IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                          
219000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
219100         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
219200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
219300         MOVE SPACE               TO WS-ALLOCATE-REF                      
219400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
219500         PERFORM S02-WRITE-W57071A                                        
219600       END-IF                                                             
219700                                                                          
219800       IF SYST-IDSEKVNR = 2                                               
219900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
220000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
220100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
220200             IN-EKH-KVANTAL * (IN-EKH-PRARTSTD)                           
220300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
220400         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
220500         IF WS-RED-IDKST > SPACE                                          
220600           MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)             
220700           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
220800         ELSE                                                             
220900           MOVE SPACE             TO R3-LINE-COST-CENTER                  
221000         END-IF                                                           
221100         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
221200         PERFORM S02-WRITE-W57071A                                        
221300       END-IF                                                             
221400     END-EVALUATE                                                         
221500     .                                                                    
221600     EJECT                                                                
221700                                                                          
221800 CEBD-SUB-EVENT-102-124 SECTION.                                          
221900     EVALUATE IN-EKH-KDEKNIVA                                             
222000     WHEN 'DET'                                                           
222100       IF SYST-IDSEKVNR = 1                                               
222200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
222300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
222400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
222500         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN  * -1            
222600         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
222700         MOVE SPACE               TO WS-ALLOCATE-DC                       
222800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
222900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
223000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
223100         PERFORM S03-WRITE-W57072                                         
223200       END-IF                                                             
223300                                                                          
223400     WHEN 'FÖRS'                                                          
223500     WHEN 'FRAKT'                                                         
223600       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
223700       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
223800       MOVE SYST-IDKST          TO WS-RED-IDKST                           
223900       IF WS-RED-IDKST > SPACE                                            
224000         MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)               
224100         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
224200       ELSE                                                               
224300         MOVE SPACE             TO R3-LINE-COST-CENTER                    
224400       END-IF                                                             
224500       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
224600               IN-EKH-SUBEL * -1                                          
224700       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
224800       MOVE SPACE               TO WS-ALLOCATE-DC                         
224900       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
225000       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
225100       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
225200       PERFORM S04-WRITE-W57073A                                          
225300                                                                          
225400     WHEN 'EMB'                                                           
225500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
225600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
225700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
225800               IN-EKH-SUBEL / WS-PRKURS-CN * -1                           
225900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
226000       MOVE SPACE               TO WS-ALLOCATE-DC                         
226100       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
226200       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
226300       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
226400       PERFORM S04-WRITE-W57073A                                          
226500                                                                          
226600     WHEN 'DDI'                                                           
226700       IF IN-EKH-SUBEL > ZERO                                             
226800         IF SYST-IDSEKVNR = 1                                             
226900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
227000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
227100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
227200                   IN-EKH-SUBEL                                           
227300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
227400           MOVE SPACE               TO WS-ALLOCATE-DC                     
227500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
227600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
227700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
227800           PERFORM S04-WRITE-W57073A                                      
227900         END-IF                                                           
228000       ELSE                                                               
228100         IF SYST-IDSEKVNR = 2                                             
228200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
228300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
228400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
228500                   IN-EKH-SUBEL                                           
228600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
228700           MOVE SPACE               TO WS-ALLOCATE-DC                     
228800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
228900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
229000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
229100           PERFORM S04-WRITE-W57073A                                      
229200         END-IF                                                           
229300       END-IF                                                             
229400     END-EVALUATE                                                         
229500     .                                                                    
229600     EJECT                                                                
229700                                                                          
229800 CEBD-SUB-EVENT-102-125 SECTION.                                          
229900     EVALUATE IN-EKH-KDEKNIVA                                             
230000     WHEN 'DET'                                                           
230100       IF SYST-IDSEKVNR = 1                                               
230200         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
230300         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
230400         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
230500            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN) +           
230600            (IN-EKH-KVANTAL *                                             
230700             IN-EKH-PRARTNTO / WS-PRKURS-CN * WS-MARKUP)                  
230800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
230900         ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                        
231000         PERFORM S03-WRITE-W57072                                         
231100       END-IF                                                             
231200                                                                          
231300       IF SYST-IDSEKVNR = 2                                               
231400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
231500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
231600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
231700            (IN-EKH-KVANTAL *                                             
231800             IN-EKH-PRARTNTO / WS-PRKURS-CN * WS-MARKUP)                  
231900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
232000         SUBTRACT R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125                  
232100         PERFORM S03-WRITE-W57072                                         
232200       END-IF                                                             
232300                                                                          
232400                                                                          
232500     WHEN 'FÖRS'                                                          
232600     WHEN 'FRAKT'                                                         
232700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
232800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
232900       MOVE SYST-IDKST          TO WS-RED-IDKST                           
233000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
233100               (IN-EKH-SUBEL / WS-PRKURS-CN)                              
233200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
233300       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
233400       PERFORM S04-WRITE-W57073A                                          
233500                                                                          
233600     WHEN 'EMB'                                                           
233700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
233800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
233900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
234000               (IN-EKH-SUBEL / WS-PRKURS-CN)                              
234100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
234200       ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                          
234300       PERFORM S04-WRITE-W57073A                                          
234400                                                                          
234500     WHEN 'DDI'                                                           
234600       IF IN-EKH-SUBEL > ZERO                                             
234700         IF SYST-IDSEKVNR = 1                                             
234800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
234900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
235000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
235100                   IN-EKH-SUBEL                                           
235200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
235300           ADD  R3-LINE-AMOUNT TO SPAR-SUMMA-102-125                      
235400           PERFORM S04-WRITE-W57073A                                      
235500         END-IF                                                           
235600       ELSE                                                               
235700         IF SYST-IDSEKVNR = 2                                             
235800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
235900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
236000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
236100                   IN-EKH-SUBEL                                           
236200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
236300           SUBTRACT  R3-LINE-AMOUNT FROM SPAR-SUMMA-102-125               
236400           PERFORM S04-WRITE-W57073A                                      
236500         END-IF                                                           
236600       END-IF                                                             
236700     END-EVALUATE                                                         
236800     .                                                                    
236900     EJECT                                                                
237000                                                                          
237100 CEBE-SUB-EVENT-102-130 SECTION.                                          
237200     EVALUATE IN-EKH-KDEKNIVA                                             
237300     WHEN 'DET'                                                           
237400       IF SYST-IDSEKVNR = 1                                               
237500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
237600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
237700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
237800         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN  * -1            
237900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
238000         PERFORM S03-WRITE-W57072                                         
238100       END-IF                                                             
238200                                                                          
238300     WHEN 'FÖRS'                                                          
238400     WHEN 'FRAKT'                                                         
238500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
238600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
238700       MOVE SYST-IDKST          TO WS-RED-IDKST                           
238800       IF WS-RED-IDKST > SPACE                                            
238900         MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)               
239000         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
239100       ELSE                                                               
239200         MOVE SPACE             TO R3-LINE-COST-CENTER                    
239300       END-IF                                                             
239400       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
239500               IN-EKH-SUBEL / WS-PRKURS-CN  * -1                          
239600       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
239700       PERFORM S04-WRITE-W57073A                                          
239800                                                                          
239900     WHEN 'EMB'                                                           
240000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
240100       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
240200       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
240300               IN-EKH-SUBEL / WS-PRKURS-CN  * -1                          
240400       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
240500       PERFORM S04-WRITE-W57073A                                          
240600                                                                          
240700     WHEN 'DDI'                                                           
240800       IF IN-EKH-SUBEL > ZERO                                             
240900         IF SYST-IDSEKVNR = 1                                             
241000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
241100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
241200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
241300                   IN-EKH-SUBEL                                           
241400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
241500           PERFORM S04-WRITE-W57073A                                      
241600         END-IF                                                           
241700       ELSE                                                               
241800         IF SYST-IDSEKVNR = 2                                             
241900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
242000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
242100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
242200                   IN-EKH-SUBEL                                           
242300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
242400           PERFORM S04-WRITE-W57073A                                      
242500         END-IF                                                           
242600       END-IF                                                             
242700                                                                          
242800     END-EVALUATE                                                         
242900     .                                                                    
243000     EJECT                                                                
243100                                                                          
243200 CEBE-SUB-EVENT-102-131 SECTION.                                          
243300     EVALUATE IN-EKH-KDEKNIVA                                             
243400     WHEN 'DET'                                                           
243500       IF SYST-IDSEKVNR = 1                                               
243600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
243700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
243800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
243900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3)            
244000         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-1                   
244100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
244200         PERFORM S03-WRITE-W57072                                         
244300       END-IF                                                             
244400                                                                          
244500       IF SYST-IDSEKVNR = 2                                               
244600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
244700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
244800         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
244900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
245000            (IN-EKH-KVANTAL *                                             
245100             IN-EKH-PRARTNTO / WS-PRKURS-CN3 * WS-MARKUP)                 
245200         MOVE R3-LINE-AMOUNT-LC TO WS-LINE-AMOUNT-121-2                   
245300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
245400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
245500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
245600         MOVE SPACE               TO WS-ALLOCATE-REF                      
245700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
245800         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
245900         IF WS-RED-IDKST > SPACE                                          
246000           MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)             
246100           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
246200         ELSE                                                             
246300           MOVE SPACE             TO R3-LINE-COST-CENTER                  
246400         END-IF                                                           
246500         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
246600         PERFORM S03-WRITE-W57072                                         
246700       END-IF                                                             
246800                                                                          
246900       IF SYST-IDSEKVNR = 3                                               
247000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
247100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
247200         COMPUTE R3-LINE-AMOUNT-LC =                                      
247300                 WS-LINE-AMOUNT-121-2 - WS-LINE-AMOUNT-121-1              
247400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
247500         PERFORM S03-WRITE-W57072                                         
247600       END-IF                                                             
247700                                                                          
247800     WHEN 'FÖRS'                                                          
247900     WHEN 'FRAKT'                                                         
248000       IF SYST-IDSEKVNR = 1                                               
248100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
248200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
248300         MOVE SYST-IDKST        TO WS-RED-IDKST                           
248400         IF WS-RED-IDKST > SPACE                                          
248500           MOVE 'HB'            TO R3-LINE-COST-CENTER(1:2)               
248600           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
248700         ELSE                                                             
248800           MOVE SPACE           TO R3-LINE-COST-CENTER                    
248900         END-IF                                                           
249000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
249100                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
249200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
249300         PERFORM S04-WRITE-W57073A                                        
249400       END-IF                                                             
249500                                                                          
249600       IF SYST-IDSEKVNR = 2                                               
249700         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
249800         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
249900         MOVE SYST-IDKST        TO WS-RED-IDKST                           
250000         IF WS-RED-IDKST > SPACE                                          
250100           MOVE 'HB'            TO R3-LINE-COST-CENTER(1:2)               
250200           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
250300         ELSE                                                             
250400           MOVE SPACE           TO R3-LINE-COST-CENTER                    
250500         END-IF                                                           
250600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
250700                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
250800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
250900         PERFORM S04-WRITE-W57073A                                        
251000       END-IF                                                             
251100                                                                          
251200     WHEN 'EMB'                                                           
251300       IF SYST-IDSEKVNR = 1                                               
251400         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
251500         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
251600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
251700                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
251800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
251900         PERFORM S04-WRITE-W57073A                                        
252000       END-IF                                                             
252100                                                                          
252200       IF SYST-IDSEKVNR = 2                                               
252300         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
252400         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
252500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
252600                 IN-EKH-SUBEL / WS-PRKURS-CN3                             
252700         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
252800         PERFORM S04-WRITE-W57073A                                        
252900       END-IF                                                             
253000     END-EVALUATE                                                         
253100     .                                                                    
253200     EJECT                                                                
253300                                                                          
253400 CEBE-SUB-EVENT-102-132 SECTION.                                          
253500     EVALUATE IN-EKH-KDEKNIVA                                             
253600     WHEN 'DET'                                                           
253700       IF SYST-IDSEKVNR = 1                                               
253800         IF IN-EKH-KVANTAL > 0                                            
253900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
254000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
254100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
254200            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
254300            (IN-EKH-KVANTAL *                                             
254400             IN-EKH-PRARTNTO / WS-PRKURS-CN3 * WS-MARKUP)                 
254500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
254600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
254700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
254800           MOVE SPACE               TO WS-ALLOCATE-REF                    
254900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
255000           PERFORM S02-WRITE-W57071A                                      
255100         END-IF                                                           
255200       END-IF                                                             
255300                                                                          
255400       IF SYST-IDSEKVNR = 2                                               
255500         IF IN-EKH-KVANTAL < 0                                            
255600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
255700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
255800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
255900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
256000            (IN-EKH-KVANTAL *                                             
256100             IN-EKH-PRARTNTO / WS-PRKURS-CN3 * WS-MARKUP)                 
256200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
256300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
256400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
256500           MOVE SPACE               TO WS-ALLOCATE-REF                    
256600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
256700           PERFORM S02-WRITE-W57071A                                      
256800         END-IF                                                           
256900       END-IF                                                             
257000                                                                          
257100       IF SYST-IDSEKVNR = 3                                               
257200         IF IN-EKH-KVANTAL < 0                                            
257300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
257400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
257500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
257600            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
257700            (IN-EKH-KVANTAL *                                             
257800             IN-EKH-PRARTNTO / WS-PRKURS-CN3 * WS-MARKUP)                 
257900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
258000           PERFORM S02-WRITE-W57071A                                      
258100         END-IF                                                           
258200       END-IF                                                             
258300                                                                          
258400       IF SYST-IDSEKVNR = 4                                               
258500         IF IN-EKH-KVANTAL > 0                                            
258600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
258700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
258800           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
258900            (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3) +          
259000            (IN-EKH-KVANTAL *                                             
259100             IN-EKH-PRARTNTO / WS-PRKURS-CN3 * WS-MARKUP)                 
259200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
259300           PERFORM S02-WRITE-W57071A                                      
259400         END-IF                                                           
259500       END-IF                                                             
259600     END-EVALUATE                                                         
259700     .                                                                    
259800     EJECT                                                                
259900                                                                          
260000 CEBE-SUB-EVENT-102-134 SECTION.                                          
260100     EVALUATE IN-EKH-KDEKNIVA                                             
260200     WHEN 'DET'                                                           
260300       IF SYST-IDSEKVNR = 1                                               
260400         IF IN-EKH-KVANTAL > 0                                            
260500           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
260600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
260700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
260800           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN  * -1          
260900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
261000           MOVE SPACE               TO WS-ALLOCATE-DC                     
261100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
261200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
261300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
261400           PERFORM S03-WRITE-W57072                                       
261500         END-IF                                                           
261600       END-IF                                                             
261700                                                                          
261800       IF SYST-IDSEKVNR = 2                                               
261900         IF IN-EKH-KVANTAL < 0                                            
262000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
262100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
262200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
262300           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN  * -1          
262400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
262500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
262600           MOVE SPACE               TO WS-ALLOCATE-DC                     
262700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
262800           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
262900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
263000           PERFORM S03-WRITE-W57072                                       
263100         END-IF                                                           
263200       END-IF                                                             
263300                                                                          
263400     WHEN 'EMB'                                                           
263500       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
263600       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
263700       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
263800               IN-EKH-SUBEL / WS-PRKURS-CN   * -1                         
263900       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
264000       MOVE SPACE               TO WS-ALLOCATE-DC                         
264100       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
264200       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
264300       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
264400       PERFORM S04-WRITE-W57073A                                          
264500                                                                          
264600     WHEN 'DDI'                                                           
264700       IF IN-EKH-SUBEL > ZERO                                             
264800         IF SYST-IDSEKVNR = 1                                             
264900           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
265000           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
265100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
265200                   IN-EKH-SUBEL                                           
265300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
265400           MOVE SPACE               TO WS-ALLOCATE-DC                     
265500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
265600           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
265700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
265800           PERFORM S04-WRITE-W57073A                                      
265900         END-IF                                                           
266000       ELSE                                                               
266100         IF SYST-IDSEKVNR = 2                                             
266200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
266300           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
266400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
266500                   IN-EKH-SUBEL                                           
266600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
266700           MOVE SPACE               TO WS-ALLOCATE-DC                     
266800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
266900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
267000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
267100           PERFORM S04-WRITE-W57073A                                      
267200         END-IF                                                           
267300       END-IF                                                             
267400                                                                          
267500     END-EVALUATE                                                         
267600     .                                                                    
267700     EJECT                                                                
267800                                                                          
267900 CEC-MAIN-EVENT-103 SECTION.                                              
268000     EVALUATE IN-EKH-KDEKSHT                                              
268100     WHEN '101'                                                           
268200          PERFORM CECA-SUB-EVENT-103-101                                  
268300     WHEN '102'                                                           
268400          PERFORM CECB-SUB-EVENT-103-102                                  
268500     END-EVALUATE                                                         
268600     .                                                                    
268700     EJECT                                                                
268800                                                                          
268900 CECA-SUB-EVENT-103-101 SECTION.                                          
269000     EVALUATE IN-EKH-KDEKNIVA                                             
269100     WHEN 'DET'                                                           
269200       IF SYST-IDSEKVNR = 1                                               
269300         IF IN-EKH-KVANTAL > 0                                            
269400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
269500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
269600           COMPUTE R3-LINE-AMOUNT    =                                    
269700                   IN-EKH-KVANTAL * IN-EKH-PRDIRLON                       
269800           MOVE R3-LINE-AMOUNT      TO SPAR-PRDIRLON                      
269900           IF IN-EKH-KDVALISO = 'CNY'                                     
270000             MOVE R3-LINE-AMOUNT  TO R3-LINE-AMOUNT-LC                    
270100           END-IF                                                         
270200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
270300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
270400           MOVE SPACE               TO WS-ALLOCATE-REF                    
270500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
270600           PERFORM S02-WRITE-W57071A                                      
270700                                                                          
270800           COMPUTE R3-LINE-AMOUNT    =                                    
270900                   IN-EKH-KVANTAL * IN-EKH-PRDMTRL                        
271000           MOVE R3-LINE-AMOUNT      TO SPAR-PRDMTRL                       
271100           IF IN-EKH-KDVALISO = 'CNY'                                     
271200             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
271300           END-IF                                                         
271400           PERFORM S02-WRITE-W57071A                                      
271500         END-IF                                                           
271600       END-IF                                                             
271700                                                                          
271800       IF SYST-IDSEKVNR = 2                                               
271900         IF IN-EKH-KVANTAL > 0                                            
272000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
272100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
272200           COMPUTE R3-LINE-AMOUNT    =                                    
272300                   IN-EKH-KVANTAL * (IN-EKH-PRDIRLON +                    
272400                                     IN-EKH-PRDMTRL)                      
272500           IF IN-EKH-KDVALISO = 'CNY'                                     
272600             MOVE R3-LINE-AMOUNT  TO R3-LINE-AMOUNT-LC                    
272700           END-IF                                                         
272800           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
272900           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
273000           MOVE SPACE               TO WS-ALLOCATE-REF                    
273100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
273200           PERFORM S11-ANALYSIS                                           
273300           PERFORM S02-WRITE-W57071A                                      
273400         END-IF                                                           
273500       END-IF                                                             
273600                                                                          
273700       IF SYST-IDSEKVNR = 3                                               
273800         IF IN-EKH-KVANTAL < 0                                            
273900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
274000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
274100           COMPUTE R3-LINE-AMOUNT    =                                    
274200                   IN-EKH-KVANTAL * IN-EKH-PRDIRLON                       
274300           MOVE R3-LINE-AMOUNT      TO SPAR-PRDIRLON                      
274400           IF IN-EKH-KDVALISO = 'CNY'                                     
274500             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
274600           END-IF                                                         
274700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
274800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
274900           MOVE SPACE               TO WS-ALLOCATE-REF                    
275000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
275100           PERFORM S02-WRITE-W57071A                                      
275200                                                                          
275300           COMPUTE R3-LINE-AMOUNT    =                                    
275400                   IN-EKH-KVANTAL * IN-EKH-PRDMTRL                        
275500           MOVE R3-LINE-AMOUNT      TO SPAR-PRDMTRL                       
275600           IF IN-EKH-KDVALISO = 'CNY'                                     
275700             MOVE R3-LINE-AMOUNT    TO R3-LINE-AMOUNT-LC                  
275800           END-IF                                                         
275900           PERFORM S02-WRITE-W57071A                                      
276000         END-IF                                                           
276100       END-IF                                                             
276200                                                                          
276300       IF SYST-IDSEKVNR = 4                                               
276400         IF IN-EKH-KVANTAL < 0                                            
276500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
276600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
276700           COMPUTE R3-LINE-AMOUNT    =                                    
276800                   IN-EKH-KVANTAL * (IN-EKH-PRDIRLON +                    
276900                                     IN-EKH-PRDMTRL)                      
277000           IF IN-EKH-KDVALISO = 'CNY'                                     
277100             MOVE R3-LINE-AMOUNT  TO R3-LINE-AMOUNT-LC                    
277200           END-IF                                                         
277300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
277400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
277500           MOVE SPACE               TO WS-ALLOCATE-REF                    
277600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
277700           PERFORM S11-ANALYSIS                                           
277800           PERFORM S02-WRITE-W57071A                                      
277900         END-IF                                                           
278000       END-IF                                                             
278100     END-EVALUATE                                                         
278200     .                                                                    
278300     EJECT                                                                
278400                                                                          
278500 CECB-SUB-EVENT-103-102 SECTION.                                          
278600     EVALUATE IN-EKH-KDEKNIVA                                             
278700     WHEN 'DET'                                                           
278800       IF SYST-IDSEKVNR = 1                                               
278900         IF IN-EKH-KVANTAL < 0                                            
279000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
279100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
279200           IF IN-EKH-KDVALISO = 'CNY'                                     
279300             COMPUTE R3-LINE-AMOUNT-LC =                                  
279400                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
279500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
279600           ELSE                                                           
279700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
279800                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS          
279900           END-IF                                                         
280000           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
280100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
280200           MOVE SPACE               TO WS-ALLOCATE-REF                    
280300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
280400           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
280500           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
280600           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
280700           PERFORM S04-WRITE-W57073A                                      
280800         END-IF                                                           
280900       END-IF                                                             
281000                                                                          
281100       IF SYST-IDSEKVNR = 2                                               
281200         IF IN-EKH-KVANTAL > 0                                            
281300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
281400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
281500           IF IN-EKH-KDVALISO = 'CNY'                                     
281600             COMPUTE R3-LINE-AMOUNT-LC =                                  
281700                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
281800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
281900           ELSE                                                           
282000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
282100                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD * W-PRKURS          
282200           END-IF                                                         
282300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
282400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
282500           MOVE SPACE               TO WS-ALLOCATE-REF                    
282600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
282700           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
282800           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
282900           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
283000           PERFORM S04-WRITE-W57073A                                      
283100         END-IF                                                           
283200       END-IF                                                             
283300                                                                          
283400     WHEN 'KALK'                                                          
283500       IF SYST-IDSEKVNR = 1                                               
283600         IF IN-EKH-SUBEL > 0                                              
283700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
283800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
283900           IF IN-EKH-KDVALISO = 'CNY'                                     
284000             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
284100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
284200           ELSE                                                           
284300             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
284400                                         W-PRKURS                         
284500           END-IF                                                         
284600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
284700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
284800           MOVE SPACE               TO WS-ALLOCATE-REF                    
284900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
285000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
285100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
285200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
285300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
285400           PERFORM S04-WRITE-W57073A                                      
285500         END-IF                                                           
285600       END-IF                                                             
285700                                                                          
285800       IF SYST-IDSEKVNR = 2                                               
285900         IF IN-EKH-SUBEL < 0                                              
286000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
286100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
286200           IF IN-EKH-KDVALISO = 'CNY'                                     
286300             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
286400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
286500           ELSE                                                           
286600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
286700                                         W-PRKURS                         
286800           END-IF                                                         
286900           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
287000           MOVE SPACE               TO WS-ALLOCATE-DC                     
287100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
287200           MOVE SPACE               TO WS-ALLOCATE-REF                    
287300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
287400           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
287500           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
287600           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
287700           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
287800           PERFORM S04-WRITE-W57073A                                      
287900         END-IF                                                           
288000       END-IF                                                             
288100                                                                          
288200       IF SYST-IDSEKVNR = 3                                               
288300         IF IN-EKH-SUBEL > 0                                              
288400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
288500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
288600           IF IN-EKH-KDVALISO = 'CNY'                                     
288700             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
288800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
288900           ELSE                                                           
289000             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
289100                                         W-PRKURS                         
289200           END-IF                                                         
289300           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
289400           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
289500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
289600           PERFORM S04-WRITE-W57073A                                      
289700         END-IF                                                           
289800       END-IF                                                             
289900                                                                          
290000       IF SYST-IDSEKVNR = 4                                               
290100         IF IN-EKH-SUBEL < 0                                              
290200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
290300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
290400           IF IN-EKH-KDVALISO = 'CNY'                                     
290500             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
290600             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
290700           ELSE                                                           
290800             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
290900                                         W-PRKURS                         
291000           END-IF                                                         
291100           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
291200           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
291300           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
291400           PERFORM S04-WRITE-W57073A                                      
291500         END-IF                                                           
291600       END-IF                                                             
291700                                                                          
291800     WHEN 'HEMT'                                                          
291900       IF SYST-IDSEKVNR = 1                                               
292000         IF IN-EKH-SUBEL > 0                                              
292100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
292200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
292300           IF IN-EKH-KDVALISO = 'CNY'                                     
292400             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
292500             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
292600           ELSE                                                           
292700             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
292800                                         W-PRKURS                         
292900           END-IF                                                         
293000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
293100           MOVE SPACE               TO WS-ALLOCATE-DC                     
293200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
293300           MOVE SPACE               TO WS-ALLOCATE-REF                    
293400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
293500           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
293600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
293700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
293800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
293900           PERFORM S04-WRITE-W57073A                                      
294000         END-IF                                                           
294100       END-IF                                                             
294200                                                                          
294300       IF SYST-IDSEKVNR = 2                                               
294400         IF IN-EKH-SUBEL < 0                                              
294500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
294600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
294700           IF IN-EKH-KDVALISO = 'CNY'                                     
294800             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
294900             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
295000           ELSE                                                           
295100             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
295200                                         W-PRKURS                         
295300           END-IF                                                         
295400           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
295500           MOVE SPACE               TO WS-ALLOCATE-DC                     
295600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
295700           MOVE SPACE               TO WS-ALLOCATE-REF                    
295800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
295900           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
296000           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
296100           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
296200           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
296300           PERFORM S04-WRITE-W57073A                                      
296400         END-IF                                                           
296500       END-IF                                                             
296600                                                                          
296700       IF SYST-IDSEKVNR = 3                                               
296800         IF IN-EKH-SUBEL > 0                                              
296900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
297000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
297100           IF IN-EKH-KDVALISO = 'CNY'                                     
297200             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
297300             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
297400           ELSE                                                           
297500             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
297600                                         W-PRKURS                         
297700           END-IF                                                         
297800           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
297900           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
298000           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
298100           PERFORM S04-WRITE-W57073A                                      
298200         END-IF                                                           
298300       END-IF                                                             
298400                                                                          
298500       IF SYST-IDSEKVNR = 4                                               
298600         IF IN-EKH-SUBEL < 0                                              
298700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
298800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
298900           IF IN-EKH-KDVALISO = 'CNY'                                     
299000             MOVE IN-EKH-SUBEL      TO R3-LINE-AMOUNT-LC                  
299100             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
299200           ELSE                                                           
299300             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = IN-EKH-SUBEL *           
299400                                         W-PRKURS                         
299500           END-IF                                                         
299600           MOVE R3-LINE-TEXT        TO WS-LINE-TEXT                       
299700           MOVE IN-EKH-IDKUNDRF     TO WS-LINE-TEXT-IDKUNDRF              
299800           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
299900           PERFORM S04-WRITE-W57073A                                      
300000         END-IF                                                           
300100       END-IF                                                             
300200                                                                          
300300     WHEN 'SUM'                                                           
300400       IF IN-EKH-SUBEL > ZERO                                             
300500         IF SYST-IDSEKVNR = 1                                             
300600           MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                    
300700           MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                     
300800           IF IN-EKH-KDVALISO = 'CNY'                                     
300900             MOVE IN-EKH-SUBEL     TO R3-LINE-AMOUNT-LC                   
301000             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
301100           ELSE                                                           
301200             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
301300                     IN-EKH-SUBEL  * W-PRKURS                             
301400           END-IF                                                         
301500           PERFORM S10-VATCODE                                            
301600           IF IN-EKH-SUVAT = ZERO                                         
301700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
301800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
301900           ELSE                                                           
302000             IF IN-EKH-KDVALISO = 'CNY'                                   
302100               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT-LC               
302200               MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT           
302300             ELSE                                                         
302400               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
302500                       IN-EKH-SUVAT * W-PRKURS                            
302600             END-IF                                                       
302700           END-IF                                                         
302800           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
302900                                                                          
303000           MOVE R3-LINE-TEXT       TO WS-LINE-TEXT                        
303100           MOVE IN-EKH-IDKUNDRF    TO WS-LINE-TEXT-IDKUNDRF               
303200           MOVE IN-EKH-IDLEVNR     TO WS-LINE-TEXT-IDLEVNR                
303300           MOVE WS-LINE-TEXT       TO R3-LINE-TEXT                        
303400           PERFORM S04-WRITE-W57073A                                      
303500         END-IF                                                           
303600       END-IF                                                             
303700                                                                          
303800       IF IN-EKH-SUBEL < ZERO                                             
303900         IF SYST-IDSEKVNR = 2                                             
304000           MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                    
304100           MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                     
304200           IF IN-EKH-KDVALISO = 'CNY'                                     
304300             MOVE IN-EKH-SUBEL     TO R3-LINE-AMOUNT-LC                   
304400             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
304500           ELSE                                                           
304600             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
304700                     IN-EKH-SUBEL * W-PRKURS                              
304800           END-IF                                                         
304900           PERFORM S10-VATCODE                                            
305000           IF IN-EKH-SUVAT = ZERO                                         
305100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
305200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
305300           ELSE                                                           
305400             IF IN-EKH-KDVALISO = 'CNY'                                   
305500               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT-LC               
305600               MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT           
305700             ELSE                                                         
305800               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
305900                       IN-EKH-SUVAT * W-PRKURS                            
306000             END-IF                                                       
306100           END-IF                                                         
306200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
306300                                                                          
306400           MOVE R3-LINE-TEXT       TO WS-LINE-TEXT                        
306500           MOVE IN-EKH-IDKUNDRF    TO WS-LINE-TEXT-IDKUNDRF               
306600           MOVE IN-EKH-IDLEVNR     TO WS-LINE-TEXT-IDLEVNR                
306700           MOVE WS-LINE-TEXT       TO R3-LINE-TEXT                        
306800           PERFORM S04-WRITE-W57073A                                      
306900         END-IF                                                           
307000       END-IF                                                             
307100                                                                          
307200     WHEN 'DDI'                                                           
307300       IF IN-EKH-SUBEL > ZERO                                             
307400         IF SYST-IDSEKVNR = 1                                             
307500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
307600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
307700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
307800                   IN-EKH-SUBEL                                           
307900           IF IN-EKH-KDVALISO = 'CNY'                                     
308000             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
308100           ELSE                                                           
308200             MOVE ZEROES              TO R3-LINE-AMOUNT                   
308300           END-IF                                                         
308400           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
308500           MOVE SPACE               TO WS-ALLOCATE-DC                     
308600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
308700           MOVE SPACE               TO WS-ALLOCATE-REF                    
308800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
308900           PERFORM S04-WRITE-W57073A                                      
309000         END-IF                                                           
309100       ELSE                                                               
309200         IF SYST-IDSEKVNR = 2                                             
309300           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
309400           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
309500           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
309600                   IN-EKH-SUBEL                                           
309700           IF IN-EKH-KDVALISO = 'CNY'                                     
309800             MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                   
309900           ELSE                                                           
310000             MOVE ZEROES              TO R3-LINE-AMOUNT                   
310100           END-IF                                                         
310200           MOVE SYST-IDKST          TO R3-LINE-COST-CENTER                
310300           MOVE SPACE               TO WS-ALLOCATE-DC                     
310400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
310500           MOVE SPACE               TO WS-ALLOCATE-REF                    
310600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
310700           PERFORM S04-WRITE-W57073A                                      
310800         END-IF                                                           
310900       END-IF                                                             
311000                                                                          
311100                                                                          
311200     END-EVALUATE                                                         
311300     .                                                                    
311400     EJECT                                                                
311500                                                                          
311600 CED-MAIN-EVENT-201 SECTION.                                              
311700     EVALUATE IN-EKH-KDEKSHT                                              
311800     WHEN '201'                                                           
311900          PERFORM CEDA-SUB-EVENT-201-201                                  
312000     WHEN '202'                                                           
312100          PERFORM CEDB-SUB-EVENT-201-202                                  
312200     END-EVALUATE                                                         
312300     .                                                                    
312400     EJECT                                                                
312500                                                                          
312600 CEDA-SUB-EVENT-201-201 SECTION.                                          
312700                                                                          
312800     EVALUATE IN-EKH-KDEKNIVA                                             
312900     WHEN 'DET'                                                           
313000       MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                           
313100       IF NOT DIS134-BYTESREN-CN                                          
313200          IF SYST-IDSEKVNR = 1                                            
313300            MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                  
313400            MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                   
313500            COMPUTE R3-LINE-AMOUNT-LC =                                   
313600                    IN-EKH-KVANTAL * IN-EKH-PRARTSTD                      
313700            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
313800            PERFORM S11-ANALYSIS                                          
313900            PERFORM S03-WRITE-W57072                                      
314000          END-IF                                                          
314100                                                                          
314200          IF SYST-IDSEKVNR = 2                                            
314300            MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                  
314400            MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                   
314500            COMPUTE R3-LINE-AMOUNT-LC =                                   
314600                    IN-EKH-KVANTAL * IN-EKH-PRARTSTD                      
314700            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
314800            MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                    
314900            MOVE SPACE               TO WS-ALLOCATE-DISTR                 
315000            MOVE SPACE               TO WS-ALLOCATE-REF                   
315100            MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                  
315200            PERFORM S03-WRITE-W57072                                      
315300          END-IF                                                          
315400       ELSE                                                               
315500          IF SYST-IDSEKVNR = 3                                            
315600            MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                  
315700            MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                   
315800            COMPUTE R3-LINE-AMOUNT-LC =                                   
315900                    IN-EKH-KVANTAL * IN-EKH-PRARTSTD                      
316000            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
316100            MOVE SYST-IDANALYS       TO R3-LINE-ORDER                     
316200            PERFORM S03-WRITE-W57072                                      
316300          END-IF                                                          
316400                                                                          
316500          IF SYST-IDSEKVNR = 4                                            
316600            MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                  
316700            MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                   
316800            COMPUTE R3-LINE-AMOUNT-LC =                                   
316900                    IN-EKH-KVANTAL * IN-EKH-PRARTSTD                      
317000            MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                      
317100            MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                    
317200            MOVE SPACE               TO WS-ALLOCATE-DISTR                 
317300            MOVE SPACE               TO WS-ALLOCATE-REF                   
317400            MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                  
317500            PERFORM S03-WRITE-W57072                                      
317600          END-IF                                                          
317700                                                                          
317800       END-IF                                                             
317900     END-EVALUATE                                                         
318000     .                                                                    
318100     EJECT                                                                
318200                                                                          
318300 CEDB-SUB-EVENT-201-202 SECTION.                                          
318400                                                                          
318500     EVALUATE IN-EKH-KDEKNIVA                                             
318600     WHEN 'DET'                                                           
318700       IF SYST-IDSEKVNR = 1                                               
318800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
318900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
319000         COMPUTE R3-LINE-AMOUNT-LC =                                      
319100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
319200         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
319300         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
319400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
319500         MOVE SPACE               TO WS-ALLOCATE-REF                      
319600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
319700         PERFORM S03-WRITE-W57072                                         
319800       END-IF                                                             
319900                                                                          
320000       IF SYST-IDSEKVNR = 2                                               
320100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
320200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
320300         COMPUTE R3-LINE-AMOUNT-LC =                                      
320400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
320500         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
320600         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
320700         PERFORM S03-WRITE-W57072                                         
320800       END-IF                                                             
320900                                                                          
321000     END-EVALUATE                                                         
321100     .                                                                    
321200     EJECT                                                                
321300                                                                          
321400 CEF-MAIN-EVENT-203 SECTION.                                              
321500     EVALUATE IN-EKH-KDEKSHT                                              
321600     WHEN '201'                                                           
321700          PERFORM CEFA-SUB-EVENT-203-201                                  
321800     END-EVALUATE                                                         
321900     .                                                                    
322000     EJECT                                                                
322100                                                                          
322200 CEFA-SUB-EVENT-203-201 SECTION.                                          
322300     EVALUATE IN-EKH-KDEKNIVA                                             
322400     WHEN 'DET'                                                           
322500       IF SYST-IDSEKVNR = 1                                               
322600         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
322700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
322800         IF IN-EKH-PRARTSTD > 0                                           
322900           COMPUTE R3-LINE-AMOUNT-LC =                                    
323000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
323100         ELSE                                                             
323200           COMPUTE R3-LINE-AMOUNT-LC =                                    
323300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
323400         END-IF                                                           
323500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
323600         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
323700         IF WS-RED-IDKST > SPACE                                          
323800           MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)             
323900           MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)             
324000         ELSE                                                             
324100           MOVE SPACE             TO R3-LINE-COST-CENTER                  
324200         END-IF                                                           
324300         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
324400         IF IN-EKH-IDKONTO = 432301                                       
324500           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
324600         END-IF                                                           
324700         PERFORM S03-WRITE-W57072                                         
324800       END-IF                                                             
324900                                                                          
325000       IF SYST-IDSEKVNR = 2                                               
325100         MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                         
325200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
325300         COMPUTE R3-LINE-AMOUNT-LC =                                      
325400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
325500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
325600         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
325700         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
325800         MOVE SPACE             TO WS-ALLOCATE-REF                        
325900         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
326000         IF IN-EKH-IDKONTO = 432301                                       
326100           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
326200         END-IF                                                           
326300         PERFORM S03-WRITE-W57072                                         
326400       END-IF                                                             
326500     END-EVALUATE                                                         
326600     .                                                                    
326700     EJECT                                                                
326800                                                                          
326900 CEG-MAIN-EVENT-204 SECTION.                                              
327000     EVALUATE IN-EKH-KDEKSHT                                              
327100     WHEN '201'                                                           
327200          PERFORM CEGA-SUB-EVENT-204-201                                  
327300     WHEN '301'                                                           
327400          PERFORM CEGB-SUB-EVENT-204-301                                  
327500     END-EVALUATE                                                         
327600     .                                                                    
327700     EJECT                                                                
327800                                                                          
327900 CEGA-SUB-EVENT-204-201 SECTION.                                          
328000     EVALUATE IN-EKH-KDEKNIVA                                             
328100     WHEN 'DET'                                                           
328200         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
328300         MOVE IN-EKH-IDARTNR      TO TEST-IDARTNR                         
328400* R-FAKTURA                                                               
328500       IF SYST-IDSEKVNR = 1                                               
328600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
328700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
328800         COMPUTE R3-LINE-AMOUNT-LC =                                      
328900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
329000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
329100         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
329200         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
329300         MOVE SPACE               TO WS-ALLOCATE-DC                       
329400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
329500         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
329600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
329700         PERFORM S03-WRITE-W57072                                         
329800       END-IF                                                             
329900                                                                          
330000       IF SYST-IDSEKVNR = 2                                               
330100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
330200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
330300         COMPUTE R3-LINE-AMOUNT-LC =                                      
330400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
330500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
330600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
330700         MOVE SPACE               TO WS-ALLOCATE-DC                       
330800         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
330900         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
331000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
331100         PERFORM S03-WRITE-W57072                                         
331200       END-IF                                                             
331300     END-EVALUATE                                                         
331400     .                                                                    
331500     EJECT                                                                
331600                                                                          
331700 CEGB-SUB-EVENT-204-301 SECTION.                                          
331800     EVALUATE IN-EKH-KDEKNIVA                                             
331900     WHEN 'DET'                                                           
332000         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
332100         MOVE IN-EKH-IDARTNR      TO TEST-IDARTNR                         
332200* R-FAKTURA                                                               
332300       IF SYST-IDSEKVNR = 1                                               
332400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
332500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
332600         COMPUTE R3-LINE-AMOUNT-LC =                                      
332700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
332800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
332900         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
333000         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
333100         PERFORM S03-WRITE-W57072                                         
333200       END-IF                                                             
333300                                                                          
333400       IF SYST-IDSEKVNR = 2                                               
333500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
333600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
333700         COMPUTE R3-LINE-AMOUNT-LC =                                      
333800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
333900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
334000         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
334100         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
334200         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
334300         MOVE SPACE             TO WS-ALLOCATE-REF                        
334400         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
334500         PERFORM S03-WRITE-W57072                                         
334600       END-IF                                                             
334700                                                                          
334800       IF SYST-IDSEKVNR = 3                                               
334900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
335000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
335100         COMPUTE R3-LINE-AMOUNT-LC =                                      
335200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
335300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
335400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
335500         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
335600         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
335700         MOVE SPACE             TO WS-ALLOCATE-REF                        
335800         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
335900         PERFORM S03-WRITE-W57072                                         
336000       END-IF                                                             
336100                                                                          
336200     WHEN 'FÖRS'                                                          
336300     WHEN 'FRAKT'                                                         
336400       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
336500       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
336600       MOVE SYST-IDKST          TO WS-RED-IDKST                           
336700       IF WS-RED-IDKST > SPACE                                            
336800         MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)               
336900         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
337000       ELSE                                                               
337100         MOVE SPACE             TO R3-LINE-COST-CENTER                    
337200       END-IF                                                             
337300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
337400               IN-EKH-SUBEL * -1                                          
337500       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
337600       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
337700       PERFORM S04-WRITE-W57073A                                          
337800                                                                          
337900     WHEN 'EMB'                                                           
338000       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
338100       MOVE SYST-IDKST          TO WS-RED-IDKST                           
338200       IF WS-RED-IDKST > SPACE                                            
338300         MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)               
338400         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
338500       ELSE                                                               
338600         MOVE SPACE             TO R3-LINE-COST-CENTER                    
338700       END-IF                                                             
338800       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
338900       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
339000               IN-EKH-SUBEL * -1                                          
339100       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
339200       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
339300       PERFORM S04-WRITE-W57073A                                          
339400                                                                          
339500     END-EVALUATE                                                         
339600     .                                                                    
339700     EJECT                                                                
339800                                                                          
339900 CEI-MAIN-EVENT-302 SECTION.                                              
340000     EVALUATE IN-EKH-KDEKSHT                                              
340100     WHEN '301'                                                           
340200          PERFORM CEIA-SUB-EVENT-302-301                                  
340300     WHEN '302'                                                           
340400          PERFORM CEIB-SUB-EVENT-302-302                                  
340500     END-EVALUATE                                                         
340600     .                                                                    
340700     EJECT                                                                
340800                                                                          
340900 CEIA-SUB-EVENT-302-301 SECTION.                                          
341000     EVALUATE IN-EKH-KDEKNIVA                                             
341100     WHEN 'DET'                                                           
341200       IF SYST-IDSEKVNR = 1                                               
341300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
341400         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
341500         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
341600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
341700         COMPUTE R3-LINE-AMOUNT-LC =                                      
341800                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
341900         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
342000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
342100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
342200         MOVE SPACE               TO WS-ALLOCATE-REF                      
342300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
342400         PERFORM S02-WRITE-W57071A                                        
342500       END-IF                                                             
342600                                                                          
342700       IF SYST-IDSEKVNR = 2                                               
342800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
342900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
343000         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
343100         IF WS-RED-IDKST > SPACE                                          
343200            MOVE 'HB'             TO R3-LINE-COST-CENTER(1:2)             
343300            MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)            
343400         ELSE                                                             
343500             MOVE SPACE           TO R3-LINE-COST-CENTER                  
343600         END-IF                                                           
343700         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
343800         COMPUTE R3-LINE-AMOUNT-LC =                                      
343900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
344000         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
344100         MOVE SPACE               TO WS-LINE-TEXT                         
344200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
344300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
344400         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
344500         PERFORM S02-WRITE-W57071A                                        
344600       END-IF                                                             
344700     END-EVALUATE                                                         
344800     .                                                                    
344900     EJECT                                                                
345000                                                                          
345100 CEIB-SUB-EVENT-302-302 SECTION.                                          
345200     EVALUATE IN-EKH-KDEKNIVA                                             
345300     WHEN 'DET'                                                           
345400       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
345500         IF SYST-IDSEKVNR = 1                                             
345600           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
345700           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
345800           IF BET-KDTRADP(3:2) NOT = SPACE                                
345900             MOVE '1'             TO WS-ACCOUNT-4                         
346000             MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER              
346100           ELSE                                                           
346200             MOVE '3'             TO WS-ACCOUNT-4                         
346300             MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER             
346400           END-IF                                                         
346500           COMPUTE R3-LINE-AMOUNT-LC =                                    
346600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
346700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
346800           PERFORM S02-WRITE-W57071A                                      
346900         END-IF                                                           
347000                                                                          
347100         IF SYST-IDSEKVNR = 2                                             
347200           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
347300           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
347400           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
347500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
347600           COMPUTE R3-LINE-AMOUNT-LC =                                    
347700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
347800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
347900           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
348000           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
348100           MOVE SPACE             TO WS-ALLOCATE-REF                      
348200           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
348300           PERFORM S02-WRITE-W57071A                                      
348400         END-IF                                                           
348500       ELSE                                                               
348600         IF SYST-IDSEKVNR = 3                                             
348700           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
348800           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
348900           IF BET-KDTRADP(3:2) NOT = SPACE                                
349000             MOVE '1'             TO WS-ACCOUNT-4                         
349100             MOVE BET-KDTRADP     TO R3-LINE-TRADING-PARTNER              
349200           ELSE                                                           
349300             MOVE '3'             TO WS-ACCOUNT-4                         
349400             MOVE BET-KDTRADP(1:2) TO R3-LINE-TRADING-PARTNER             
349500           END-IF                                                         
349600           COMPUTE R3-LINE-AMOUNT-LC =                                    
349700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
349800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
349900           PERFORM S02-WRITE-W57071A                                      
350000         END-IF                                                           
350100                                                                          
350200         IF SYST-IDSEKVNR = 4                                             
350300           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
350400           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                           
350500           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
350600           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
350700           COMPUTE R3-LINE-AMOUNT-LC =                                    
350800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
350900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
351000           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
351100           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
351200           MOVE SPACE             TO WS-ALLOCATE-REF                      
351300           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
351400           PERFORM S02-WRITE-W57071A                                      
351500         END-IF                                                           
351600       END-IF                                                             
351700     END-EVALUATE                                                         
351800     .                                                                    
351900     EJECT                                                                
352000                                                                          
352100 CEJ-MAIN-EVENT-303 SECTION.                                              
352200     EVALUATE IN-EKH-KDEKSHT                                              
352300     WHEN '3XX'                                                           
352400          PERFORM CEJ301-SUB-EVENT-303-3XX                                
352500     WHEN '301'                                                           
352600          PERFORM CEJ301-SUB-EVENT-303-301                                
352700     WHEN '307'                                                           
352800          PERFORM CEJ307-SUB-EVENT-303-307                                
352900     WHEN '310'                                                           
353000          PERFORM CEJ310-SUB-EVENT-303-310                                
353100     WHEN '311'                                                           
353200          PERFORM CEJ311-SUB-EVENT-303-311                                
353300     WHEN '371'                                                           
353400          PERFORM CEJ371-SUB-EVENT-303-371                                
353500     WHEN '377'                                                           
353600          PERFORM CEJ377-SUB-EVENT-303-377                                
353700     WHEN '387'                                                           
353800          PERFORM CEJ387-SUB-EVENT-303-387                                
353900     WHEN '391'                                                           
354000          PERFORM CEJ391-SUB-EVENT-303-391                                
354100     END-EVALUATE                                                         
354200     .                                                                    
354300     EJECT                                                                
354400                                                                          
354500 CEJ301-SUB-EVENT-303-3XX SECTION.                                        
354600     EVALUATE IN-EKH-KDEKNIVA                                             
354700                                                                          
354800     WHEN 'FÖRS'                                                          
354900     WHEN 'LEG'                                                           
355000     WHEN 'FRAKT'                                                         
355100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
355200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
355300       MOVE SYST-IDKST          TO WS-RED-IDKST                           
355400       IF WS-RED-IDKST > SPACE                                            
355500         MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)               
355600         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
355700       ELSE                                                               
355800         MOVE SPACE             TO R3-LINE-COST-CENTER                    
355900       END-IF                                                             
356000       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
356100       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
356200               (IN-EKH-SUBEL / WS-PRKURS-CN3) * -1                        
356300       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
356400       MOVE SPACE               TO WS-ALLOCATE-DC                         
356500       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
356600       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
356700       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
356800       PERFORM S03-WRITE-W57072                                           
356900                                                                          
357000     WHEN 'LAND'                                                          
357100       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
357200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
357300       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
357400               (IN-EKH-SUBEL / WS-PRKURS-CN3) * -1                        
357500       MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                         
357600       MOVE SPACE               TO WS-ALLOCATE-DC                         
357700       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
357800       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
357900       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
358000       PERFORM S03-WRITE-W57072                                           
358100                                                                          
358200     WHEN 'DDI'                                                           
358300       IF IN-EKH-SUBEL > ZERO                                             
358400         IF SYST-IDSEKVNR = 1                                             
358500           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
358600           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
358700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
358800                   IN-EKH-SUBEL                                           
358900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
359000           MOVE SPACE               TO WS-ALLOCATE-DC                     
359100           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
359200           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
359300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
359400           PERFORM S04-WRITE-W57073A                                      
359500         END-IF                                                           
359600       ELSE                                                               
359700         IF SYST-IDSEKVNR = 2                                             
359800           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
359900           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
360000           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
360100                   IN-EKH-SUBEL                                           
360200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
360300           MOVE SPACE               TO WS-ALLOCATE-DC                     
360400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
360500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
360600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
360700           PERFORM S04-WRITE-W57073A                                      
360800         END-IF                                                           
360900       END-IF                                                             
361000                                                                          
361100     END-EVALUATE                                                         
361200     .                                                                    
361300     EJECT                                                                
361400                                                                          
361500 CEJ301-SUB-EVENT-303-301 SECTION.                                        
361600     EVALUATE IN-EKH-KDEKNIVA                                             
361700     WHEN 'DET'                                                           
361800       IF SYST-IDSEKVNR = 1                                               
361900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
362000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
362100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
362200         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3 * -1            
362300         MOVE R3-LINE-AMOUNT-LC   TO R3-LINE-AMOUNT                       
362400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
362500         MOVE SPACE               TO WS-LINE-TEXT                         
362600         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
362700         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
362800         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
362900         MOVE SPACE               TO WS-ALLOCATE-DC                       
363000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
363100         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
363200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
363300         PERFORM S03-WRITE-W57072                                         
363400       END-IF                                                             
363500                                                                          
363600     END-EVALUATE                                                         
363700     .                                                                    
363800     EJECT                                                                
363900                                                                          
364000 CEJ307-SUB-EVENT-303-307 SECTION.                                        
364100     EVALUATE IN-EKH-KDEKNIVA                                             
364200     WHEN 'DET'                                                           
364300       IF SYST-IDSEKVNR = 1                                               
364400         MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                      
364500         MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                       
364600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
364700         IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3 * -1            
364800         MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                        
364900         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
365000         MOVE SPACE              TO WS-LINE-TEXT                          
365100         MOVE IN-EKH-KDEKHHT     TO WS-LINE-TEXT-KDEKHHT                  
365200         MOVE IN-EKH-KDEKSHT     TO WS-LINE-TEXT-KDEKSHT                  
365300         MOVE WS-LINE-TEXT       TO R3-LINE-TEXT                          
365400         MOVE SPACE               TO WS-ALLOCATE-DC                       
365500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
365600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
365700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
365800         PERFORM S03-WRITE-W57072                                         
365900       END-IF                                                             
366000                                                                          
366100     END-EVALUATE                                                         
366200     .                                                                    
366300     EJECT                                                                
366400                                                                          
366500 CEJ310-SUB-EVENT-303-310 SECTION.                                        
366600     EVALUATE IN-EKH-KDEKNIVA                                             
366700     WHEN 'DET'                                                           
366800       IF SYST-IDSEKVNR = 1                                               
366900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
367000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
367100         COMPUTE R3-LINE-AMOUNT-LC =                                      
367200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
367300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
367400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
367500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
367600         MOVE SPACE               TO WS-ALLOCATE-REF                      
367700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
367800         MOVE SPACE               TO WS-LINE-TEXT                         
367900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
368000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
368100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
368200         MOVE SPACE               TO WS-ALLOCATE-DC                       
368300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
368400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
368500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
368600         PERFORM S02-WRITE-W57071A                                        
368700       END-IF                                                             
368800                                                                          
368900       IF SYST-IDSEKVNR = 2                                               
369000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
369100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
369200         COMPUTE R3-LINE-AMOUNT-LC =                                      
369300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
369400         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
369500         PERFORM S11-ANALYSIS                                             
369600         MOVE SPACE               TO WS-LINE-TEXT                         
369700         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
369800         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
369900         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
370000         MOVE SPACE               TO WS-ALLOCATE-DC                       
370100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
370200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
370300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
370400         PERFORM S02-WRITE-W57071A                                        
370500       END-IF                                                             
370600     END-EVALUATE                                                         
370700     .                                                                    
370800     EJECT                                                                
370900                                                                          
371000 CEJ311-SUB-EVENT-303-311 SECTION.                                        
371100     EVALUATE IN-EKH-KDEKNIVA                                             
371200     WHEN 'DET'                                                           
371300        IF SYST-IDSEKVNR = 1                                              
371400          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
371500          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
371600          COMPUTE R3-LINE-AMOUNT-LC =                                     
371700                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
371800          MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                        
371900          MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER             
372000          MOVE SPACE               TO WS-LINE-TEXT                        
372100          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
372200          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
372300          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
372400          MOVE SPACE               TO WS-ALLOCATE-DC                      
372500          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
372600          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
372700          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
372800          PERFORM S02-WRITE-W57071A                                       
372900        END-IF                                                            
373000                                                                          
373100        IF SYST-IDSEKVNR = 2                                              
373200          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
373300          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
373400          COMPUTE R3-LINE-AMOUNT-LC =                                     
373500                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
373600          MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                        
373700          MOVE SPACE               TO WS-LINE-TEXT                        
373800          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
373900          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
374000          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
374100          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
374200          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
374300          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
374400          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
374500          PERFORM S02-WRITE-W57071A                                       
374600        END-IF                                                            
374700     END-EVALUATE                                                         
374800     .                                                                    
374900     EJECT                                                                
375000                                                                          
375100 CEJ371-SUB-EVENT-303-371 SECTION.                                        
375200     EVALUATE IN-EKH-KDEKNIVA                                             
375300     WHEN 'DET'                                                           
375400       IF SYST-IDSEKVNR = 1                                               
375500         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
375600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
375700         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
375800           IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN3               
375900         MOVE R3-LINE-AMOUNT-LC   TO  R3-LINE-AMOUNT                      
376000         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
376100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
376200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
376300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
376400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
376500         PERFORM S04-WRITE-W57073A                                        
376600       END-IF                                                             
376700                                                                          
376800     WHEN 'LAND'                                                          
376900       IF SYST-IDSEKVNR = 1                                               
377000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
377100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
377200         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
377300         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
377400               R3-LINE-AMOUNT-LC / WS-PRKURS-CN3                          
377500         MOVE R3-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                    
377600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
377700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
377800         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
377900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
378000         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
378100         PERFORM S02-WRITE-W57071A                                        
378200       END-IF                                                             
378300                                                                          
378400     WHEN 'DDI'                                                           
378500       IF IN-EKH-SUBEL > ZERO                                             
378600         IF SYST-IDSEKVNR = 1                                             
378700           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
378800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
378900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
379000                   IN-EKH-SUBEL                                           
379100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
379200           MOVE SPACE               TO WS-ALLOCATE-DC                     
379300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
379400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
379500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
379600           PERFORM S02-WRITE-W57071A                                      
379700         END-IF                                                           
379800       ELSE                                                               
379900         IF SYST-IDSEKVNR = 2                                             
380000           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
380100           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
380200           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
380300                   IN-EKH-SUBEL                                           
380400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
380500           MOVE SPACE               TO WS-ALLOCATE-DC                     
380600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
380700           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
380800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
380900           PERFORM S02-WRITE-W57071A                                      
381000         END-IF                                                           
381100       END-IF                                                             
381200     END-EVALUATE                                                         
381300     .                                                                    
381400     EJECT                                                                
381500                                                                          
381600 CEJ377-SUB-EVENT-303-377 SECTION.                                        
381700     EVALUATE IN-EKH-KDEKNIVA                                             
381800     WHEN 'DET'                                                           
381900       IF SYST-IDSEKVNR = 1                                               
382000         IF IN-EKH-KVANTAL < 0                                            
382100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
382200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
382300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
382400             IN-EKH-KVANTAL * IN-EKH-PRARTNTO                             
382500           COMPUTE R3-LINE-AMOUNT ROUNDED =                               
382600                 R3-LINE-AMOUNT-LC * WS-PRKURS-CN2                        
382700           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
382800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
382900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
383000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
383100           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
383200           PERFORM S04-WRITE-W57073A                                      
383300         END-IF                                                           
383400       END-IF                                                             
383500                                                                          
383600     END-EVALUATE                                                         
383700     .                                                                    
383800     EJECT                                                                
383900                                                                          
384000 CEJ387-SUB-EVENT-303-387 SECTION.                                        
384100     EVALUATE IN-EKH-KDEKNIVA                                             
384200     WHEN 'DET'                                                           
384300       IF SYST-IDSEKVNR = 1                                               
384400         MOVE SYST-IDKONTO       TO WS-R3-ACCOUNT-10                      
384500         MOVE WS-R3-ACCOUNT-6    TO R3-LINE-ACCOUNT                       
384600         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
384700*             IN-EKH-KVANTAL * IN-EKH-PRARTNTO * -1                       
384800         (IN-EKH-KVANTAL * IN-EKH-PRARTNTO / WS-PRKURS-CN) * -1           
384900         MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                        
385000         MOVE W-BET-IDPARTNR-NUM TO R3-LINE-PA-CUSTOMER                   
385100         MOVE SPACE              TO WS-LINE-TEXT                          
385200         MOVE IN-EKH-KDEKHHT     TO WS-LINE-TEXT-KDEKHHT                  
385300         MOVE IN-EKH-KDEKSHT     TO WS-LINE-TEXT-KDEKSHT                  
385400         MOVE WS-LINE-TEXT       TO R3-LINE-TEXT                          
385500         MOVE SPACE               TO WS-ALLOCATE-DC                       
385600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
385700         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
385800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
385900         PERFORM S03-WRITE-W57072                                         
386000       END-IF                                                             
386100     END-EVALUATE                                                         
386200     .                                                                    
386300     EJECT                                                                
386400                                                                          
386500 CEJ391-SUB-EVENT-303-391 SECTION.                                        
386600     EVALUATE IN-EKH-KDEKNIVA                                             
386700     WHEN 'DET'                                                           
386800       IF SYST-IDSEKVNR = 1                                               
386900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
387000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
387100         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
387200              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
387300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
387400         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
387500         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
387600         MOVE SPACES              TO WS-LINE-TEXT                         
387700         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
387800         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
387900         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
388000         MOVE SPACE               TO WS-ALLOCATE-DC                       
388100         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
388200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
388300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
388400         PERFORM S03-WRITE-W57072                                         
388500       END-IF                                                             
388600                                                                          
388700       IF SYST-IDSEKVNR = 2                                               
388800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
388900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
389000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
389100              IN-EKH-KVANTAL *  IN-EKH-PRARTSTD * -1                      
389200         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
389300         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
389400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
389500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
389600         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
389700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
389800         MOVE SPACES              TO WS-LINE-TEXT                         
389900         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
390000         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
390100         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
390200         PERFORM S03-WRITE-W57072                                         
390300       END-IF                                                             
390400                                                                          
390500     WHEN 'FÖRS'                                                          
390600     WHEN 'LEG'                                                           
390700     WHEN 'FRAKT'                                                         
390800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
390900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
391000       MOVE SYST-IDKST          TO WS-RED-IDKST                           
391100       IF WS-RED-IDKST > SPACE                                            
391200         MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)               
391300         MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)               
391400       ELSE                                                               
391500         MOVE SPACE             TO R3-LINE-COST-CENTER                    
391600       END-IF                                                             
391700       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
391800       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
391900               IN-EKH-SUBEL * -1                                          
392000       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
392100       MOVE SPACE               TO WS-ALLOCATE-DC                         
392200       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
392300       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
392400       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
392500       PERFORM S03-WRITE-W57072                                           
392600                                                                          
392700     WHEN 'LAND'                                                          
392800       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
392900       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
393000       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
393100               IN-EKH-SUBEL * -1                                          
393200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
393300       MOVE SPACE               TO WS-ALLOCATE-DC                         
393400       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
393500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
393600       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
393700       PERFORM S03-WRITE-W57072                                           
393800                                                                          
393900     END-EVALUATE                                                         
394000     .                                                                    
394100     EJECT                                                                
394200                                                                          
394300 CEK-MAIN-EVENT-401 SECTION.                                              
394400     EVALUATE IN-EKH-KDEKNIVA                                             
394500                                                                          
394600* PRISÄNDRING LÖPANDE                                                     
394700     WHEN 'DET'                                                           
394800       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
394900                           IN-EKH-PRARTSTD                                
395000       IF SYST-IDSEKVNR = 1                                               
395100* PRISHÖJNING                                                             
395200         IF WS-BELOPP > 0                                                 
395300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
395400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
395500           COMPUTE R3-LINE-AMOUNT-LC =                                    
395600                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
395700           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
395800           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
395900           MOVE 0000406098          TO R3-LINE-PA-CUSTOMER                
396000           PERFORM S11-ANALYSIS                                           
396100           PERFORM S02-WRITE-W57071A                                      
396200         END-IF                                                           
396300       END-IF                                                             
396400                                                                          
396500       IF SYST-IDSEKVNR = 2                                               
396600* PRISHÖJNING                                                             
396700         IF WS-BELOPP > 0                                                 
396800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
396900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
397000           COMPUTE R3-LINE-AMOUNT-LC =                                    
397100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
397200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
397300           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
397400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
397500           MOVE SPACE               TO WS-ALLOCATE-REF                    
397600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
397700           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
397800           MOVE 0000406098          TO R3-LINE-PA-CUSTOMER                
397900           PERFORM S02-WRITE-W57071A                                      
398000         END-IF                                                           
398100       END-IF                                                             
398200                                                                          
398300       IF SYST-IDSEKVNR = 3                                               
398400* PRISSÄNKNING                                                            
398500         IF WS-BELOPP < 0                                                 
398600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
398700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
398800           COMPUTE R3-LINE-AMOUNT-LC =                                    
398900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
399000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
399100           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
399200           MOVE 0000406098          TO R3-LINE-PA-CUSTOMER                
399300           PERFORM S11-ANALYSIS                                           
399400           PERFORM S02-WRITE-W57071A                                      
399500         END-IF                                                           
399600       END-IF                                                             
399700                                                                          
399800       IF SYST-IDSEKVNR = 4                                               
399900* PRISSÄNKNING                                                            
400000         IF WS-BELOPP < 0                                                 
400100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
400200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
400300           COMPUTE R3-LINE-AMOUNT-LC =                                    
400400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
400500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
400600           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
400700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
400800           MOVE SPACE               TO WS-ALLOCATE-REF                    
400900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
401000           MOVE 'CN  '              TO R3-LINE-TRADING-PARTNER            
401100           MOVE 0000406098          TO R3-LINE-PA-CUSTOMER                
401200           PERFORM S02-WRITE-W57071A                                      
401300         END-IF                                                           
401400       END-IF                                                             
401500     END-EVALUATE                                                         
401600     .                                                                    
401700     EJECT                                                                
401800                                                                          
401900 CEL-MAIN-EVENT-402 SECTION.                                              
402000     EVALUATE IN-EKH-KDEKNIVA                                             
402100     WHEN 'DET'                                                           
402200       IF SYST-IDSEKVNR = 1                                               
402300         IF IN-EKH-KVANTAL > 0                                            
402400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
402500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
402600           COMPUTE R3-LINE-AMOUNT-LC =                                    
402700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
402800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
402900           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
403000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
403100           MOVE SPACE               TO WS-ALLOCATE-REF                    
403200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
403300           PERFORM S02-WRITE-W57071A                                      
403400         END-IF                                                           
403500       END-IF                                                             
403600                                                                          
403700       IF SYST-IDSEKVNR = 2                                               
403800         IF IN-EKH-KVANTAL < 0                                            
403900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
404000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
404100           COMPUTE R3-LINE-AMOUNT-LC =                                    
404200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
404300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
404400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
404500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
404600           MOVE SPACE               TO WS-ALLOCATE-REF                    
404700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
404800           PERFORM S02-WRITE-W57071A                                      
404900         END-IF                                                           
405000       END-IF                                                             
405100     END-EVALUATE                                                         
405200     .                                                                    
405300     EJECT                                                                
405400                                                                          
405500 CEM-MAIN-EVENT-403 SECTION.                                              
405600     EVALUATE IN-EKH-KDEKSHT                                              
405700     WHEN '401'                                                           
405800     WHEN '402'                                                           
405900     WHEN '403'                                                           
406000     WHEN '404'                                                           
406100     WHEN '405'                                                           
406200     WHEN '407'                                                           
406300     WHEN '408'                                                           
406400     WHEN '409'                                                           
406500          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
406600     WHEN '410'                                                           
406700          PERFORM CEMB-SUB-EVENT-403-410                                  
406800     END-EVALUATE                                                         
406900     .                                                                    
407000     EJECT                                                                
407100                                                                          
407200 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
407300     EVALUATE IN-EKH-KDEKNIVA                                             
407400     WHEN 'DET'                                                           
407500       IF SYST-IDSEKVNR = 1                                               
407600         IF IN-EKH-KVANTAL > 0                                            
407700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
407800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
407900           COMPUTE R3-LINE-AMOUNT-LC =                                    
408000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
408100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
408200           PERFORM S11-ANALYSIS                                           
408300           PERFORM S02-WRITE-W57071A                                      
408400         END-IF                                                           
408500       END-IF                                                             
408600                                                                          
408700       IF SYST-IDSEKVNR = 2                                               
408800         IF IN-EKH-KVANTAL > 0                                            
408900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
409000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
409100           COMPUTE R3-LINE-AMOUNT-LC =                                    
409200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
409300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
409400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
409500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
409600           MOVE SPACE               TO WS-ALLOCATE-REF                    
409700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
409800           PERFORM S02-WRITE-W57071A                                      
409900         END-IF                                                           
410000       END-IF                                                             
410100                                                                          
410200       IF SYST-IDSEKVNR = 3                                               
410300         IF IN-EKH-KVANTAL < 0                                            
410400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
410500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
410600           COMPUTE R3-LINE-AMOUNT-LC =                                    
410700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
410800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
410900           PERFORM S11-ANALYSIS                                           
411000           PERFORM S02-WRITE-W57071A                                      
411100         END-IF                                                           
411200       END-IF                                                             
411300                                                                          
411400       IF SYST-IDSEKVNR = 4                                               
411500         IF IN-EKH-KVANTAL < 0                                            
411600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
411700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
411800           COMPUTE R3-LINE-AMOUNT-LC =                                    
411900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
412000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
412100           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
412200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
412300           MOVE SPACE               TO WS-ALLOCATE-REF                    
412400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
412500           PERFORM S02-WRITE-W57071A                                      
412600         END-IF                                                           
412700       END-IF                                                             
412800     END-EVALUATE                                                         
412900     .                                                                    
413000     EJECT                                                                
413100                                                                          
413200 CEMB-SUB-EVENT-403-410 SECTION.                                          
413300     EVALUATE IN-EKH-KDEKNIVA                                             
413400     WHEN 'DET'                                                           
413500       IF SYST-IDSEKVNR = 1                                               
413600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
413700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
413800         COMPUTE R3-LINE-AMOUNT-LC =                                      
413900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
414000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
414100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
414200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
414300         MOVE SPACE               TO WS-ALLOCATE-REF                      
414400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
414500         PERFORM S02-WRITE-W57071A                                        
414600       END-IF                                                             
414700                                                                          
414800       IF SYST-IDSEKVNR = 2                                               
414900         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
415000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
415100         COMPUTE R3-LINE-AMOUNT-LC =                                      
415200                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
415300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
415400         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
415500         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
415600         MOVE SPACE               TO WS-ALLOCATE-REF                      
415700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
415800         PERFORM S02-WRITE-W57071A                                        
415900       END-IF                                                             
416000     END-EVALUATE                                                         
416100     .                                                                    
416200     EJECT                                                                
416300                                                                          
416400 CEN-MAIN-EVENT-404 SECTION.                                              
416500     EVALUATE IN-EKH-KDEKNIVA                                             
416600     WHEN 'DET'                                                           
416700       IF SYST-IDSEKVNR = 1                                               
416800* KONTO EJ MANUELLT REGISTRERAT                                           
416900         IF IN-EKH-IDKONTO = 0                                            
417000           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
417100           IF DIST18-SCRAP-NDC-SC                                         
417200           OR DIST18-SCRAP-NDC-SC-LOCAL                                   
417300           OR DIST18-SCRAP-NDC-QUAL                                       
417400             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
417500             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
417600             COMPUTE R3-LINE-AMOUNT-LC =                                  
417700                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
417800             MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                     
417900             PERFORM S11-ANALYSIS                                         
418000             PERFORM S02-WRITE-W57071A                                    
418100           END-IF                                                         
418200         END-IF                                                           
418300       END-IF                                                             
418400                                                                          
418500       IF SYST-IDSEKVNR = 2                                               
418600* KONTO MANUELLT REGISTRERAT                                              
418700         IF IN-EKH-IDKONTO > 0                                            
418800           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
418900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
419000           COMPUTE R3-LINE-AMOUNT-LC =                                    
419100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
419200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
419300           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
419400           IF WS-RED-IDKST > SPACE                                        
419500             MOVE 'HB'              TO R3-LINE-COST-CENTER(1:2)           
419600             MOVE WS-RED-IDKST(1:5) TO R3-LINE-COST-CENTER(3:5)           
419700           ELSE                                                           
419800             MOVE SPACE             TO R3-LINE-COST-CENTER                
419900           END-IF                                                         
420000           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
420100           PERFORM S02-WRITE-W57071A                                      
420200         END-IF                                                           
420300       END-IF                                                             
420400                                                                          
420500       IF SYST-IDSEKVNR = 3                                               
420600         IF IN-EKH-IDKUNDRF = 'OBJ'                                       
420700           CONTINUE                                                       
420800         ELSE                                                             
420900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
421000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
421100           COMPUTE R3-LINE-AMOUNT-LC =                                    
421200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
421300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
421400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
421500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
421600           MOVE SPACE               TO WS-ALLOCATE-REF                    
421700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
421800           PERFORM S02-WRITE-W57071A                                      
421900         END-IF                                                           
422000       END-IF                                                             
422100                                                                          
422200       IF SYST-IDSEKVNR = 4                                               
422300         IF IN-EKH-IDKUNDRF = 'OBJ'                                       
422400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
422500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
422600           COMPUTE R3-LINE-AMOUNT-LC =                                    
422700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
422800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
422900           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
423000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
423100           MOVE SPACE               TO WS-ALLOCATE-REF                    
423200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
423300           PERFORM S02-WRITE-W57071A                                      
423400         END-IF                                                           
423500       END-IF                                                             
423600     END-EVALUATE                                                         
423700     .                                                                    
423800     EJECT                                                                
423900                                                                          
424000 CEQ-MAIN-EVENT-501 SECTION.                                              
424100     EVALUATE IN-EKH-KDEKNIVA                                             
424200     WHEN 'DET'                                                           
424300       IF SYST-IDSEKVNR = 1                                               
424400         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
424500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
424600         COMPUTE R3-LINE-AMOUNT-LC =                                      
424700                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
424800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
424900         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
425000         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
425100         MOVE SPACE               TO WS-ALLOCATE-REF                      
425200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
425300         PERFORM S03-WRITE-W57072                                         
425400       END-IF                                                             
425500                                                                          
425600       IF SYST-IDSEKVNR = 2                                               
425700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
425800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
425900         COMPUTE R3-LINE-AMOUNT-LC =                                      
426000                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
426100         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
426200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
426300         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
426400         MOVE SPACE               TO WS-ALLOCATE-REF                      
426500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
426600         PERFORM S03-WRITE-W57072                                         
426700       END-IF                                                             
426800     END-EVALUATE                                                         
426900     .                                                                    
427000     EJECT                                                                
427100                                                                          
427200 CER-MAIN-EVENT-502 SECTION.                                              
427300     EVALUATE IN-EKH-KDEKSHT                                              
427400     WHEN '501'                                                           
427500          PERFORM CERA-SUB-EVENT-502-501                                  
427600     WHEN '502'                                                           
427700          PERFORM CERB-SUB-EVENT-502-502                                  
427800     WHEN '503'                                                           
427900          PERFORM CERB-SUB-EVENT-502-503                                  
428000     END-EVALUATE                                                         
428100     .                                                                    
428200     EJECT                                                                
428300                                                                          
428400 CERA-SUB-EVENT-502-501 SECTION.                                          
428500     EVALUATE IN-EKH-KDEKNIVA                                             
428600     WHEN 'DET'                                                           
428700       IF SYST-IDSEKVNR = 1                                               
428800* HÄR BOKAS FÖRLORAT KOLLI                                                
428900         IF IN-EKH-KVANTAL < 0                                            
429000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
429100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
429200           COMPUTE R3-LINE-AMOUNT-LC =                                    
429300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
429400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
429500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
429600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
429700           MOVE SPACE               TO WS-ALLOCATE-REF                    
429800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
429900           PERFORM S02-WRITE-W57071A                                      
430000         END-IF                                                           
430100       END-IF                                                             
430200                                                                          
430300       IF SYST-IDSEKVNR = 2                                               
430400* HÄR BOKAS FÖRLORAT KOLLI                                                
430500         IF IN-EKH-KVANTAL < 0                                            
430600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
430700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
430800           COMPUTE R3-LINE-AMOUNT-LC =                                    
430900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
431000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
431100           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
431200           PERFORM S02-WRITE-W57071A                                      
431300         END-IF                                                           
431400       END-IF                                                             
431500                                                                          
431600       IF SYST-IDSEKVNR = 3                                               
431700* HÄR BOKAS ÅTERFUNNET KOLLI                                              
431800         IF IN-EKH-KVANTAL > 0                                            
431900           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
432000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
432100           COMPUTE R3-LINE-AMOUNT-LC =                                    
432200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
432300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
432400           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
432500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
432600           MOVE SPACE               TO WS-ALLOCATE-REF                    
432700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
432800           PERFORM S02-WRITE-W57071A                                      
432900         END-IF                                                           
433000       END-IF                                                             
433100                                                                          
433200       IF SYST-IDSEKVNR = 4                                               
433300* HÄR BOKAS ÅTERFUNNET KOLLI                                              
433400         IF IN-EKH-KVANTAL > 0                                            
433500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
433600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
433700           COMPUTE R3-LINE-AMOUNT-LC =                                    
433800                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
433900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
434000           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
434100           PERFORM S02-WRITE-W57071A                                      
434200         END-IF                                                           
434300       END-IF                                                             
434400     END-EVALUATE                                                         
434500     .                                                                    
434600     EJECT                                                                
434700                                                                          
434800 CERB-SUB-EVENT-502-502     SECTION.                                      
434900     EVALUATE IN-EKH-KDEKNIVA                                             
435000     WHEN 'DET'                                                           
435100       IF SYST-IDSEKVNR = 1                                               
435200* HÄR BOKAS ÖVERLEVERNAS                                                  
435300         IF IN-EKH-KVANTAL > 0                                            
435400           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
435500           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
435600           COMPUTE R3-LINE-AMOUNT-LC =                                    
435700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
435800           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
435900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
436000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
436100           MOVE SPACE               TO WS-ALLOCATE-REF                    
436200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
436300           PERFORM S02-WRITE-W57071A                                      
436400         END-IF                                                           
436500       END-IF                                                             
436600                                                                          
436700       IF SYST-IDSEKVNR = 2                                               
436800* HÄR BOKAS ÖVERLEVERNAS                                                  
436900         IF IN-EKH-KVANTAL > 0                                            
437000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
437100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
437200           COMPUTE R3-LINE-AMOUNT-LC =                                    
437300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
437400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
437500           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
437600           MOVE SPACE               TO WS-ALLOCATE-REF                    
437700           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
437800           PERFORM S02-WRITE-W57071A                                      
437900         END-IF                                                           
438000       END-IF                                                             
438100     END-EVALUATE                                                         
438200     .                                                                    
438300     EJECT                                                                
438400                                                                          
438500 CERB-SUB-EVENT-502-503     SECTION.                                      
438600     EVALUATE IN-EKH-KDEKNIVA                                             
438700     WHEN 'DET'                                                           
438800       IF SYST-IDSEKVNR = 1                                               
438900* HÄR BOKAS UNDERLEVERNAS                                                 
439000         IF IN-EKH-KVANTAL < 0                                            
439100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
439200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
439300           COMPUTE R3-LINE-AMOUNT-LC =                                    
439400                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
439500           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
439600           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
439700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
439800           MOVE SPACE               TO WS-ALLOCATE-REF                    
439900           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
440000           PERFORM S02-WRITE-W57071A                                      
440100         END-IF                                                           
440200       END-IF                                                             
440300                                                                          
440400       IF SYST-IDSEKVNR = 2                                               
440500* HÄR BOKAS UNDERLEVERNAS                                                 
440600         IF IN-EKH-KVANTAL < 0                                            
440700           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
440800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
440900           COMPUTE R3-LINE-AMOUNT-LC =                                    
441000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
441100           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
441200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
441300           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
441400           MOVE SPACE               TO WS-ALLOCATE-REF                    
441500           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
441600           PERFORM S02-WRITE-W57071A                                      
441700         END-IF                                                           
441800       END-IF                                                             
441900     END-EVALUATE                                                         
442000     .                                                                    
442100     EJECT                                                                
442200                                                                          
442300 CES-MAIN-EVENT-503 SECTION.                                              
442400     EVALUATE IN-EKH-KDEKSHT                                              
442500     WHEN '501'                                                           
442600          PERFORM CESA-SUB-EVENT-503-501                                  
442700     WHEN '502'                                                           
442800          PERFORM CESB-SUB-EVENT-503-502                                  
442900     WHEN '503'                                                           
443000          PERFORM CESB-SUB-EVENT-503-503                                  
443100     WHEN '504'                                                           
443200          PERFORM CESC-SUB-EVENT-503-504                                  
443300     END-EVALUATE                                                         
443400     .                                                                    
443500     EJECT                                                                
443600                                                                          
443700 CESA-SUB-EVENT-503-501 SECTION.                                          
443800     EVALUATE IN-EKH-KDEKNIVA                                             
443900     WHEN 'DET'                                                           
444000       IF SYST-IDSEKVNR = 1                                               
444100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
444200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
444300         COMPUTE R3-LINE-AMOUNT-LC =                                      
444400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
444500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
444600         PERFORM S11-ANALYSIS                                             
444700         PERFORM S02-WRITE-W57071A                                        
444800       END-IF                                                             
444900                                                                          
445000       IF SYST-IDSEKVNR = 2                                               
445100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
445200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
445300         COMPUTE R3-LINE-AMOUNT-LC =                                      
445400                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
445500         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
445600         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
445700         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
445800         MOVE SPACE               TO WS-ALLOCATE-REF                      
445900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
446000         PERFORM S02-WRITE-W57071A                                        
446100       END-IF                                                             
446200     END-EVALUATE                                                         
446300     .                                                                    
446400     EJECT                                                                
446500                                                                          
446600 CESB-SUB-EVENT-503-502     SECTION.                                      
446700     EVALUATE IN-EKH-KDEKNIVA                                             
446800     WHEN 'DET'                                                           
446900       IF SYST-IDSEKVNR = 1                                               
447000* HÄR BOKAS ÖVERLEVERANS                                                  
447100         IF IN-EKH-KVANTAL > 0                                            
447200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
447300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
447400           COMPUTE R3-LINE-AMOUNT-LC =                                    
447500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
447600           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
447700           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
447800           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
447900           MOVE SPACE               TO WS-ALLOCATE-REF                    
448000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
448100           PERFORM S02-WRITE-W57071A                                      
448200         END-IF                                                           
448300       END-IF                                                             
448400                                                                          
448500       IF SYST-IDSEKVNR = 2                                               
448600* HÄR BOKAS ÖVERLEVERANS                                                  
448700         IF IN-EKH-KVANTAL > 0                                            
448800           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
448900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
449000           COMPUTE R3-LINE-AMOUNT-LC =                                    
449100                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
449200           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
449300           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
449400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
449500           MOVE SPACE               TO WS-ALLOCATE-REF                    
449600           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
449700           PERFORM S02-WRITE-W57071A                                      
449800         END-IF                                                           
449900       END-IF                                                             
450000     END-EVALUATE                                                         
450100     .                                                                    
450200     EJECT                                                                
450300                                                                          
450400 CESB-SUB-EVENT-503-503     SECTION.                                      
450500     EVALUATE IN-EKH-KDEKNIVA                                             
450600     WHEN 'DET'                                                           
450700       IF SYST-IDSEKVNR = 1                                               
450800* HÄR BOKAS UNDERLEVERANS                                                 
450900         IF IN-EKH-KVANTAL < 0                                            
451000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
451100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
451200           COMPUTE R3-LINE-AMOUNT-LC =                                    
451300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
451400           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
451500           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
451600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
451700           MOVE SPACE               TO WS-ALLOCATE-REF                    
451800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
451900           PERFORM S02-WRITE-W57071A                                      
452000         END-IF                                                           
452100       END-IF                                                             
452200                                                                          
452300       IF SYST-IDSEKVNR = 2                                               
452400* HÄR BOKAS UNDERLEVERANS                                                 
452500         IF IN-EKH-KVANTAL < 0                                            
452600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
452700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
452800           COMPUTE R3-LINE-AMOUNT-LC =                                    
452900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
453000           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
453100           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
453200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
453300           MOVE SPACE               TO WS-ALLOCATE-REF                    
453400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
453500           PERFORM S02-WRITE-W57071A                                      
453600         END-IF                                                           
453700       END-IF                                                             
453800     END-EVALUATE                                                         
453900     .                                                                    
454000     EJECT                                                                
454100                                                                          
454200 CESC-SUB-EVENT-503-504 SECTION.                                          
454300     EVALUATE IN-EKH-KDEKNIVA                                             
454400     WHEN 'DET'                                                           
454500       IF SYST-IDSEKVNR = 1                                               
454600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
454700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
454800         COMPUTE R3-LINE-AMOUNT-LC =                                      
454900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
455000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
455100         PERFORM S11-ANALYSIS                                             
455200         PERFORM S02-WRITE-W57071A                                        
455300       END-IF                                                             
455400                                                                          
455500       IF SYST-IDSEKVNR = 2                                               
455600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
455700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
455800         COMPUTE R3-LINE-AMOUNT-LC =                                      
455900                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
456000         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
456100         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
456200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
456300         MOVE SPACE               TO WS-ALLOCATE-REF                      
456400         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
456500         PERFORM S02-WRITE-W57071A                                        
456600       END-IF                                                             
456700     END-EVALUATE                                                         
456800     .                                                                    
456900     EJECT                                                                
457000                                                                          
457100 CF-BUILD-COMMON-210-PART SECTION.                                        
457200     MOVE SPACE              TO R3-LINE-R3                                
457300     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
457400                                R3-LINE-DUE-DATE                          
457500                                R3-LINE-AMOUNT                            
457600                                R3-LINE-AMOUNT-LC                         
457700                                R3-LINE-TAX-AMOUNT                        
457800                                R3-LINE-TAX-AMOUNT-LC                     
457900                                R3-LINE-NUMBER-OF-DAYS                    
458000                                R3-LINE-QUANTITY                          
458100                                R3-LINE-SAMNR                             
458200     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
458300     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
458400     MOVE 'CN05'             TO R3-LINE-COMPANY-CODE                      
458500     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
458600     IF SYST-KDPOST = '31'                                                
458700       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
458800     ELSE                                                                 
458900       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
459000     END-IF                                                               
459100     .                                                                    
459200     EJECT                                                                
459300                                                                          
459400 CG-SCHEDULE-LINE-AP SECTION.                                             
459500     MOVE NEJ                     TO WS-HEADER-SW                         
459600     MOVE JA                      TO WS-LINE-SW                           
459700     EVALUATE IN-EKH-KDEKHHT                                              
459800     WHEN '102'                                                           
459900       IF IN-EKH-KDEKSHT = '130'                                          
460000       OR IN-EKH-KDEKSHT = '134'                                          
460100         IF IN-EKH-KDEKSHT = '130'                                        
460200           PERFORM CGA-MAIN-EVENT-102-130                                 
460300         ELSE                                                             
460400           PERFORM CGA-MAIN-EVENT-102-134                                 
460500         END-IF                                                           
460600       ELSE                                                               
460700         IF IN-EKH-KDEKSHT = '120'                                        
460800         OR IN-EKH-KDEKSHT = '124'                                        
460900         OR IN-EKH-KDEKSHT = '125'                                        
461000           IF IN-EKH-KDEKSHT = '125'                                      
461100             PERFORM CGA-MAIN-EVENT-102-125                               
461200           ELSE                                                           
461300             PERFORM CGA-MAIN-EVENT-102-12X                               
461400           END-IF                                                         
461500         ELSE                                                             
461600           PERFORM CGA-MAIN-EVENT-102                                     
461700         END-IF                                                           
461800       END-IF                                                             
461900     WHEN '303'                                                           
462000       IF IN-EKH-KDEKSHT = '371'                                          
462100         PERFORM S81-GET-CURRENCY-RATE                                    
462200         PERFORM CGA-MAIN-EVENT-303-371                                   
462300       ELSE                                                               
462400         IF IN-EKH-KDEKSHT = '3XX'                                        
462500           PERFORM S81-GET-CURRENCY-RATE                                  
462600           PERFORM CGA-MAIN-EVENT-303-3XX                                 
462700         ELSE                                                             
462800           PERFORM CGA-MAIN-EVENT-303                                     
462900         END-IF                                                           
463000       END-IF                                                             
463100     END-EVALUATE                                                         
463200     .                                                                    
463300     EJECT                                                                
463400                                                                          
463500 CGA-MAIN-EVENT-102     SECTION.                                          
463600     EVALUATE IN-EKH-KDEKNIVA                                             
463700     WHEN 'SUM'                                                           
463800       IF IN-EKH-SUBEL > ZERO                                             
463900         IF SYST-IDSEKVNR = 1                                             
464000           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
464100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
464200           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
464300           PERFORM S10-VATCODE                                            
464400           IF IN-EKH-SUVAT = ZERO                                         
464500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
464600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
464700           ELSE                                                           
464800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
464900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
465000           END-IF                                                         
465100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
465200                                                                          
465300           PERFORM S04-WRITE-W57073A                                      
465400         END-IF                                                           
465500       END-IF                                                             
465600                                                                          
465700       IF IN-EKH-SUBEL < ZERO                                             
465800         IF SYST-IDSEKVNR = 2                                             
465900           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
466000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
466100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
466200           PERFORM S10-VATCODE                                            
466300           IF IN-EKH-SUVAT = ZERO                                         
466400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
466500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
466600           ELSE                                                           
466700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
466800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
466900           END-IF                                                         
467000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
467100                                                                          
467200           PERFORM S04-WRITE-W57073A                                      
467300         END-IF                                                           
467400       END-IF                                                             
467500     END-EVALUATE                                                         
467600     .                                                                    
467700     EJECT                                                                
467800                                                                          
467900 CGA-MAIN-EVENT-102-12X SECTION.                                          
468000     EVALUATE IN-EKH-KDEKNIVA                                             
468100     WHEN 'SUM'                                                           
468200       IF IN-EKH-SUBEL > ZERO                                             
468300         IF SYST-IDSEKVNR = 1                                             
468400           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
468500           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
468600           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
468700                   R3-LINE-AMOUNT    / WS-PRKURS-CN  * -1                 
468800           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
468900           PERFORM S10-VATCODE                                            
469000           IF IN-EKH-SUVAT = ZERO                                         
469100             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
469200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
469300           ELSE                                                           
469400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
469500             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
469600                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-CN  * -1           
469700             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
469800           END-IF                                                         
469900           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
470000                                                                          
470100           PERFORM S04-WRITE-W57073A                                      
470200         END-IF                                                           
470300       END-IF                                                             
470400                                                                          
470500       IF IN-EKH-SUBEL < ZERO                                             
470600         IF SYST-IDSEKVNR = 2                                             
470700           MOVE IN-EKH-IDLEVNR     TO R3-LINE-ACCOUNT                     
470800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
470900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
471000                   R3-LINE-AMOUNT    / WS-PRKURS-CN  * -1                 
471100           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
471200           PERFORM S10-VATCODE                                            
471300           IF IN-EKH-SUVAT = ZERO                                         
471400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
471500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
471600           ELSE                                                           
471700             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
471800             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
471900                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-CN  * -1           
472000             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
472100           END-IF                                                         
472200           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
472300                                                                          
472400           PERFORM S04-WRITE-W57073A                                      
472500         END-IF                                                           
472600       END-IF                                                             
472700     END-EVALUATE                                                         
472800     .                                                                    
472900     EJECT                                                                
473000                                                                          
473100 CGA-MAIN-EVENT-102-125 SECTION.                                          
473200     EVALUATE IN-EKH-KDEKNIVA                                             
473300     WHEN 'SUM'                                                           
473400       IF IN-EKH-SUBEL > ZERO                                             
473500         IF SYST-IDSEKVNR = 1                                             
473600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
473700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
473800           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
473900           MOVE SPAR-SUMMA-102-125 TO R3-LINE-AMOUNT                      
474000           MOVE SPAR-SUMMA-102-125 TO R3-LINE-AMOUNT-LC                   
474100           PERFORM S10-VATCODE                                            
474200           IF IN-EKH-SUVAT = ZERO                                         
474300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
474400             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
474500           ELSE                                                           
474600             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
474700             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
474800                     R3-LINE-TAX-AMOUNT / WS-PRKURS-CN  * -1              
474900             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
475000           END-IF                                                         
475100           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
475200           MOVE ZERO               TO SPAR-SUMMA-102-125                  
475300                                                                          
475400           PERFORM S04-WRITE-W57073A                                      
475500         END-IF                                                           
475600       END-IF                                                             
475700                                                                          
475800       IF IN-EKH-SUBEL < ZERO                                             
475900         IF SYST-IDSEKVNR = 2                                             
476000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
476100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
476200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
476300           MOVE SPAR-SUMMA-102-125 TO R3-LINE-AMOUNT                      
476400           MOVE SPAR-SUMMA-102-125 TO R3-LINE-AMOUNT-LC                   
476500           PERFORM S10-VATCODE                                            
476600           IF IN-EKH-SUVAT = ZERO                                         
476700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
476800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
476900           ELSE                                                           
477000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
477100             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
477200                     R3-LINE-TAX-AMOUNT / WS-PRKURS-CN  * -1              
477300             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
477400           END-IF                                                         
477500           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
477600           MOVE ZERO               TO SPAR-SUMMA-102-125                  
477700                                                                          
477800           PERFORM S04-WRITE-W57073A                                      
477900         END-IF                                                           
478000       END-IF                                                             
478100     END-EVALUATE                                                         
478200     .                                                                    
478300     EJECT                                                                
478400                                                                          
478500 CGA-MAIN-EVENT-102-130 SECTION.                                          
478600     EVALUATE IN-EKH-KDEKNIVA                                             
478700     WHEN 'SUM'                                                           
478800       IF IN-EKH-SUBEL > ZERO                                             
478900         IF SYST-IDSEKVNR = 1                                             
479000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
479100           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
479200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
479300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
479400                   R3-LINE-AMOUNT    / WS-PRKURS-CN  * -1                 
479500           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
479600           PERFORM S10-VATCODE                                            
479700           IF IN-EKH-SUVAT = ZERO                                         
479800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
479900             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
480000           ELSE                                                           
480100             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
480200             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
480300                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-CN  * -1           
480400             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
480500           END-IF                                                         
480600           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
480700                                                                          
480800           PERFORM S04-WRITE-W57073A                                      
480900         END-IF                                                           
481000       END-IF                                                             
481100                                                                          
481200       IF IN-EKH-SUBEL < ZERO                                             
481300         IF SYST-IDSEKVNR = 2                                             
481400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
481500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
481600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
481700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
481800                   R3-LINE-AMOUNT    / WS-PRKURS-CN                       
481900           MOVE R3-LINE-AMOUNT-LC  TO R3-LINE-AMOUNT                      
482000           PERFORM S10-VATCODE                                            
482100           IF IN-EKH-SUVAT = ZERO                                         
482200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
482300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
482400           ELSE                                                           
482500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
482600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
482700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-CN                 
482800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
482900           END-IF                                                         
483000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
483100                                                                          
483200           PERFORM S04-WRITE-W57073A                                      
483300         END-IF                                                           
483400       END-IF                                                             
483500     END-EVALUATE                                                         
483600     .                                                                    
483700     EJECT                                                                
483800                                                                          
483900 CGA-MAIN-EVENT-102-134 SECTION.                                          
484000     EVALUATE IN-EKH-KDEKNIVA                                             
484100     WHEN 'SUM'                                                           
484200       IF IN-EKH-SUBEL > ZERO                                             
484300         IF SYST-IDSEKVNR = 1                                             
484400           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
484500           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
484600           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
484700           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
484800                   R3-LINE-AMOUNT    / WS-PRKURS-CN  * -1                 
484900           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
485000           PERFORM S10-VATCODE                                            
485100           IF IN-EKH-SUVAT = ZERO                                         
485200             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
485300             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
485400           ELSE                                                           
485500             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
485600             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
485700                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-CN  * -1           
485800             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
485900           END-IF                                                         
486000           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
486100                                                                          
486200           PERFORM S04-WRITE-W57073A                                      
486300         END-IF                                                           
486400       END-IF                                                             
486500                                                                          
486600       IF IN-EKH-SUBEL < ZERO                                             
486700         IF SYST-IDSEKVNR = 2                                             
486800           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
486900           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
487000           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
487100           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
487200                   R3-LINE-AMOUNT    / WS-PRKURS-CN                       
487300           MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                       
487400           PERFORM S10-VATCODE                                            
487500           IF IN-EKH-SUVAT = ZERO                                         
487600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
487700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
487800           ELSE                                                           
487900             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
488000             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
488100                     R3-LINE-TAX-AMOUNT    / WS-PRKURS-CN                 
488200             MOVE R3-LINE-TAX-AMOUNT-LC TO R3-LINE-TAX-AMOUNT             
488300           END-IF                                                         
488400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
488500                                                                          
488600           PERFORM S04-WRITE-W57073A                                      
488700         END-IF                                                           
488800       END-IF                                                             
488900     END-EVALUATE                                                         
489000     .                                                                    
489100     EJECT                                                                
489200                                                                          
489300 CGA-MAIN-EVENT-303 SECTION.                                              
489400     EVALUATE IN-EKH-KDEKNIVA                                             
489500     WHEN 'SUM'                                                           
489600       IF SYST-IDSEKVNR = 1                                               
489700         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
489800         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
489900         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
490000               R3-LINE-AMOUNT-LC / WS-PRKURS-CN                           
490100         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
490200         PERFORM S10-VATCODE                                              
490300         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
490400         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
490500                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-CN                     
490600         MOVE R3-LINE-TAX-AMOUNT   TO R3-LINE-TAX-AMOUNT-LC               
490700         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
490800                                                                          
490900         PERFORM S04-WRITE-W57073A                                        
491000       END-IF                                                             
491100     END-EVALUATE                                                         
491200     .                                                                    
491300     EJECT                                                                
491400                                                                          
491500 CGA-MAIN-EVENT-303-3XX SECTION.                                          
491600     EVALUATE IN-EKH-KDEKNIVA                                             
491700     WHEN 'SUM'                                                           
491800       IF SYST-IDSEKVNR = 1                                               
491900         MOVE IN-EKH-IDLEVNR       TO R3-LINE-ACCOUNT                     
492000         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
492100         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
492200               R3-LINE-AMOUNT-LC / WS-PRKURS-CN3                          
492300         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
492400         PERFORM S10-VATCODE                                              
492500         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
492600         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
492700                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS-CN3                    
492800         MOVE R3-LINE-TAX-AMOUNT   TO R3-LINE-TAX-AMOUNT-LC               
492900         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
493000                                                                          
493100         PERFORM S04-WRITE-W57073A                                        
493200       END-IF                                                             
493300     END-EVALUATE                                                         
493400     .                                                                    
493500     EJECT                                                                
493600                                                                          
493700 CGA-MAIN-EVENT-303-371 SECTION.                                          
493800     EVALUATE IN-EKH-KDEKNIVA                                             
493900     WHEN 'SUM'                                                           
494000       IF SYST-IDSEKVNR = 1                                               
494100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
494200         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
494300         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
494400         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
494500               R3-LINE-AMOUNT-LC / WS-PRKURS-CN3                          
494600         MOVE R3-LINE-AMOUNT       TO R3-LINE-AMOUNT-LC                   
494700         PERFORM S10-VATCODE                                              
494800         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
494900         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
495000                 R3-LINE-TAX-AMOUNT-LC * WS-PRKURS-CN2                    
495100         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
495200                                                                          
495300         PERFORM S04-WRITE-W57073A                                        
495400       END-IF                                                             
495500     END-EVALUATE                                                         
495600     .                                                                    
495700     EJECT                                                                
495800                                                                          
495900 CH-BUILD-COMMON-310-PART SECTION.                                        
496000     MOVE SPACE              TO R3-LINE-R3                                
496100     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
496200                                R3-LINE-DUE-DATE                          
496300                                R3-LINE-AMOUNT                            
496400                                R3-LINE-AMOUNT-LC                         
496500                                R3-LINE-TAX-AMOUNT                        
496600                                R3-LINE-TAX-AMOUNT-LC                     
496700                                R3-LINE-NUMBER-OF-DAYS                    
496800                                R3-LINE-QUANTITY                          
496900                                R3-LINE-SAMNR                             
497000     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
497100     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
497200     MOVE 'CN05'             TO R3-LINE-COMPANY-CODE                      
497300     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
497400     IF SYST-KDPOST = '31'                                                
497500       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
497600     ELSE                                                                 
497700       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
497800     END-IF                                                               
497900     .                                                                    
498000     EJECT                                                                
498100                                                                          
498200 CI-SCHEDULE-LINE-AR SECTION.                                             
498300     MOVE NEJ                     TO WS-HEADER-SW                         
498400     MOVE JA                      TO WS-LINE-SW                           
498500     EVALUATE IN-EKH-KDEKHHT                                              
498600     WHEN '204'                                                           
498700         PERFORM CIA-MAIN-EVENT-204                                       
498800     END-EVALUATE                                                         
498900     .                                                                    
499000     EJECT                                                                
499100                                                                          
499200 CIA-MAIN-EVENT-204     SECTION.                                          
499300     EVALUATE IN-EKH-KDEKNIVA                                             
499400     WHEN 'SUM'                                                           
499500       IF IN-EKH-SUBEL > ZERO                                             
499600         IF SYST-IDSEKVNR = 1                                             
499700           IF IN-EKH-KDEKSHT = '301'                                      
499800             MOVE IN-EKH-IDLEVNR TO R3-LINE-ACCOUNT                       
499900           ELSE                                                           
500000             MOVE W-BET-IDPARTNR-NUM TO R3-LINE-ACCOUNT                   
500100           END-IF                                                         
500200           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT-LC                   
500300           COMPUTE R3-LINE-AMOUNT ROUNDED =                               
500400                   R3-LINE-AMOUNT-LC / WS-PRKURS                          
500500           PERFORM S10-VATCODE                                            
500600           IF IN-EKH-SUVAT = ZERO                                         
500700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
500800             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
500900           ELSE                                                           
501000             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT-LC               
501100             COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                         
501200                     R3-LINE-TAX-AMOUNT-LC / WS-PRKURS                    
501300           END-IF                                                         
501400           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
501500                                                                          
501600           PERFORM S04-WRITE-W57073A                                      
501700         END-IF                                                           
501800       END-IF                                                             
501900                                                                          
502000     END-EVALUATE                                                         
502100     .                                                                    
502200     EJECT                                                                
502300                                                                          
502400 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
502500     MOVE ZERO             TO LOGG-W57073                                 
502600     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
502700     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
502800     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
502900     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
503000     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
503100     MOVE SPACE TO LOGG-IDVERGL(10:1)                                     
503200     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
503300     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
503400     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
503500     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
503600     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
503700     MOVE 'VCCN'           TO LOGG-KDTRADP                                
503800                                                                          
503900****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
504000     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
504100     MOVE IN-FIL-TIREGDAT  TO WS-TIREGDAT                                 
504200     MOVE WS-TIREGDAT-TOT  TO AVST-DAREGDAT                               
504300     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
504400     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
504500     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
504600     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
504700     MOVE SPACE TO AVST-IDVERGL(10:1)                                     
504800     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
504900     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
505000     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
505100     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
505200     MOVE IN-EKH-PRARTSTD  TO AVST-PRAVCOST                               
505300     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
505400     .                                                                    
505500     EJECT                                                                
505600                                                                          
505700 Z-FINI SECTION.                                                          
505800     CLOSE W57066                                                         
505900           W57070                                                         
506000           W57071A                                                        
506100           W57072A                                                        
506200           W57073A                                                        
506300           W57075                                                         
506400           W5706N                                                         
506500           W51310                                                         
506600                                                                          
506700     MOVE 'S' TO POSTSUM-OPKOD                                            
506800     CALL POSTSUM USING POSTSUM-PARM                                      
506900     .                                                                    
507000     EJECT                                                                
507100                                                                          
507200 S01-READ-W57066  SECTION.                                                
507300     READ W57066 INTO IN-AREA                                             
507400     AT END                                                               
507500        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
507600        SET END-OF-W57066 TO TRUE                                         
507700                                                                          
507800     NOT AT END                                                           
507900        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
508000        MOVE 'W57066'     TO POSTSUM-FDNAMN                               
508100        MOVE 'W57068D1'   TO POSTSUM-DDNAMN2                              
508200        CALL POSTSUM USING POSTSUM-PARM                                   
508300     END-READ                                                             
508400     .                                                                    
508500                                                                          
508600 S02-WRITE-W57071A SECTION.                                               
508700     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
508800     MOVE SPACE                 TO 71LINE-POST                            
508900     IF WS-LINE-SW = JA                                                   
509000       IF IN-EKH-KDSORT = 'SW'                                            
509100         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
509200         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
509300         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
509400       ELSE                                                               
509500         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
509600         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
509700         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
509800       END-IF                                                             
509900       WRITE 71LINE-POST        FROM R3-LINE-R3                           
510000       PERFORM S20-CREATE-WRITE-LOG                                       
510100     ELSE                                                                 
510200       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
510300     END-IF                                                               
510400                                                                          
510500     IF WS-LINE-SW = JA                                                   
510600       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
510700     ELSE                                                                 
510800       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
510900     END-IF                                                               
511000     MOVE 'W57071A'             TO POSTSUM-FDNAMN                         
511100     MOVE 'W57068D2'            TO POSTSUM-DDNAMN2                        
511200     CALL POSTSUM USING POSTSUM-PARM                                      
511300     .                                                                    
511400                                                                          
511500 S002-WRITE-W57071A-HEAD SECTION.                                         
511600     MOVE SPACE                 TO 71LINE-POST                            
511700     IF IN-EKH-KDSORT = 'SW'                                              
511800       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
511900       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
512000       MOVE WS-TEXT           TO R3-LINE-TEXT                             
512100     ELSE                                                                 
512200       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
512300       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
512400       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
512500     END-IF                                                               
512600     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
512700                                                                          
512800     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
512900     MOVE 'W57071A'             TO POSTSUM-FDNAMN                         
513000     MOVE 'W57068D2'            TO POSTSUM-DDNAMN2                        
513100     CALL POSTSUM USING POSTSUM-PARM                                      
513200     .                                                                    
513300                                                                          
513400 S03-WRITE-W57072 SECTION.                                                
513500     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
513600     IF IN-EKH-KDSORT = 'SW'                                              
513700       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
513800       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
513900       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
514000     ELSE                                                                 
514100       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
514200       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
514300       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
514400     END-IF                                                               
514500     WRITE 72LINE-POST        FROM R3-LINE-R3                             
514600                                                                          
514700     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
514800     MOVE 'W57072A'           TO POSTSUM-FDNAMN                           
514900     MOVE 'W57068D3'          TO POSTSUM-DDNAMN2                          
515000     CALL POSTSUM USING POSTSUM-PARM                                      
515100                                                                          
515200     PERFORM S20-CREATE-WRITE-LOG                                         
515300     .                                                                    
515400                                                                          
515500 S04-WRITE-W57073A SECTION.                                               
515600     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
515700     MOVE SPACE                 TO 73LINE-POST                            
515800     IF WS-LINE-SW = JA                                                   
515900       IF IN-EKH-KDSORT = 'SW'                                            
516000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
516100         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
516200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
516300       ELSE                                                               
516400         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
516500         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
516600         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
516700       END-IF                                                             
516800       WRITE 73LINE-POST        FROM R3-LINE-R3                           
516900       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
517000     ELSE                                                                 
517100       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
517200       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
517300     END-IF                                                               
517400                                                                          
517500     MOVE 'W57073A'             TO POSTSUM-FDNAMN                         
517600     MOVE 'W57068D4'            TO POSTSUM-DDNAMN2                        
517700     CALL POSTSUM USING POSTSUM-PARM                                      
517800                                                                          
517900     IF WS-LINE-SW = JA                                                   
518000       PERFORM S20-CREATE-WRITE-LOG                                       
518100     END-IF                                                               
518200     .                                                                    
518300                                                                          
518400 S004-WRITE-W57073A-HEAD SECTION.                                         
518500     MOVE SPACE                 TO 73LINE-POST                            
518600     IF IN-EKH-KDSORT = 'SW'                                              
518700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
518800       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
518900       MOVE WS-TEXT           TO R3-LINE-TEXT                             
519000     ELSE                                                                 
519100       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
519200       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
519300       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
519400     END-IF                                                               
519500     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
519600                                                                          
519700     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
519800     MOVE 'W57073A'             TO POSTSUM-FDNAMN                         
519900     MOVE 'W57068D4'            TO POSTSUM-DDNAMN2                        
520000     CALL POSTSUM USING POSTSUM-PARM                                      
520100     .                                                                    
520200                                                                          
520300 S10-VATCODE SECTION.                                                     
520400     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
520500     PERFORM IMS-GU-WDB601                                                
520600     IF DCS-KDDC = SPACE                                                  
520700       MOVE NEJ              TO WDB6-A-SW                                 
520800     ELSE                                                                 
520900       MOVE JA               TO WDB6-A-SW                                 
521000     END-IF                                                               
521100                                                                          
521200     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
521300     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
521400     IF IN-EKH-SUVAT = ZERO                                               
521500       MOVE 'V0'     TO R3-LINE-TAX-CODE                                  
521600     ELSE                                                                 
521700       IF  WS-DATE-YYMM(1:2) = 18                                         
521800       AND IN-EKH-IDVERGL < 60000080                                      
521900         MOVE 'A1'     TO R3-LINE-TAX-CODE                                
522000       ELSE                                                               
522100         MOVE 'A5'     TO R3-LINE-TAX-CODE                                
522200       END-IF                                                             
522300     END-IF                                                               
522400     IF IN-EKH-BEVAT = 'XX'                                               
522500       MOVE 'V0'     TO R3-LINE-TAX-CODE                                  
522600     END-IF                                                               
522700     .                                                                    
522800     EJECT                                                                
522900                                                                          
523000 S11-ANALYSIS SECTION.                                                    
523100     MOVE WS-R3-ACCOUNT-10        TO W-IDKONTO-5122                       
523200     MOVE R3-LINE-PROFIT-CENTER   TO W-IDPRCTR-5122                       
523300     PERFORM IMS-GU-5122                                                  
523400     IF SEGMENT-SAKNAS                                                    
523500       MOVE '0449????????'        TO R3-LINE-ORDER                        
523600     ELSE                                                                 
523700       MOVE 5122-IDANALYS         TO R3-LINE-ORDER                        
523800     END-IF                                                               
523900     .                                                                    
524000     EJECT                                                                
524100                                                                          
524200 S12-PROFITCENTER SECTION.                                                
524300     MOVE IN-EKH-IDDISTR          TO WS-IDDISTR                           
524400     MOVE IN-EKH-IDKUNDNR         TO WS-IDKUNDNR                          
524500                                                                          
524600     PERFORM IMS-GU-5121                                                  
524700     MOVE +999999                 TO W-IDKONTO-5122-MIN                   
524800                                     W-IDKONTO-5122-MAX                   
524900     PERFORM IMS-GNP-5122                                                 
525000     PERFORM UNTIL SEGMENT-SAKNAS                                         
525100     OR 5122-IDANALYS = WS-IDDISTR-IDKUNDNR                               
525200       PERFORM IMS-GNP-5122                                               
525300     END-PERFORM                                                          
525400                                                                          
525500     IF SEGMENT-SAKNAS                                                    
525600       MOVE '??????????'          TO R3-LINE-PROFIT-CENTER                
525700     ELSE                                                                 
525800       MOVE 5122-IDPRCTR          TO R3-LINE-PROFIT-CENTER                
525900     END-IF                                                               
526000     .                                                                    
526100     EJECT                                                                
526200                                                                          
526300 S20-CREATE-WRITE-LOG SECTION.                                            
526400     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
526500     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
526600     IF SYST-IDPTYP = '610'                                               
526700       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
526800     ELSE                                                                 
526900       MOVE ZERO                      TO WS-IDLEVNR                       
527000       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
527100                          FOR CHARACTERS BEFORE INITIAL SPACE             
527200       IF WS-IDLEVNR   > ZERO                                             
527300          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
527400                                      TO LOGG-IDKONTO                     
527500       END-IF                                                             
527600     END-IF                                                               
527700     IF R3-LINE-COST-CENTER NOT = SPACE                                   
527800       MOVE R3-LINE-COST-CENTER(3:5)  TO LOGG-IDKST                       
527900     END-IF                                                               
528000     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
528100     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
528200     IF R3-LINE-AMOUNT-LC             =  0                                
528300       MOVE R3-LINE-AMOUNT            TO LOGG-SUBEL                       
528400     ELSE                                                                 
528500       MOVE R3-LINE-AMOUNT-LC         TO LOGG-SUBEL                       
528600     END-IF                                                               
528700     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
528800     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
528900                                                                          
529000     PERFORM S21-WRITE-W57075                                             
529100     PERFORM S22-WRITE-W57070                                             
529200                                                                          
529300     IF R3-LINE-TAX-AMOUNT    NOT = ZERO                                  
529400       MOVE R3-LINE-TAX-AMOUNT        TO LOGG-SUBEL                       
529500       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
529600       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
529700                                                                          
529800       PERFORM S21-WRITE-W57075                                           
529900     END-IF                                                               
530000     .                                                                    
530100     EJECT                                                                
530200                                                                          
530300 S21-WRITE-W57075 SECTION.                                                
530400     IF DCS-IDDC NOT = LOGG-IDDC                                          
530500        MOVE LOGG-IDDC TO W-IDDC-B6                                       
530600        PERFORM IMS-GU-WDB601                                             
530700     END-IF                                                               
530800     IF DCS-KDDC = SPACE                                                  
530900       MOVE NEJ              TO WDB6-A-SW                                 
531000     ELSE                                                                 
531100       MOVE JA               TO WDB6-A-SW                                 
531200     END-IF                                                               
531300                                                                          
531400     IF LOGG-KDEKHHT = '501' AND LOGG-KDEKSHT = '501'                     
531500       IF  WDB6-A-FINNS                                                   
531600       AND (DCS-NDC-PF                                                    
531700       OR   DCS-SDC)                                                      
531800         MOVE 'Y'     TO LOGG-FLLSBOK                                     
531900       END-IF                                                             
532000     END-IF                                                               
532100     IF  WDB6-A-FINNS                                                     
532200     AND DCS-DDC                                                          
532300       MOVE 'N'       TO LOGG-FLLSBOK                                     
532400     END-IF                                                               
532500     WRITE LOGG-POST FROM LOGG-W57073                                     
532600                                                                          
532700     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
532800     MOVE 'W57075'    TO POSTSUM-FDNAMN                                   
532900     MOVE 'W57068D5'  TO POSTSUM-DDNAMN2                                  
533000     CALL POSTSUM USING POSTSUM-PARM                                      
533100     .                                                                    
533200                                                                          
533300 S22-WRITE-W57070 SECTION.                                                
533400     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
533500     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
533600     MOVE R3-LINE-AMOUNT-LC     TO AVST-SUBEL                             
533700                                                                          
533800     IF R3-LINE-AMOUNT-SIGN = '+'                                         
533900       IF AVST-SUBEL < +0                                                 
534000         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
534100       END-IF                                                             
534200       IF AVST-KVANTAL < +0                                               
534300         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
534400       END-IF                                                             
534500     ELSE                                                                 
534600       IF AVST-SUBEL > +0                                                 
534700         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
534800       END-IF                                                             
534900       IF AVST-KVANTAL > +0                                               
535000         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
535100       END-IF                                                             
535200     END-IF                                                               
535300                                                                          
535400     IF DCS-IDDC NOT = AVST-IDDC                                          
535500        MOVE AVST-IDDC  TO W-IDDC-B6                                      
535600        PERFORM IMS-GU-WDB601                                             
535700     END-IF                                                               
535800     IF DCS-KDDC = SPACE                                                  
535900       MOVE NEJ              TO WDB6-A-SW                                 
536000     ELSE                                                                 
536100       MOVE JA               TO WDB6-A-SW                                 
536200     END-IF                                                               
536300                                                                          
536400     IF AVST-KDEKHHT = '501' AND AVST-KDEKSHT = '501'                     
536500       IF  WDB6-A-FINNS                                                   
536600       AND (DCS-NDC-PF OR DCS-SDC)                                        
536700         MOVE 'Y'               TO AVST-FLLSBOK                           
536800       END-IF                                                             
536900     END-IF                                                               
537000     IF  WDB6-A-FINNS                                                     
537100     AND DCS-DDC                                                          
537200       MOVE 'N'                 TO AVST-FLLSBOK                           
537300     END-IF                                                               
537400                                                                          
537500     IF AVST-IDKONTO(1:4) = '1454'                                        
537600       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
537700       WRITE AVST-POST FROM AVST-W57070                                   
537800                                                                          
537900       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
538000       MOVE 'W57070'            TO POSTSUM-FDNAMN                         
538100       MOVE 'W57068D6'          TO POSTSUM-DDNAMN2                        
538200       CALL POSTSUM USING POSTSUM-PARM                                    
538300     END-IF                                                               
538400     .                                                                    
538500     EJECT                                                                
538600                                                                          
538700 S30-READ-DATABASE-B2-B1 SECTION.                                         
538800     IF IN-EKH-KDEKHHT = '205'                                            
538900     OR                  '304'                                            
539000     OR (IN-EKH-KDEKHHT = '204'                                           
539100     AND IN-EKH-KDEKSHT = '253')                                          
539200       MOVE 'VO'              TO CIA-IDARTPRE-IN                          
539300       MOVE IN-EKH-IDKUNDNR   TO CIA-IDARTBET-IN                          
539400       CALL W009CIA USING        CIA-W009CIA                              
539500       MOVE CIA-IDARTBET-UT   TO W-WDB1-IDPARTNR                          
539600       MOVE WC-IDFTG-CN       TO W-WDB1-IDFTG                             
539700       PERFORM IMS-GU-WDB101                                              
539800       IF SEGMENT-SAKNAS                                                  
539900                                                                          
540000         MOVE SPACE         TO BET-KDTRADP                                
540100         MOVE ZERO          TO BET-IDPARTNR                               
540200         MOVE '????'        TO WS-KDBETVIL                                
540300         MOVE '???'         TO WS-KDVALISO-WDB1                           
540400       ELSE                                                               
540500         MOVE BET-KDBETVIL  TO WS-KDBETVIL                                
540600         MOVE BET-KDVALISO  TO WS-KDVALISO-WDB1                           
540700       END-IF                                                             
540800     ELSE                                                                 
540900       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
541000       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
541100       PERFORM IMS-GU-WDB201                                              
541200       IF SEGMENT-SAKNAS                                                  
541300**** OM MAN SKICKAR PÅ EXPORT SÅ KAN TILLÄGGSKOSTNADERNA HAMNA PÅ         
541400**** KUND 0 OCH OM DEN SAKNAS SÅ SÄTTER VI EN ANNAN DEFAULT               
541500*        IF IN-EKH-IDDISTR = 9111                                         
541600*        OR IN-EKH-IDDISTR = 9141                                         
541700*        OR IN-EKH-IDDISTR = 9143                                         
541800*        OR IN-EKH-IDDISTR = 9144                                         
541900*        OR IN-EKH-IDDISTR = 9145                                         
542000*        OR IN-EKH-IDDISTR = 9146                                         
542100         IF IN-EKH-IDLEVNR = '1441'                                       
542200           MOVE IN-EKH-IDLEVNR  TO W-WDB1-IDPARTNR                        
542300         ELSE                                                             
542400           MOVE 'CN100'         TO W-WDB1-IDPARTNR                        
542500         END-IF                                                           
542600**** 401-420 WILL NOT HAVE DISTRICT AND CUSTOMER                          
542700*        IF (IN-EKH-KDEKHHT = '401'                                       
542800*        AND IN-EKH-KDEKSHT = '420')                                      
542900*          MOVE 'CN'            TO WS-KDTRADP                             
543000*        END-IF                                                           
543100       ELSE                                                               
543200         MOVE GMT-IDPARTNR      TO W-WDB1-IDPARTNR                        
543300**** IF BOUNCE DISTRICT REPLACE PARMA WITH 1441                           
543400*        IF IN-EKH-IDDISTR = 9141                                         
543500*        OR IN-EKH-IDDISTR = 9143                                         
543600*        OR IN-EKH-IDDISTR = 9144                                         
543700*        OR IN-EKH-IDDISTR = 9145                                         
543800*        OR IN-EKH-IDDISTR = 9146                                         
543900*        OR IN-EKH-IDDISTR = 9271                                         
544000*        OR IN-EKH-IDDISTR = 9272                                         
544100*        OR IN-EKH-IDDISTR = 9273                                         
544200*        OR IN-EKH-IDDISTR = 9274                                         
544300*          IF W-WDB1-IDPARTNR = '352343'                                  
544400*            MOVE '1441'            TO W-WDB1-IDPARTNR                    
544500*          IF W-WDB1-IDPARTNR = '4113'                                    
544600*            MOVE '1441'            TO W-WDB1-IDPARTNR                    
544700*          END-IF                                                         
544800*        END-IF                                                           
544900         IF IN-EKH-IDLEVNR = '1441'                                       
545000           MOVE IN-EKH-IDLEVNR    TO W-WDB1-IDPARTNR                      
545100         END-IF                                                           
545200*        MOVE GMT-IDDISTR           TO W-IDDISTR-5124                     
545300*        MOVE GMT-IDKUNDNR          TO W-IDKUNDNR-5124                    
545400*        PERFORM IMS-GU-WDGX5124                                          
545500*        IF SEGMENT-FINNS                                                 
545600*          MOVE 5124-KDTRADP        TO WS-KDTRADP                         
545700*        ELSE                                                             
545800*          MOVE 'CN'                TO WS-KDTRADP                         
545900*        END-IF                                                           
546000       END-IF                                                             
546100       MOVE WC-IDFTG-CN       TO W-WDB1-IDFTG                             
546200       PERFORM IMS-GU-WDB101                                              
546300       IF SEGMENT-SAKNAS                                                  
546400         DISPLAY 'BETALARUPPG. SAKNAS '                                   
546500         DISPLAY IN-EKH-IDVERGL                                           
546600         DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                       
546700         DISPLAY W-WDB1-IDPARTNR                                          
546800         DISPLAY W-WDB1-IDFTG                                             
546900                                                                          
547000         MOVE SPACE         TO BET-KDTRADP                                
547100         MOVE ZERO          TO BET-IDPARTNR                               
547200         MOVE '????'        TO WS-KDBETVIL                                
547300         MOVE '???'         TO WS-KDVALISO-WDB1                           
547400       ELSE                                                               
547500         MOVE BET-KDBETVIL  TO WS-KDBETVIL                                
547600       END-IF                                                             
547700       MOVE 'CNY'           TO WS-KDVALISO-WDB1                           
547800     END-IF                                                               
547900                                                                          
548000     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
548100     MOVE ZERO TO TALLY                                                   
548200     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
548300                 FOR CHARACTERS BEFORE INITIAL SPACE                      
548400     IF TALLY = ZERO                                                      
548500       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
548600     ELSE                                                                 
548700       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
548800                                TO W-BET-IDPARTNR-NUM                     
548900     END-IF                                                               
549000     .                                                                    
549100     EJECT                                                                
549200                                                                          
549300 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
549400     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
549500     PERFORM IMS-GU-WDB601                                                
549600     IF DCS-KDDC = SPACE                                                  
549700       MOVE NEJ              TO WDB6-A-SW                                 
549800     ELSE                                                                 
549900       MOVE JA               TO WDB6-A-SW                                 
550000     END-IF                                                               
550100                                                                          
550200     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
550300       IF IN-EKH-KDEKSHT NOT = '406'                                      
550400         IF IN-EKH-FLDCET = NEJ                                           
550500           PERFORM S42-SKAPA-RW2-INV-POSTER                               
550600         END-IF                                                           
550700       END-IF                                                             
550800     END-IF                                                               
550900                                                                          
551000     IF IN-EKH-KDEKNIVA = 'DET'                                           
551100       IF  IN-EKH-KDEKHHT = '204'                                         
551200       AND (IN-EKH-KDEKSHT = '201' OR '202' OR '251')                     
551300         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
551400       END-IF                                                             
551500                                                                          
551600       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
551700       AND (WDB6-A-FINNS                                                  
551800       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
551900       OR   DCS-DDC OR DCS-NDC-PF))                                       
552000         IF IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '304'             
552100           CONTINUE                                                       
552200         ELSE                                                             
552300           PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                            
552400         END-IF                                                           
552500       END-IF                                                             
552600                                                                          
552700       IF IN-FIL-IDPGM = 'W4183000'                                       
552800       AND (WDB6-A-FINNS                                                  
552900       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
553000       OR   DCS-DDC OR DCS-NDC-PF))                                       
553100         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
553200       END-IF                                                             
553300     END-IF                                                               
553400     .                                                                    
553500     EJECT                                                                
553600                                                                          
553700 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
553800     MOVE 'RW2'              TO RW2-IDPTYP                                
553900     MOVE 'RW2'              TO WS-IDPTYP                                 
554000     MOVE ZERO               TO RW2-IDDISTR                               
554100     IF DCS-KDDC = SPACE OR DCS-DDC                                       
554200       MOVE WC-CDC-SE        TO RW2-IDDC                                  
554300     ELSE                                                                 
554400       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
554500     END-IF                                                               
554600     IF IN-EKH-KVANTAL < +0                                               
554700       MOVE '0422'           TO RW2-KDWRTYP                               
554800     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
554900     ELSE                                                                 
555000       MOVE '0421'           TO RW2-KDWRTYP                               
555100       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
555200     END-IF                                                               
555300                                                                          
555400     IF RW2-SUARTSTD NOT = +0                                             
555500       PERFORM S70-WRITE-W51310                                           
555600     END-IF                                                               
555700     .                                                                    
555800     EJECT                                                                
555900                                                                          
556000 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
556100     MOVE '0110'             TO RW1-KDWRTYP                               
556200     IF DCS-KDDC = SPACE OR DCS-DDC                                       
556300       MOVE WC-CDC-SE        TO RW1-IDDC                                  
556400     ELSE                                                                 
556500       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
556600     END-IF                                                               
556700     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
556800     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
556900     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
557000                                                                          
557100     IF RW1-SUARTSTD NOT = +0                                             
557200       MOVE 'RW1' TO WS-IDPTYP                                            
557300       PERFORM S70-WRITE-W51310                                           
557400     END-IF                                                               
557500     .                                                                    
557600     EJECT                                                                
557700                                                                          
557800 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
557900     MOVE '0110'             TO RW1-KDWRTYP                               
558000     IF DCS-KDDC = SPACE OR DCS-DDC                                       
558100       MOVE WC-CDC-SE        TO RW1-IDDC                                  
558200     ELSE                                                                 
558300       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
558400     END-IF                                                               
558500     IF IN-EKH-KDANMORS = '30'                                            
558600       MOVE ZERO             TO RW1-SUARTSTD                              
558700     ELSE                                                                 
558800      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
558900     END-IF                                                               
559000     IF IN-EKH-KDANMORS = '30' OR '80'                                    
559100       MOVE ZERO             TO RW1-SUARTSJK                              
559200     ELSE                                                                 
559300      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
559400     END-IF                                                               
559500     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
559600                                                                          
559700     IF RW1-SUARTSTD NOT = +0                                             
559800       MOVE 'RW1' TO WS-IDPTYP                                            
559900       PERFORM S70-WRITE-W51310                                           
560000     END-IF                                                               
560100     .                                                                    
560200     EJECT                                                                
560300                                                                          
560400 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
560500     MOVE '0110'             TO RW1-KDWRTYP                               
560600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
560700       MOVE WC-CDC-SE        TO RW1-IDDC                                  
560800     ELSE                                                                 
560900       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
561000     END-IF                                                               
561100     IF IN-EKH-KDEKSHT = '310'                                            
561200*** SKROTNING KDANMORS  13 O 23                                           
561300       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
561400     ELSE                                                                 
561500      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
561600     END-IF                                                               
561700                                                                          
561800     MOVE ZERO               TO RW1-SUARTSJK                              
561900                                RW1-SUARTFSG                              
562000     IF RW1-SUARTSTD NOT = +0                                             
562100       MOVE 'RW1' TO WS-IDPTYP                                            
562200       PERFORM S70-WRITE-W51310                                           
562300     END-IF                                                               
562400     .                                                                    
562500     EJECT                                                                
562600                                                                          
562700 S60-WRITE-W5706N SECTION.                                                
562800     WRITE SAPUT-POST  FROM IN-AREA                                       
562900                                                                          
563000     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
563100     MOVE 'W5706N'            TO POSTSUM-FDNAMN                           
563200     MOVE 'W57068D7'          TO POSTSUM-DDNAMN2                          
563300     CALL POSTSUM USING POSTSUM-PARM                                      
563400     .                                                                    
563500     EJECT                                                                
563600                                                                          
563700 S70-WRITE-W51310 SECTION.                                                
563800     IF WS-IDPTYP  = 'RW2'                                                
563900       IF DCS-KDDC = SPACE OR DCS-DDC                                     
564000         MOVE WC-CDC-SE        TO INV-IDDC                                
564100       ELSE                                                               
564200         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
564300       END-IF                                                             
564400       IF IN-EKH-KVANTAL < +0                                             
564500         MOVE '003'            TO INV-IDPTYP                              
564600       COMPUTE INV-SUARTSTD =                                             
564700                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
564800       ELSE                                                               
564900         MOVE '002'            TO INV-IDPTYP                              
565000         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
565100       END-IF                                                             
565200       MOVE SPACE TO WS-IDPTYP                                            
565300       MOVE 0                  TO INV-ADLAGOMR                            
565400       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
565500       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
565600     END-IF                                                               
565700     IF WS-IDPTYP  = 'RW1'                                                
565800       IF DCS-KDDC = SPACE OR DCS-DDC                                     
565900         MOVE WC-CDC-SE        TO INV-IDDC                                
566000       ELSE                                                               
566100         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
566200       END-IF                                                             
566300       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
566400       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
566500       MOVE 0                  TO INV-ADLAGOMR                            
566600       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
566700       MOVE '001'              TO INV-IDPTYP                              
566800       MOVE SPACE              TO WS-IDPTYP                               
566900     END-IF                                                               
567000     WRITE INV-POST  FROM INV-W51310                                      
567100                                                                          
567200     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
567300     MOVE 'W51310'            TO POSTSUM-FDNAMN                           
567400     MOVE 'W57068D8'          TO POSTSUM-DDNAMN2                          
567500     CALL POSTSUM USING POSTSUM-PARM                                      
567600     .                                                                    
567700     EJECT                                                                
567800                                                                          
567900 S13-GET-LANDING-COST SECTION.                                            
568000     MOVE '71'                   TO W-IDDC-B6                             
568100     PERFORM IMS-GU-WDB601                                                
568200     IF SEGMENT-FINNS                                                     
568300       PERFORM IMS-GNP-WDB617                                             
568400       IF SEGMENT-FINNS                                                   
568500         IF PROC-TILANDCO >  IN-EKH-DAVERDAT                              
568600           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
568700         ELSE                                                             
568800           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
568900         END-IF                                                           
569000       END-IF                                                             
569100     END-IF                                                               
569200     .                                                                    
569300     EJECT                                                                
569400 S80-GET-CURRENCY-RATE SECTION.                                           
569500     MOVE +0                  TO W-ANT                                    
569600     IF (IN-EKH-KDEKHHT = '102'                                           
569700     AND IN-EKH-KDEKSHT = '124')                                          
569800     OR (IN-EKH-KDEKHHT = '102'                                           
569900     AND IN-EKH-KDEKSHT = '134')                                          
570000       INSPECT IN-EKH-IDFAKT-EXP TALLYING W-ANT FOR CHARACTERS            
570100             BEFORE INITIAL ' '                                           
570200       MOVE IN-EKH-IDFAKT-EXP(1:W-ANT) TO W-IDFAKT                        
570300     ELSE                                                                 
570400       INSPECT IN-EKH-IDVERGL TALLYING W-ANT FOR CHARACTERS               
570500             BEFORE INITIAL ' '                                           
570600       MOVE IN-EKH-IDVERGL(1:W-ANT) TO W-IDFAKT                           
570700     END-IF                                                               
570800     MOVE IN-EKH-IDARTNR TO W-IDARTNR                                     
570900     PERFORM IMS-GU-WDL601                                                
571000     IF SEGMENT-SAKNAS                                                    
571100       CONTINUE                                                           
571200     ELSE                                                                 
571300       PERFORM IMS-GNP-WDL611                                             
571400       IF SEGMENT-SAKNAS                                                  
571500         CONTINUE                                                         
571600       ELSE                                                               
571700         COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                     
571800                                   - INL-DAINLEV                          
571900         MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                   
572000         MOVE WS-FAKTURA-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
572100         MOVE WS-FAKTURA-DATUM(5:2) TO W-DATE-AAMM(3:2)                   
572200         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
572300         MOVE WS-KDVALISO-CN        TO CURR-KDVALISO-ROW                  
572400         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
572500         IF CURR-KDSVAR = ' '                                             
572600           MOVE CURR-PRKURS-NEW     TO WS-PRKURS-CN3                      
572700         ELSE                                                             
572800           MOVE +1                  TO WS-PRKURS-CN3                      
572900         END-IF                                                           
573000       END-IF                                                             
573100     END-IF                                                               
573200     .                                                                    
573300     EJECT                                                                
573400 S81-GET-CURRENCY-RATE SECTION.                                           
573500     MOVE 'SEK'               TO R3-HEAD-CURRENCY                         
573600     MOVE 'CNY'               TO CURR-KDVALISO-ROW                        
573700     IF IN-FIL-IDPGM = 'W4183300'                                         
573800       IF IN-EKH-DAAVIDAT > ZERO                                          
573900         MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                          
574000         MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                          
574100       ELSE                                                               
574200         MOVE WS-TIAA            TO WS-TIAA-CR                            
574300         MOVE WS-TIMM            TO WS-TIMM-CR                            
574400       END-IF                                                             
574500     ELSE                                                                 
574600       MOVE WS-TIAA              TO WS-TIAA-CR                            
574700       MOVE WS-TIMM              TO WS-TIMM-CR                            
574800     END-IF                                                               
574900     MOVE WS-TIAA-CR        TO W-DATE-AAMM(1:2)                           
575000     MOVE WS-TIMM-CR        TO W-DATE-AAMM(3:2)                           
575100     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
575200     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
575300     IF CURR-KDSVAR = ' '                                                 
575400       IF IN-EKH-IDDISTR > ZERO                                           
575500         MOVE CURR-PRKURS-NEW TO WS-PRKURS-CN3                            
575600       ELSE                                                               
575700         IF WS-PRKURS = ZERO                                              
575800           MOVE 1           TO WS-PRKURS-CN3                              
575900         END-IF                                                           
576000       END-IF                                                             
576100     ELSE                                                                 
576200       MOVE 1               TO WS-PRKURS-CN3                              
576300     END-IF                                                               
576400     .                                                                    
576500     EJECT                                                                
576600* --- IMS SECTIONS ---                                                    
576700                                                                          
576800 IMS-GU-WDH521 SECTION.                                                   
576900     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
577000          DELIMITED BY SIZE INTO SSA1                                     
577100     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
577200          DELIMITED BY SIZE INTO SSA2                                     
577300     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
577400          DELIMITED BY SIZE INTO SSA3                                     
577500     MOVE '  '              TO GODK-STATUSKODER                           
577600     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
577700                                                   SSA2                   
577800                                                   SSA3                   
577900     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
578000                                                                          
578100     PERFORM IMS-STATUS-CONTROL                                           
578200     .                                                                    
578300                                                                          
578400 IMS-GNP-WDH531 SECTION.                                                  
578500     MOVE 'WDH531  '        TO SSA1                                       
578600     MOVE '  GE'            TO GODK-STATUSKODER                           
578700     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
578800     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
578900                               WS-STATUS                                  
579000     PERFORM IMS-STATUS-CONTROL                                           
579100     .                                                                    
579200     EJECT                                                                
579300                                                                          
579400 IMS-GU-WDB201 SECTION.                                                   
579500     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-KEY ')'                         
579600          DELIMITED BY SIZE INTO SSA1                                     
579700     MOVE '  GE'                 TO GODK-STATUSKODER                      
579800     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
579900      MOVE GMTA-STATUS-CODE      TO STATUS-WS                             
580000     PERFORM IMS-STATUS-CONTROL                                           
580100     .                                                                    
580200     EJECT                                                                
580300                                                                          
580400 IMS-GU-WDB101 SECTION.                                                   
580500     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
580600          DELIMITED BY SIZE INTO SSA1                                     
580700     MOVE '  GE'               TO GODK-STATUSKODER                        
580800     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
580900     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
581000     PERFORM IMS-STATUS-CONTROL                                           
581100     .                                                                    
581200     EJECT                                                                
581300                                                                          
581400 IMS-GU-5122 SECTION.                                                     
581500     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
581600            DELIMITED BY SIZE INTO SSA1                                   
581700     STRING 'WDGX5122(KEY5122  =' W-WDGXKEY-5122-X ')'                    
581800            DELIMITED BY SIZE INTO SSA2                                   
581900     MOVE '  GE'           TO GODK-STATUSKODER                            
582000     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5122 SSA1 SSA2            
582100     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
582200     PERFORM IMS-STATUS-CONTROL                                           
582300     .                                                                    
582400                                                                          
582500 IMS-GU-5121 SECTION.                                                     
582600     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
582700            DELIMITED BY SIZE INTO SSA1                                   
582800     MOVE '    '           TO GODK-STATUSKODER                            
582900     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5121 SSA1                 
583000     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
583100     PERFORM IMS-STATUS-CONTROL                                           
583200     .                                                                    
583300                                                                          
583400 IMS-GNP-5122 SECTION.                                                    
583500     STRING 'WDGX5122(KEY5122 >=' W-WDGXKEY-5122-MIN-X                    
583600                    '&KEY5122 <=' W-WDGXKEY-5122-MAX-X ')'                
583700            DELIMITED BY SIZE INTO SSA1                                   
583800     MOVE '  GE'           TO GODK-STATUSKODER                            
583900     CALL CBLTDLI USING GNP 5121-PCB DLI-IO-WDGX5122 SSA1                 
584000     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
584100     PERFORM IMS-STATUS-CONTROL                                           
584200     .                                                                    
584300     EJECT                                                                
584400                                                                          
584500 IMS-GU-WDB601    SECTION.                                                
584600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
584700          DELIMITED BY SIZE INTO SSA1                                     
584800     MOVE '  GE' TO GODK-STATUSKODER                                      
584900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
585000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
585100     PERFORM IMS-STATUS-CONTROL                                           
585200     IF SEGMENT-SAKNAS                                                    
585300        MOVE SPACE TO DCS-KDDC                                            
585400     END-IF                                                               
585500     .                                                                    
585600     EJECT                                                                
585700                                                                          
585800 IMS-GNP-WDB617 SECTION.                                                  
585900     MOVE 'WDB617   ' TO SSA1                                             
586000     MOVE '  GE'        TO GODK-STATUSKODER                               
586100     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
586200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
586300     PERFORM IMS-STATUS-CONTROL                                           
586400     .                                                                    
586500     SKIP3                                                                
586600                                                                          
586700 IMS-GU-WDL601   SECTION.                                                 
586800     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
586900          DELIMITED BY SIZE INTO SSA1                                     
587000     MOVE '  GE' TO GODK-STATUSKODER                                      
587100     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L601 SSA1                 
587200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
587300     PERFORM IMS-STATUS-CONTROL                                           
587400     .                                                                    
587500     SKIP3                                                                
587600                                                                          
587700 IMS-GNP-WDL611   SECTION.                                                
587800     STRING 'WDL611  (IDFAKT   =' W-IDFAKT-X ')'                          
587900          DELIMITED BY SIZE INTO SSA1                                     
588000     MOVE '  GE' TO GODK-STATUSKODER                                      
588100     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-L611 SSA1                 
588200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
588300     PERFORM IMS-STATUS-CONTROL                                           
588400     .                                                                    
588500     SKIP3                                                                
588600                                                                          
588700 IMS-GU-WDGX9306 SECTION.                                                 
588800     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
588900             DELIMITED BY SIZE INTO SSA1                                  
589000     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
589100             DELIMITED BY SIZE INTO SSA2                                  
589200     MOVE '  GE'   TO GODK-STATUSKODER                                    
589300     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
589400     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
589500     PERFORM IMS-STATUS-CONTROL                                           
589600     .                                                                    
589700     SKIP3                                                                
589800                                                                          
589900 IMS-GNP-WDGX9308 SECTION.                                                
590000     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
590100             DELIMITED BY SIZE INTO SSA1                                  
590200     MOVE '  GE'   TO GODK-STATUSKODER                                    
590300     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
590400     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
590500     PERFORM IMS-STATUS-CONTROL                                           
590600     .                                                                    
590700     SKIP3                                                                
590800                                                                          
590900 IMS-GNP-WDGX9308-FIRST SECTION.                                          
591000     MOVE 'WDGX9308*F' TO SSA1                                            
591100     MOVE '  GE'   TO GODK-STATUSKODER                                    
591200     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
591300     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
591400     PERFORM IMS-STATUS-CONTROL                                           
591500     .                                                                    
591600     SKIP3                                                                
591700 IMS-GU-WDGX5124 SECTION.                                                 
591800                                                                          
591900     STRING 'WDG201  (WDGXKEY  =' W-WDGX5123-X ')'                        
592000          DELIMITED BY SIZE INTO SSA1                                     
592100     STRING 'WDGX5124(KY5124   =' W-KY5124-X ')'                          
592200          DELIMITED BY SIZE INTO SSA2                                     
592300     MOVE '  GE' TO GODK-STATUSKODER                                      
592400     CALL CBLTDLI USING GU 5124-PCB DLI-IO-WDGX5124 SSA1 SSA2             
592500     MOVE 5124-STATUS-CODE TO STATUS-WS                                   
592600     PERFORM IMS-STATUS-CONTROL                                           
592700     .                                                                    
592800     SKIP3                                                                
592900                                                                          
593000 IMS-STATUS-CONTROL SECTION.                                              
593100     SET STATUS-IX TO 1                                                   
593200     SEARCH GODK-STATUS                                                   
593300       AT END                                                             
593400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
593500           DELIMITED BY SIZE INTO FELTEXT                                 
593600         DISPLAY FELTEXT                                                  
593700         CALL FELLOG                                                      
593800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
593900         CONTINUE                                                         
594000     END-SEARCH                                                           
594100     .                                                                    
