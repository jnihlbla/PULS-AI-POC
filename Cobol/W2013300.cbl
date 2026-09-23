000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2013300.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   MARS 1988.                                               
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700****************************************************************          
000800*                                                              *          
000900*  2133      N Y P O N  - S Y S T E M E T                      *          
001000*                                                              *          
001100*  BILD FÖR ANSKAFFNING OCH NYKÖP AV NYA ARTIKLAR.             *          
001200*  INFORMATIONEN SOM VISAS PÅ BILDEN ÄR REGISTRERAD AV BEREDARE*          
001300*  OCH ANSKAFFARE SAMT FÖR PV-ARTIKLAR ÄVEN INFORMATION SOM    *          
001400*  ERHÅLLITS FRÅN PV:S SYSTEM TIKO.                            *          
001500*                                                              *          
001600*  UPPDATERING AV ANSKAFFNINGS- OCH KÖPDATA KAN UTFÖRAS, SAMT  *          
001700*  SIMULERING AV PRELIMINÄRT SÄKERHETSLAGER OCH MAXPUNKT.      *          
001800*                                                              *          
001900*  I OCH MED ATT NYKÖP AUTOMATISKT KAN ÖVERFÖRAS TILL PV,      *          
002000*  BORTTAGES MÖJLIGHETEN ATT SKRIVA NYKÖPSRAPPORT FRÅN         *          
002100*  DETTA PGM (FLNYRAPP)                                        *          
002200*                                                              *          
002300*  SPLIT-ÄNDRING:                                              *          
002400*  FTG-UPPDATERING BORTTAGEN MEN                               *          
002500*  INMATNINGSFÄLTET KVAR I MID, MOD OCH FORMAT.                *          
002600*                                                              *          
002700*        ÄT NOV 93.                                            *          
002800*       +----------------------------------------------------+ *          
002900*       ! X-TRANSAR  KAN KOMMA TILL DETTA PGM OCH            ! *          
003000*       ! KOMMER FRÅN DISPATCHERN SOM FÅR INDATA FRÅN RUTIN  ! *          
003100*       ! W200B1. "MIDDAR" SKAPAS DÄR I PGM W2423300. (PROD) ! *          
003200*       ! NÄR KONTROLLREGLER MOT INDATA ÄNDRAS I 2133, V.G.  ! *          
003300*       ! UPPDATERA ÄVEN W2423300.                           ! *          
003400*       +----------------------------------------------------+ *          
003500****************************************************************          
003600     EJECT                                                                
003700*    INDATA.                                                              
003800*        TRANSAKTION: W2T133                                              
003900*                     W2T133U                                             
004000*                     W2T133X                                             
004100*        MID:         W2I13301                                            
004200*    UTDATA.                                                              
004300*        MOD:         W2O13301                                            
004400*    SUBPROGRAM.                                                          
004500*        FELLOG                                                           
004600*        CBLTDLI                                                          
004700*        W005INIT                                                         
004800*                                                                         
004900*  ÄNDRAD FÖR ETRACKER NO 1456859, INSTALLERAD 2004-11-07                 
005000*                                                                         
005100*  ÄNDRAD FÖR ETRACKER NO 10249768 BRANDON KÖPANMODAN TO SRM(NAP)         
005200*                                  NY PG=25 FÖR BRANDONARTIKLAR.          
005300*                                                                         
005400*  2015-05-06 ETRACKER NO 10211650 SW ART KÖPANMODAN TO SI+(EPIC)         
005500*                                                                         
005600                                                                          
005700     EJECT                                                                
005800 ENVIRONMENT DIVISION.                                                    
005900     SKIP3                                                                
006000 DATA DIVISION.                                                           
006100 WORKING-STORAGE SECTION.                                                 
006200*    -COPY WY2000W3                                                       
006300     SKIP3                                                                
006400*    -COPY WY2000W1                                                       
006500     SKIP3                                                                
006600*                                                                         
006700******************************************************************        
006800*          W O R K I N G  S T O R A G E  S E C T I O N           *        
006900******************************************************************        
007000*                                                                         
007100 77  PROGRAM-NAMN                PIC X(08)  VALUE 'W2013300'.             
007200 77  JA                          PIC X(01)  VALUE 'J'.                    
007210 77  YES                         PIC X(01)  VALUE 'Y'.                    
007300 77  NEJ                         PIC X(01)  VALUE 'N'.                    
007400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +590 COMP SYNC.         
007500 77  WS-IDARTNR                  PIC X(09).                               
007600 77  WS-IDLEVNR                  PIC X(05).                               
007700 77  WS-IDLEVNR-NUM              PIC X(05).                               
007800 77  WS-KDSVAR                   PIC X      VALUE SPACE.                  
007900 77  WS-IDPROJ                   PIC X(4)   VALUE SPACE.                  
008000 77  FL-SHIP-UPD                 PIC X      VALUE 'N'.                    
008010 77  WS-A                        PIC X(01)  VALUE 'A'.                    
008100                                                                          
008200 01  WS-IDTRANS                  PIC X(04).                               
008300     88  EGEN-BILD                          VALUE '2133'.                 
008400 01  WS-IDANSK                   PIC 9(3).                                
008500 01  FILLER REDEFINES WS-IDANSK.                                          
008600     03  WS-IDANSK-POS1-2        PIC 9(2).                                
008700     03  FILLER                  PIC 9.                                   
008800 01  WS-DAFINLEV.                                                         
008900     03  WS-DAFINLEV-SEKEL       PIC 9(2).                                
009000     03  WS-DAFINLEV-DATUM       PIC 9(6).                                
009100 01  WS-IDINK                    PIC X(04)  VALUE SPACE.                  
009200 01  TEST-IDINK                  PIC 9(03)  VALUE ZERO.                   
009300     EJECT                                                                
009400*01  -COPY W200PRNC                                                       
009500     EJECT                                                                
009600*01  -COPY WWKONLEV                                                       
009700     EJECT                                                                
009800 01  WS-IDPLANGR-LEV-AG-TEST     PIC 9(01).                               
009900     88 GODK-IDPLANGR-LEV-AG                VALUE 1                       
010000                                                  2                       
010100                                                  3                       
010200                                                  4                       
010300                                                  5                       
010400                                                  6                       
010500                                                  7                       
010600                                                  8                       
010700                                                  9.                      
010800                                                                          
010900 01  WS-KDPRODSL-TEST1           PIC 9(02).                               
011000     88 KDPRODSL-SKALL-KOPA                 VALUE 11 51                   
011100                                                  13 52                   
011200                                                  14 53                   
011300                                                  15 54                   
011400                                                  16 55                   
011500                                                  17 56                   
011600                                                  18 57                   
011700                                                  19 58                   
011800                                                  21 59                   
011900                                                  23 91                   
012000                                                  24 93                   
012100                                                  25 94                   
012200                                                  26 95                   
012300                                                  27 96                   
012400                                                  28 97                   
012500                                                  29 98                   
012600                                                  31                      
012610                                                  32                      
012620                                                  33                      
012630                                                  34                      
012640                                                  35                      
012650                                                  36                      
012660                                                  37                      
012670                                                  38                      
012700                                                  39.                     
012800                                                                          
012900                                                                          
013000 01  WS-KDPRODSL-TEST2           PIC 9(02).                               
013100     88 KDPRODSL-LYNK                       VALUE 31 THRU 39.             
013300                                                                          
013400 01  WS-KDPRODSL-TEST4           PIC 9(02).                               
013500     88 KDPRODSL-KOP-MASTE-UTFORAS          VALUE 11  91                  
013600                                                  13  93                  
013700                                                  14  94                  
013800                                                  15  95                  
013900                                                  16  96                  
014000                                                  17  97                  
014010                                                  32                      
014020                                                  33                      
014030                                                  34                      
014100                                                  42                      
014200                                                  43                      
014300                                                  44                      
014400                                                  45                      
014500                                                  46                      
014600                                                  48                      
014700                                                  52                      
014800                                                  53                      
014900                                                  54.                     
015000                                                                          
015100     EJECT                                                                
015200*                                                                         
015300******************************************************************        
015400*                     S W I T C H A R                            *        
015500******************************************************************        
015600*                                                                         
015700 01  SWITCHAR.                                                            
015800     05  SW-INPUT-RAETT          PIC X(01)  VALUE 'J'.                    
015900     05  SW-IDINK-RAETT          PIC X(01)  VALUE 'J'.                    
016000     05  SW-ARTIKEL-FINNS-PA-NYPON PIC X(01) VALUE 'N'.                   
016100     05  SW-TRAFF                PIC X(01)  VALUE 'N'.                    
016200     05  SW-IDLEVNR-BYTE         PIC X(01)  VALUE 'N'.                    
016300     05  SW-SLACKTA-FALT-FEL     PIC X(01)  VALUE 'N'.                    
016400     05  SW-ARTIKEL-TILL-BASL    PIC X(01)  VALUE 'N'.                    
016500     05  SW-BESTAVT              PIC X(01)  VALUE 'N'.                    
016600     05  SW-AVTAL-FINNS          PIC X(01)  VALUE 'N'.                    
016700     05  SW-REPKOP-OK            PIC X(01)  VALUE 'N'.                    
016800     05  SW-REPKOP-AVTAL         PIC X(01)  VALUE 'N'.                    
016900     05  SW-NYKOP-OK             PIC X(01)  VALUE 'J'.                    
017000     05  SW-IDFTG                PIC X(01)  VALUE 'J'.                    
017100         88 IDFTG-OK                        VALUE 'J'.                    
017200                                                                          
017300*                                                                         
017400******************************************************************        
017500*               D I V E R S E  S P A R F Ä L T                   *        
017600******************************************************************        
017700*                                                                         
017800 01  SPAR-DIVERSE.                                                        
017900                                                                          
018000     05  SPAR-IDANSK-FOM             PIC 9(03)  VALUE ZERO.               
018100     05  SPAR-IDANSK-TOM             PIC 9(03)  VALUE ZERO.               
018200     05  SPAR-IDPROJ-VALT            PIC X(04)  VALUE SPACE.              
018300     05  SPAR-IDANSK-MIN             PIC 9(03)  VALUE ZERO.               
018400     05  SPAR-IDANSK                 PIC 9(03) VALUE ZERO COMP-3.         
018500     05  SPAR-IDPROJ-MIN             PIC X(04)  VALUE SPACE.              
018600     05  SPAR-FLPISK-MIN             PIC X(01)  VALUE SPACE.              
018700     05  SPAR-DAFINLEV-MIN           PIC 9(08)  VALUE ZERO.               
018800     05  SPAR-IDAO-MIN               PIC X(10)  VALUE SPACE.              
018900                                                                          
019000     05  SPAR-MID-KDHF               PIC 9  VALUE ZERO.                   
019100                                                                          
019200     05  SPAR-TIFINLV-AAVVD          PIC 9(05)  VALUE ZERO.               
019300     05  SPAR-TIFINLV-AAVVD-R  REDEFINES  SPAR-TIFINLV-AAVVD.             
019400         10  SPAR-TIFINLV-AAVV       PIC 9(04).                           
019500         10  FILLER                  PIC 9(01).                           
019600                                                                          
019700     05  SPAR-ETTPUNKTNOLL           PIC 9V9    VALUE 1.                  
019800     05  SPAR-MFL                    PIC X(03)  VALUE 'MFL'.              
019900                                                                          
020000     05  SPAR-W009VADD-DATUM         PIC S9(5)  VALUE ZERO COMP-3.        
020100     05  SPAR-W009VADD-ANTAL         PIC S9(3)  VALUE ZERO COMP-3.        
020200                                                                          
020300     05  SPAR-DAGENS-DATUM           PIC 9(06)  VALUE ZERO.               
020400     05  SPAR-DAGENS-DATUM-AAVV.                                          
020500         10  SPAR-DAGENS-DATUM-AA    PIC 9(02)  VALUE ZERO.               
020600         10  SPAR-DAGENS-DATUM-VV    PIC 9(02)  VALUE ZERO.               
020700     05  SPAR-DAGENS-DATUM-AAVV-R REDEFINES SPAR-DAGENS-DATUM-AAVV        
020800                                     PIC 9(04).                           
020900                                                                          
021000     05  SPAR-TILEVBEG               PIC 9(06)  VALUE ZERO.               
021100     05  SPAR-TILEVBEG2              PIC 9(04)  VALUE ZERO.               
021200     05  SPAR-FLERS                  PIC X(01)  VALUE 'N'.                
021300     05  SPAR-IDARTNR-ERS1           PIC 9(09)  VALUE ZERO.               
021400     05  SPAR-KDGK                   PIC 9(01)  VALUE ZERO.               
021500     05  SPAR-KDERS                  PIC 9(02)  VALUE ZERO.               
021600     05  SPAR-SUTPO-TOT              PIC 9(09)  VALUE ZERO                
021700                                                COMP-3.                   
021800     05  SPAR-IDLKTO                 PIC 9(07)  VALUE 5714540.            
021900     05  SPAR-IDLKTO-R  REDEFINES  SPAR-IDLKTO.                           
022000         10  SPAR-IDLKTO-FF          PIC 9(02).                           
022100         10  SPAR-IDLKTO-POS3-7      PIC 9(05).                           
022200                                                                          
022300     05  SPAR-KDSORT                 PIC X(02)  VALUE SPACE.              
022400                                                                          
022500     05  SPAR-KVPB-TOTALT            PIC 9(09)V9(01) VALUE ZERO.          
022600                                                                          
022700     05  SPAR-ARSOMS                 PIC 9(09)  VALUE ZERO.               
022800     05  SPAR-KVMP                   PIC 9(09)  VALUE ZERO.               
022900     05  SPAR-VVKL                   PIC 9(01)  VALUE ZERO.               
023000     05  SPAR-KFAKTOR                PIC 9V9    VALUE ZERO.               
023100     05  SPAR-KVMAD                  PIC 9(09)  VALUE ZERO.               
023200     05  SPAR-KVSLAGER               PIC 9(07)  VALUE ZERO.               
023300                                                                          
023400     05  SPAR-RESLJUST-C1            PIC 9V9    VALUE ZERO.               
023500     05  SPAR-TISLJUST-C1            PIC 9(04)  VALUE 9999.               
023600                                                                          
023700     05  SPAR-IDFTG                  PIC 9(02)  VALUE 57.                 
023800     05  SPAR-IDFKNGRP               PIC 9(04)  VALUE ZERO.               
023900     05  SPAR-GTID                   PIC 9(02)  VALUE ZERO.               
024000     05  SPAR-KVQ                    PIC 9(07)  VALUE ZERO.               
024100     05  SPAR-IDLEVNR                PIC X(05)  VALUE SPACE.              
024200     05  SPAR-KDPRODSL-ARTC          PIC 9(02)  VALUE ZERO.               
024300     05  SPAR-KDRESBED               PIC X(01)  VALUE SPACE.              
024400     05  SPAR-IDINK                  PIC X(04)  VALUE SPACE.              
024500     05  SPAR-KDHF                   PIC 9(01)  VALUE ZERO.               
024600     05  SPAR-KDEMBKOD               PIC 9(3)   COMP-3 VALUE ZERO.        
024700     05  SPAR-BEFT                   PIC 9(3)   COMP-3 VALUE ZERO.        
024800     05  SPAR-RAD-IND                PIC 9(02)  VALUE ZERO.               
024900     05  SPAR-RAD-IND-MAX            PIC 9(02)  VALUE 12.                 
025000                                                                          
025100     05  SPAR-KDPRODSL-KPS           PIC 9(03)  VALUE ZERO.               
025200     05  SPAR-IDFTG-KPS              PIC 9(02)  VALUE ZERO.               
025300     05  SPAR-FLLSRDEL               PIC X(01)  VALUE 'J'.                
025400     05  SPAR-KDUART                 PIC X(01)  VALUE SPACE.              
025500                                                                          
025600     05  SPAR-PRARTSTD-ARTC          PIC 9(7)V9(2) VALUE ZERO.            
025700                                                                          
025800 01      SPAR-PRARTSTD.                                                   
025900         10  SPAR-PRARTSTD-HELTAL    PIC 9(07) VALUE ZERO.                
026000         10  SPAR-PRARTSTD-PUNKT     PIC X(01) VALUE SPACE.               
026100         10  SPAR-PRARTSTD-DECIMAL   PIC 9(02) VALUE ZERO.                
026200                                                                          
026300 01      SPAR-PRARTSTD-R.                                                 
026400         10  SPAR-PRARTSTD-R-HELTAL  PIC 9(07).                           
026500         10  SPAR-PRARTSTD-R-DECIMAL PIC 9(02).                           
026600                                                                          
026700 01      SPAR-PRARTSTD-RED  REDEFINES  SPAR-PRARTSTD-R.                   
026800         10 SPAR-PRARTSTD-HDTAL      PIC 9(07)V9(02).                     
026900                                                                          
027000 01  SPAR-KVPB-C1.                                                        
027100         10  SPAR-KVPB-C1-HELTAL     PIC 9(06)  VALUE ZERO.               
027200         10  SPAR-KVPB-C1-PUNKT      PIC X(01)  VALUE SPACE.              
027300         10  SPAR-KVPB-C1-DECIMAL    PIC 9(01)  VALUE ZERO.               
027400                                                                          
027500 01  SPAR-KVPB-C1-R.                                                      
027600         10  SPAR-KVPB-C1-R-HELTAL   PIC 9(06).                           
027700         10  SPAR-KVPB-C1-R-DECIMAL  PIC 9(01).                           
027800 01  SPAR-KVPB-C1-RED   REDEFINES  SPAR-KVPB-C1-R.                        
027900         10  SPAR-KVPB-C1-HDTAL      PIC 9(06)V9(01).                     
028000                                                                          
028100 01  SPAR-KVPROG                     PIC 9(7)   VALUE ZERO.               
028200 01  SPAR-IDBERED                    PIC 9(3)   VALUE ZERO.               
028300     EJECT                                                                
028400*                                                                         
028500******************************************************************        
028600*           D Y N A M I S K A  S U B P R O G R A M               *        
028700******************************************************************        
028800*                                                                         
028900 01  DYNAMISKA-SUBPROGRAM.                                                
029000     05  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
029100     05  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
029200     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
029300     05  WDECEDIT                PIC X(08)  VALUE 'WDECEDIT'.             
029400     05  WKPSKONV                PIC X(08)  VALUE 'WKPSKONV'.             
029500     05  W009VADD                PIC X(08)  VALUE 'W009VADD'.             
029600     03  W005INIT                PIC X(8)   VALUE 'W005INIT'.             
029700     03  WMEDKONV                PIC X(8)   VALUE 'WMEDKONV'.             
029800                                                                          
029900     EJECT                                                                
030000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
030100*01 -COPY WMEDAREA                                                        
030200     EJECT                                                                
030300 01  MESSAGE-CODES.                                                       
030400     03  INF-REFILL-PART        PIC X(3)    VALUE '434'.                  
030500                                                                          
030600******************************************************************        
030700*    F E L M E D D E L A N D E N                                          
030800******************************************************************        
030900*                                                                         
031000 01  FEL-MEDDELANDEN.                                                     
031100     03  FEL-1                   PIC X(32) VALUE                          
031200            'ARTIKELNUMMER EJ NUMERISKT      '.                           
031300     03  FEL-2                   PIC X(35) VALUE                          
031400            'UPPLYSTA FÄLT FEL,ELLER SE INFOMED.'.                        
031500     03  FEL-3                   PIC X(32) VALUE                          
031600            'ARTIKELNUMMER SAKNAS PÅ ARTREG  '.                           
031700     03  FEL-4                   PIC X(34) VALUE                          
031800            'ARTIKEL UTGÅNGEN, UPPD EJ TILLÅTEN'.                         
031900     03  FEL-5                   PIC X(34) VALUE                          
032000            'ARTIKEL ERSATT, UPPD EJ TILLÅTEN  '.                         
032100     03  FEL-7                   PIC X(34) VALUE                          
032200            'UPPDAT I PRISDEL FÅR EJ SKE       '.                         
032300     03  FEL-8                   PIC X(47) VALUE                          
032400            'MASKIN. KÖP VIA INKÖP EJ MÖJLIGT ,PROJ-K SAKNAS'.            
032500     03  FEL-9                   PIC X(34) VALUE                          
032600            'ARTIKELNUMMER SAKNAS PÅ NYPON     '.                         
032700     03  FEL-10                  PIC X(34) VALUE                          
032800            'INKÖP HAR REDAN KÖPT ARTIKELN     '.                         
032900     03  FEL-11                  PIC X(34) VALUE                          
033000            'SIMULERING FÅR EJ SKE             '.                         
033100     03  FEL-12                  PIC X(34) VALUE                          
033200            'KÖP FÅR EJ GÖRAS                  '.                         
033300     03  FEL-13                  PIC X(34) VALUE                          
033400            'FÖRSTA KÖP REDAN REGISTRERAT      '.                         
033500     03  FEL-14                  PIC X(52) VALUE                          
033600         'ANGIVET KONTO MEDFÖR PRODSL-BYTE, KONTAKTA BEREDARE!'.          
033700     03  FEL-15                  PIC X(56) VALUE                          
033800     '1:A AVROPV. STÄMMER EJ MED 1:A INLEV, KONTAKTA BEREDARE!'.          
033900     03  FEL-16                  PIC X(46) VALUE                          
034000            'OM KÖP EJ SKALL GÖRAS, LÄGG NOLL I ÅRSPROGNOS!'.             
034100     03  FEL-17                  PIC X(47) VALUE                          
034200            'MASKIN. KÖP VIA INKÖP EJ MÖJLIGT ,INKNR FEL    '.            
034300     03  FEL-19                  PIC X(47) VALUE                          
034400            'KÖP EJ KLART, REPKÖP EJ TILLÅTET               '.            
034500     03  FEL-20                  PIC X(47) VALUE                          
034600            'AVTAL/KONCERN/HF FINNS, REPKÖP EJ TILLÅTET     '.            
034700     03  FEL-22                  PIC X(47) VALUE                          
034800            'NYKÖP EJ GJORT, REPKÖP EJ TILLÅTET             '.            
034900     03  FEL-23                  PIC X(47) VALUE                          
035000            'PRIS SAKNAS, NYKÖP EJ GJORT, REPKÖP EJ TILLÅTET'.            
035100     03  FEL-24                  PIC X(47) VALUE                          
035200            'ÄNDRING/ANNULLATION FÖR NEDCAR EJ TILLÅTET'.                 
035300     03  FEL-25                  PIC X(47) VALUE                          
035400            'ANVÄND MOTSV LEVNR, SE BILD 2111'.                           
035500*    ENGELSK TEXT                                                         
035600     03  FEL-101                 PIC X(32) VALUE                          
035700            'PART NO. NOT NUMERIC            '.                           
035800     03  FEL-102                 PIC X(35) VALUE                          
035900            'ERROR IN HIGH LIGHTED FIELDS       '.                        
036000     03  FEL-103                 PIC X(32) VALUE                          
036100            'PART NO. MISSING IN PART BASE   '.                           
036200     03  FEL-104                 PIC X(34) VALUE                          
036300            'PART EXPIRED,   UPD. NOT ALLOWED  '.                         
036400     03  FEL-105                 PIC X(34) VALUE                          
036500            'PART REPLACED,  UPD. NOT ALLOWED  '.                         
036600     03  FEL-107                 PIC X(34) VALUE                          
036700            'UPDATE IN PRICE AREA NOT ALLOWED  '.                         
036800     03  FEL-108                 PIC X(47) VALUE                          
036900            'MACHIN. PURCHASE NOT POSSIBLE, PROJ-K MISSING  '.            
037000     03  FEL-109                 PIC X(34) VALUE                          
037100            'PART NO MISSING IN THE NYPON BASE '.                         
037200     03  FEL-110                 PIC X(34) VALUE                          
037300            'PURCHASE ALREADY DONE             '.                         
037400     03  FEL-111                 PIC X(34) VALUE                          
037500            'SIMULATION NOT ALLOWED            '.                         
037600     03  FEL-112                 PIC X(34) VALUE                          
037700            'PURCHASE NOT ALLOWED              '.                         
037800     03  FEL-113                 PIC X(34) VALUE                          
037900            'FIRST PURCHASE ALREADY REGISTRED  '.                         
038000     03  FEL-114                 PIC X(52) VALUE                          
038100         'SPEC ACCOUNT LEAD TO CHANGE OF PRODSL,CONTACT PREP.!'.          
038200     03  FEL-115                 PIC X(56) VALUE                          
038300     '1:ST ARREA DISAGREE WITH 1:ST DELIVERY,   CONTACT PREP.!'.          
038400     03  FEL-116                 PIC X(56) VALUE                          
038500     'IF NOT PURCHASE, SET ZERO AS START-VALUE TO YEAR-PROGNOS'.          
038600     03  FEL-117                 PIC X(47) VALUE                          
038700            'MACHIN. PURCHASE NOT ALLOWED, PURCH.NO. ERROR  '.            
038800     03  FEL-119                 PIC X(47) VALUE                          
038900            'PURCHASE NOT DONE THROUGH, REP.PURCH. NOT ALL. '.            
039000     03  FEL-120                 PIC X(47) VALUE                          
039100            'AGREE/COMBINE/HF EXIST, REP.PURCH. NOT ALLOWED '.            
039200     03  FEL-122                 PIC X(47) VALUE                          
039300            'FIRST PURCHASE NOT DONE, REP.PURCH. NOT ALLOWED'.            
039400     03  FEL-123                 PIC X(47) VALUE                          
039500            'PRICE MISS.,NO 1ST PURCH EX, REP.PURCH NOT ALL.'.            
039600     03  FEL-124                 PIC X(47) VALUE                          
039700            'CHANGE/ANNULMENT FOR NEDCAR NOT ALLOWED'.                    
039800     03  FEL-125                  PIC X(47) VALUE                         
039900            'USE EQUAL SUPPLIER, SEE SCREEN 2111'.                        
040000                                                                          
040100 01  INFO-MEDDELANDEN.                                                    
040200                                                                          
040300     03  MED-1                   PIC X(32) VALUE                          
040400            'MER INFO PÅ NÄSTA SIDA          '.                           
040500     03  MED-2                   PIC X(32) VALUE                          
040600            'TRYCK PF11 FÖR UPPDATERING      '.                           
040700     03  MED-3                   PIC X(20) VALUE                          
040800            'UPPDATERING UTFÖRD '.                                        
040900     03  MED-4                   PIC X(20) VALUE                          
041000            'PASSIVMÄRKT ARTIKEL'.                                        
041100     03  MED-5                   PIC X(23) VALUE                          
041200            'LOKALTILLVERKAD ARTIKEL'.                                    
041300     03  MED-6                   PIC X(23) VALUE                          
041400            'ARTIKEL ERSATT         '.                                    
041500     03  MED-7                   PIC X(23) VALUE                          
041600            'MILITÄRARTIKEL         '.                                    
041700     03  MED-8                   PIC X(23) VALUE                          
041800            'SPECIALARTIKEL         '.                                    
041900     03  MED-9                   PIC X(23) VALUE                          
042000            'PISKARTIKEL            '.                                    
042100     03  MED-10                  PIC X(23) VALUE                          
042200            'SISTA ARTIKEL FRÅN KÖN '.                                    
042300     03  MED-11                  PIC X(41) VALUE                          
042400            'UPPDATERING UTFÖRD, NYKÖPSRAPPORT PRINTAD'.                  
042500     03  MED-12                  PIC X(41) VALUE                          
042600            'UPPDATERING UTFÖRD, INFO TILL INKÖP PV   '.                  
042700     03  MED-13                  PIC X(42) VALUE                          
042800            'UPPDATERING UTFÖRD, INFO TILL INKÖP NEDCAR'.                 
042900     03  MED-14                  PIC X(41) VALUE                          
043000            'INKÖPARE EJ AUKTORISERAD AV INKÖP LV     '.                  
043100     03  MED-15                  PIC X(23) VALUE                          
043200            'MONTERINGSANVISNING    '.                                    
043300     03  MED-16                  PIC X(42) VALUE                          
043400            'BORTTAG UTFÖRT, EJ INFO TILL INKÖP NEDCAR'.                  
043500     03  MED-17                  PIC X(23) VALUE                          
043600            'INGÅR I SATS '.                                              
043700     03  MED-18                  PIC X(23) VALUE                          
043800            'LYNK ARTIKEL '.                                              
043900*    ENGELSK TEXT                                                         
044000     03  MED-101                 PIC X(32) VALUE                          
044100            'MORE INFORMATION IN NEXT PAGE   '.                           
044200     03  MED-102                 PIC X(32) VALUE                          
044300            'PRESS PF11  TO UPDATE          '.                            
044400     03  MED-103                 PIC X(20) VALUE                          
044500            'UPDATE DONE        '.                                        
044600     03  MED-104                 PIC X(20) VALUE                          
044700            'PART PASSIVE MARKED'.                                        
044800     03  MED-105                 PIC X(23) VALUE                          
044900            'LOCAL PRODUCED PART    '.                                    
045000     03  MED-106                 PIC X(23) VALUE                          
045100            'PART REPLACED          '.                                    
045200     03  MED-107                 PIC X(23) VALUE                          
045300            'MILITARY PART          '.                                    
045400     03  MED-108                 PIC X(23) VALUE                          
045500            'SPECIAL PART           '.                                    
045600     03  MED-109                 PIC X(23) VALUE                          
045700            'PISK PART              '.                                    
045800     03  MED-110                 PIC X(23) VALUE                          
045900            'LAST PART FROM CUE     '.                                    
046000     03  MED-111                  PIC X(41) VALUE                         
046100            'UPPDATERING UTFÖRD,                      '.                  
046200     03  MED-112                 PIC X(41) VALUE                          
046300            'UPDATE DONE, INFORMATION TO PURCHASE VCC '.                  
046400     03  MED-113                 PIC X(42) VALUE                          
046500            'UPDATE DONE, INFORMATION TO PURCH. NEDCAR'.                  
046600     03  MED-114                 PIC X(41) VALUE                          
046700            'INKÖPARE EJ AUKTORISERAD                 '.                  
046800     03  MED-115                 PIC X(23) VALUE                          
046900            'INSTRUCTION            '.                                    
047000     03  MED-116                 PIC X(42) VALUE                          
047100            'DELETE DONE, NO INFORM. TO PURCH. NEDCAR'.                   
047200     03  MED-117                 PIC X(23) VALUE                          
047300            'INCLUDED IN KIT '.                                           
047400     03  MED-118                 PIC X(23) VALUE                          
047500            'LYNK PART       '.                                           
047600                                                                          
047700 01  MSG-KOM-MEDDELANDEN.                                                 
047800     03  FEL-ERR-FIELD           PIC X(3)    VALUE '001'.                 
047900     03  FEL-UNREG-PART          PIC X(3)    VALUE '017'.                 
048000     03  FEL-OBSOLETED-PART      PIC X(3)    VALUE '018'.                 
048100     03  FEL-NOT-NUMERIC-PARTNO  PIC X(3)    VALUE '020'.                 
048200     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
048300     03  OK-GODKANT-FEL          PIC X(3)    VALUE '114'.                 
048400                                                                          
048500     EJECT                                                                
048600*                                                                         
048700******************************************************************        
048800*   C O P Y T E X T   NYPON-ROT SPARAS EV. BEROENDE PÅ WKPSKONV  *        
048900******************************************************************        
049000*                                                                         
049100 01  IMS-WS-0.                                                            
049200     03  FILLER                  PIC X(16)   VALUE 'NYPONCOPY'.           
049300     SKIP3                                                                
049400*01  AREA -COPY WDD201          -PRE SPAR-                                
049500     EJECT                                                                
049600*                                                                         
049700******************************************************************        
049800*                    C O P Y T E X T E R    (DYNAMISKA ANROP)    *        
049900******************************************************************        
050000*                                                                         
050100 01  IMS-WS-1.                                                            
050200     03  FILLER                  PIC X(16)   VALUE 'RDAT-AREA'.           
050300     SKIP3                                                                
050400*01  -COPY WDATAREA                                                       
050500     EJECT                                                                
050600 01  IMS-WS-2.                                                            
050700     03  FILLER                  PIC X(16)   VALUE 'RDEC-AREA'.           
050800     SKIP3                                                                
050900*01  -COPY WDECAREA                                                       
051000     EJECT                                                                
051100 01  IMS-WS-3.                                                            
051200     03  FILLER                  PIC X(16)   VALUE 'WKPSKONV '.           
051300     SKIP3                                                                
051400*01  -COPY WKPSAREA                                                       
051500     EJECT                                                                
051600 01  IMS-WS-4.                                                            
051700     03  FILLER                  PIC X(16)   VALUE 'W005INIT '.           
051800     SKIP3                                                                
051900*01  -COPY WMSGINIT                                                       
052000     EJECT                                                                
052100*                                                                         
052200******************************************************************        
052300*              N Y C K L A R  T I L L  D L I                     *        
052400******************************************************************        
052500*                                                                         
052600 01  NYCKLAR-TILL-DLI.                                                    
052700     03  W-IDARTNR-X.                                                     
052800         05  W-IDARTNR            PIC S9(09) COMP-3 VALUE ZERO.           
052900                                                                          
053000     03  W-KDANSKQ-X.                                                     
053100         05  W-KDANSKQ            PIC  X(01)        VALUE '1'.            
053200                                                                          
053300     03  W-IDLEVNR-X.                                                     
053400         05  W-IDLEVNR            PIC X(05)  VALUE SPACE.                 
053500                                                                          
053600     03  W-IDSKYLT-X.                                                     
053700         05  W-IDSKYLT            PIC X(03)  VALUE SPACE.                 
053800                                                                          
053900     03  W-KDNOTTYP-X.                                                    
054000         05  W-KDNOTTYP           PIC S9(01) COMP-3 VALUE ZERO.           
054100                                                                          
054200*2141-KOPPLING:                                                           
054300     03  W-WDD2B1KY-MIN.                                                  
054400         05  W-FLPISK-MIN         PIC X(1).                               
054500         05  W-IDANSK-MIN         PIC S9(3)  COMP-3.                      
054600         05  W-DAFINLEV-MIN       PIC  9(8).                              
054700         05  W-IDAO-MIN           PIC X(10).                              
054800         05  W-IDPROJ-MIN         PIC X(4).                               
054900         05  W-IDARTNR-MIN        PIC S9(9)  COMP-3.                      
055000                                                                          
055100     03  W-WDD2B1KY-MAX.                                                  
055200         05  W-FLPISK-MAX         PIC X(1).                               
055300         05  W-IDANSK-MAX         PIC S9(3)  COMP-3.                      
055400         05  W-DAFINLEV-MAX       PIC  9(8).                              
055500         05  W-IDAO-MAX           PIC X(10).                              
055600         05  W-IDPROJ-MAX         PIC X(4).                               
055700         05  W-IDARTNR-MAX        PIC S9(9)  COMP-3.                      
055800                                                                          
055900     03  W-WDD7A1KY-MIN.                                                  
056000         05  W-IDARTNR-MIN7       PIC S9(9)  COMP-3 VALUE ZERO.           
056100         05  FILLER               PIC X(7)   VALUE LOW-VALUE.             
056200                                                                          
056300     03  W-WDD7A1KY-MAX.                                                  
056400         05  W-IDARTNR-MAX7       PIC S9(9)  COMP-3                       
056500                                  VALUE ZERO.                             
056600         05  FILLER               PIC X(7)   VALUE HIGH-VALUE.            
056700                                                                          
056800     03  W-1131KEY-X.                                                     
056900         05  FILLER               PIC X(04)  VALUE '1131'.                
057000         05  W-KDPRODSL1          PIC S9(3)  VALUE ZERO COMP-3.           
057100         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
057200                                                                          
057300     03  W-1132KEY-X.                                                     
057400         05  W-IDPROJK            PIC X(04)  VALUE SPACE.                 
057500         05  W-IDPROJOBJ          PIC X(04)  VALUE SPACE.                 
057600         05  W-IDPROJ             PIC X(04)  VALUE SPACE.                 
057700         05  FILLER               PIC X(03)  VALUE LOW-VALUE.             
057800                                                                          
057900     03  W-1137KEY-X.                                                     
058000         05  FILLER               PIC X(04)  VALUE '1137'.                
058100         05  W-KDPRODSL2          PIC S9(3)  VALUE ZERO COMP-3.           
058200         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
058300                                                                          
058400     03  W-1138KEY-X.                                                     
058500         05  W-IDUSER             PIC X(08)  VALUE SPACE.                 
058600                                                                          
058700     03  W-1141KEY-X.                                                     
058800         05  FILLER               PIC X(04)  VALUE '1141'.                
058900         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
059000                                                                          
059100     03  W-1142KEY-X.                                                     
059200         05  FILLER               PIC X(01)  VALUE '1'.                   
059300                                                                          
059400     03  W-WDG3KEY-2221-X.                                                
059500         05  FILLER               PIC X(04)  VALUE '2221'.                
059600         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
059700                                                                          
059800     EJECT                                                                
059900*                                                                         
060000******************************************************************        
060100*                    M I D-C O P Y T E X T                       *        
060200******************************************************************        
060300*                                                                         
060400*                        ****    MFS OCH SKÄRMHANTERING                   
060500 01  IMS-WS-3.                                                            
060600     03  FILLER                  PIC X(16)   VALUE 'MFS-WS'.              
060700     SKIP3                                                                
060800*01  MID -COPY W2I13301                                                   
060900     EJECT                                                                
061000*01  MID -COPY W2I14101  -PRE 2141-                                       
061100     EJECT                                                                
061200*                                                                         
061300******************************************************************        
061400*                    M S G - A R E O R                           *        
061500******************************************************************        
061600*                                                                         
061700 01  IMS-WS-4.                                                            
061800     03  FILLER              PIC X(16)  VALUE 'MSG-KOM-AREA'.             
061900*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
062000*01  -COPY WMSGKOM                                                        
062100     EJECT                                                                
062200     SKIP3                                                                
062300     03  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.            
062400*01  -COPY WMSGAREA                                                       
062500     EJECT                                                                
062600*                                                                         
062700******************************************************************        
062800*                    M O D-C O P Y T E X T                       *        
062900******************************************************************        
063000*                                                                         
063100*    03  MOD -COPY W2O13301  -RED MSG-AREA.                               
063200     EJECT                                                                
063300*                                                                         
063400******************************************************************        
063500*                    M F S - A R E A                             *        
063600******************************************************************        
063700*                                                                         
063800 01  IMS-WS-6.                                                            
063900     03  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.            
064000     SKIP3                                                                
064100*01  -COPY WMFSAREA.                                                      
064200     EJECT                                                                
064300*                                                                         
064400******************************************************************        
064500*    A R B E T S A R E O R  I M S - S E K T I O N E R N A        *        
064600******************************************************************        
064700*                                                                         
064800 01  IMS-WS-7.                                                            
064900     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
065000     SKIP3                                                                
065100*****                    **** STATUS-KOD FRÅN IMS                         
065200     03  STATUS-WS               PIC X(2).                                
065300         88  SEGMENT-FINNS                   VALUE '  '.                  
065400         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
065500     SKIP3                                                                
065600     03  GODK-STATUSKODER.                                                
065700         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
065800     SKIP3                                                                
065900 01  IMS-WS-8.                                                            
066000     03  FILLER                  PIC X(09)   VALUE 'SSA:ER   '.           
066100     SKIP3                                                                
066200 01  SSA1                        PIC X(128).                              
066300 01  SSA2                        PIC X(128).                              
066400 01  SSA3                        PIC X(128).                              
066500     EJECT                                                                
066600*                                                                         
066700******************************************************************        
066800*            I M S  F U N K T I O N S K O D E R                  *        
066900******************************************************************        
067000*                                                                         
067100*                                                                         
067200 01  IMS-WS-9.                                                            
067300     03  FILLER                  PIC X(16)   VALUE ' IMS-FUNK'.           
067400     SKIP3                                                                
067500*01  -COPY W0003                                                          
067600     EJECT                                                                
067700*                                                                         
067800******************************************************************        
067900*            D L I  I N P U T-O U T P U T A R E A -1             *        
068000******************************************************************        
068100*                                                                         
068200 01  IMS-WS-10.                                                           
068300     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA1'.           
068400     SKIP3                                                                
068500 01  DLI-IO-AREA1.                                                        
068600     03  IO-AREA1                  PIC X(900) VALUE SPACE.                
068700     SKIP3                                                                
068800*                                                                         
068900******************************************************************        
069000*            S E G M E N T C O P Y T E X T E R                   *        
069100******************************************************************        
069200*                                                                         
069300*    03  ARTC -COPY WDK601                   -RED IO-AREA1.               
069400     EJECT                                                                
069500*    03  ARTC -COPY WDK611                   -RED IO-AREA1.               
069600     EJECT                                                                
069700*    03  ARTC -COPY WDK622                   -RED IO-AREA1.               
069800     EJECT                                                                
069900*    03  ARTC -COPY WDK623                   -RED IO-AREA1.               
070000     EJECT                                                                
070100*    03  ARTC -COPY WDK625                   -RED IO-AREA1.               
070200     EJECT                                                                
070300*    03  BENA -COPY WDD311      -PRE BENA11- -RED IO-AREA1.               
070400     EJECT                                                                
070500*    03  ERSB -COPY WDD7A1      -PRE ERSB01- -RED IO-AREA1.               
070600     EJECT                                                                
070700*    03  XXBN -COPY WDGX2222    -PRE XXBN-   -RED IO-AREA1.               
070800     EJECT                                                                
070900*    03  XXAQ -COPY WDGX1132    -PRE XXAQ-   -RED IO-AREA1.               
071000     EJECT                                                                
071100*    03  XXAT -COPY WDGX1138    -PRE XXAT-   -RED IO-AREA1.               
071200     EJECT                                                                
071300*    03  XXAV -COPY WDGX1142    -PRE XXAV-   -RED IO-AREA1.               
071400     EJECT                                                                
071500*    03  ARTI -COPY WDD2B1      -PRE ARTI-   -RED IO-AREA1.               
071600     EJECT                                                                
071700*    03  ARTM -COPY WDK901                   -RED IO-AREA1.               
071800     EJECT                                                                
071900*                                                                         
072000******************************************************************        
072100*            D L I  I N P U T-O U T P U T A R E A -2             *        
072200******************************************************************        
072300*                                                                         
072400 01  IMS-WS-16.                                                           
072500     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA2'.           
072600     SKIP3                                                                
072700 01  DLI-IO-AREA2.                                                        
072800     03  IO-AREA2                  PIC X(550)   VALUE SPACE.              
072900     SKIP3                                                                
073000*    03  ARTG -COPY WDD201        -PRE NYPON-  -RED IO-AREA2.             
073100     EJECT                                                                
073200*                                                                         
073300******************************************************************        
073400*            D L I  I N P U T-O U T P U T A R E A -3             *        
073500******************************************************************        
073600*                                                                         
073700 01  IMS-WS-17.                                                           
073800     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA3'.           
073900     SKIP3                                                                
074000 01  DLI-IO-AREA3.                                                        
074100     03  IO-AREA3                  PIC X(100)   VALUE SPACE.              
074200     SKIP3                                                                
074300*    03  LEVA -COPY WDF101      -PRE LEVA01- -RED IO-AREA3.               
074400     EJECT                                                                
074500*                                                                         
074600******************************************************************        
074700*            D L I  I N P U T-O U T P U T A R E A  W D H 8       *        
074800******************************************************************        
074900 01  DLI-IO-AREA5.                                                        
075000*    03  -COPY WDH801                                                     
075100     EJECT                                                                
075200******************************************************************        
075300*            L I N K A G E  S E C T I O N                        *        
075400******************************************************************        
075500*                                                                         
075600 LINKAGE SECTION.                                                         
075700     SKIP2                                                                
075800*01  -COPY W0009     -PRE MSG-                                            
075900     EJECT                                                                
076000*01  -COPY W0009     -PRE ALT-                                            
076100     EJECT                                                                
076200*01  -COPY W0009     -PRE MSGKOM-                                         
076300     EJECT                                                                
076400*01  -COPY W0008     -PRE USEA-                                           
076500         05  FILLER           PIC X.                                      
076600     EJECT                                                                
076700*01  -COPY W0008     -PRE ARTC-                                           
076800         05  FILLER              PIC X.                                   
076900     EJECT                                                                
077000*01  -COPY W0008     -PRE BENA-                                           
077100         05  FILLER              PIC X.                                   
077200     EJECT                                                                
077300*01  -COPY W0008     -PRE LEVA-                                           
077400         05  FILLER              PIC X.                                   
077500     EJECT                                                                
077600*01  -COPY W0008     -PRE ERSB-                                           
077700         05  FILLER              PIC X.                                   
077800     EJECT                                                                
077900*01  -COPY W0008     -PRE ARTG-                                           
078000         05  FILLER              PIC X.                                   
078100     EJECT                                                                
078200*01  -COPY W0008     -PRE XXBN-                                           
078300         05  FILLER              PIC X.                                   
078400     EJECT                                                                
078500*01  -COPY W0008     -PRE XXAQ-                                           
078600         05  FILLER              PIC X.                                   
078700     EJECT                                                                
078800*01  -COPY W0008     -PRE XXAT-                                           
078900         05  FILLER              PIC X.                                   
079000     EJECT                                                                
079100*01  -COPY W0008     -PRE XXAV-                                           
079200         05  FILLER              PIC X.                                   
079300     EJECT                                                                
079400*01  -COPY W0008     -PRE ARTI-                                           
079500         05  FILLER              PIC X.                                   
079600     EJECT                                                                
079700*01  -COPY W0008     -PRE ARTM-                                           
079800         05  FILLER              PIC X.                                   
079900     EJECT                                                                
080000*01  -COPY W0008     -PRE PRIG-                                           
080100      05 FILLER                  PIC X(18).                               
080200     EJECT                                                                
080300 PROCEDURE DIVISION USING MSG-PCB ALT-PCB MSGKOM-PCB                      
080400     USEA-PCB                                                             
080500     ARTC-PCB BENA-PCB LEVA-PCB ERSB-PCB ARTG-PCB                         
080600     XXBN-PCB XXAQ-PCB XXAT-PCB                                           
080700     XXAV-PCB ARTI-PCB ARTM-PCB PRIG-PCB.                                 
080800                                                                          
080900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB MSGKOM-PCB                     
081000     USEA-PCB                                                             
081100     ARTC-PCB BENA-PCB LEVA-PCB ERSB-PCB ARTG-PCB                         
081200     XXBN-PCB XXAQ-PCB XXAT-PCB                                           
081300     XXAV-PCB ARTI-PCB ARTM-PCB PRIG-PCB.                                 
081400                                                                          
081500     PERFORM IMS-GET-MSG                                                  
081600     IF SEGMENT-FINNS                                                     
081700        PERFORM IMS-GET-WMSGKOM-MSG                                       
081800        PERFORM A-INIT-SPARA-INPUT                                        
081900        IF MFS-IDPFK = '8'                                                
082000           PERFORM G-FIXA-MIN-MAX-NYCKLAR                                 
082100        END-IF                                                            
082200        IF WS-IDARTNR NUMERIC                                             
082300           MOVE WS-IDARTNR  TO W-IDARTNR                                  
082400           PERFORM IMS-GHU-ARTC01                                         
082500           IF SEGMENT-FINNS                                               
082600              IF ART-KDERS-UTG > ZERO                                     
082700                 IF MSGI-IDLAND-SPR = 'GB'                                
082800                    MOVE FEL-104            TO MOD-TEMFSFEL               
082900                 ELSE                                                     
083000                    MOVE FEL-4              TO MOD-TEMFSFEL               
083100                 END-IF                                                   
083200                 MOVE FEL-OBSOLETED-PART TO MSG-KOM-IDMFSMED              
083300                 MOVE '2'                TO MSG-KOM-KDSVAR                
083400              ELSE                                                        
083500                 IF MFS-UPDATE  OR  MFS-UPD-X                             
083600                    PERFORM E-KOLLA-INPUT                                 
083700                    IF SW-INPUT-RAETT = JA                                
083800                       PERFORM F-UPPDATERA-OCH-VISA-BILD                  
083900                       MOVE OK-BEHANDLAD  TO MSG-KOM-IDMFSMED             
084000                    ELSE                                                  
084100                       IF MSGI-IDLAND-SPR = 'GB'                          
084200                          MOVE FEL-102            TO MOD-TEMFSFEL         
084300                       ELSE                                               
084400                          MOVE FEL-2              TO MOD-TEMFSFEL         
084500                       END-IF                                             
084600                       MOVE FEL-ERR-FIELD TO MSG-KOM-IDMFSMED             
084700                       MOVE '2'           TO MSG-KOM-KDSVAR               
084800                    END-IF                                                
084900                 ELSE                                                     
085000                    IF MID-IDARTNR-IN = ALL '+' AND                       
085100                       MFS-IDPFK      = SPACE   AND                       
085200                       EGEN-BILD                                          
085300                       PERFORM C-KOLLA-INPUT-SIMULERING                   
085400                       IF SW-INPUT-RAETT = JA                             
085500                          PERFORM D-SIMULERA                              
085600                       ELSE                                               
085700                          IF MSGI-IDLAND-SPR = 'GB'                       
085800                             MOVE FEL-102        TO MOD-TEMFSFEL          
085900                          ELSE                                            
086000                             MOVE FEL-2          TO MOD-TEMFSFEL          
086100                          END-IF                                          
086200                       END-IF                                             
086300                       PERFORM IMS-GHU-ARTC01                             
086400                    END-IF                                                
086500                    PERFORM B-VISA-BILD                                   
086600                 END-IF                                                   
086700              END-IF                                                      
086800           ELSE                                                           
086900              IF MSGI-IDLAND-SPR = 'GB'                                   
087000                 MOVE FEL-103            TO MOD-TEMFSFEL                  
087100              ELSE                                                        
087200                 MOVE FEL-3              TO MOD-TEMFSFEL                  
087300              END-IF                                                      
087400              MOVE FEL-UNREG-PART   TO MSG-KOM-IDMFSMED                   
087500              MOVE '2'              TO MSG-KOM-KDSVAR                     
087600           END-IF                                                         
087700        ELSE                                                              
087800           IF MSGI-IDLAND-SPR = 'GB'                                      
087900              MOVE FEL-101            TO MOD-TEMFSFEL                     
088000           ELSE                                                           
088100              MOVE FEL-1              TO MOD-TEMFSFEL                     
088200           END-IF                                                         
088300           MOVE FEL-NOT-NUMERIC-PARTNO TO MSG-KOM-IDMFSMED                
088400           MOVE '2'                    TO MSG-KOM-KDSVAR                  
088500        END-IF                                                            
088600                                                                          
088700        PERFORM H-FIXA-MIN-NYCKLAR                                        
088800                                                                          
088900        MOVE MFS-RENSA-FAELT        TO MOD-FLNYRAPP-IN                    
089000        MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLNYRAPP-IN-ATTR               
089100        IF MFS-UPD-X                                                      
089200          PERFORM IMS-INSERT-WMSGKOM-MSG                                  
089300********* DISPATCHERN                                                     
089400        ELSE                                                              
089500          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
089600          PERFORM IMS-INSERT-MSG                                          
089700        END-IF                                                            
089800                                                                          
089900     END-IF                                                               
090000                                                                          
090100     MOVE ZERO TO RETURN-CODE                                             
090200     GOBACK.                                                              
090300     EJECT                                                                
090400 A-INIT-SPARA-INPUT SECTION.                                              
090500     SKIP2                                                                
090600     IF MSG-DUBBLA-TRANSKODER                                             
090700         MOVE MSG-IDTRANS-2  TO MFS-IDTRANS WS-IDTRANS                    
090800         IF MFS-IDTRANS = '2141'                                          
090900            MOVE MSG-INDATA-MINUS-2-TRANSKODER TO 2141-MID                
091000         ELSE                                                             
091100            MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13301            
091200         END-IF                                                           
091300         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
091400     ELSE                                                                 
091500         MOVE MSG-IDTRANS-1  TO MFS-IDTRANS WS-IDTRANS                    
091600         IF MFS-IDTRANS = '2141'                                          
091700            MOVE MSG-INDATA-MINUS-1-TRANSKOD TO 2141-MID                  
091800         ELSE                                                             
091900            MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I13301              
092000         END-IF                                                           
092100         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
092200     END-IF                                                               
092300     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
092400     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
092500                                                                          
092600     IF EGEN-BILD                                                         
092700        CONTINUE                                                          
092800     ELSE                                                                 
092900        MOVE SPACE TO MFS-KDTRTYP                                         
093000                      MFS-IDPFK                                           
093100     END-IF                                                               
093200     EJECT                                                                
093300*2141-KOPPLING:                                                           
093400     IF MFS-IDTRANS = '2141'                                              
093500        PERFORM AC-KONTR-INIT-SLFLT                                       
093600                                                                          
093700        MOVE 1              TO SPAR-RAD-IND                               
093800        PERFORM UNTIL SPAR-RAD-IND > SPAR-RAD-IND-MAX OR                  
093900                      SW-TRAFF = JA                                       
094000           IF 2141-MID-SELECT-ARTIKEL (SPAR-RAD-IND) = ALL '+'            
094100              ADD 1         TO SPAR-RAD-IND                               
094200           ELSE                                                           
094300              INSPECT 2141-MID-IDARTNR (SPAR-RAD-IND) REPLACING           
094400                      LEADING SPACE BY ZERO                               
094500              MOVE 2141-MID-IDARTNR (SPAR-RAD-IND) TO                     
094600                                                   MID-IDARTNR-IN         
094700              MOVE JA       TO SW-TRAFF                                   
094800           END-IF                                                         
094900        END-PERFORM                                                       
095000     ELSE                                                                 
095100        IF EGEN-BILD                                                      
095200           PERFORM AA-KONTR-SLFLT                                         
095300           IF SW-SLACKTA-FALT-FEL = JA                                    
095400              PERFORM AB-INIT-SLFLT                                       
095500           END-IF                                                         
095600        ELSE                                                              
095700           PERFORM AB-INIT-SLFLT                                          
095800        END-IF                                                            
095900     END-IF                                                               
096000                                                                          
096100*-- FIXA NUMERISKA UPD-FÄLT VID ALL '+' PGA COBOL 6.2                     
096200     IF EGEN-BILD                                                         
096300       IF MID-KDHF           = ALL '+'                                    
096400         MOVE ZERO           TO SPAR-MID-KDHF                             
096500       ELSE                                                               
096600         MOVE MID-KDHF       TO SPAR-MID-KDHF                             
096700       END-IF                                                             
096800     END-IF                                                               
096900                                                                          
097000     MOVE LOW-VALUE         TO MOD-W2O13301                               
097100     MOVE 'W2O13301'        TO MFS-IDMOD                                  
097200     MOVE '2133'            TO MOD-IDTRANS                                
097300                                                                          
097400     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL                               
097500                               MOD-TEMFSINF                               
097600                               MOD-IDARTNR-IN                             
097700                                                                          
097800     ACCEPT SPAR-DAGENS-DATUM FROM DATE                                   
097900                                                                          
098000     IF MFS-UPD-X                                                         
098100****************  DISPATCHANROP                                           
098200                                                                          
098300       IF MID-IDARTNR-IN = ALL '+'                                        
098400          INSPECT MID-IDARTNR-UT REPLACING LEADING SPACE BY ZERO          
098500          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
098600       ELSE                                                               
098700          MOVE MID-IDARTNR-IN TO WS-IDARTNR                               
098800          MOVE SPACE          TO MFS-KDTRTYP                              
098900                                 MFS-IDPFK                                
099000       END-IF                                                             
099100                                                                          
099200     ELSE                                                                 
099300                                                                          
099400       IF MID-IDARTNR-IN = ALL '+'                                        
099500          CONTINUE                                                        
099600       ELSE                                                               
099700          MOVE SPACE          TO MFS-KDTRTYP                              
099800                                 MFS-IDPFK                                
099900       END-IF                                                             
100000                                                                          
100100       MOVE ALL '+' TO MSGI-WMSGINIT                                      
100200       MOVE '001'             TO MSGI-KDCALL                              
100300       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
100400       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
100500       MOVE '2133'            TO MSGI-IDTRANS                             
100600       IF MFS-IDTRANS = '2133'                                            
100700       OR (MID-IDARTNR-IN NUMERIC                                         
100800       AND MID-IDARTNR-IN > ZERO)                                         
100900           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
101000       END-IF                                                             
101100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
101200       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
101300       INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                     
101400     END-IF                                                               
101500                                                                          
101600     IF MFS-UPDATE                                                        
101700        IF MID-PRISDEL  = ALL '+'  AND                                    
101800           MID-KOPDEL   = ALL '+'                                         
101900           MOVE SPACE       TO MFS-KDTRTYP                                
102000        END-IF                                                            
102100     END-IF                                                               
102200                                                                          
102300     MOVE WS-IDARTNR        TO MOD-IDARTNR-UT                             
102400     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE.              
102500                                                                          
102600     EJECT                                                                
102700 AA-KONTR-SLFLT SECTION.                                                  
102800******************************************************************        
102900*    OM MAN KOMMER FRÅN EGEN-BILD KONTROLLERAS ALLA SLÄCKTA FÄLT          
103000*    SOM MÅSTE VARA NUMERISKA SÅ ATT DET INTE KOMMIT SKRÄP FRÅN           
103100*    MFS:EN                                                               
103200******************************************************************        
103300     SKIP2                                                                
103400     IF MID-IDANSK-FOM NUMERIC                                            
103500        MOVE MID-IDANSK-FOM             TO SPAR-IDANSK-FOM                
103600     ELSE                                                                 
103700        MOVE JA                         TO SW-SLACKTA-FALT-FEL            
103800     END-IF                                                               
103900                                                                          
104000     IF MID-IDANSK-TOM NUMERIC                                            
104100        MOVE MID-IDANSK-TOM             TO SPAR-IDANSK-TOM                
104200     ELSE                                                                 
104300        MOVE JA                         TO SW-SLACKTA-FALT-FEL            
104400     END-IF                                                               
104500                                                                          
104600     IF MID-IDANSK-MIN NUMERIC                                            
104700        MOVE MID-IDANSK-MIN             TO SPAR-IDANSK-MIN                
104800     ELSE                                                                 
104900        MOVE JA                         TO SW-SLACKTA-FALT-FEL            
105000     END-IF                                                               
105100                                                                          
105200     IF MID-TIFINLEV-MIN NUMERIC                                          
105300        MOVE MID-TIFINLEV-MIN           TO WS-DAFINLEV-DATUM              
105400        IF WS-DAFINLEV-DATUM > 500000                                     
105500          MOVE 19 TO WS-DAFINLEV-SEKEL                                    
105600        ELSE                                                              
105700          MOVE 20 TO WS-DAFINLEV-SEKEL                                    
105800        END-IF                                                            
105900        MOVE WS-DAFINLEV                TO SPAR-DAFINLEV-MIN              
106000     ELSE                                                                 
106100        MOVE JA                         TO SW-SLACKTA-FALT-FEL            
106200     END-IF                                                               
106300                                                                          
106400     IF SW-SLACKTA-FALT-FEL = JA                                          
106500        CONTINUE                                                          
106600     ELSE                                                                 
106700        MOVE MID-IDPROJ-VALT            TO SPAR-IDPROJ-VALT               
106800        MOVE MID-IDPROJ-MIN             TO SPAR-IDPROJ-MIN                
106900        MOVE MID-FLPISK-MIN             TO SPAR-FLPISK-MIN                
107000        MOVE MID-IDAO-MIN               TO SPAR-IDAO-MIN                  
107100     END-IF.                                                              
107200     EJECT                                                                
107300 AB-INIT-SLFLT   SECTION.                                                 
107400******************************************************************        
107500*    INITIERING ALLA SLÄCKTA FÄLT                                         
107600******************************************************************        
107700     SKIP2                                                                
107800     MOVE ZERO                       TO SPAR-IDANSK-FOM                   
107900                                        SPAR-IDANSK-TOM                   
108000                                        SPAR-IDANSK-MIN                   
108100                                        SPAR-DAFINLEV-MIN                 
108200     MOVE SPACE                      TO SPAR-IDPROJ-VALT                  
108300                                        SPAR-IDPROJ-MIN                   
108400                                        SPAR-FLPISK-MIN                   
108500                                        SPAR-IDAO-MIN.                    
108600     EJECT                                                                
108700 AC-KONTR-INIT-SLFLT SECTION.                                             
108800******************************************************************        
108900*    OM MAN KOMMER FRÅN 2141-BILDEN KOLLAS DESS NYCKLAR OCH 2133-         
109000*    MIN-NYCKLAR INITIERAS MED ZERO OCH SPACE.                            
109100******************************************************************        
109200     SKIP2                                                                
109300     INSPECT 2141-MID-IDANSK-FROM-UT                                      
109400                  REPLACING LEADING SPACE BY ZERO                         
109500     INSPECT 2141-MID-IDANSK-TOM-UT                                       
109600                  REPLACING LEADING SPACE BY ZERO                         
109700                                                                          
109800     IF 2141-MID-IDANSK-FROM-UT NUMERIC                                   
109900        MOVE 2141-MID-IDANSK-FROM-UT  TO SPAR-IDANSK-FOM                  
110000     ELSE                                                                 
110100        MOVE JA                       TO SW-SLACKTA-FALT-FEL              
110200     END-IF                                                               
110300                                                                          
110400     IF 2141-MID-IDANSK-TOM-UT NUMERIC                                    
110500        MOVE 2141-MID-IDANSK-TOM-UT   TO SPAR-IDANSK-TOM                  
110600     ELSE                                                                 
110700        MOVE JA                       TO SW-SLACKTA-FALT-FEL              
110800     END-IF                                                               
110900                                                                          
111000     IF SW-SLACKTA-FALT-FEL = JA                                          
111100        MOVE ZERO                     TO SPAR-IDANSK-FOM                  
111200                                         SPAR-IDANSK-TOM                  
111300        MOVE SPACE                    TO SPAR-IDPROJ-VALT                 
111400     ELSE                                                                 
111500        MOVE 2141-MID-IDPROJ-UT       TO SPAR-IDPROJ-VALT                 
111600     END-IF                                                               
111700                                                                          
111800     MOVE ZERO                        TO SPAR-IDANSK-MIN                  
111900                                         SPAR-DAFINLEV-MIN                
112000                                                                          
112100     MOVE SPACE                       TO SPAR-FLPISK-MIN                  
112200                                         SPAR-IDPROJ-MIN                  
112300                                         SPAR-IDAO-MIN.                   
112400     EJECT                                                                
112500 B-VISA-BILD SECTION.                                                     
112600     SKIP2                                                                
112700     PERFORM BA-LAS-ARTREG                                                
112800                                                                          
112900     IF SPAR-FLERS = JA                                                   
113000*       T I L L K O M M A N D E  A R T I K E L                            
113100*       H Ä M T A  D E N / D E  E R S A T T A                             
113200        PERFORM S06-LAS-ERSATTREG-ARTREG                                  
113300     ELSE                                                                 
113400        MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-ERS1                  
113500                                        MOD-KDERS-1                       
113600     END-IF                                                               
113700                                                                          
113800     PERFORM S07-LAS-BENREG                                               
113900     PERFORM BD-LAS-NYPONREG                                              
114000                                                                          
114100     IF MID-IDARTNR-IN = ALL '+'  AND                                     
114200        MFS-IDPFK      = SPACE    AND                                     
114300        EGEN-BILD                                                         
114400*       S I M U L E R I N G  H A R  S K E T T                             
114500        CONTINUE                                                          
114600     ELSE                                                                 
114700        PERFORM S03-RENSA-MOD-INMATNINGSFAELT                             
114800        PERFORM S04-RENSA-MOD-SIMULERINGSFAELT                            
114900     END-IF.                                                              
115000                                                                          
115100     EJECT                                                                
115200 BA-LAS-ARTREG SECTION.                                                   
115300     SKIP2                                                                
115400*WDK601                                                                   
115500*                                                                         
115600     MOVE ART-FLERS                  TO SPAR-FLERS                        
115700     MOVE ART-TIFINLV                TO MOD-TIFINLV                       
115800     MOVE ART-IDLEVNR                TO MOD-IDLEVNR                       
115900                                        SPAR-IDLEVNR                      
116000     MOVE ART-IDAO (1)               TO MOD-IDAO                          
116100     MOVE ART-IDFTG                  TO MOD-IDFTG                         
116200     MOVE ART-KDSORT                 TO MOD-KDSORT                        
116300     MOVE ART-KDPRODSL               TO SPAR-KDPRODSL-ARTC                
116400                                        WS-KDPRODSL-TEST2                 
116500                                                                          
116600     IF KDPRODSL-LYNK                                                     
116700        IF MSGI-IDLAND-SPR = 'GB'                                         
116800           MOVE MED-118    TO MOD-TEMFSINF                                
116900        ELSE                                                              
117000           MOVE MED-18     TO MOD-TEMFSINF                                
117100        END-IF                                                            
117200     END-IF                                                               
117300                                                                          
117400     IF ART-FLIART = 'J'                                                  
117500        IF MSGI-IDLAND-SPR = 'GB'                                         
117600           MOVE MED-117    TO MOD-TEMFSINF                                
117700        ELSE                                                              
117800           MOVE MED-17     TO MOD-TEMFSINF                                
117900        END-IF                                                            
118000     END-IF                                                               
118100                                                                          
118200*WDK611                                                                   
118300*                                                                         
118400     PERFORM IMS-GNP-ARTC11                                               
118500     MOVE CLAG-IDBERED               TO MOD-IDBERED                       
118600     MOVE CLAG-IDPROJ                TO MOD-IDPROJ                        
118700     MOVE CLAG-IDKAT (1)             TO MOD-IDKAT-1                       
118800     MOVE CLAG-IDKAT (2)             TO MOD-IDKAT-2                       
118900     MOVE CLAG-IDKAT (3)             TO MOD-IDKAT-3                       
119000                                                                          
119100     IF CLAG-IDPROENH(1) = SPACE OR ZERO                                  
119200        IF CLAG-IDPROENH(2) = SPACE OR ZERO                               
119300           IF CLAG-IDPROENH(3) = SPACE OR ZERO                            
119400             MOVE MFS-RENSA-FAELT    TO MOD-IDPROENH                      
119500           ELSE                                                           
119600             MOVE CLAG-IDPROENH(3)   TO MOD-IDPROENH                      
119700           END-IF                                                         
119800        ELSE                                                              
119900           MOVE CLAG-IDPROENH(2)     TO MOD-IDPROENH                      
120000        END-IF                                                            
120100     ELSE                                                                 
120200        MOVE CLAG-IDPROENH(1)        TO MOD-IDPROENH                      
120300     END-IF                                                               
120400                                                                          
120500     MOVE CLAG-IDANSK                TO MOD-IDANSK                        
120600     MOVE CLAG-IDPLANGR-AG           TO MOD-IDPLANGR-AG                   
120700     MOVE CLAG-IDPLANGR-LEV          TO MOD-IDPLANGR-LEV                  
120800     MOVE CLAG-KDHF                  TO MOD-KDHF                          
120900     MOVE CLAG-IDINK                 TO MOD-IDINK                         
121000                                        SPAR-IDINK                        
121100                                                                          
121200     MOVE CLAG-TIPBLOCK              TO MOD-TIPBLOCK                      
121300     MOVE CLAG-FLMPB                 TO MOD-FLMPB-C1                      
121400     MOVE CLAG-KVPB-SEP              TO MOD-KVPB-C1                       
121500     MOVE CLAG-RESLJUST              TO MOD-RESLJUST-C1                   
121600                                        SPAR-RESLJUST-C1                  
121700     MOVE CLAG-PRARTSTD              TO MOD-PRARTSTD                      
121800                                        SPAR-PRARTSTD-HDTAL               
121900                                                                          
121910     IF CLAG-KDTIPPR = ZERO                                               
121920        MOVE YES                     TO MOD-KDTIPPR                       
121930     ELSE                                                                 
121940       IF CLAG-KDTIPPR = 1                                                
121950        MOVE NEJ                     TO MOD-KDTIPPR                       
121960       ELSE                                                               
121970         IF CLAG-KDTIPPR = 3                                              
121980           MOVE WS-A                 TO MOD-KDTIPPR                       
121990         END-IF                                                           
121991       END-IF                                                             
121992     END-IF                                                               
121993                                                                          
122600     MOVE CLAG-IDLKTO                TO MOD-IDLKTO                        
122700                                                                          
122800     IF CLAG-KDUART = 'P'                                                 
122900        IF MSGI-IDLAND-SPR = 'GB'                                         
123000           MOVE MED-104    TO MOD-TEMFSINF                                
123100        ELSE                                                              
123200           MOVE MED-4      TO MOD-TEMFSINF                                
123300        END-IF                                                            
123400     ELSE                                                                 
123500        IF CLAG-KDUART = 'M'                                              
123600           IF MSGI-IDLAND-SPR = 'GB'                                      
123700              MOVE MED-107    TO MOD-TEMFSINF                             
123800           ELSE                                                           
123900              MOVE MED-7      TO MOD-TEMFSINF                             
124000           END-IF                                                         
124100        ELSE                                                              
124200           IF CLAG-KDUART = 'S'                                           
124300              IF MSGI-IDLAND-SPR = 'GB'                                   
124400                 MOVE MED-108    TO MOD-TEMFSINF                          
124500              ELSE                                                        
124600                 MOVE MED-8      TO MOD-TEMFSINF                          
124700              END-IF                                                      
124800           ELSE                                                           
124900              IF CLAG-KDUART = 'A'                                        
125000                 IF MSGI-IDLAND-SPR = 'GB'                                
125100                    MOVE MED-115    TO MOD-TEMFSINF                       
125200                 ELSE                                                     
125300                    MOVE MED-15     TO MOD-TEMFSINF                       
125400                 END-IF                                                   
125500              END-IF                                                      
125600           END-IF                                                         
125700        END-IF                                                            
125800     END-IF                                                               
125900                                                                          
126000     IF CLAG-KDERS > 10                                                   
126100        IF MSGI-IDLAND-SPR = 'GB'                                         
126200           MOVE MED-106    TO MOD-TEMFSINF                                
126300        ELSE                                                              
126400           MOVE MED-6      TO MOD-TEMFSINF                                
126500        END-IF                                                            
126600     END-IF                                                               
126700                                                                          
126800     IF CLAG-IDDC-REF NOT = SPACE                                         
126900        MOVE INF-REFILL-PART  TO MED-IDMFSFEL                             
127000        CALL WMEDKONV      USING MED-WMEDAREA                             
127100        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
127200     END-IF                                                               
127300*WDK625                                                                   
127400*                                                                         
127500     MOVE 1                          TO W-KDNOTTYP                        
127600     PERFORM IMS-GHNP-ARTC25                                              
127700     IF SEGMENT-FINNS                                                     
127800        MOVE NOT-TEARTNOT            TO MOD-IDANSK-NOT-IN                 
127900     ELSE                                                                 
128000        IF MID-IDARTNR-IN = ALL '+'  AND                                  
128100           MFS-IDPFK      = SPACE    AND                                  
128200           EGEN-BILD                                                      
128300* ---      S I M U L E R I N G  H A R  S K E T T                          
128400           CONTINUE                                                       
128500        ELSE                                                              
128600           MOVE MFS-RENSA-FAELT      TO MOD-IDANSK-NOT-IN                 
128700        END-IF                                                            
128800     END-IF                                                               
128900                                                                          
129000     MOVE 6                          TO W-KDNOTTYP                        
129100     PERFORM IMS-GHNP-ARTC25                                              
129200     IF SEGMENT-FINNS                                                     
129300        MOVE NOT-TEARTNOT            TO MOD-VARNOT                        
129400     ELSE                                                                 
129500        MOVE MFS-RENSA-FAELT         TO MOD-VARNOT                        
129600     END-IF                                                               
129700                                                                          
129800*WDK901                                                                   
129900*                                                                         
130000     PERFORM IMS-GU-ARTM01                                                
130100     IF SEGMENT-FINNS                                                     
130200        ADD ART-SUTPO-TOT            TO SPAR-SUTPO-TOT                    
130300        MOVE SPAR-SUTPO-TOT          TO MOD-KVBASL                        
130400     END-IF                                                               
130500                                                                          
130600     .                                                                    
130700     EJECT                                                                
130800 BD-LAS-NYPONREG SECTION.                                                 
130900     SKIP2                                                                
131000     PERFORM IMS-GHU-ARTG01-MED-GE                                        
131100     IF SEGMENT-FINNS                                                     
131200                                                                          
131300        MOVE JA                    TO SW-ARTIKEL-FINNS-PA-NYPON           
131400                                                                          
131500*---    TEST PÅ OM INFO FRÅN NYPON SKALL VISAS ISTÄLLET                   
131600*---    FÖR ARTREG                                                        
131700                                                                          
131800        IF SPAR-PRARTSTD-HDTAL = ZERO                                     
131900           MOVE NYPON-ART-IDINK      TO MOD-IDINK                         
132000           IF SPAR-IDLEVNR = SPACE OR '9996' OR '9997'                    
132100                             OR '9998'                                    
132200              MOVE NYPON-ART-IDLEVNR TO MOD-IDLEVNR                       
132300           END-IF                                                         
132400           MOVE NYPON-ART-IDANSK     TO MOD-IDANSK                        
132500           MOVE NYPON-ART-PRARTBES   TO MOD-PRARTSTD                      
132600           IF SPAR-IDLKTO        = ZERO  OR                               
132700              SPAR-RESLJUST-C1   = ZERO                                   
132800*---          RS-PROJ-BASEN HÄMTA HÄRIFRÅN OM VISNING GAV ZERO            
132900              MOVE SPAR-KDPRODSL-ARTC          TO W-KDPRODSL1             
133000              PERFORM IMS-GU-WLXXAQ01-UNIK                                
133100              IF SEGMENT-FINNS                                            
133200                 MOVE NYPON-ART-IDPROJK        TO W-IDPROJK               
133300                 MOVE NYPON-ART-IDPROJOBJ      TO W-IDPROJOBJ             
133400                 MOVE NYPON-ART-IDPROJ         TO W-IDPROJ                
133500                 PERFORM IMS-GNP-WLXXAQ11-FIRST                           
133600                 IF SEGMENT-FINNS                                         
133700                    IF SPAR-IDLKTO = ZERO                                 
133800                       MOVE XXAQ-1132-IDLKTO   TO SPAR-IDLKTO             
133900                                                  MOD-IDLKTO              
134000                    END-IF                                                
134100                                                                          
134200                    IF SPAR-RESLJUST-C1   = ZERO                          
134300                       MOVE XXAQ-1132-RESLJUST-C1 TO                      
134400                                              MOD-RESLJUST-C1             
134500                    END-IF                                                
134600                 END-IF                                                   
134700              END-IF                                                      
134800           END-IF                                                         
134900        END-IF                                                            
135000                                                                          
135100        MOVE NYPON-ART-IDANSK-REG    TO MOD-IDANSK-REG                    
135200        MOVE NYPON-ART-TIANSKREG     TO MOD-TIANSKREG                     
135300        MOVE NYPON-ART-TEORSAK       TO MOD-TEORSAK                       
135400        MOVE NYPON-ART-IDPROJK       TO MOD-IDPROJK                       
135500        MOVE NYPON-ART-KVARTAR1      TO MOD-KVARTAR1                      
135600        MOVE NYPON-ART-KVARTAR2      TO MOD-KVARTAR2                      
135700        MOVE NYPON-ART-KVARTAR3      TO MOD-KVARTAR3                      
135800        MOVE NYPON-ART-FLBYTES       TO MOD-FLBYTES                       
135900        MOVE NYPON-ART-KVARTVAGN     TO MOD-KVARTVAGN                     
136000        MOVE NYPON-ART-IDMATKTO      TO MOD-IDMATKTO                      
136100        MOVE NYPON-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV                 
136200        MOVE NYPON-ART-IDAVD         TO MOD-IDAVD                         
136300        MOVE NYPON-ART-KVPROG        TO MOD-KVPROG                        
136400                                                                          
136500        IF NYPON-ART-KVBASL > ZERO                                        
136600           MOVE MFS-ADD-LYS-UPP-FAELT                                     
136700                                     TO MOD-KVBASL-ATTR                   
136800        END-IF                                                            
136900                                                                          
137000        MOVE NYPON-ART-KDSTAINK      TO MOD-KDSTAINK                      
137100                                                                          
137200        IF NYPON-ART-TISTOMREG = ZERO                                     
137300           MOVE MFS-RENSA-FAELT      TO MOD-KVBASL-KLAR                   
137400        ELSE                                                              
137500           MOVE NYPON-ART-TISTOMREG   TO TMP1-YYMMDD                      
137600           MOVE SPAR-DAGENS-DATUM     TO TMP2-YYMMDD                      
137700           PERFORM WY2000P1                                               
137800           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
137900              MOVE '*'               TO MOD-KVBASL-KLAR                   
138000           ELSE                                                           
138100              MOVE MFS-RENSA-FAELT   TO MOD-KVBASL-KLAR                   
138200           END-IF                                                         
138300        END-IF                                                            
138400                                                                          
138500        IF NYPON-ART-FLPISK = JA                                          
138600           IF MSGI-IDLAND-SPR = 'GB'                                      
138700              MOVE MED-109    TO MOD-TEMFSINF                             
138800           ELSE                                                           
138900              MOVE MED-9      TO MOD-TEMFSINF                             
139000           END-IF                                                         
139100        END-IF                                                            
139200                                                                          
139300     ELSE                                                                 
139400        MOVE MFS-RENSA-FAELT         TO MOD-IDANSK-REG                    
139500                                        MOD-TIANSKREG                     
139600                                        MOD-TEORSAK                       
139700                                        MOD-IDPROJK                       
139800                                        MOD-KVARTAR1                      
139900                                        MOD-KVARTAR2                      
140000                                        MOD-KVARTAR3                      
140100                                        MOD-KVARTVAGN                     
140200                                        MOD-IDMATKTO                      
140300                                        MOD-IDAVD                         
140400                                        MOD-KVBASL                        
140500                                        MOD-KDSTAINK                      
140600                                        MOD-IDARTNR-MOTSV                 
140700                                        MOD-KVPROG                        
140800     END-IF.                                                              
140900     EJECT                                                                
141000 C-KOLLA-INPUT-SIMULERING SECTION.                                        
141100     SKIP2                                                                
141200     MOVE ART-KDPRODSL               TO SPAR-KDPRODSL-ARTC                
141300     MOVE JA                         TO SW-INPUT-RAETT                    
141400                                                                          
141500*WDK611                                                                   
141600*                                                                         
141700     PERFORM IMS-GHNP-ARTC11                                              
141800     IF CLAG-PRARTSTD > ZERO                                              
141900        MOVE NEJ                     TO SW-INPUT-RAETT                    
142000        IF MSGI-IDLAND-SPR = 'GB'                                         
142100           MOVE FEL-111            TO MOD-TEMFSINF                        
142200        ELSE                                                              
142300           MOVE FEL-11             TO MOD-TEMFSINF                        
142400        END-IF                                                            
142500*---    SIMULERING FÅR EJ SKE OM PRIS > 0                                 
142600     ELSE                                                                 
142700        IF MID-PRARTSTD = ALL '+' OR MID-RESLJUST-C1 = ALL '+'            
142800           PERFORM IMS-GHU-ARTG01-MED-GE                                  
142900           IF SEGMENT-FINNS                                               
143000              MOVE JA                   TO                                
143100                                        SW-ARTIKEL-FINNS-PA-NYPON         
143200           END-IF                                                         
143300        END-IF                                                            
143400                                                                          
143500        IF MID-PRARTSTD = ALL '+'                                         
143600           MOVE MFS-RENSA-FAELT         TO MOD-PRARTSTD-IN                
143700           IF SW-ARTIKEL-FINNS-PA-NYPON = JA                              
143800              IF NYPON-ART-PRARTBES > ZERO                                
143900                 MOVE MFS-NUM-FAELT-RAETT TO                              
144000                                          MOD-PRARTSTD-IN-ATTR            
144100              ELSE                                                        
144200                 MOVE NEJ               TO SW-INPUT-RAETT                 
144300                 MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTSTD-IN-ATTR           
144400**** ---         OM ÄVEN NYPONPRISET ÄR 0,-MÅSTE MID VARA IFYLLD!         
144500              END-IF                                                      
144600           ELSE                                                           
144700              MOVE NEJ               TO SW-INPUT-RAETT                    
144800              MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTSTD-IN-ATTR              
144900           END-IF                                                         
145000        ELSE                                                              
145100           MOVE MID-PRARTSTD         TO SPAR-PRARTSTD                     
145200           IF SPAR-PRARTSTD-HELTAL   NUMERIC  AND                         
145300              SPAR-PRARTSTD-DECIMAL  NUMERIC  AND                         
145400              SPAR-PRARTSTD-PUNKT  = '.'                                  
145500              IF SPAR-PRARTSTD-HELTAL  > ZERO  OR                         
145600                 SPAR-PRARTSTD-DECIMAL > ZERO                             
145700                 MOVE MFS-NUM-FAELT-RAETT                                 
145800                                     TO MOD-PRARTSTD-IN-ATTR              
145900                 MOVE SPAR-PRARTSTD-HELTAL                                
146000                                     TO SPAR-PRARTSTD-R-HELTAL            
146100                 MOVE SPAR-PRARTSTD-DECIMAL                               
146200                                     TO SPAR-PRARTSTD-R-DECIMAL           
146300              ELSE                                                        
146400                 MOVE MFS-NUM-FAELT-FEL                                   
146500                                     TO MOD-PRARTSTD-IN-ATTR              
146600                 MOVE NEJ            TO SW-INPUT-RAETT                    
146700              END-IF                                                      
146800           ELSE                                                           
146900              MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTSTD-IN-ATTR              
147000              MOVE NEJ               TO SW-INPUT-RAETT                    
147100           END-IF                                                         
147200           MOVE MFS-ROER-EJ-FAELT    TO MOD-PRARTSTD-IN                   
147300        END-IF                                                            
147400                                                                          
147500        IF MID-KVPB-C1 = ALL '+'                                          
147600           MOVE MFS-RENSA-FAELT         TO MOD-KVPB-C1-IN                 
147700           MOVE MFS-NUM-FAELT-FEL       TO MOD-KVPB-C1-IN-ATTR            
147800           MOVE NEJ                     TO SW-INPUT-RAETT                 
147900        ELSE                                                              
148000           MOVE MID-KVPB-C1          TO SPAR-KVPB-C1                      
148100           IF SPAR-KVPB-C1-HELTAL    NUMERIC  AND                         
148200              SPAR-KVPB-C1-DECIMAL   NUMERIC  AND                         
148300              SPAR-KVPB-C1-PUNKT      = '.'                               
148400                 MOVE MFS-NUM-FAELT-RAETT                                 
148500                                     TO MOD-KVPB-C1-IN-ATTR               
148600                 MOVE SPAR-KVPB-C1-HELTAL                                 
148700                                     TO SPAR-KVPB-C1-R-HELTAL             
148800                 MOVE SPAR-KVPB-C1-DECIMAL                                
148900                                     TO SPAR-KVPB-C1-R-DECIMAL            
149000           ELSE                                                           
149100              MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-C1-IN-ATTR               
149200              MOVE NEJ               TO SW-INPUT-RAETT                    
149300           END-IF                                                         
149400           MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-C1-IN                    
149500        END-IF                                                            
149600                                                                          
149700        IF MID-RESLJUST-C1 = ALL '+'                                      
149800           MOVE MFS-RENSA-FAELT                   TO                      
149900                                         MOD-RESLJUST-C1-IN               
150000           IF SW-ARTIKEL-FINNS-PA-NYPON = JA                              
150100              MOVE SPAR-KDPRODSL-ARTC             TO W-KDPRODSL1          
150200              PERFORM IMS-GU-WLXXAQ01-UNIK                                
150300              IF SEGMENT-FINNS                                            
150400                 MOVE NYPON-ART-IDPROJK           TO W-IDPROJK            
150500                 MOVE NYPON-ART-IDPROJOBJ         TO W-IDPROJOBJ          
150600                 MOVE NYPON-ART-IDPROJ            TO W-IDPROJ             
150700                 PERFORM IMS-GNP-WLXXAQ11-FIRST                           
150800                 IF SEGMENT-FINNS                                         
150900                    IF XXAQ-1132-IDLKTO > ZERO                            
151000                       IF XXAQ-1132-RESLJUST-C1 > ZERO                    
151100                          MOVE XXAQ-1132-RESLJUST-C1 TO                   
151200                                                 SPAR-RESLJUST-C1         
151300                       ELSE                                               
151400                          MOVE SPAR-ETTPUNKTNOLL TO                       
151500                                                 SPAR-RESLJUST-C1         
151600                       END-IF                                             
151700                    ELSE                                                  
151800                       MOVE SPAR-ETTPUNKTNOLL     TO                      
151900                                                 SPAR-RESLJUST-C1         
152000                    END-IF                                                
152100                 ELSE                                                     
152200                    MOVE SPAR-ETTPUNKTNOLL       TO                       
152300                                                 SPAR-RESLJUST-C1         
152400                 END-IF                                                   
152500              ELSE                                                        
152600                 MOVE SPAR-ETTPUNKTNOLL          TO                       
152700                                                 SPAR-RESLJUST-C1         
152800              END-IF                                                      
152900           ELSE                                                           
153000              MOVE SPAR-ETTPUNKTNOLL             TO                       
153100                                                 SPAR-RESLJUST-C1         
153200           END-IF                                                         
153300        ELSE                                                              
153400           MOVE MID-RESLJUST-C1            TO DEC-IDFRIDATA               
153500           MOVE 1                          TO DEC-KVHELTAL                
153600                                              DEC-KVDECIMAL               
153700           PERFORM S98-WDECEDIT                                           
153800           IF DEC-KDSVAR-OK                                               
153900              IF DEC-IDEDITDATA  > 0 AND < 9.8                            
154000                 MOVE MFS-NUM-FAELT-RAETT  TO                             
154100                                           MOD-RESLJUST-C1-IN-ATTR        
154200                 MOVE DEC-IDEDITDATA       TO SPAR-RESLJUST-C1            
154300              ELSE                                                        
154400                 MOVE MFS-NUM-FAELT-FEL    TO                             
154500                                           MOD-RESLJUST-C1-IN-ATTR        
154600                 MOVE NEJ                  TO SW-INPUT-RAETT              
154700              END-IF                                                      
154800           ELSE                                                           
154900              MOVE MFS-NUM-FAELT-FEL       TO                             
155000                                           MOD-RESLJUST-C1-IN-ATTR        
155100              MOVE NEJ                     TO SW-INPUT-RAETT              
155200           END-IF                                                         
155300           MOVE MFS-ROER-EJ-FAELT          TO                             
155400                                           MOD-RESLJUST-C1-IN             
155500        END-IF                                                            
155600     END-IF                                                               
155700                                                                          
155800     PERFORM CA-KOLLA-OM-INMATAT-OVRIGA.                                  
155900                                                                          
156000     EJECT                                                                
156100 CA-KOLLA-OM-INMATAT-OVRIGA SECTION.                                      
156200     SKIP2                                                                
156300     IF MID-PRISDEL  = ALL '+'  AND                                       
156400        MID-KOPDEL   = ALL '+'                                            
156500        CONTINUE                                                          
156600     ELSE                                                                 
156700        IF MID-IDLEVNR        = ALL '+'                                   
156800           MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-IN                       
156900        ELSE                                                              
157000           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                       
157100           MOVE MFS-ALFA-FAELT-RAETT                                      
157200                                  TO MOD-IDLEVNR-IN-ATTR                  
157300        END-IF                                                            
157400                                                                          
157500        IF MID-IDPLANGR-AG    = ALL '+'                                   
157600           MOVE MFS-RENSA-FAELT   TO MOD-IDPLANGR-AG-IN                   
157700        ELSE                                                              
157800           MOVE MFS-ROER-EJ-FAELT TO MOD-IDPLANGR-AG-IN                   
157900           MOVE MFS-NUM-FAELT-RAETT                                       
158000                               TO MOD-IDPLANGR-AG-IN-ATTR                 
158100        END-IF                                                            
158200                                                                          
158300        IF MID-IDPLANGR-LEV   = ALL '+'                                   
158400           MOVE MFS-RENSA-FAELT   TO MOD-IDPLANGR-LEV-IN                  
158500        ELSE                                                              
158600           MOVE MFS-ROER-EJ-FAELT TO MOD-IDPLANGR-LEV-IN                  
158700           MOVE MFS-NUM-FAELT-RAETT                                       
158800                               TO MOD-IDPLANGR-LEV-IN-ATTR                
158900        END-IF                                                            
159000                                                                          
159100        IF MID-IDANSK         = ALL '+'                                   
159200           MOVE MFS-RENSA-FAELT   TO MOD-IDANSK-IN                        
159300        ELSE                                                              
159400           MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-IN                        
159500           MOVE MFS-NUM-FAELT-RAETT                                       
159600                                  TO MOD-IDANSK-IN-ATTR                   
159700        END-IF                                                            
159800                                                                          
159900        IF MID-KDTIPPR        = ALL '+'                                   
160000           MOVE MFS-RENSA-FAELT   TO MOD-KDTIPPR-IN                       
160100        ELSE                                                              
160200           MOVE MFS-ROER-EJ-FAELT TO MOD-KDTIPPR-IN                       
160300           MOVE MFS-ALFA-FAELT-RAETT                                      
160400                                  TO MOD-KDTIPPR-IN-ATTR                  
160500        END-IF                                                            
160600                                                                          
160700        IF MID-IDLKTO-POS3-7  = ALL '+'                                   
160800           MOVE MFS-RENSA-FAELT   TO MOD-IDLKTO-IN                        
160900        ELSE                                                              
161000           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLKTO-IN                        
161100           MOVE MFS-NUM-FAELT-RAETT                                       
161200                                  TO MOD-IDLKTO-IN-ATTR                   
161300        END-IF                                                            
161400                                                                          
161500        IF MID-TIPBLOCK       = ALL '+'                                   
161600           MOVE MFS-RENSA-FAELT   TO MOD-TIPBLOCK-IN                      
161700        ELSE                                                              
161800           MOVE MFS-ROER-EJ-FAELT TO MOD-TIPBLOCK-IN                      
161900           MOVE MFS-ALFA-FAELT-RAETT                                      
162000                                  TO MOD-TIPBLOCK-IN-ATTR                 
162100        END-IF                                                            
162200                                                                          
162300        IF MID-FLMPB-C1       = ALL '+'                                   
162400           MOVE MFS-RENSA-FAELT   TO MOD-FLMPB-C1-IN                      
162500        ELSE                                                              
162600           MOVE MFS-ROER-EJ-FAELT TO MOD-FLMPB-C1-IN                      
162700           MOVE MFS-ALFA-FAELT-RAETT                                      
162800                                  TO MOD-FLMPB-C1-IN-ATTR                 
162900        END-IF                                                            
163000                                                                          
163100        IF MID-KDHF           = ALL '+'                                   
163200           MOVE MFS-RENSA-FAELT   TO MOD-KDHF-IN                          
163300        ELSE                                                              
163400           MOVE MFS-ROER-EJ-FAELT TO MOD-KDHF-IN                          
163500           MOVE MFS-NUM-FAELT-RAETT                                       
163600                                  TO MOD-KDHF-IN-ATTR                     
163700        END-IF                                                            
163800                                                                          
163900        IF MID-IDANSK-NOT     = ALL '+'                                   
164000           MOVE MFS-RENSA-FAELT   TO MOD-IDANSK-NOT-IN                    
164100        ELSE                                                              
164200           MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-NOT-IN                    
164300           MOVE MFS-ALFA-FAELT-RAETT                                      
164400                                  TO                                      
164500                                  MOD-IDANSK-NOT-IN-ATTR                  
164600        END-IF                                                            
164700                                                                          
164800        IF MID-IDINK          = ALL '+'                                   
164900           MOVE MFS-RENSA-FAELT   TO MOD-IDINK-IN                         
165000        ELSE                                                              
165100           MOVE MFS-ROER-EJ-FAELT TO MOD-IDINK-IN                         
165200           MOVE MFS-ALFA-FAELT-RAETT                                      
165300                                  TO MOD-IDINK-IN-ATTR                    
165400        END-IF                                                            
165500                                                                          
165600        IF MID-KDKOPTYP       = ALL '+'                                   
165700           MOVE MFS-RENSA-FAELT   TO MOD-KDKOPTYP-IN                      
165800        ELSE                                                              
165900           MOVE MFS-ROER-EJ-FAELT TO MOD-KDKOPTYP-IN                      
166000           MOVE MFS-ALFA-FAELT-RAETT                                      
166100                                  TO MOD-KDKOPTYP-IN-ATTR                 
166200        END-IF                                                            
166300                                                                          
166400        IF MID-TILEVBEG       = ALL '+'                                   
166500           MOVE MFS-RENSA-FAELT   TO MOD-TILEVBEG-IN                      
166600        ELSE                                                              
166700           MOVE MFS-ROER-EJ-FAELT TO MOD-TILEVBEG-IN                      
166800           MOVE MFS-NUM-FAELT-RAETT                                       
166900                                  TO MOD-TILEVBEG-IN-ATTR                 
167000        END-IF                                                            
167100                                                                          
167200        IF MID-KVLEVBEG       = ALL '+'                                   
167300           MOVE MFS-RENSA-FAELT   TO MOD-KVLEVBEG-IN                      
167400        ELSE                                                              
167500           MOVE MFS-ROER-EJ-FAELT TO MOD-KVLEVBEG-IN                      
167600           MOVE MFS-NUM-FAELT-RAETT                                       
167700                                  TO MOD-KVLEVBEG-IN-ATTR                 
167800        END-IF                                                            
167900                                                                          
168000        IF MID-KVPROG         = ALL '+'                                   
168100           MOVE MFS-RENSA-FAELT   TO MOD-KVPROG-IN                        
168200        ELSE                                                              
168300           MOVE MFS-ROER-EJ-FAELT TO MOD-KVPROG-IN                        
168400           MOVE MFS-NUM-FAELT-RAETT                                       
168500                                  TO MOD-KVPROG-IN-ATTR                   
168600        END-IF                                                            
168700                                                                          
168800        IF MID-TEANSINK       = ALL '+'                                   
168900           MOVE MFS-RENSA-FAELT   TO MOD-TEANSINK-IN                      
169000        ELSE                                                              
169100           MOVE MFS-ROER-EJ-FAELT TO MOD-TEANSINK-IN                      
169200           MOVE MFS-ALFA-FAELT-RAETT                                      
169300                                  TO MOD-TEANSINK-IN-ATTR                 
169400        END-IF                                                            
169500                                                                          
169600           MOVE MFS-RENSA-FAELT      TO MOD-FLNYRAPP-IN                   
169700     END-IF.                                                              
169800     EJECT                                                                
169900 D-SIMULERA SECTION.                                                      
170000     SKIP2                                                                
170100     PERFORM DA-ARSOMSATTNING-VVKL                                        
170200     PERFORM DB-KFAKT-GTID-KVMAD-KVSLAGER                                 
170300     PERFORM DC-KVQ-KVMP                                                  
170400                                                                          
170500     MOVE MFS-ADD-SAETT-CURSOR       TO MOD-KDKOPTYP-IN-ATTR.             
170600     EJECT                                                                
170700 DA-ARSOMSATTNING-VVKL SECTION.                                           
170800     SKIP2                                                                
170900*****************************************                                 
171000*1. BERÄKNA PROGNOSTISERAD ÅRSOMSÄTTNING*                                 
171100*****************************************                                 
171200                                                                          
171300     MOVE SPAR-KVPB-C1-HDTAL         TO SPAR-KVPB-TOTALT                  
171400                                                                          
171500     IF MID-PRARTSTD = ALL '+'                                            
171600        MOVE NYPON-ART-PRARTBES      TO SPAR-PRARTSTD-HDTAL               
171700     END-IF                                                               
171800                                                                          
171900     COMPUTE SPAR-ARSOMS = SPAR-KVPB-TOTALT * 12 *                        
172000                           SPAR-PRARTSTD-HDTAL                            
172100     EVALUATE TRUE                                                        
172200        WHEN SPAR-ARSOMS > ZERO   AND < 500                               
172300           MOVE 1                    TO SPAR-VVKL                         
172400        WHEN SPAR-ARSOMS > 499    AND < 5001                              
172500           MOVE 2                    TO SPAR-VVKL                         
172600        WHEN SPAR-ARSOMS > 5000   AND < 50001                             
172700           MOVE 3                    TO SPAR-VVKL                         
172800        WHEN SPAR-ARSOMS > 50000  AND < 500001                            
172900           MOVE 4                    TO SPAR-VVKL                         
173000        WHEN SPAR-ARSOMS > 500000                                         
173100           MOVE 5                    TO SPAR-VVKL                         
173200     END-EVALUATE.                                                        
173300     EJECT                                                                
173400 DB-KFAKT-GTID-KVMAD-KVSLAGER SECTION.                                    
173500     SKIP2                                                                
173600****************************************************************          
173700*2. BERÄKNA UNGEFÄRLIG K-FAKTOR (SERVICEGRAD) OCH GARDERINGSTID*          
173800****************************************************************          
173900                                                                          
174000     EVALUATE TRUE                                                        
174100        WHEN SPAR-VVKL = 1                                                
174200           MOVE 0.3                  TO SPAR-KFAKTOR                      
174300           MOVE 20                   TO SPAR-GTID                         
174400        WHEN SPAR-VVKL = 2                                                
174500           MOVE 0.5                  TO SPAR-KFAKTOR                      
174600           MOVE 20                   TO SPAR-GTID                         
174700        WHEN SPAR-VVKL = 3                                                
174800           MOVE 1                    TO SPAR-KFAKTOR                      
174900           MOVE 14                   TO SPAR-GTID                         
175000        WHEN SPAR-VVKL = 4                                                
175100           MOVE 1.4                  TO SPAR-KFAKTOR                      
175200           MOVE 11                   TO SPAR-GTID                         
175300        WHEN SPAR-VVKL = 5                                                
175400           MOVE 1.5                  TO SPAR-KFAKTOR                      
175500           MOVE 10                   TO SPAR-GTID                         
175600     END-EVALUATE                                                         
175700                                                                          
175800                                                                          
175900******************************************************************        
176000*3. BERÄKNA PRELIMINÄR KVMAD (MEDELAVIKELSE VERKL. FÖRBR OCH PROG)        
176100******************************************************************        
176200     COMPUTE SPAR-KVMAD = SPAR-KVPB-TOTALT ** 0.85                        
176300                                                                          
176400                                                                          
176500***************************************                                   
176600*4. BERÄKNA PRELIMINÄRT SÄKERHETSLAGER*                                   
176700***************************************                                   
176800     COMPUTE SPAR-KVSLAGER = SPAR-KFAKTOR * SPAR-RESLJUST-C1 *            
176900                             SPAR-KVMAD   *  0.39            *            
177000                             SPAR-GTID    ** 0.65                         
177100                                                                          
177200     IF SPAR-KVSLAGER = ZERO                                              
177300        MOVE 1                          TO MOD-KVSLAGER                   
177400     ELSE                                                                 
177500        MOVE SPAR-KVSLAGER              TO MOD-KVSLAGER                   
177600     END-IF.                                                              
177700                                                                          
177800                                                                          
177900     EJECT                                                                
178000 DC-KVQ-KVMP SECTION.                                                     
178100     SKIP2                                                                
178200*****************************************                                 
178300*BERÄKNA UNGEFÄRLIG HEMTAGNINGSKVANT (Q)*                                 
178400*****************************************                                 
178500     EVALUATE TRUE                                                        
178600        WHEN SPAR-VVKL = 1                                                
178700           COMPUTE SPAR-KVQ = 96  * SPAR-KVPB-TOTALT / 4.33               
178800        WHEN SPAR-VVKL = 2                                                
178900           COMPUTE SPAR-KVQ = 36  * SPAR-KVPB-TOTALT / 4.33               
179000        WHEN SPAR-VVKL = 3                                                
179100           COMPUTE SPAR-KVQ =  9  * SPAR-KVPB-TOTALT / 4.33               
179200        WHEN SPAR-VVKL = 4                                                
179300           COMPUTE SPAR-KVQ =  3  * SPAR-KVPB-TOTALT / 4.33               
179400        WHEN SPAR-VVKL = 5                                                
179500           COMPUTE SPAR-KVQ = 1.8 * SPAR-KVPB-TOTALT / 4.33               
179600     END-EVALUATE                                                         
179700                                                                          
179800                                                                          
179900*******************************************************                   
180000*BERÄKNA PRELIMINÄR MAXPUNKT (MAXIMALT TILLÅTET LAGER)*                   
180100*******************************************************                   
180200     EVALUATE TRUE                                                        
180300        WHEN SPAR-VVKL = 1                                                
180400           COMPUTE SPAR-KVMP = SPAR-KVSLAGER + SPAR-KVQ +                 
180500                               96 * SPAR-KVPB-TOTALT / 4.33               
180600        WHEN SPAR-VVKL = 2                                                
180700           COMPUTE SPAR-KVMP = SPAR-KVSLAGER + SPAR-KVQ +                 
180800                               36 * SPAR-KVPB-TOTALT / 4.33               
180900        WHEN SPAR-VVKL = 3                                                
181000           COMPUTE SPAR-KVMP = SPAR-KVSLAGER + SPAR-KVQ +                 
181100                               18 * SPAR-KVPB-TOTALT / 4.33               
181200        WHEN SPAR-VVKL = 4                                                
181300           COMPUTE SPAR-KVMP = SPAR-KVSLAGER + SPAR-KVQ +                 
181400                               12 * SPAR-KVPB-TOTALT / 4.33               
181500        WHEN SPAR-VVKL = 5                                                
181600           COMPUTE SPAR-KVMP = SPAR-KVSLAGER + SPAR-KVQ +                 
181700                                9 * SPAR-KVPB-TOTALT / 4.33               
181800     END-EVALUATE                                                         
181900                                                                          
182000                                                                          
182100     MOVE SPAR-KVMP                  TO MOD-KVMP.                         
182200     EJECT                                                                
182300 E-KOLLA-INPUT SECTION.                                                   
182400     SKIP2                                                                
182500     MOVE JA                         TO SW-INPUT-RAETT                    
182600                                                                          
182700     MOVE ART-IDLEVNR                TO SPAR-IDLEVNR                      
182800     MOVE ART-TIFINLV                TO SPAR-TIFINLV-AAVVD                
182900     MOVE ART-IDFTG                  TO SPAR-IDFTG                        
183000     MOVE ART-IDFKNGRP               TO SPAR-IDFKNGRP                     
183100     MOVE ART-KDSORT                 TO SPAR-KDSORT                       
183200     MOVE ART-KDPRODSL               TO SPAR-KDPRODSL-ARTC                
183300                                        WS-KDPRODSL-TEST1                 
183400                                        WS-KDPRODSL-TEST2                 
183500                                                                          
183600*- ÄNDRING  PGA OMSTÄLLNING KTO*  980826**                                
183700                                        SPAR-KDPRODSL-KPS                 
183800******************************************                                
183900                                                                          
184000     PERFORM IMS-GHNP-ARTC11                                              
184100     MOVE CLAG-KDHF                  TO SPAR-KDHF                         
184200     MOVE CLAG-IDINK                 TO SPAR-IDINK  WS-IDINK              
184300     MOVE CLAG-KDUART                TO SPAR-KDUART                       
184400     IF CLAG-KDAVT = 1 OR                                                 
184500        CLAG-KDAVT = 3 OR                                                 
184600        CLAG-KDAVT = 4                                                    
184700        MOVE JA TO SW-AVTAL-FINNS                                         
184800     END-IF                                                               
184900     MOVE CLAG-PRARTSTD              TO SPAR-PRARTSTD-ARTC                
185000     MOVE CLAG-IDPROJ                TO WS-IDPROJ                         
185100     MOVE CLAG-KDEMBKOD-2            TO SPAR-KDEMBKOD                     
185200     MOVE CLAG-BEFT                  TO SPAR-BEFT                         
185300     MOVE CLAG-KDERS                 TO SPAR-KDERS                        
185400                                                                          
185500     PERFORM EG-KOLL-BEST-AVT                                             
185600                                                                          
185700*--  N Y P O N                                                            
185800     PERFORM IMS-GHU-ARTG01-MED-GE                                        
185900     IF SEGMENT-FINNS                                                     
186000        MOVE JA                      TO SW-ARTIKEL-FINNS-PA-NYPON         
186100        MOVE NYPON-ART-KDRESBED      TO SPAR-KDRESBED                     
186200        MOVE NYPON-ART-KVPROG        TO SPAR-KVPROG                       
186300     ELSE                                                                 
186400        MOVE ZERO                    TO SPAR-KVPROG                       
186500     END-IF                                                               
186600                                                                          
186700     EJECT                                                                
186800     IF MID-PRISDEL = ALL '+'                                             
186900*---    ALLA INFÄLT I PRISDEL TOMMA                                       
187000        CONTINUE                                                          
187100     ELSE                                                                 
187200        IF SPAR-PRARTSTD-ARTC  > ZERO OR                                  
187300           SW-ARTIKEL-FINNS-PA-NYPON = NEJ                                
187400           MOVE NEJ               TO SW-INPUT-RAETT                       
187500           MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-IN                       
187600                                     MOD-IDPLANGR-AG-IN                   
187700                                     MOD-IDPLANGR-LEV-IN                  
187800                                     MOD-IDANSK-IN                        
187900                                     MOD-PRARTSTD-IN                      
188000                                     MOD-KDTIPPR-IN                       
188100                                     MOD-IDFTG-IN                         
188200                                     MOD-IDLKTO-IN                        
188300                                     MOD-KVPB-C1-IN                       
188400                                     MOD-TIPBLOCK-IN                      
188500                                     MOD-FLMPB-C1-IN                      
188600                                     MOD-KDHF-IN                          
188700                                     MOD-RESLJUST-C1-IN                   
188800           IF MSGI-IDLAND-SPR = 'GB'                                      
188900              MOVE FEL-107            TO MOD-TEMFSINF                     
189000           ELSE                                                           
189100              MOVE FEL-7              TO MOD-TEMFSINF                     
189200           END-IF                                                         
189300        ELSE                                                              
189400           PERFORM EA-INDATAKOLL1                                         
189500           MOVE SPAR-KDPRODSL-KPS TO WS-KDPRODSL-TEST1                    
189600                                                                          
189700        END-IF                                                            
189800     END-IF                                                               
189900     EJECT                                                                
190000     IF SW-INPUT-RAETT = JA                                               
190100      IF MID-KOPDEL = ALL '+'                                             
190200        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
190300           MOVE SPAR-KDPRODSL-KPS   TO WS-KDPRODSL-TEST4                  
190400           IF KDPRODSL-KOP-MASTE-UTFORAS                                  
190500              MOVE SPAR-IDLEVNR        TO WS-IDLEVNR-KONCERN              
190600              IF KONCERN-LEV OR SPAR-KDHF > ZERO                          
190700                 CONTINUE                                                 
190800              ELSE                                                        
190900                 IF SPAR-KDUART = SPACE                                   
191000                    IF SW-BESTAVT = NEJ                                   
191100                     IF SPAR-KVPROG > ZERO                                
191200                       IF SPAR-KDSORT = 'SW'                              
191300                          CONTINUE                                        
191400                       ELSE                                               
191500                          MOVE NEJ           TO SW-INPUT-RAETT            
191600                          IF MSGI-IDLAND-SPR = 'GB'                       
191700                             MOVE FEL-116        TO MOD-TEMFSINF          
191800                          ELSE                                            
191900                             MOVE FEL-16         TO MOD-TEMFSINF          
192000                          END-IF                                          
192100*----                     K Ö P  M Å S T E  L Ä G G A S ......            
192200                          MOVE MFS-ADD-SAETT-CURSOR                       
192300                                   TO MOD-KDKOPTYP-IN-ATTR                
192400                       END-IF                                             
192500                     END-IF                                               
192600                    END-IF                                                
192700                 END-IF                                                   
192800              END-IF                                                      
192900           END-IF                                                         
193000        END-IF                                                            
193100      ELSE                                                                
193200        IF (SPAR-KDPRODSL-ARTC = 18 OR 19 OR 25 OR 29)  AND               
193300            SPAR-KDSORT = 'SW'                                            
193400                                                                          
193500           MOVE NEJ TO SW-INPUT-RAETT                                     
193600           PERFORM EB-RENSA-KOPFAELT                                      
193700           IF MSGI-IDLAND-SPR = 'GB'                                      
193800              MOVE FEL-112 TO MOD-TEMFSINF                                
193900           ELSE                                                           
194000              MOVE FEL-12  TO MOD-TEMFSINF                                
194100           END-IF                                                         
194200        ELSE                                                              
194300           IF MID-KDKOPTYP NOT = 'R'                                      
194400              PERFORM EN-KDKOPTYP-N                                       
194500           ELSE                                                           
194600              PERFORM ER-KDKOPTYP-R                                       
194700           END-IF                                                         
194800        END-IF                                                            
194900      END-IF                                                              
195000     END-IF                                                               
195100                                                                          
195200     IF SW-INPUT-RAETT = JA                                               
195300***--   ÄNDRINGAR OCH ANNULLATIONER EJ TILLÅTNA                           
195400***--   OM TRANS (1142) TILL NEDCAR REDAN SKICKATS                        
195500        IF SPAR-IDINK (1:3) NUMERIC                                       
195600           MOVE SPAR-IDINK (1:3)    TO TEST-IDINK                         
195700        ELSE                                                              
195800           IF SPAR-IDINK (2:3) NUMERIC                                    
195900              MOVE SPAR-IDINK (2:3) TO TEST-IDINK                         
196000           ELSE                                                           
196100              MOVE ZERO             TO TEST-IDINK                         
196200           END-IF                                                         
196300        END-IF                                                            
196400     END-IF                                                               
196500                                                                          
196600     IF SW-INPUT-RAETT = NEJ                                              
196700        PERFORM S02-ROER-EJ-FAELT-OVRIGA                                  
196800     END-IF                                                               
196900     .                                                                    
197000     EJECT                                                                
197100 EA-INDATAKOLL1 SECTION.                                                  
197200     SKIP2                                                                
197300                                                                          
197400*RS-PROJ-BASEN         HÄMTA HÄRIFRÅN OM EJ INMATAT                       
197500     IF MID-IDLKTO-POS3-7 = ALL '+' OR                                    
197600        MID-RESLJUST-C1 = ALL '+'                                         
197700        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
197800           AND  MID-KDKOPTYP NOT = 'R'                                    
197900           MOVE SPAR-KDPRODSL-ARTC   TO W-KDPRODSL1                       
198000           PERFORM IMS-GU-WLXXAQ01-UNIK                                   
198100           IF SEGMENT-FINNS                                               
198200              MOVE NYPON-ART-IDPROJK    TO W-IDPROJK                      
198300              MOVE NYPON-ART-IDPROJOBJ  TO W-IDPROJOBJ                    
198400              MOVE NYPON-ART-IDPROJ     TO W-IDPROJ                       
198500              PERFORM IMS-GNP-WLXXAQ11-FIRST                              
198600              IF SEGMENT-FINNS                                            
198700                 IF MID-RESLJUST-C1 = ALL '+'                             
198800                    MOVE XXAQ-1132-RESLJUST-C1 TO SPAR-RESLJUST-C1        
198900                    IF XXAQ-1132-RESLJUST-C1 = ZERO                       
199000                       MOVE ZERO               TO SPAR-TISLJUST-C1        
199100                    END-IF                                                
199200                 END-IF                                                   
199300              ELSE                                                        
199400                 IF MID-RESLJUST-C1 = ALL '+'                             
199500                    MOVE ZERO                  TO SPAR-TISLJUST-C1        
199600                 END-IF                                                   
199700              END-IF                                                      
199800           ELSE                                                           
199900              IF MID-RESLJUST-C1 = ALL '+'                                
200000                 MOVE ZERO                     TO SPAR-TISLJUST-C1        
200100              END-IF                                                      
200200           END-IF                                                         
200300        ELSE                                                              
200400           IF MID-RESLJUST-C1 = ALL '+'                                   
200500              MOVE ZERO                     TO SPAR-TISLJUST-C1           
200600           END-IF                                                         
200700        END-IF                                                            
200800     END-IF                                                               
200900                                                                          
201000     IF MID-RESLJUST-C1 = ALL '+'                                         
201100        MOVE MFS-RENSA-FAELT   TO MOD-RESLJUST-C1-IN                      
201200     ELSE                                                                 
201300        MOVE MID-RESLJUST-C1   TO DEC-IDFRIDATA                           
201400        MOVE 1                 TO DEC-KVHELTAL                            
201500                                  DEC-KVDECIMAL                           
201600        PERFORM S98-WDECEDIT                                              
201700        IF DEC-KDSVAR-OK                                                  
201800           IF DEC-IDEDITDATA > 0 AND < 9.8                                
201900              MOVE MFS-NUM-FAELT-RAETT                                    
202000                               TO MOD-RESLJUST-C1-IN-ATTR                 
202100              MOVE DEC-IDEDITDATA                                         
202200                               TO SPAR-RESLJUST-C1                        
202300           ELSE                                                           
202400              MOVE MFS-NUM-FAELT-FEL                                      
202500                               TO MOD-RESLJUST-C1-IN-ATTR                 
202600              MOVE NEJ         TO SW-INPUT-RAETT                          
202700           END-IF                                                         
202800        ELSE                                                              
202900           MOVE MFS-NUM-FAELT-FEL                                         
203000                               TO MOD-RESLJUST-C1-IN-ATTR                 
203100           MOVE NEJ            TO SW-INPUT-RAETT                          
203200        END-IF                                                            
203300        MOVE MFS-ROER-EJ-FAELT TO MOD-RESLJUST-C1-IN                      
203400     END-IF                                                               
203500                                                                          
203600     IF MID-IDPLANGR-LEV = ALL '+'                                        
203700        MOVE MFS-RENSA-FAELT      TO MOD-IDPLANGR-LEV-IN                  
203800     ELSE                                                                 
203900        MOVE MID-IDPLANGR-LEV     TO WS-IDPLANGR-LEV-AG-TEST              
204000        IF GODK-IDPLANGR-LEV-AG                                           
204100           MOVE MFS-NUM-FAELT-RAETT                                       
204200                                  TO MOD-IDPLANGR-LEV-IN-ATTR             
204300        ELSE                                                              
204400           MOVE MFS-NUM-FAELT-FEL                                         
204500                                  TO MOD-IDPLANGR-LEV-IN-ATTR             
204600           MOVE NEJ               TO SW-INPUT-RAETT                       
204700        END-IF                                                            
204800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPLANGR-LEV-IN                  
204900     END-IF                                                               
205000                                                                          
205100     IF MID-IDPLANGR-AG = ALL '+'                                         
205200        MOVE MFS-RENSA-FAELT      TO MOD-IDPLANGR-AG-IN                   
205300     ELSE                                                                 
205400        MOVE MID-IDPLANGR-AG      TO WS-IDPLANGR-LEV-AG-TEST              
205500        IF GODK-IDPLANGR-LEV-AG                                           
205600           MOVE MFS-NUM-FAELT-RAETT                                       
205700                                  TO MOD-IDPLANGR-AG-IN-ATTR              
205800        ELSE                                                              
205900           MOVE MFS-NUM-FAELT-FEL                                         
206000                                  TO MOD-IDPLANGR-AG-IN-ATTR              
206100           MOVE NEJ               TO SW-INPUT-RAETT                       
206200        END-IF                                                            
206300        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPLANGR-AG-IN                   
206400     END-IF                                                               
206500                                                                          
206600     IF MID-IDLEVNR = ALL '+'                                             
206700        IF SPAR-IDLEVNR NOT = SPACE                                       
206800           CONTINUE                                                       
206900        ELSE                                                              
207000           IF SW-ARTIKEL-FINNS-PA-NYPON = JA                              
207100              AND MID-KDKOPTYP NOT = 'R'                                  
207200              IF NYPON-ART-IDLEVNR NOT = SPACE                            
207300*---             KOLLA ATT NYPON-ARTIKELNS LEVERANTÖR FINNS PÅ            
207400*---             LEVERANTÖRSREG                                           
207500                 MOVE NYPON-ART-IDLEVNR TO W-IDLEVNR                      
207600                                           SPAR-IDLEVNR                   
207700                 PERFORM IMS-GU-LEVA01                                    
207800                 IF SEGMENT-FINNS                                         
207900                   MOVE NYPON-ART-IDLEVNR TO WS-IDLEVNR-NUM               
208000                   INSPECT WS-IDLEVNR-NUM REPLACING                       
208100                                                ALL SPACE BY ZERO         
208200                   IF WS-IDLEVNR-NUM NUMERIC                              
208300                     IF LEVA01-LEV-IDLEVNR-MOTSV NOT = SPACE              
208400                       MOVE MFS-ALFA-FAELT-FEL                            
208500                                        TO MOD-IDLEVNR-IN-ATTR            
208600                       MOVE NEJ         TO SW-INPUT-RAETT                 
208700                       IF MSGI-IDLAND-SPR = 'GB'                          
208800                          MOVE FEL-125  TO MOD-TEMFSINF                   
208900                       ELSE                                               
209000                          MOVE FEL-25   TO MOD-TEMFSINF                   
209100                       END-IF                                             
209200                     ELSE                                                 
209300                       MOVE MFS-ALFA-FAELT-RAETT                          
209400                                        TO MOD-IDLEVNR-IN-ATTR            
209500                     END-IF                                               
209600                   ELSE                                                   
209700                       MOVE MFS-ALFA-FAELT-RAETT                          
209800                                        TO MOD-IDLEVNR-IN-ATTR            
209900                   END-IF                                                 
210000                 ELSE                                                     
210100                    MOVE MFS-ALFA-FAELT-FEL                               
210200                                        TO MOD-IDLEVNR-IN-ATTR            
210300                    MOVE NEJ            TO SW-INPUT-RAETT                 
210400                 END-IF                                                   
210500              ELSE                                                        
210600**SI+                                                                     
210700                IF KDPRODSL-SKALL-KOPA                                    
210800                 MOVE MFS-ALFA-FAELT-RAETT                                
210900                                     TO MOD-IDLEVNR-IN-ATTR               
211000                ELSE                                                      
211100                 MOVE MFS-ALFA-FAELT-FEL                                  
211200                                     TO MOD-IDLEVNR-IN-ATTR               
211300                 MOVE NEJ            TO SW-INPUT-RAETT                    
211400                END-IF                                                    
211500              END-IF                                                      
211600           ELSE                                                           
211700              IF MID-KDKOPTYP = 'N'                                       
211800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR           
211900                 MOVE NEJ               TO SW-INPUT-RAETT                 
212000              ELSE                                                        
212100                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR         
212200              END-IF                                                      
212300           END-IF                                                         
212400        END-IF                                                            
212500        MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                       
212600     ELSE                                                                 
212700        IF MID-IDLEVNR NOT = SPACE                                        
212800           IF (MID-IDLEVNR = '1002' AND                                   
212900               SPAR-KDPRODSL-ARTC > 90)                                   
213000               OR                                                         
213100              ((SPAR-IDLEVNR NOT = SPACE) AND                             
213200              (SPAR-IDLEVNR NOT = '9996') AND                             
213300              (SPAR-IDLEVNR NOT = '9997') AND                             
213400              (SPAR-IDLEVNR NOT = '9998'))                                
213500                 MOVE MFS-ALFA-FAELT-FEL                                  
213600                                     TO MOD-IDLEVNR-IN-ATTR               
213700                 MOVE NEJ            TO SW-INPUT-RAETT                    
213800           ELSE                                                           
213900              MOVE MID-IDLEVNR       TO W-IDLEVNR                         
214000                                        SPAR-IDLEVNR                      
214100              PERFORM IMS-GU-LEVA01                                       
214200              IF SEGMENT-FINNS                                            
214300                MOVE MID-IDLEVNR TO WS-IDLEVNR-NUM                        
214400                INSPECT WS-IDLEVNR-NUM REPLACING                          
214500                                             ALL SPACE BY ZERO            
214600                IF WS-IDLEVNR-NUM NUMERIC                                 
214700                  IF LEVA01-LEV-IDLEVNR-MOTSV NOT = SPACE                 
214800                    MOVE MFS-ALFA-FAELT-FEL                               
214900                                     TO MOD-IDLEVNR-IN-ATTR               
215000                    MOVE NEJ         TO SW-INPUT-RAETT                    
215100                    IF MSGI-IDLAND-SPR = 'GB'                             
215200                       MOVE FEL-125  TO MOD-TEMFSINF                      
215300                    ELSE                                                  
215400                       MOVE FEL-25   TO MOD-TEMFSINF                      
215500                    END-IF                                                
215600                  ELSE                                                    
215700                    MOVE MFS-ALFA-FAELT-RAETT                             
215800                                     TO MOD-IDLEVNR-IN-ATTR               
215900                  END-IF                                                  
216000                ELSE                                                      
216100                    MOVE MFS-ALFA-FAELT-RAETT                             
216200                                     TO MOD-IDLEVNR-IN-ATTR               
216300                END-IF                                                    
216400              ELSE                                                        
216500                MOVE MFS-ALFA-FAELT-FEL                                   
216600                                TO MOD-IDLEVNR-IN-ATTR                    
216700                MOVE NEJ TO SW-INPUT-RAETT                                
216800              END-IF                                                      
216900           END-IF                                                         
217000        END-IF                                                            
217100        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDLEVNR-IN                       
217200     END-IF                                                               
217300                                                                          
217400     IF MID-IDANSK = ALL '+'                                              
217500        IF (SPAR-IDLEVNR = '9996' OR '9997' OR '9998' OR                  
217600                           '9999')      OR                                
217700           (SPAR-KDPRODSL-KPS NOT = 11)                                   
217800           IF SW-ARTIKEL-FINNS-PA-NYPON = JA                              
217900              CONTINUE                                                    
218000           ELSE                                                           
218100              MOVE MFS-NUM-FAELT-FEL                                      
218200                                 TO MOD-IDANSK-IN-ATTR                    
218300              MOVE NEJ           TO SW-INPUT-RAETT                        
218400           END-IF                                                         
218500        END-IF                                                            
218600        MOVE MFS-RENSA-FAELT      TO MOD-IDANSK-IN                        
218700     ELSE                                                                 
218800        IF MID-IDANSK NUMERIC                                             
218900           IF MID-IDANSK > ZERO                                           
219000              MOVE MFS-NUM-FAELT-RAETT                                    
219100                                  TO MOD-IDANSK-IN-ATTR                   
219200           ELSE                                                           
219300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-IN-ATTR                
219400              MOVE NEJ            TO SW-INPUT-RAETT                       
219500           END-IF                                                         
219600        ELSE                                                              
219700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-IN-ATTR                   
219800           MOVE NEJ               TO SW-INPUT-RAETT                       
219900        END-IF                                                            
220000        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDANSK-IN                        
220100     END-IF                                                               
220200                                                                          
220300     IF MID-KVPB-C1  = ALL '+'                                            
220400        MOVE NEJ               TO SW-INPUT-RAETT                          
220500        MOVE MFS-RENSA-FAELT   TO MOD-KVPB-C1-IN                          
220600        MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-C1-IN-ATTR                     
220700     ELSE                                                                 
220800        MOVE MID-KVPB-C1          TO SPAR-KVPB-C1                         
220900        IF SPAR-KVPB-C1-HELTAL    NUMERIC  AND                            
221000           SPAR-KVPB-C1-DECIMAL   NUMERIC  AND                            
221100           SPAR-KVPB-C1-PUNKT      = '.'                                  
221200           MOVE MFS-NUM-FAELT-RAETT                                       
221300                                  TO MOD-KVPB-C1-IN-ATTR                  
221400           MOVE SPAR-KVPB-C1-HELTAL                                       
221500                                  TO SPAR-KVPB-C1-R-HELTAL                
221600           MOVE SPAR-KVPB-C1-DECIMAL                                      
221700                                  TO SPAR-KVPB-C1-R-DECIMAL               
221800        ELSE                                                              
221900           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-C1-IN-ATTR                  
222000           MOVE NEJ               TO SW-INPUT-RAETT                       
222100        END-IF                                                            
222200        MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-C1-IN                          
222300     END-IF                                                               
222400                                                                          
222500     IF MID-PRARTSTD = ALL '+'                                            
222600        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
222700           IF NYPON-ART-PRARTBES > ZERO                                   
222800              CONTINUE                                                    
222900           ELSE                                                           
223000              MOVE MFS-NUM-FAELT-FEL                                      
223100                                  TO MOD-PRARTSTD-IN-ATTR                 
223200              MOVE NEJ            TO SW-INPUT-RAETT                       
223300           END-IF                                                         
223400        ELSE                                                              
223500           MOVE MFS-NUM-FAELT-FEL                                         
223600                                  TO MOD-PRARTSTD-IN-ATTR                 
223700           MOVE NEJ               TO SW-INPUT-RAETT                       
223800                                                                          
223900        END-IF                                                            
224000        MOVE MFS-RENSA-FAELT   TO MOD-PRARTSTD-IN                         
224100     ELSE                                                                 
224200        MOVE MID-PRARTSTD         TO SPAR-PRARTSTD                        
224300        IF SPAR-PRARTSTD-HELTAL   NUMERIC  AND                            
224400           SPAR-PRARTSTD-DECIMAL  NUMERIC  AND                            
224500           SPAR-PRARTSTD-PUNKT  = '.'                                     
224600           IF SPAR-PRARTSTD-HELTAL  > ZERO  OR                            
224700              SPAR-PRARTSTD-DECIMAL > ZERO                                
224800              MOVE MFS-NUM-FAELT-RAETT                                    
224900                                  TO MOD-PRARTSTD-IN-ATTR                 
225000              MOVE SPAR-PRARTSTD-HELTAL                                   
225100                                  TO SPAR-PRARTSTD-R-HELTAL               
225200              MOVE SPAR-PRARTSTD-DECIMAL                                  
225300                                  TO SPAR-PRARTSTD-R-DECIMAL              
225400           ELSE                                                           
225500              MOVE MFS-NUM-FAELT-FEL                                      
225600                                  TO MOD-PRARTSTD-IN-ATTR                 
225700              MOVE NEJ            TO SW-INPUT-RAETT                       
225800           END-IF                                                         
225900        ELSE                                                              
226000           MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTSTD-IN-ATTR                 
226100           MOVE NEJ               TO SW-INPUT-RAETT                       
226200        END-IF                                                            
226300        MOVE MFS-ROER-EJ-FAELT    TO MOD-PRARTSTD-IN                      
226400     END-IF                                                               
226500                                                                          
226600     IF MID-KDTIPPR = ALL '+'                                             
226700        MOVE MFS-RENSA-FAELT   TO MOD-KDTIPPR-IN                          
226800     ELSE                                                                 
226900        IF MID-KDTIPPR = JA OR YES OR NEJ OR WS-A                         
227000           MOVE MFS-ALFA-FAELT-RAETT                                      
227100                               TO MOD-KDTIPPR-IN-ATTR                     
227200        ELSE                                                              
227300           MOVE MFS-ALFA-FAELT-FEL                                        
227400                               TO MOD-KDTIPPR-IN-ATTR                     
227500           MOVE NEJ            TO SW-INPUT-RAETT                          
227600        END-IF                                                            
227700        MOVE MFS-ROER-EJ-FAELT TO MOD-KDTIPPR-IN                          
227800     END-IF                                                               
227900                                                                          
228000     IF MID-IDANSK-NOT = ALL '+'                                          
228100        MOVE MFS-RENSA-FAELT   TO MOD-IDANSK-NOT-IN                       
228200     ELSE                                                                 
228300        MOVE MFS-ALFA-FAELT-RAETT                                         
228400                               TO MOD-IDANSK-NOT-IN-ATTR                  
228500        MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-NOT-IN                       
228600     END-IF                                                               
228700                                                                          
228800     IF MID-IDINK = ALL '+'                                               
228900        MOVE MFS-RENSA-FAELT   TO MOD-IDINK-IN                            
229000        IF SPAR-IDINK > SPACE                                             
229100*---       IDINK FRÅN ARTC                                                
229200           CONTINUE                                                       
229300        ELSE                                                              
229400           IF SW-ARTIKEL-FINNS-PA-NYPON = JA                              
229500              IF NYPON-ART-IDINK > SPACE                                  
229600                 MOVE NYPON-ART-IDINK TO SPAR-IDINK                       
229700              END-IF                                                      
229800           END-IF                                                         
229900        END-IF                                                            
230000     ELSE                                                                 
230100        IF MID-IDINK (1:3)  NUMERIC  OR                                   
230200           MID-IDINK (2:3)  NUMERIC                                       
230300           MOVE MFS-ALFA-FAELT-RAETT                                      
230400                               TO MOD-IDINK-IN-ATTR                       
230500           MOVE MID-IDINK      TO SPAR-IDINK                              
230600        ELSE                                                              
230700           MOVE MFS-ALFA-FAELT-FEL                                        
230800                               TO MOD-IDINK-IN-ATTR                       
230900           MOVE NEJ            TO SW-INPUT-RAETT                          
231000        END-IF                                                            
231100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDINK-IN                            
231200     END-IF                                                               
231300                                                                          
231400     IF MID-KDHF = ALL '+'                                                
231500        MOVE MFS-RENSA-FAELT      TO MOD-KDHF-IN                          
231600     ELSE                                                                 
231700        IF MID-KDHF NOT NUMERIC                                           
231800          MOVE MFS-NUM-FAELT-FEL                                          
231900                                  TO MOD-KDHF-IN-ATTR                     
232000          MOVE NEJ                TO SW-INPUT-RAETT                       
232100          MOVE MFS-ROER-EJ-FAELT  TO MOD-KDHF-IN                          
232200        ELSE                                                              
232300          IF MID-KDHF = 0 OR 2 OR 3 OR 4                                  
232400             MOVE MFS-NUM-FAELT-RAETT                                     
232500                                  TO MOD-KDHF-IN-ATTR                     
232600             MOVE MID-KDHF        TO SPAR-KDHF                            
232700          ELSE                                                            
232800             MOVE MFS-NUM-FAELT-FEL                                       
232900                                  TO MOD-KDHF-IN-ATTR                     
233000             MOVE NEJ             TO SW-INPUT-RAETT                       
233100          END-IF                                                          
233200          MOVE MFS-ROER-EJ-FAELT  TO MOD-KDHF-IN                          
233300        END-IF                                                            
233400     END-IF                                                               
233500                                                                          
233600     IF MID-TIPBLOCK NOT = ALL '+'                                        
233700        MOVE 'AAMMDD'          TO DAT-KDDATFORM                           
233800        MOVE MID-TIPBLOCK      TO DAT-I-TIDATUM                           
233900                                                                          
234000                                                                          
234100        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
234200                            DAT-O-TIDATUM DAT-KDSVAR                      
234300                                                                          
234400        IF DAT-KDSVAR-OK                                                  
234500          MOVE MFS-NUM-FAELT-RAETT                                        
234600                               TO MOD-TIPBLOCK-IN-ATTR                    
234700        ELSE                                                              
234800          MOVE MFS-NUM-FAELT-FEL                                          
234900                               TO MOD-TIPBLOCK-IN-ATTR                    
235000          MOVE NEJ             TO SW-INPUT-RAETT                          
235100        END-IF                                                            
235200        MOVE MFS-ROER-EJ-FAELT TO MOD-TIPBLOCK-IN                         
235300     ELSE                                                                 
235400        MOVE MFS-NUM-FAELT-RAETT TO MOD-TIPBLOCK-IN-ATTR                  
235500     END-IF                                                               
235600                                                                          
235700     IF MID-FLMPB-C1 = ALL '+'                                            
235800        MOVE MFS-RENSA-FAELT   TO MOD-FLMPB-C1-IN                         
235900     ELSE                                                                 
236000        IF MID-FLMPB-C1 = JA OR NEJ                                       
236100           MOVE MFS-ALFA-FAELT-RAETT                                      
236200                               TO MOD-FLMPB-C1-IN-ATTR                    
236300        ELSE                                                              
236400           MOVE MFS-ALFA-FAELT-FEL                                        
236500                               TO MOD-FLMPB-C1-IN-ATTR                    
236600           MOVE NEJ            TO SW-INPUT-RAETT                          
236700        END-IF                                                            
236800        MOVE MFS-ROER-EJ-FAELT TO MOD-FLMPB-C1-IN                         
236900     END-IF                                                               
237000     .                                                                    
237100     EJECT                                                                
237200 EB-RENSA-KOPFAELT SECTION.                                               
237300     SKIP3                                                                
237400     MOVE MFS-RENSA-FAELT  TO MOD-KDKOPTYP-IN                             
237500                              MOD-TILEVBEG-IN                             
237600                              MOD-KVLEVBEG-IN                             
237700                              MOD-KVPROG-IN                               
237800                              MOD-TEANSINK-IN.                            
237900     EJECT                                                                
238000 EC-ROER-EJ-KOPFAELT SECTION.                                             
238100     SKIP2                                                                
238200     IF MID-KDKOPTYP = ALL '+'                                            
238300        MOVE MFS-RENSA-FAELT     TO MOD-KDKOPTYP-IN                       
238400     ELSE                                                                 
238500        MOVE MFS-ROER-EJ-FAELT   TO MOD-KDKOPTYP-IN                       
238600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKOPTYP-IN-ATTR                 
238700     END-IF                                                               
238800                                                                          
238900     IF MID-TILEVBEG = ALL '+'                                            
239000        MOVE MFS-RENSA-FAELT     TO MOD-TILEVBEG-IN                       
239100     ELSE                                                                 
239200        MOVE MFS-ROER-EJ-FAELT   TO MOD-TILEVBEG-IN                       
239300        MOVE MFS-NUM-FAELT-RAETT TO MOD-TILEVBEG-IN-ATTR                  
239400     END-IF                                                               
239500                                                                          
239600     IF MID-KVLEVBEG = ALL '+'                                            
239700        MOVE MFS-RENSA-FAELT     TO MOD-KVLEVBEG-IN                       
239800     ELSE                                                                 
239900        MOVE MFS-ROER-EJ-FAELT   TO MOD-KVLEVBEG-IN                       
240000        MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEVBEG-IN-ATTR                  
240100     END-IF                                                               
240200                                                                          
240300     IF MID-KVPROG = ALL '+'                                              
240400        MOVE MFS-RENSA-FAELT     TO MOD-KVPROG-IN                         
240500     ELSE                                                                 
240600        MOVE MFS-ROER-EJ-FAELT   TO MOD-KVPROG-IN                         
240700        MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPROG-IN-ATTR                    
240800     END-IF                                                               
240900                                                                          
241000     IF MID-TEANSINK = ALL '+'                                            
241100        CONTINUE                                                          
241200     ELSE                                                                 
241300        MOVE MFS-ROER-EJ-FAELT   TO MOD-TEANSINK-IN                       
241400        MOVE MFS-NUM-FAELT-RAETT TO MOD-TEANSINK-IN-ATTR                  
241500     END-IF                                                               
241600     .                                                                    
241700     EJECT                                                                
241800 ED-INDATAKOLL2 SECTION.                                                  
241900     SKIP2                                                                
242000     IF MID-KDKOPTYP = 'N' OR MID-KDKOPTYP = 'R'                          
242100        MOVE MFS-ALFA-FAELT-RAETT          TO MOD-KDKOPTYP-IN-ATTR        
242200     ELSE                                                                 
242300        MOVE MFS-ALFA-FAELT-FEL            TO MOD-KDKOPTYP-IN-ATTR        
242400        MOVE NEJ                           TO SW-INPUT-RAETT              
242500     END-IF                                                               
242600     MOVE MFS-ROER-EJ-FAELT                TO MOD-KDKOPTYP-IN             
242700                                                                          
242800     IF MID-TILEVBEG = ALL '+'                                            
242900        MOVE MFS-RENSA-FAELT      TO MOD-TILEVBEG-IN                      
243000        MOVE MFS-NUM-FAELT-FEL    TO MOD-TILEVBEG-IN-ATTR                 
243100        MOVE NEJ                  TO SW-INPUT-RAETT                       
243200     ELSE                                                                 
243300        IF MID-TILEVBEG      = 9999          AND                          
243400          (KDPRODSL-SKALL-KOPA)                                           
243500*---       HF/KONCERNMÄRKNING I EFTERHAND                                 
243600           IF (MID-KDKOPTYP  = 'R' OR 'N')     AND                        
243700              (MID-KVLEVBEG       = ALL '+')   AND                        
243800              (MID-TEANSINK       = ALL '+')                              
243900               IF (MID-KVPROG = ALL '+' AND SPAR-KVPROG = ZERO)           
244000                   OR MID-KVPROG = ZERO                                   
244100                    MOVE MFS-NUM-FAELT-RAETT                              
244200                                           TO MOD-TILEVBEG-IN-ATTR        
244300                    MOVE '999999'          TO SPAR-TILEVBEG               
244400               ELSE                                                       
244500                 MOVE MFS-NUM-FAELT-RAETT                                 
244600                                        TO MOD-TILEVBEG-IN-ATTR           
244700                 MOVE NEJ               TO SW-INPUT-RAETT                 
244800                 MOVE MFS-RENSA-FAELT   TO MOD-TILEVBEG-IN                
244900                                           MOD-KDKOPTYP-IN                
245000                                           MOD-KVLEVBEG-IN                
245100                                           MOD-KVPROG-IN                  
245200                                           MOD-TEANSINK-IN                
245300              END-IF                                                      
245400           ELSE                                                           
245500              MOVE NEJ               TO SW-INPUT-RAETT                    
245600              MOVE MFS-RENSA-FAELT   TO MOD-TILEVBEG-IN                   
245700                                        MOD-KDKOPTYP-IN                   
245800                                        MOD-KVLEVBEG-IN                   
245900                                        MOD-KVPROG-IN                     
246000                                        MOD-TEANSINK-IN                   
246100           END-IF                                                         
246200        ELSE                                                              
246300           IF MID-TILEVBEG     = ZERO       AND                           
246400           (KDPRODSL-SKALL-KOPA)                                          
246500*---          ANNULERING AV KÖP                                           
246600              IF (MID-KVLEVBEG      = ALL '+')   AND                      
246700                 (MID-TEANSINK      = ALL '+')                            
246800                 IF (MID-KVPROG = ALL '+' AND SPAR-KVPROG = ZERO)         
246900                   OR MID-KVPROG = ZERO                                   
247000                    MOVE MFS-NUM-FAELT-RAETT                              
247100                                        TO MOD-TILEVBEG-IN-ATTR           
247200                    MOVE MID-TILEVBEG   TO SPAR-TILEVBEG                  
247300                 ELSE                                                     
247400                    MOVE MFS-NUM-FAELT-RAETT                              
247500                                        TO MOD-TILEVBEG-IN-ATTR           
247600                    MOVE MFS-ADD-SAETT-CURSOR                             
247700                                        TO MOD-KVPROG-IN-ATTR             
247800                    MOVE NEJ               TO SW-INPUT-RAETT              
247900                 END-IF                                                   
248000              ELSE                                                        
248100                 MOVE MFS-NUM-FAELT-FEL                                   
248200                                   TO MOD-TILEVBEG-IN-ATTR                
248300                 MOVE NEJ               TO SW-INPUT-RAETT                 
248400              END-IF                                                      
248500           ELSE                                                           
248600              MOVE 'AAVV'               TO DAT-KDDATFORM                  
248700              MOVE MID-TILEVBEG         TO SPAR-TILEVBEG2                 
248800              MOVE SPAR-TILEVBEG2       TO DAT-I-TIDATUM                  
248900              PERFORM S99-WDATKONV                                        
249000              IF DAT-KDSVAR-OK                                            
249100                 MOVE DAT-TIAAMMDD      TO SPAR-TILEVBEG                  
249200                 MOVE SPAR-DAGENS-DATUM TO DAT-I-TIDATUM                  
249300                 MOVE 'AAMMDD'          TO DAT-KDDATFORM                  
249400                 PERFORM S99-WDATKONV                                     
249500                 IF DAT-KDSVAR-OK                                         
249600                    MOVE DAT-TIAA       TO SPAR-DAGENS-DATUM-AA           
249700                    MOVE DAT-TIVV       TO SPAR-DAGENS-DATUM-VV           
249800                    MOVE SPAR-TILEVBEG2         TO TMP1-YYWW              
249900                    MOVE SPAR-DAGENS-DATUM-AAVV-R TO TMP2-YYWW            
250000                    PERFORM WY2000P3                                      
250100                    IF TMP1-YYWW < TMP2-YYWW                              
250200*---                  *INMATAD AVROPSVECKA < DAGENS-AAVV*                 
250300                       MOVE MFS-NUM-FAELT-FEL                             
250400                                        TO MOD-TILEVBEG-IN-ATTR           
250500                       MOVE NEJ         TO SW-INPUT-RAETT                 
250600                       IF MSGI-IDLAND-SPR = 'GB'                          
250700                          MOVE FEL-115     TO MOD-TEMFSINF                
250800                       ELSE                                               
250900                          MOVE FEL-15      TO MOD-TEMFSINF                
251000                       END-IF                                             
251100                    ELSE                                                  
251200                       MOVE SPAR-TILEVBEG2         TO TMP1-YYWW           
251300                       MOVE SPAR-DAGENS-DATUM-AAVV-R TO TMP2-YYWW         
251400                       PERFORM WY2000P3                                   
251500                       IF TMP1-YYWW > TMP2-YYWW + 200                     
251600*---                     *INMATAD AVROPSVECKA > DAGENS-AAVV +2ÅR*         
251700                          MOVE MFS-NUM-FAELT-FEL                          
251800                                        TO MOD-TILEVBEG-IN-ATTR           
251900                          MOVE NEJ      TO SW-INPUT-RAETT                 
252000                          IF MSGI-IDLAND-SPR = 'GB'                       
252100                             MOVE FEL-115     TO MOD-TEMFSINF             
252200                          ELSE                                            
252300                             MOVE FEL-15      TO MOD-TEMFSINF             
252400                          END-IF                                          
252500                       ELSE                                               
252600                          MOVE SPAR-TILEVBEG2    TO TMP1-YYWW             
252700                          MOVE SPAR-TIFINLV-AAVV TO TMP2-YYWW             
252800                          PERFORM WY2000P3                                
252900                          IF TMP1-YYWW > TMP2-YYWW                        
253000*---                         *INMATAD AVROPSV FÅR VARA STÖRRE             
253100*---                         *TIFINLV                                     
253200                                MOVE SPAR-DAGENS-DATUM-AAVV-R             
253300                                         TO SPAR-W009VADD-DATUM           
253400                                MOVE +10 TO SPAR-W009VADD-ANTAL           
253500                                PERFORM S96-W009VADD                      
253600                                MOVE SPAR-W009VADD-DATUM TO               
253700                                     SPAR-DAGENS-DATUM-AAVV-R             
253800                                MOVE SPAR-TILEVBEG2 TO TMP1-YYWW          
253900                                MOVE SPAR-DAGENS-DATUM-AAVV-R             
254000                                                    TO TMP2-YYWW          
254100                                PERFORM WY2000P3                          
254200                                IF TMP1-YYWW > TMP2-YYWW                  
254300                                   AND MID-KDKOPTYP NOT = 'R'             
254400*---                              *INMATAD AVROPSV. >                     
254500*---                                       DAGENS-AAVV + 10V*             
254600                                   MOVE MFS-NUM-FAELT-FEL                 
254700                                         TO MOD-TILEVBEG-IN-ATTR          
254800                                   MOVE NEJ   TO SW-INPUT-RAETT           
254900                                   IF MSGI-IDLAND-SPR = 'GB'              
255000                                      MOVE FEL-115 TO MOD-TEMFSINF        
255100                                   ELSE                                   
255200                                      MOVE FEL-15  TO MOD-TEMFSINF        
255300                                   END-IF                                 
255400                                ELSE                                      
255500                                   MOVE MFS-NUM-FAELT-RAETT               
255600                                        TO MOD-TILEVBEG-IN-ATTR           
255700                                END-IF                                    
255800                          ELSE                                            
255900                             MOVE SPAR-TIFINLV-AAVV                       
256000                                        TO SPAR-W009VADD-DATUM            
256100                             MOVE -52   TO SPAR-W009VADD-ANTAL            
256200                             PERFORM S96-W009VADD                         
256300                             MOVE SPAR-W009VADD-DATUM                     
256400                                        TO SPAR-TIFINLV-AAVV              
256500                             MOVE SPAR-TILEVBEG2    TO TMP1-YYWW          
256600                             MOVE SPAR-TIFINLV-AAVV TO TMP2-YYWW          
256700                             PERFORM WY2000P3                             
256800                             IF TMP1-YYWW < TMP2-YYWW                     
256900*---                           *INMAT.AVROPSV. < TIFINLV -52V.*           
257000                                MOVE MFS-NUM-FAELT-FEL                    
257100                                        TO MOD-TILEVBEG-IN-ATTR           
257200                                MOVE NEJ TO SW-INPUT-RAETT                
257300                                IF MSGI-IDLAND-SPR = 'GB'                 
257400                                   MOVE FEL-115 TO MOD-TEMFSINF           
257500                                ELSE                                      
257600                                   MOVE FEL-15  TO MOD-TEMFSINF           
257700                                END-IF                                    
257800                             ELSE                                         
257900                                MOVE MFS-NUM-FAELT-RAETT                  
258000                                        TO MOD-TILEVBEG-IN-ATTR           
258100                             END-IF                                       
258200                          END-IF                                          
258300                       END-IF                                             
258400                    END-IF                                                
258500                 END-IF                                                   
258600              ELSE                                                        
258700                 MOVE MFS-NUM-FAELT-FEL TO MOD-TILEVBEG-IN-ATTR           
258800                 MOVE NEJ               TO SW-INPUT-RAETT                 
258900              END-IF                                                      
259000           END-IF                                                         
259100        END-IF                                                            
259200        MOVE MFS-ROER-EJ-FAELT    TO MOD-TILEVBEG-IN                      
259300     END-IF                                                               
259400                                                                          
259500     IF SPAR-PRARTSTD-ARTC = ZERO AND                                     
259600        MID-KDKOPTYP = 'R'                                                
259700        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOPTYP-IN-ATTR                   
259800        MOVE NEJ                TO SW-INPUT-RAETT                         
259900        IF MSGI-IDLAND-SPR = 'GB'                                         
260000           MOVE FEL-123 TO MOD-TEMFSINF                                   
260100        ELSE                                                              
260200           MOVE FEL-23  TO MOD-TEMFSINF                                   
260300        END-IF                                                            
260400     END-IF                                                               
260500                                                                          
260600     IF MID-KVLEVBEG = ALL '+'                                            
260700        IF MID-TILEVBEG = 9999 OR ZERO                                    
260800           CONTINUE                                                       
260900        ELSE                                                              
261000           IF KDPRODSL-SKALL-KOPA                                         
261100              MOVE MFS-NUM-FAELT-FEL TO MOD-KVLEVBEG-IN-ATTR              
261200              MOVE NEJ               TO SW-INPUT-RAETT                    
261300           END-IF                                                         
261400        END-IF                                                            
261500        MOVE MFS-RENSA-FAELT   TO MOD-KVLEVBEG-IN                         
261600     ELSE                                                                 
261700        IF MID-KVLEVBEG NUMERIC                                           
261800           IF MID-KVLEVBEG > ZERO                                         
261900              MOVE MFS-NUM-FAELT-RAETT                                    
262000                                  TO MOD-KVLEVBEG-IN-ATTR                 
262100           ELSE                                                           
262200              MOVE MFS-NUM-FAELT-FEL                                      
262300                                  TO MOD-KVLEVBEG-IN-ATTR                 
262400              MOVE NEJ               TO SW-INPUT-RAETT                    
262500           END-IF                                                         
262600        ELSE                                                              
262700           MOVE MFS-NUM-FAELT-FEL TO MOD-KVLEVBEG-IN-ATTR                 
262800           MOVE NEJ               TO SW-INPUT-RAETT                       
262900        END-IF                                                            
263000        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVLEVBEG-IN                      
263100     END-IF                                                               
263200                                                                          
263300     IF MID-KVPROG = ALL '+'                                              
263400        IF MID-TILEVBEG = 9999  OR  ZERO                                  
263500           IF SPAR-KVPROG = ZERO                                          
263600              CONTINUE                                                    
263700           ELSE                                                           
263800              MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-IN-ATTR                
263900              MOVE NEJ               TO SW-INPUT-RAETT                    
264000           END-IF                                                         
264100        ELSE                                                              
264200           IF KDPRODSL-SKALL-KOPA                                         
264300              IF SPAR-KVPROG = ZERO                                       
264400                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-IN-ATTR             
264500                 MOVE NEJ               TO SW-INPUT-RAETT                 
264600              END-IF                                                      
264700           END-IF                                                         
264800        END-IF                                                            
264900        MOVE MFS-RENSA-FAELT      TO MOD-KVPROG-IN                        
265000     ELSE                                                                 
265100        IF MID-KVPROG NUMERIC                                             
265200          IF MID-TILEVBEG = 9999 OR ZERO                                  
265300             IF MID-KVPROG = ZERO                                         
265400                MOVE MFS-NUM-FAELT-RAETT                                  
265500                                 TO MOD-KVPROG-IN-ATTR                    
265600                MOVE MID-KVPROG  TO SPAR-KVPROG                           
265700             ELSE                                                         
265800                MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-IN-ATTR              
265900                MOVE NEJ               TO SW-INPUT-RAETT                  
266000             END-IF                                                       
266100          ELSE                                                            
266200             IF MID-KVPROG > ZERO                                         
266300                MOVE MFS-NUM-FAELT-RAETT                                  
266400                                 TO MOD-KVPROG-IN-ATTR                    
266500                MOVE MID-KVPROG  TO SPAR-KVPROG                           
266600             ELSE                                                         
266700                MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-IN-ATTR              
266800                MOVE NEJ               TO SW-INPUT-RAETT                  
266900             END-IF                                                       
267000          END-IF                                                          
267100        ELSE                                                              
267200           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-IN-ATTR                   
267300           MOVE NEJ               TO SW-INPUT-RAETT                       
267400        END-IF                                                            
267500        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPROG-IN                        
267600     END-IF                                                               
267700                                                                          
267800     IF MID-TEANSINK = ALL '+'                                            
267900        MOVE MFS-RENSA-FAELT   TO MOD-TEANSINK-IN                         
268000     ELSE                                                                 
268100        MOVE MFS-NUM-FAELT-RAETT                                          
268200                               TO MOD-TEANSINK-IN-ATTR                    
268300        MOVE MFS-ROER-EJ-FAELT TO MOD-TEANSINK-IN                         
268400     END-IF                                                               
268500     .                                                                    
268600     EJECT                                                                
268700 EE-KOLLA-INKOPARE-PV-NEDCAR SECTION.                                     
268800                                                                          
268900     IF SPAR-IDINK (1:3) NUMERIC                                          
269000        MOVE SPAR-IDINK (1:3)     TO TEST-IDINK                           
269100     ELSE                                                                 
269200        IF SPAR-IDINK (2:3)  NUMERIC                                      
269300           MOVE SPAR-IDINK (2:3)  TO TEST-IDINK                           
269400        ELSE                                                              
269500           MOVE ZERO              TO TEST-IDINK                           
269600        END-IF                                                            
269700     END-IF                                                               
269800     IF KDPRODSL-SKALL-KOPA       AND                                     
269900        (MID-TILEVBEG        < 9999)                                      
270000*--  VAFFÖR GÖR MAN PÅ DET SÄTTET OVAN? EJ KÖPA ÅR 2000?                  
270100       IF (TEST-IDINK (1:1) NOT = '4' AND                                 
270200           SPAR-KDPRODSL-ARTC = 19)                                       
270300            MOVE NEJ             TO SW-INPUT-RAETT                        
270400            IF MSGI-IDLAND-SPR = 'GB'                                     
270500               MOVE FEL-117 TO MOD-TEMFSINF                               
270600            ELSE                                                          
270700               MOVE FEL-17  TO MOD-TEMFSINF                               
270800            END-IF                                                        
270900            MOVE NEJ TO SW-IDINK-RAETT                                    
271000       ELSE                                                               
271100*--  KÖPANMODAN SKICKAS TILL SRM FÖR BRANDON PG, OAVSETT IDINK.           
271200        IF SPAR-KDPRODSL-ARTC = 25                                        
271300          CONTINUE                                                        
271400        ELSE                                                              
271500          IF (TEST-IDINK < 100 OR > 999) OR                               
271600              TEST-IDINK = 987                                            
271700              MOVE NEJ             TO SW-INPUT-RAETT                      
271800              IF MSGI-IDLAND-SPR = 'GB'                                   
271900                 MOVE FEL-117 TO MOD-TEMFSINF                             
272000              ELSE                                                        
272100                 MOVE FEL-17  TO MOD-TEMFSINF                             
272200              END-IF                                                      
272300              MOVE NEJ TO SW-IDINK-RAETT                                  
272400          END-IF                                                          
272500        END-IF                                                            
272600       END-IF                                                             
272700     END-IF                                                               
272800     .                                                                    
272900     EJECT                                                                
273000 EG-KOLL-BEST-AVT          SECTION.                                       
273100     SKIP2                                                                
273200     MOVE NEJ TO SW-REPKOP-OK                                             
273300     MOVE JA  TO SW-NYKOP-OK                                              
273400     PERFORM IMS-GNP-ARTC22                                               
273500     IF SEGMENT-FINNS                                                     
273600        MOVE JA TO SW-BESTAVT                                             
273700        PERFORM UNTIL SEGMENT-SAKNAS                                      
273800           IF MID-KDKOPTYP = 'R'                                          
273900              IF BEST-KDBEH-BEST = 1 OR                                   
274000                 BEST-KDBEH-BEST = 2 OR                                   
274100                 BEST-KDBEH-BEST = 5                                      
274200                 MOVE JA TO SW-REPKOP-OK                                  
274300              END-IF                                                      
274400           ELSE                                                           
274500              IF MID-KDKOPTYP = 'N'                                       
274600                 IF BEST-KDBEH-BEST = 1 OR                                
274700                    BEST-KDBEH-BEST = 5 OR                                
274800                    BEST-KDBEH-BEST = 6                                   
274900                    MOVE NEJ TO SW-NYKOP-OK                               
275000                 END-IF                                                   
275100              END-IF                                                      
275200           END-IF                                                         
275300           PERFORM IMS-GNP-ARTC22                                         
275400        END-PERFORM                                                       
275500     END-IF                                                               
275600                                                                          
275700     MOVE NEJ TO SW-REPKOP-AVTAL                                          
275800     PERFORM IMS-GNP-ARTC23                                               
275900     IF SEGMENT-FINNS                                                     
276000        MOVE JA TO SW-BESTAVT                                             
276100        MOVE NEJ TO SW-NYKOP-OK                                           
276200        IF MID-KDKOPTYP = 'R'                                             
276300           PERFORM UNTIL SEGMENT-SAKNAS                                   
276400              IF AVT-IDLEVNR-AVT = SPAR-IDLEVNR                           
276500                 MOVE JA TO SW-REPKOP-AVTAL                               
276600              ELSE                                                        
276700                 MOVE JA TO SW-REPKOP-OK                                  
276800              END-IF                                                      
276900              PERFORM IMS-GNP-ARTC23                                      
277000           END-PERFORM                                                    
277100        END-IF                                                            
277200     END-IF                                                               
277300     .                                                                    
277400     EJECT                                                                
277500 EN-KDKOPTYP-N SECTION.                                                   
277600                                                                          
277700     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
277800        IF SW-NYKOP-OK = NEJ                                              
277900           MOVE NEJ           TO SW-INPUT-RAETT                           
278000           PERFORM EB-RENSA-KOPFAELT                                      
278100           IF MSGI-IDLAND-SPR = 'GB'                                      
278200              MOVE FEL-113 TO MOD-TEMFSINF                                
278300           ELSE                                                           
278400              MOVE FEL-13  TO MOD-TEMFSINF                                
278500           END-IF                                                         
278600        ELSE                                                              
278700           IF MID-KVPROG         = ZERO     AND                           
278800              MID-TILEVBEG       = ALL '+'  AND                           
278900              MID-KVLEVBEG       = ALL '+'  AND                           
279000              MID-TEANSINK       = ALL '+'  AND                           
279100              SPAR-PRARTSTD-ARTC = ZERO                                   
279200              CONTINUE                                                    
279300           ELSE                                                           
279400              IF (KDPRODSL-SKALL-KOPA         AND                         
279500                 NYPON-ART-IDPROJK = SPACE)                               
279600                 MOVE NEJ          TO SW-INPUT-RAETT                      
279700                 PERFORM EB-RENSA-KOPFAELT                                
279800                 IF MSGI-IDLAND-SPR = 'GB'                                
279900                    MOVE FEL-108 TO MOD-TEMFSINF                          
280000                 ELSE                                                     
280100                    MOVE FEL-8   TO MOD-TEMFSINF                          
280200                 END-IF                                                   
280300              ELSE                                                        
280400                 MOVE JA TO SW-IDINK-RAETT                                
280500                 PERFORM EE-KOLLA-INKOPARE-PV-NEDCAR                      
280600                 IF SW-IDINK-RAETT = NEJ                                  
280700                    IF MID-PRISDEL = ALL '+'                              
280800                       PERFORM EB-RENSA-KOPFAELT                          
280900                    ELSE                                                  
281000                       PERFORM EC-ROER-EJ-KOPFAELT                        
281100                       MOVE MFS-ADD-SAETT-CURSOR                          
281200                                       TO MOD-IDINK-IN-ATTR               
281300                    END-IF                                                
281400                 ELSE                                                     
281500                   IF (SPAR-KDERS > 10) OR                                
281600                      (SPAR-MID-KDHF  = 1 OR 2 OR 3 OR 4 ) OR             
281700                      (SPAR-PRARTSTD-ARTC = ZERO       AND                
281800                       MID-PRARTSTD       = ALL '+'    AND                
281900                       NYPON-ART-PRARTBES = ZERO)                         
282000                      MOVE NEJ         TO SW-INPUT-RAETT                  
282100                      PERFORM EB-RENSA-KOPFAELT                           
282200                      IF MSGI-IDLAND-SPR = 'GB'                           
282300                         MOVE FEL-112 TO MOD-TEMFSINF                     
282400                      ELSE                                                
282500                         MOVE FEL-12  TO MOD-TEMFSINF                     
282600                      END-IF                                              
282700                   ELSE                                                   
282800                      IF WS-IDINK (1:3) NUMERIC                           
282900                         MOVE WS-IDINK (1:3)     TO TEST-IDINK            
283000                      ELSE                                                
283100                         IF WS-IDINK (2:3)  NUMERIC                       
283200                            MOVE WS-IDINK (2:3)  TO TEST-IDINK            
283300                         ELSE                                             
283400                            MOVE ZERO            TO TEST-IDINK            
283500                         END-IF                                           
283600                     END-IF                                               
283700                      MOVE SPAR-IDLEVNR TO                                
283800                                       WS-IDLEVNR-KONCERN                 
283900                      IF SPAR-PRARTSTD-ARTC = ZERO                        
284000                         IF KONCERN-LEV OR SPAR-KDHF > ZERO               
284100                            MOVE NEJ     TO SW-INPUT-RAETT                
284200                            PERFORM EB-RENSA-KOPFAELT                     
284300                            IF MSGI-IDLAND-SPR = 'GB'                     
284400                               MOVE FEL-112 TO MOD-TEMFSINF               
284500                            ELSE                                          
284600                               MOVE FEL-12  TO MOD-TEMFSINF               
284700                            END-IF                                        
284800                         ELSE                                             
284900                            PERFORM ED-INDATAKOLL2                        
285000                         END-IF                                           
285100                      ELSE                                                
285200*---                     LEVNR/HF FRÅN ARTC I SPAR....                    
285300                         IF KONCERN-LEV OR SPAR-KDHF > ZERO               
285400                            IF  KDPRODSL-SKALL-KOPA                       
285500                               AND                                        
285600                               MID-TILEVBEG   = 9999  AND                 
285700                               MID-KVLEVBEG = ALL '+' AND                 
285800                               MID-TEANSINK = ALL '+'                     
285900                               PERFORM ED-INDATAKOLL2                     
286000                            ELSE                                          
286100                               MOVE NEJ TO SW-INPUT-RAETT                 
286200                               PERFORM EB-RENSA-KOPFAELT                  
286300                               IF MSGI-IDLAND-SPR = 'GB'                  
286400                                  MOVE FEL-112 TO MOD-TEMFSINF            
286500                               ELSE                                       
286600                                  MOVE FEL-12  TO MOD-TEMFSINF            
286700                               END-IF                                     
286800                            END-IF                                        
286900                         ELSE                                             
287000                            PERFORM ED-INDATAKOLL2                        
287100                         END-IF                                           
287200                      END-IF                                              
287300                   END-IF                                                 
287400                 END-IF                                                   
287500              END-IF                                                      
287600           END-IF                                                         
287700        END-IF                                                            
287800     ELSE                                                                 
287900        MOVE NEJ                  TO SW-INPUT-RAETT                       
288000        IF MSGI-IDLAND-SPR = 'GB'                                         
288100           MOVE FEL-109 TO MOD-TEMFSINF                                   
288200        ELSE                                                              
288300           MOVE FEL-9   TO MOD-TEMFSINF                                   
288400        END-IF                                                            
288500     END-IF                                                               
288600     .                                                                    
288700     EJECT                                                                
288800 ER-KDKOPTYP-R SECTION.                                                   
288900                                                                          
289000     IF SW-AVTAL-FINNS = JA OR SW-REPKOP-AVTAL = JA                       
289100        MOVE NEJ                  TO SW-INPUT-RAETT                       
289200        IF MSGI-IDLAND-SPR = 'GB'                                         
289300           MOVE FEL-120 TO MOD-TEMFSINF                                   
289400        ELSE                                                              
289500           MOVE FEL-20  TO MOD-TEMFSINF                                   
289600        END-IF                                                            
289700        MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDKOPTYP-IN-ATTR                 
289800     ELSE                                                                 
289900        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
290000           IF SW-REPKOP-OK = NEJ                                          
290100             MOVE NEJ                  TO SW-INPUT-RAETT                  
290200             IF MSGI-IDLAND-SPR = 'GB'                                    
290300                MOVE FEL-122 TO MOD-TEMFSINF                              
290400             ELSE                                                         
290500                MOVE FEL-22  TO MOD-TEMFSINF                              
290600             END-IF                                                       
290700             MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDKOPTYP-IN-ATTR            
290800           ELSE                                                           
290900             MOVE JA TO SW-IDINK-RAETT                                    
291000             PERFORM EE-KOLLA-INKOPARE-PV-NEDCAR                          
291100             IF SW-IDINK-RAETT = NEJ                                      
291200               IF MID-PRISDEL = ALL '+'                                   
291300                  PERFORM EB-RENSA-KOPFAELT                               
291400               ELSE                                                       
291500                  PERFORM EC-ROER-EJ-KOPFAELT                             
291600                  MOVE MFS-ADD-SAETT-CURSOR                               
291700                                  TO MOD-IDINK-IN-ATTR                    
291800               END-IF                                                     
291900             ELSE                                                         
292000               IF (SPAR-KDERS > 10 ) OR                                   
292100                  (SPAR-MID-KDHF    = 1 OR 2 OR 3 OR 4 )                  
292200                   MOVE NEJ         TO SW-INPUT-RAETT                     
292300                   PERFORM EB-RENSA-KOPFAELT                              
292400                   IF MSGI-IDLAND-SPR = 'GB'                              
292500                      MOVE FEL-112 TO MOD-TEMFSINF                        
292600                   ELSE                                                   
292700                      MOVE FEL-12  TO MOD-TEMFSINF                        
292800                   END-IF                                                 
292900               ELSE                                                       
293000                  IF WS-IDINK (1:3) NUMERIC                               
293100                     MOVE WS-IDINK (1:3)     TO TEST-IDINK                
293200                  ELSE                                                    
293300                     IF WS-IDINK (2:3)  NUMERIC                           
293400                        MOVE WS-IDINK (2:3)  TO TEST-IDINK                
293500                     ELSE                                                 
293600                        MOVE ZERO            TO TEST-IDINK                
293700                     END-IF                                               
293800                  END-IF                                                  
293900                  MOVE SPAR-IDLEVNR TO                                    
294000                                     WS-IDLEVNR-KONCERN                   
294100*---                    LEVNR/HF FRÅN ARTC I SPAR....                     
294200                  IF KONCERN-LEV OR SPAR-KDHF > ZERO                      
294300                     IF (KDPRODSL-SKALL-KOPA)                             
294400                        AND                                               
294500                        MID-TILEVBEG   = 9999  AND                        
294600                        MID-KVLEVBEG = ALL '+' AND                        
294700                        MID-TEANSINK = ALL '+'                            
294800                                                                          
294900                        PERFORM ED-INDATAKOLL2                            
295000                     ELSE                                                 
295100                        MOVE NEJ TO SW-INPUT-RAETT                        
295200                        PERFORM EB-RENSA-KOPFAELT                         
295300                        IF MSGI-IDLAND-SPR = 'GB'                         
295400                           MOVE FEL-112 TO MOD-TEMFSINF                   
295500                        ELSE                                              
295600                           MOVE FEL-12  TO MOD-TEMFSINF                   
295700                        END-IF                                            
295800                     END-IF                                               
295900                  ELSE                                                    
296000                     PERFORM ED-INDATAKOLL2                               
296100                  END-IF                                                  
296200               END-IF                                                     
296300             END-IF                                                       
296400           END-IF                                                         
296500        ELSE                                                              
296600           IF SW-REPKOP-OK = NEJ                                          
296700             MOVE NEJ                  TO SW-INPUT-RAETT                  
296800             IF MSGI-IDLAND-SPR = 'GB'                                    
296900                MOVE FEL-122 TO MOD-TEMFSINF                              
297000             ELSE                                                         
297100                MOVE FEL-22  TO MOD-TEMFSINF                              
297200             END-IF                                                       
297300             MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDKOPTYP-IN-ATTR            
297400           ELSE                                                           
297500             MOVE JA TO SW-IDINK-RAETT                                    
297600             PERFORM EE-KOLLA-INKOPARE-PV-NEDCAR                          
297700             IF SW-IDINK-RAETT = NEJ                                      
297800                IF MID-PRISDEL = ALL '+'                                  
297900                   PERFORM EB-RENSA-KOPFAELT                              
298000                ELSE                                                      
298100                   PERFORM EC-ROER-EJ-KOPFAELT                            
298200                   MOVE MFS-ADD-SAETT-CURSOR                              
298300                                 TO MOD-IDINK-IN-ATTR                     
298400                END-IF                                                    
298500             ELSE                                                         
298600                IF (SPAR-MID-KDHF    = 1 OR 2 OR 3 OR 4 )                 
298700                OR (SPAR-KDERS > 10)                                      
298800                    MOVE NEJ         TO SW-INPUT-RAETT                    
298900                    PERFORM EB-RENSA-KOPFAELT                             
299000                    IF MSGI-IDLAND-SPR = 'GB'                             
299100                       MOVE FEL-112 TO MOD-TEMFSINF                       
299200                    ELSE                                                  
299300                       MOVE FEL-12  TO MOD-TEMFSINF                       
299400                    END-IF                                                
299500                ELSE                                                      
299600                    MOVE SPAR-IDLEVNR TO WS-IDLEVNR-KONCERN               
299700*---                LEVNR/HF FRÅN ARTC I SPAR....                         
299800                    IF KONCERN-LEV OR SPAR-KDHF > ZERO                    
299900                      IF (KDPRODSL-SKALL-KOPA)                            
300000                          AND                                             
300100                          MID-TILEVBEG   = 9999  AND                      
300200                          MID-KVLEVBEG = ALL '+' AND                      
300300                          MID-TEANSINK = ALL '+'                          
300400                                                                          
300500                          PERFORM ED-INDATAKOLL2                          
300600                      ELSE                                                
300700                          MOVE NEJ TO SW-INPUT-RAETT                      
300800                          PERFORM EB-RENSA-KOPFAELT                       
300900                          IF MSGI-IDLAND-SPR = 'GB'                       
301000                             MOVE FEL-112 TO MOD-TEMFSINF                 
301100                          ELSE                                            
301200                             MOVE FEL-12  TO MOD-TEMFSINF                 
301300                          END-IF                                          
301400                      END-IF                                              
301500                    ELSE                                                  
301600                        PERFORM ED-INDATAKOLL2                            
301700                    END-IF                                                
301800                END-IF                                                    
301900             END-IF                                                       
302000           END-IF                                                         
302100        END-IF                                                            
302200     END-IF                                                               
302300     .                                                                    
302400     EJECT                                                                
302500 F-UPPDATERA-OCH-VISA-BILD SECTION.                                       
302600     SKIP2                                                                
302700*WDK601                                                                   
302800*                                                                         
302900     PERFORM IMS-GHU-ARTC01                                               
303000     MOVE ART-FLERS                  TO SPAR-FLERS                        
303100     MOVE ART-TIFINLV                TO MOD-TIFINLV                       
303200     MOVE ART-IDAO (1)               TO MOD-IDAO                          
303300     MOVE 1                          TO SPAR-KDGK                         
303400     MOVE ART-KDPRODSL               TO SPAR-KDPRODSL-ARTC                
303500                                                                          
303600     IF MID-PRISDEL = ALL '+'                                             
303700        MOVE ART-IDFTG TO SPAR-IDLKTO-FF                                  
303800     ELSE                                                                 
303900       IF MID-KDKOPTYP NOT = 'R'                                          
304000        IF MID-IDLEVNR = ALL '+'                                          
304100           IF ART-IDLEVNR NOT = SPACE                                     
304200              MOVE ART-IDLEVNR       TO SPAR-IDLEVNR                      
304300              PERFORM FB-HTR-2221-IDLEVNR-BYTE                            
304400           ELSE                                                           
304500              MOVE NYPON-ART-IDLEVNR    TO ART-IDLEVNR                    
304600                                           SPAR-IDLEVNR                   
304700              MOVE JA                   TO FL-SHIP-UPD                    
304800              PERFORM IMS-REPL-ARTC                                       
304900              PERFORM FB-HTR-2221-IDLEVNR-BYTE                            
305000           END-IF                                                         
305100        ELSE                                                              
305200           MOVE MID-IDLEVNR             TO ART-IDLEVNR                    
305300                                           SPAR-IDLEVNR                   
305400           MOVE JA                      TO FL-SHIP-UPD                    
305500           PERFORM IMS-REPL-ARTC                                          
305600           PERFORM FB-HTR-2221-IDLEVNR-BYTE                               
305700        END-IF                                                            
305800      END-IF                                                              
305900     END-IF                                                               
306000                                                                          
306100     MOVE SPAR-IDLEVNR               TO MOD-IDLEVNR                       
306200     MOVE SPAR-IDLKTO-FF             TO MOD-IDFTG                         
306300                                                                          
306400*WDK611                                                                   
306500*                                                                         
306600     PERFORM IMS-GHNP-ARTC11                                              
306700     IF FL-SHIP-UPD = JA                                                  
306800       MOVE SPAR-IDLEVNR             TO CLAG-IDLEVNR-SHIP                 
306900     END-IF                                                               
307000     MOVE CLAG-IDBERED               TO MOD-IDBERED                       
307100                                        SPAR-IDBERED                      
307200     MOVE CLAG-IDPROJ                TO MOD-IDPROJ                        
307300                                        WS-IDPROJ                         
307400     MOVE CLAG-IDKAT (1)             TO MOD-IDKAT-1                       
307500     MOVE CLAG-IDKAT (2)             TO MOD-IDKAT-2                       
307600     MOVE CLAG-IDKAT (3)             TO MOD-IDKAT-3                       
307700     MOVE CLAG-FLLSRDEL              TO SPAR-FLLSRDEL                     
307800     MOVE CLAG-KDUART                TO SPAR-KDUART                       
307900                                                                          
308000     IF CLAG-IDPROENH(1) = SPACE OR ZERO                                  
308100        IF CLAG-IDPROENH(2) = SPACE OR ZERO                               
308200           IF CLAG-IDPROENH(3) = SPACE OR ZERO                            
308300              MOVE MFS-RENSA-FAELT    TO MOD-IDPROENH                     
308400           ELSE                                                           
308500              MOVE CLAG-IDPROENH(3)   TO MOD-IDPROENH                     
308600           END-IF                                                         
308700        ELSE                                                              
308800           MOVE CLAG-IDPROENH(2)      TO MOD-IDPROENH                     
308900        END-IF                                                            
309000     ELSE                                                                 
309100        MOVE CLAG-IDPROENH(1)         TO MOD-IDPROENH                     
309200     END-IF                                                               
309300                                                                          
309400     IF MID-PRISDEL = ALL '+'                                             
309500        CONTINUE                                                          
309600     ELSE                                                                 
309700       IF MID-KDKOPTYP NOT = 'R'                                          
309800        IF MID-PRARTSTD = ALL '+'                                         
309900           MOVE NYPON-ART-PRARTBES      TO CLAG-PRARTSTD                  
310000                                           CLAG-PRINK                     
310100                                           CLAG-PRARTSJK                  
310200        ELSE                                                              
310300           MOVE SPAR-PRARTSTD-HDTAL     TO CLAG-PRARTSTD                  
310400                                           CLAG-PRINK                     
310500                                           CLAG-PRARTSJK                  
310600        END-IF                                                            
310700        MOVE ZERO TO CLAG-PRHEMTAG                                        
310800        PERFORM FG-LOGGA-ART-PRISANDRING                                  
310900                                                                          
310910        IF MID-KDTIPPR = ALL '+'                                          
310920           IF KDPRODSL-SKALL-KOPA                                         
310930              IF SW-ARTIKEL-FINNS-PA-NYPON = JA                           
310940                 IF NYPON-ART-KDSTAINK = 3                                
310950                    MOVE 3                 TO CLAG-KDTIPPR                
310960                 ELSE                                                     
310970                   IF NYPON-ART-KDSTAINK = 1                              
310980                     MOVE 1                TO CLAG-KDTIPPR                
310990                   ELSE                                                   
310991                     MOVE 0                TO CLAG-KDTIPPR                
310992                   END-IF                                                 
310993                 END-IF                                                   
310994              ELSE                                                        
310995                 MOVE 0                 TO CLAG-KDTIPPR                   
310996              END-IF                                                      
310997           ELSE                                                           
310998              MOVE 0                    TO CLAG-KDTIPPR                   
310999           END-IF                                                         
311000        ELSE                                                              
311001           IF MID-KDTIPPR = NEJ                                           
311002              MOVE 1                    TO CLAG-KDTIPPR                   
311003           ELSE                                                           
311004             IF MID-KDTIPPR = JA OR YES                                   
311005               MOVE 0                   TO CLAG-KDTIPPR                   
311006             ELSE                                                         
311007               IF MID-KDTIPPR = WS-A                                      
311008                 MOVE 3                 TO CLAG-KDTIPPR                   
311009               END-IF                                                     
311010             END-IF                                                       
311011           END-IF                                                         
311012        END-IF                                                            
313100                                                                          
313200        MOVE SPAR-IDLKTO                TO CLAG-IDLKTO                    
313300                                                                          
313400        MOVE SPAR-KVPB-C1-HDTAL         TO CLAG-KVPB-SEP                  
313500                                           CLAG-KVPB-HIST                 
313600        IF MID-TIPBLOCK NOT = ALL '+'                                     
313700           MOVE MID-TIPBLOCK            TO CLAG-TIPBLOCK                  
313800        END-IF                                                            
313900        IF MID-FLMPB-C1 = ALL '+'                                         
314000          IF SPAR-KVPB-C1-HDTAL > ZERO                                    
314100            MOVE JA                     TO CLAG-FLMPB                     
314200          ELSE                                                            
314300            MOVE NEJ                    TO CLAG-FLMPB                     
314400          END-IF                                                          
314500        ELSE                                                              
314600           MOVE MID-FLMPB-C1            TO CLAG-FLMPB                     
314700        END-IF                                                            
314800                                                                          
314900        MOVE 'IDAG  '                   TO DAT-KDDATFORM                  
315000        PERFORM S99-WDATKONV                                              
315100        MOVE DAT-TIAAVVD                TO CLAG-TIPBDAT                   
315200                                                                          
315300        COMPUTE CLAG-KVMAD-SEP = CLAG-KVPB-SEP ** 0.85                    
315400        MOVE CLAG-KVMAD-SEP             TO CLAG-KVMAD-TOT                 
315500                                                                          
315600        MOVE SPAR-RESLJUST-C1           TO CLAG-RESLJUST                  
315700        MOVE SPAR-TISLJUST-C1           TO CLAG-TISLJUST                  
315800                                                                          
315900        IF MID-IDPLANGR-LEV = ALL '+'                                     
316000           MOVE 1                       TO CLAG-IDPLANGR-LEV              
316100        ELSE                                                              
316200           MOVE MID-IDPLANGR-LEV        TO CLAG-IDPLANGR-LEV              
316300        END-IF                                                            
316400                                                                          
316500        IF MID-IDPLANGR-AG = ALL '+'                                      
316600           EVALUATE TRUE                                                  
316700              WHEN SPAR-KDPRODSL-KPS = 11 OR 21 OR 91                     
316800                 MOVE 1                 TO CLAG-IDPLANGR-AG               
316900              WHEN SPAR-KDPRODSL-KPS = 13 OR 23 OR 93                     
317000                 MOVE 1                 TO CLAG-IDPLANGR-AG               
317100              WHEN SPAR-KDPRODSL-KPS = 14 OR 24 OR 94                     
317200                 MOVE 4                 TO CLAG-IDPLANGR-AG               
317300              WHEN SPAR-KDPRODSL-KPS = 15 OR 25 OR 95                     
317400                 MOVE 3                 TO CLAG-IDPLANGR-AG               
317500              WHEN SPAR-KDPRODSL-KPS = 16 OR 26 OR 96                     
317600                 MOVE 3                 TO CLAG-IDPLANGR-AG               
317700              WHEN SPAR-KDPRODSL-KPS = 17 OR 27 OR 97                     
317800                 MOVE 3                 TO CLAG-IDPLANGR-AG               
317900              WHEN SPAR-KDPRODSL-KPS = 18 OR 28 OR 98                     
318000                 MOVE 3                 TO CLAG-IDPLANGR-AG               
318100              WHEN SPAR-KDPRODSL-KPS = 19 OR 29 OR 99                     
318200                 MOVE 9                 TO CLAG-IDPLANGR-AG               
318300              WHEN OTHER                                                  
318400                 MOVE 9                 TO CLAG-IDPLANGR-AG               
318500           END-EVALUATE                                                   
318600                                                                          
318700           IF SPAR-KDPRODSL-KPS = 11 OR 13 OR 14 OR 15 OR                 
318800                                  16 OR 17 OR 18 OR 19                    
318900                                  OR 21 OR 23 OR 24 OR 25 OR 26 OR        
319000                                  27 OR 28 OR 29                          
319100                                  OR 91 OR 93 OR 94 OR 95 OR 96 OR        
319200                                  97 OR 98 OR 99                          
319300             IF SPAR-IDLEVNR = '1002' OR '8261' OR '8265'                 
319400                 MOVE 9 TO CLAG-IDPLANGR-AG                               
319500             END-IF                                                       
319600           END-IF                                                         
319700        ELSE                                                              
319800           MOVE MID-IDPLANGR-AG         TO CLAG-IDPLANGR-AG               
319900        END-IF                                                            
320000                                                                          
320100        IF SPAR-IDLEVNR = '9996' OR '9997' OR '9998' OR '9999'            
320200           CONTINUE                                                       
320300        ELSE                                                              
320400           MOVE SPAR-IDLEVNR            TO W-IDLEVNR                      
320500           PERFORM IMS-GU-LEVA01                                          
320600           MOVE LEVA01-LEV-KDGK         TO SPAR-KDGK                      
320700        END-IF                                                            
320800                                                                          
320900        IF MID-IDANSK = ALL '+'                                           
321000           IF (SPAR-IDLEVNR = '9996' OR '9997' OR                         
321100                             '9998' OR '9999') OR                         
321200              (CLAG-IDPLANGR-AG = 9)                                      
321300              MOVE NYPON-ART-IDANSK     TO CLAG-IDANSK                    
321400           ELSE                                                           
321500              MOVE LEVA01-LEV-IDANSK-PG (CLAG-IDPLANGR-AG)                
321600                                        TO CLAG-IDANSK                    
321700           END-IF                                                         
321800        ELSE                                                              
321900           MOVE MID-IDANSK              TO CLAG-IDANSK                    
322000        END-IF                                                            
322100                                                                          
322200        MOVE CLAG-IDANSK                TO NYPON-ART-IDANSK               
322300        MOVE SPAR-IDINK                 TO CLAG-IDINK                     
322400                                                                          
322500        IF MID-KDHF = ALL '+'                                             
322600           MOVE ZERO                    TO CLAG-KDHF                      
322700        ELSE                                                              
322800           MOVE MID-KDHF                TO CLAG-KDHF                      
322900           IF MID-KDHF = 2 OR 3 OR 4                                      
323000              MOVE 8                    TO CLAG-KVVECKOR-LT               
323100              MOVE 16                   TO CLAG-KVVECKOR-AT               
323200           END-IF                                                         
323300                                                                          
323400           IF MID-KDHF > ZERO                                             
323500              MOVE 4                    TO CLAG-KDAVT                     
323600           END-IF                                                         
323700        END-IF                                                            
323800                                                                          
323900        IF SPAR-IDLEVNR = '1000'  OR '1002'                               
324000           MOVE 1                       TO CLAG-KDKSP                     
324100*---       S A T S - A R T I K E L                                        
324200        ELSE                                                              
324300           MOVE SPAR-IDLEVNR               TO WS-IDLEVNR-KONCERN          
324400           IF (SPAR-KDPRODSL-KPS = 14 OR 24 OR 94)  OR                    
324500              (CLAG-KDHF        =  1 OR  2 OR  3 OR  4) OR                
324600              KONCERN-LEV                                                 
324700              MOVE ZERO                    TO CLAG-KDKSP                  
324800*---          B Y T E S  - A R T I K E L / HF / K O N C E R N             
324900           ELSE                                                           
325000              MOVE 2                       TO CLAG-KDKSP                  
325100           END-IF                                                         
325200        END-IF                                                            
325300        MOVE SPAR-KDGK                     TO CLAG-KDGK                   
325400                                                                          
325500        PERFORM IMS-REPL-ARTC                                             
325600                                                                          
325700       END-IF                                                             
325800     END-IF                                                               
325900                                                                          
326000     MOVE CLAG-IDPLANGR-AG              TO MOD-IDPLANGR-AG                
326100     MOVE CLAG-IDPLANGR-LEV             TO MOD-IDPLANGR-LEV               
326200     MOVE CLAG-IDANSK                   TO MOD-IDANSK SPAR-IDANSK         
326300     MOVE CLAG-KDHF                     TO MOD-KDHF                       
326400     MOVE CLAG-IDINK                    TO MOD-IDINK                      
326500     MOVE CLAG-KVPB-SEP                 TO MOD-KVPB-C1                    
326600     MOVE CLAG-TIPBLOCK                 TO MOD-TIPBLOCK                   
326700     MOVE CLAG-FLMPB                    TO MOD-FLMPB-C1                   
326800     MOVE CLAG-RESLJUST                 TO MOD-RESLJUST-C1                
326900     MOVE CLAG-PRARTSTD                 TO MOD-PRARTSTD                   
327000     MOVE CLAG-IDLKTO                   TO SPAR-IDLKTO                    
327100     MOVE SPAR-IDLKTO                   TO MOD-IDLKTO                     
327200                                                                          
327210     IF CLAG-KDTIPPR = 0                                                  
327220       MOVE YES                        TO MOD-KDTIPPR                     
327230     ELSE                                                                 
327240       IF CLAG-KDTIPPR = 1                                                
327250         MOVE NEJ                        TO MOD-KDTIPPR                   
327260       ELSE                                                               
327270         IF CLAG-KDTIPPR = 3                                              
327280           MOVE WS-A                    TO MOD-KDTIPPR                    
327290         END-IF                                                           
327291       END-IF                                                             
327300     END-IF                                                               
327800                                                                          
327900     IF CLAG-KDUART = 'P'                                                 
328000        IF MSGI-IDLAND-SPR = 'GB'                                         
328100           MOVE MED-104    TO MOD-TEMFSINF                                
328200        ELSE                                                              
328300           MOVE MED-4      TO MOD-TEMFSINF                                
328400        END-IF                                                            
328500     ELSE                                                                 
328600        IF CLAG-KDUART = 'M'                                              
328700           IF MSGI-IDLAND-SPR = 'GB'                                      
328800              MOVE MED-107    TO MOD-TEMFSINF                             
328900           ELSE                                                           
329000              MOVE MED-7      TO MOD-TEMFSINF                             
329100           END-IF                                                         
329200        ELSE                                                              
329300           IF CLAG-KDUART = 'S'                                           
329400              IF MSGI-IDLAND-SPR = 'GB'                                   
329500                 MOVE MED-108    TO MOD-TEMFSINF                          
329600              ELSE                                                        
329700                 MOVE MED-8      TO MOD-TEMFSINF                          
329800              END-IF                                                      
329900           END-IF                                                         
330000        END-IF                                                            
330100     END-IF                                                               
330200                                                                          
330300     IF CLAG-KDERS > 10                                                   
330400        IF MSGI-IDLAND-SPR = 'GB'                                         
330500           MOVE MED-106    TO MOD-TEMFSINF                                
330600        ELSE                                                              
330700           MOVE MED-6      TO MOD-TEMFSINF                                
330800        END-IF                                                            
330900     END-IF                                                               
331000                                                                          
331100*WDK625                                                                   
331200*                                                                         
331300     MOVE 1                             TO W-KDNOTTYP                     
331400     PERFORM IMS-GHNP-ARTC25                                              
331500     IF MID-IDANSK-NOT = ALL '+'                                          
331600        IF SEGMENT-FINNS                                                  
331700           MOVE NOT-TEARTNOT            TO MOD-IDANSK-NOT-IN              
331800        END-IF                                                            
331900     ELSE                                                                 
332000       IF MID-KDKOPTYP NOT = 'R'                                          
332100        IF MID-IDANSK-NOT = SPACE                                         
332200           IF SEGMENT-FINNS                                               
332300              PERFORM IMS-DELETE                                          
332400              MOVE SPACE                TO MOD-IDANSK-NOT-IN              
332500           END-IF                                                         
332600        ELSE                                                              
332700           MOVE MID-IDANSK-NOT          TO NOT-TEARTNOT                   
332800                                           MOD-IDANSK-NOT-IN              
332900           MOVE 1                       TO NOT-KDNOTTYP                   
333000           IF SEGMENT-FINNS                                               
333100              PERFORM IMS-REPL-ARTC                                       
333200           ELSE                                                           
333300              PERFORM IMS-ISRT-ARTC25                                     
333400           END-IF                                                         
333500         END-IF                                                           
333600        END-IF                                                            
333700     END-IF                                                               
333800                                                                          
333900     MOVE 6                          TO W-KDNOTTYP                        
334000     PERFORM IMS-GHNP-ARTC25                                              
334100     IF SEGMENT-FINNS                                                     
334200        MOVE NOT-TEARTNOT            TO MOD-VARNOT                        
334300     ELSE                                                                 
334400        MOVE MFS-RENSA-FAELT         TO MOD-VARNOT                        
334500     END-IF                                                               
334600                                                                          
334700*WDK901                                                                   
334800     PERFORM IMS-GU-ARTM01                                                
334900     IF SEGMENT-FINNS                                                     
335000        ADD ART-SUTPO-TOT       TO SPAR-SUTPO-TOT                         
335100        MOVE SPAR-SUTPO-TOT          TO MOD-KVBASL                        
335200     END-IF                                                               
335300                                                                          
335400     IF MID-PRISDEL = ALL '+'                                             
335500        CONTINUE                                                          
335600     ELSE                                                                 
335700                                                                          
335800*TTEST  PRISDELSUPPD HAR SKETT, USERID HÄMTAR VILKEN ANSKAFFARE           
335900*TTEST  SOM UTFÖRT UPPDATERINGEN                                          
336000                                                                          
336100        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
336200           MOVE SPAR-KDPRODSL-KPS          TO W-KDPRODSL2                 
336300                                                                          
336400           MOVE MSG-SIGNON-USERID          TO W-IDUSER                    
336500           PERFORM IMS-GU-WLXXAT11                                        
336600                                                                          
336700           IF SEGMENT-FINNS                                               
336800              MOVE XXAT-1138-IDANSK        TO NYPON-ART-IDANSK-REG        
336900           ELSE                                                           
337000              MOVE ZERO                    TO NYPON-ART-IDANSK-REG        
337100           END-IF                                                         
337200                                                                          
337300           MOVE SPAR-DAGENS-DATUM          TO NYPON-ART-TIANSKREG         
337400           MOVE ZERO                       TO NYPON-ART-KDANSKQ           
337500        END-IF                                                            
337600                                                                          
337700*       IF SPAR-KDPRODSL-ARTC = SPAR-KDPRODSL-KPS                         
337800*          CONTINUE                                                       
337900*       ELSE                                                              
338000*          PERFORM FD-KDPRODSL-BYTE                                       
338100*       END-IF                                                            
338200     END-IF                                                               
338300                                                                          
338400     IF SPAR-FLERS = JA                                                   
338500********TILLKOMMANDE ARTIKEL,HÄMTA DEN/DE ERSATTA                         
338600        PERFORM S06-LAS-ERSATTREG-ARTREG                                  
338700     ELSE                                                                 
338800        MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-ERS1                  
338900                                        MOD-KDERS-1                       
339000     END-IF                                                               
339100                                                                          
339200     PERFORM S07-LAS-BENREG                                               
339300                                                                          
339400     MOVE SPAR-KDSORT                TO MOD-KDSORT                        
339500                                                                          
339600     IF MSGI-IDLAND-SPR = 'GB'                                            
339700        MOVE MED-103    TO MOD-TEMFSINF                                   
339800     ELSE                                                                 
339900        MOVE MED-3      TO MOD-TEMFSINF                                   
340000     END-IF                                                               
340100                                                                          
340200     IF MID-KOPDEL = ALL '+'                                              
340300        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
340400           IF KDPRODSL-SKALL-KOPA                                         
340500             MOVE MID-IDLEVNR           TO WS-IDLEVNR-KONCERN             
340600             IF (KONCERN-LEV)    OR                                       
340700                (SPAR-MID-KDHF    = 1 OR 2 OR 3 OR 4)                     
340800*---            HÄNDELSE SKAPAS TILL INKÖP FÖR ATT TALA OM                
340900*---            ATT VI HAR HF/KONCERNMÄRKT ARTIKELN OCH ATT               
341000*---            INGET BEHOV FRÅN OSS KOMMER.                              
341100                IF KDPRODSL-SKALL-KOPA                                    
341200                  IF SPAR-KDPRODSL-ARTC > 90 AND                          
341300                    (SPAR-IDBERED = 65 OR 66)                             
341400                     CONTINUE                                             
341500                  ELSE                                                    
341600                     IF SPAR-IDINK (1:3) NUMERIC                          
341700                        MOVE SPAR-IDINK (1:3)    TO TEST-IDINK            
341800                     ELSE                                                 
341900                        IF SPAR-IDINK (2:3) NUMERIC                       
342000                           MOVE SPAR-IDINK (2:3) TO TEST-IDINK            
342100                        ELSE                                              
342200                           MOVE ZERO             TO TEST-IDINK            
342300                        END-IF                                            
342400                     END-IF                                               
342500**                   IF KDPRODSL-LYNK                                     
342600**                      CONTINUE                                          
342700**                   ELSE                                                 
342800                     IF NYPON-ART-KDANSKQ = '4'                           
342900                        MOVE '4' TO W-1142KEY-X                           
343000                        PERFORM IMS-GHU-WLXXAV11-GE                       
343100                        IF SEGMENT-FINNS                                  
343200                           PERFORM IMS-DELETE-1142                        
343300                        END-IF                                            
343400                     ELSE                                                 
343500                        PERFORM FH-HTR-1141-GAMLA                         
343600                     END-IF                                               
343700**                   END-IF                                               
343800                  END-IF                                                  
343900                END-IF                                                    
344000                MOVE '999999'           TO NYPON-ART-TILEVBEG             
344100                MOVE ZERO               TO NYPON-ART-KDANSKQ              
344200                MOVE SPAR-DAGENS-DATUM  TO NYPON-ART-TIINKOP              
344300             END-IF                                                       
344400           END-IF                                                         
344500        END-IF                                                            
344600     ELSE                                                                 
344700        IF MID-KDKOPTYP = 'R'                                             
344800           MOVE 'R'                  TO NYPON-ART-KDKOPTYP                
344900           MOVE ZERO                 TO NYPON-ART-TIMOTSI                 
345000        ELSE                                                              
345100           MOVE 'N'                  TO NYPON-ART-KDKOPTYP                
345200           MOVE ZERO                 TO NYPON-ART-TIMOTSI                 
345300        END-IF                                                            
345400                                                                          
345500        IF MID-TILEVBEG = ALL '+'                                         
345600           CONTINUE                                                       
345700        ELSE                                                              
345800           MOVE SPAR-TILEVBEG        TO NYPON-ART-TILEVBEG                
345900        END-IF                                                            
346000                                                                          
346100        IF MID-KVLEVBEG = ALL '+'                                         
346200           MOVE ZERO                 TO NYPON-ART-KVLEVBEG                
346300        ELSE                                                              
346400           MOVE MID-KVLEVBEG         TO NYPON-ART-KVLEVBEG                
346500        END-IF                                                            
346600                                                                          
346700        IF MID-KVPROG = ALL '+'                                           
346800           CONTINUE                                                       
346900        ELSE                                                              
347000           MOVE MID-KVPROG           TO NYPON-ART-KVPROG                  
347100        END-IF                                                            
347200        MOVE NYPON-ART-KVPROG        TO SPAR-KVPROG                       
347300                                                                          
347400        IF MID-TEANSINK = ALL '+'                                         
347500           MOVE SPACE                   TO NYPON-ART-TEANSINK             
347600        ELSE                                                              
347700           MOVE MID-TEANSINK            TO NYPON-ART-TEANSINK             
347800        END-IF                                                            
347900        EJECT                                                             
348000        IF MID-TILEVBEG = ALL '+'                                         
348100           CONTINUE                                                       
348200        ELSE                                                              
348300           IF KDPRODSL-SKALL-KOPA                                         
348400              IF (MID-TILEVBEG =  9999)                                   
348500*  * ---         HÄNDELSE TILL INKÖP SKAPAS FÖR ATT TALA OM ATT           
348600*  * ---         ARTIKELN I EFTERHAND BLIVIT HF/KONCERNMÄRKT OCH          
348700*  * ---         SKALL DÄRMED EJ KÖPAS AV INKÖP.                          
348800                 MOVE ZERO              TO NYPON-ART-KVLEVBEG             
348900                                           NYPON-ART-KVPROG               
349000                                           NYPON-ART-KDANSKQ              
349100                 MOVE SPACE             TO NYPON-ART-TEANSINK             
349200                 MOVE SPAR-DAGENS-DATUM TO NYPON-ART-TIINKOP              
349300                 MOVE 'D'               TO WS-KDSVAR                      
349400              ELSE                                                        
349500                 IF MID-TILEVBEG = ZERO                                   
349600*  * ---            HÄNDELSE TILL INKÖP  SKAPAS FÖR ATT TALA OM AT        
349700*  * ---            VI VILL ANNULERA ETT KÖP.                             
349800                    MOVE ZERO           TO NYPON-ART-KVLEVBEG             
349900                                           NYPON-ART-KVPROG               
350000                                           NYPON-ART-KDANSKQ              
350100                    MOVE SPACE          TO NYPON-ART-TEANSINK             
350200                    MOVE 'D'            TO WS-KDSVAR                      
350300                 ELSE                                                     
350400                   IF NYPON-ART-KDANSKQ = 2                               
350500                       MOVE 'C' TO WS-KDSVAR                              
350600                   ELSE                                                   
350700                       MOVE 'N' TO WS-KDSVAR                              
350800                   END-IF                                                 
350900                   MOVE SPAR-DAGENS-DATUM TO                              
351000                                         NYPON-ART-TIINKOP                
351100                   IF SPAR-KDPRODSL-ARTC = 25 OR 18                       
351200                     MOVE 2 TO NYPON-ART-KDANSKQ                          
351300                   ELSE                                                   
351400                     IF SPAR-KDSORT = 'SW'                                
351500                        MOVE 2 TO NYPON-ART-KDANSKQ                       
351600                     ELSE                                                 
351700                       IF (SPAR-KDEMBKOD  = 20 OR                         
351800                       SPAR-KDEMBKOD  = 25 OR                             
351900                       SPAR-KDEMBKOD  = 50 OR                             
352000                       SPAR-KDEMBKOD  = 80 OR                             
352100                       SPAR-KDEMBKOD  = 30 OR                             
352200                       SPAR-KDEMBKOD  = 35 OR                             
352300                       SPAR-KDEMBKOD  = 40 OR                             
352400                       SPAR-KDEMBKOD  = 45)                               
352500                       OR (SPAR-BEFT = 93 OR 95 OR 98 OR 99)              
352510                       OR KDPRODSL-LYNK                                   
352600                          MOVE 2 TO NYPON-ART-KDANSKQ                     
352700                       ELSE                                               
352800                          MOVE 4 TO NYPON-ART-KDANSKQ                     
352900                       END-IF                                             
353000                       IF (SPAR-KDPRODSL-ARTC = 19 OR 29 OR               
353010                                                39 OR 59)                 
353100                              AND (TEST-IDINK > 399 AND                   
353200                               TEST-IDINK < 500)                          
353300                          MOVE 2 TO NYPON-ART-KDANSKQ                     
353400                       END-IF                                             
353500                     END-IF                                               
353600                   END-IF                                                 
353700                 END-IF                                                   
353800              END-IF                                                      
353900              IF  KDPRODSL-SKALL-KOPA                                     
354000**            AND (NOT KDPRODSL-LYNK )                                    
354100                IF SPAR-KDPRODSL-ARTC > 90 AND                            
354200                  (SPAR-IDBERED = 65 OR 66)                               
354300                   CONTINUE                                               
354400                ELSE                                                      
354500                   IF SPAR-IDINK (1:3) NUMERIC                            
354600                      MOVE SPAR-IDINK (1:3)    TO TEST-IDINK              
354700                   ELSE                                                   
354800                      IF SPAR-IDINK (2:3) NUMERIC                         
354900                         MOVE SPAR-IDINK (2:3) TO TEST-IDINK              
355000                      ELSE                                                
355100                         MOVE ZERO             TO TEST-IDINK              
355200                      END-IF                                              
355300                   END-IF                                                 
355400                                                                          
355500                   IF (SPAR-KDPRODSL-ARTC = 25) OR                        
355600                      (SPAR-KDSORT = 'SW') OR                             
355700                      (SPAR-KDPRODSL-ARTC = 18)                           
355800                                                                          
355900                     IF NYPON-ART-KDANSKQ NOT = 2                         
356000                        MOVE 2 TO NYPON-ART-KDANSKQ                       
356100                     END-IF                                               
356200                     PERFORM FH-HTR-1141-GAMLA                            
356300                   ELSE                                                   
356400                     IF (SPAR-KDPRODSL-ARTC =                             
356500                         19 OR 29 OR 59) AND                              
356600                        (TEST-IDINK > 399 AND                             
356700                         TEST-IDINK < 500)                                
356800                        MOVE 2 TO NYPON-ART-KDANSKQ                       
356900                        PERFORM FH-HTR-1141-GAMLA                         
357000                     ELSE                                                 
357100                        IF NYPON-ART-KDANSKQ = 0                          
357200                           MOVE '4' TO W-1142KEY-X                        
357300                           PERFORM IMS-GHU-WLXXAV11-GE                    
357400                           IF SEGMENT-FINNS                               
357500                              PERFORM IMS-DELETE-1142                     
357600                           ELSE                                           
357700                              PERFORM FH-HTR-1141-GAMLA                   
357800                           END-IF                                         
357900                        ELSE                                              
358000                           PERFORM FC-HTR-1141-TILL-INKOP-PV              
358100                        END-IF                                            
358200                     END-IF                                               
358300                   END-IF                                                 
358400                END-IF                                                    
358500              END-IF                                                      
358600           END-IF                                                         
358700                                                                          
358800        END-IF                                                            
358900                                                                          
359000     END-IF                                                               
359100                                                                          
359200     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
359300        PERFORM IMS-REPL-NYPON                                            
359400                                                                          
359500        MOVE NYPON-ART-IDANSK-REG    TO MOD-IDANSK-REG                    
359600        MOVE NYPON-ART-TIANSKREG     TO MOD-TIANSKREG                     
359700        MOVE NYPON-ART-TEORSAK       TO MOD-TEORSAK                       
359800        MOVE NYPON-ART-IDPROJK       TO MOD-IDPROJK                       
359900        MOVE NYPON-ART-KVARTAR1      TO MOD-KVARTAR1                      
360000        MOVE NYPON-ART-KVARTAR2      TO MOD-KVARTAR2                      
360100        MOVE NYPON-ART-KVARTAR3      TO MOD-KVARTAR3                      
360200        MOVE NYPON-ART-FLBYTES       TO MOD-FLBYTES                       
360300        MOVE NYPON-ART-KVARTVAGN     TO MOD-KVARTVAGN                     
360400        MOVE NYPON-ART-IDMATKTO      TO MOD-IDMATKTO                      
360500        MOVE NYPON-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV                 
360600        MOVE NYPON-ART-IDAVD         TO MOD-IDAVD                         
360700        MOVE NYPON-ART-KVPROG        TO MOD-KVPROG                        
360800                                                                          
360900        IF NYPON-ART-KVBASL > ZERO                                        
361000           MOVE MFS-ADD-LYS-UPP-FAELT                                     
361100                                     TO MOD-KVBASL-ATTR                   
361200        END-IF                                                            
361300                                                                          
361400        MOVE NYPON-ART-KDSTAINK      TO MOD-KDSTAINK                      
361500                                                                          
361600        IF NYPON-ART-TISTOMREG = ZERO                                     
361700           MOVE MFS-RENSA-FAELT         TO MOD-KVBASL-KLAR                
361800        ELSE                                                              
361900           MOVE NYPON-ART-TISTOMREG   TO TMP1-YYMMDD                      
362000           MOVE SPAR-DAGENS-DATUM     TO TMP2-YYMMDD                      
362100           PERFORM WY2000P1                                               
362200           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
362300              MOVE '*'                  TO MOD-KVBASL-KLAR                
362400           ELSE                                                           
362500              MOVE MFS-RENSA-FAELT      TO MOD-KVBASL-KLAR                
362600           END-IF                                                         
362700        END-IF                                                            
362800     ELSE                                                                 
362900       IF MID-KDKOPTYP = 'R'                                              
363000          PERFORM FF-ISRT-NYPON                                           
363100       END-IF                                                             
363200     END-IF                                                               
363300                                                                          
363400     PERFORM S03-RENSA-MOD-INMATNINGSFAELT                                
363500     PERFORM S05-FORMATETS-ATTRIBUT                                       
363600                                                                          
363700     .                                                                    
363800     EJECT                                                                
363900 FB-HTR-2221-IDLEVNR-BYTE SECTION.                                        
364000     SKIP3                                                                
364100     MOVE JA                         TO SW-IDLEVNR-BYTE                   
364200     MOVE SPACE                      TO XXBN-2222-WDGX2222                
364300     MOVE W-IDARTNR                  TO XXBN-2222-IDARTNR                 
364400     MOVE SPAR-IDLEVNR               TO XXBN-2222-IDLEVNR                 
364500     MOVE 'W201'                     TO XXBN-2222-IDSYSTEM                
364600                                                                          
364700     PERFORM IMS-ISRT-XXBN                                                
364800     .                                                                    
364900     EJECT                                                                
365000 FC-HTR-1141-TILL-INKOP-PV SECTION.                                       
365100     SKIP3                                                                
365200     IF (SPAR-KDEMBKOD  = 20 OR                                           
365300        SPAR-KDEMBKOD  = 25 OR                                            
365400        SPAR-KDEMBKOD  = 50 OR                                            
365500        SPAR-KDEMBKOD  = 80 OR                                            
365600        SPAR-KDEMBKOD  = 30 OR                                            
365700        SPAR-KDEMBKOD  = 35 OR                                            
365800        SPAR-KDEMBKOD  = 40 OR                                            
365900        SPAR-KDEMBKOD  = 45)                                              
366000     OR (SPAR-BEFT = 93 OR 95 OR 98 OR 99)                                
366010     OR KDPRODSL-LYNK                                                     
366100                                                                          
366200        MOVE '1'       TO W-1142KEY-X                                     
366300                                                                          
366400        PERFORM IMS-GU-WLXXAV11                                           
366500                                                                          
366600        IF SEGMENT-SAKNAS                                                 
366700           MOVE SPACE                   TO XXAV-1142-WDGX1142             
366800           MOVE W-1142KEY-X             TO XXAV-1142-KDSEGKEY             
366900           MOVE W-IDARTNR               TO XXAV-1142-IDARTNR              
367000           PERFORM IMS-ISRT-XXAV11                                        
367100        END-IF                                                            
367200                                                                          
367300     ELSE                                                                 
367400                                                                          
367500        MOVE '1'       TO W-1142KEY-X                                     
367600        PERFORM IMS-GHU-WLXXAV11-GE                                       
367700                                                                          
367800        IF SEGMENT-SAKNAS                                                 
367900           MOVE '4'                     TO W-1142KEY-X                    
368000           PERFORM IMS-GHU-WLXXAV11-GE                                    
368100           IF SEGMENT-SAKNAS                                              
368200             MOVE '4'                     TO W-1142KEY-X                  
368300             MOVE SPACE                   TO XXAV-1142-WDGX1142           
368400             MOVE W-1142KEY-X             TO XXAV-1142-KDSEGKEY           
368500             MOVE W-IDARTNR               TO XXAV-1142-IDARTNR            
368600             PERFORM IMS-ISRT-XXAV11                                      
368700           END-IF                                                         
368800        ELSE                                                              
368900           MOVE '4'                     TO XXAV-1142-KDSEGKEY             
369000           PERFORM IMS-REPL-XXAV11                                        
369100        END-IF                                                            
369200                                                                          
369300     END-IF                                                               
369400                                                                          
369500                                                                          
369600     IF MSGI-IDLAND-SPR = 'GB'                                            
369700        MOVE MED-112    TO MOD-TEMFSINF                                   
369800     ELSE                                                                 
369900        MOVE MED-12     TO MOD-TEMFSINF                                   
370000     END-IF                                                               
370100     .                                                                    
370200     EJECT                                                                
370300 FD-KDPRODSL-BYTE SECTION.                                                
370400     SKIP2                                                                
370500                                                                          
370600**   IF ''SW-ARTIKEL-TILL-BASL = JA'' KAN ALDRIG BLI UPPFYLLD,            
370700**   DÅ DEN ALDRIG BYTER VÄRDE FRÅN VALUE 'N'                             
370800*    IF SW-ARTIKEL-TILL-BASL = JA                                         
370900*      IF SPAR-FLLSRDEL      = JA  AND SPAR-KDUART = SPACE                
371000*          KONTOT GAV ETT "BASL-PRODUKTSLAG" OCH OVANSTÅENDE              
371100*          TEST ÄR UPPFYLLD                                               
371200*          MOVE 1                 TO NYPON-ART-DABASL                     
371300*      END-IF                                                             
371400*    END-IF                                                               
371500*    .                                                                    
371600     EJECT                                                                
371700 FF-ISRT-NYPON SECTION.                                                   
371800     SKIP2                                                                
371900     MOVE SPACE                      TO                                   
372000                                        NYPON-ART-FLAENDR                 
372100                                        NYPON-ART-FLBASL                  
372200                                        NYPON-ART-FLBERQ                  
372300                                        NYPON-ART-FLRITB                  
372400                                        NYPON-ART-FLRITC                  
372500                                        NYPON-ART-FLRITP                  
372600                                        NYPON-ART-KDARTUTG                
372700                                        NYPON-ART-FLUPG                   
372800                                        NYPON-ART-KDTPD                   
372900                                        NYPON-ART-FLUPB                   
373000                                        NYPON-ART-FLPLAKOP                
373100                                        NYPON-ART-FLUNIKRD                
373200                                        NYPON-ART-IDMATKTO                
373300                                        NYPON-ART-IDPROJOBJ               
373400                                        NYPON-ART-IDRITUTG                
373500                                        NYPON-ART-KDARTTYP                
373600                                        NYPON-ART-KDRESBED                
373700                                        NYPON-ART-TETEKNIK                
373800                                        NYPON-ART-TEANSINK                
373900                                        NYPON-ART-TEARTNOT-BASL           
374000                                        NYPON-ART-TEARTNOT                
374100                                        NYPON-ART-FLBYTES                 
374200                                        NYPON-ART-IDAO                    
374300                                        NYPON-ART-IDPROENH                
374400                                        NYPON-ART-IDPROJ                  
374500                                        NYPON-ART-IDPROJK                 
374600                                        NYPON-ART-IDRITN                  
374700                                        NYPON-ART-TEORSAK                 
374800                                        NYPON-ART-IDLEVNR                 
374900                                        NYPON-ART-IDSTEKN                 
375000                                        NYPON-ART-IDLEVNR-FORB(1)         
375100                                        NYPON-ART-IDLEVNR-FORB(2)         
375200                                        NYPON-ART-IDLEVNR-FORB(3)         
375300                                        NYPON-ART-IDLEVNR-FORB(4)         
375400                                        NYPON-ART-IDLEVNR-FORB(5)         
375500     MOVE ZERO                       TO NYPON-ART-IDANSK                  
375600                                        NYPON-ART-IDINK                   
375700                                        NYPON-ART-IDINKTEK                
375800                                        NYPON-ART-IDAVD                   
375900                                        NYPON-ART-IDANSK-REG              
376000                                        NYPON-ART-KDSTAINK                
376100                                        NYPON-ART-KVARTAR1                
376200                                        NYPON-ART-KVARTAR2                
376300                                        NYPON-ART-KVARTAR3                
376400                                        NYPON-ART-KVBASL                  
376500                                        NYPON-ART-KVLEVBEG                
376600                                        NYPON-ART-KVPROG                  
376700                                        NYPON-ART-KVUPB                   
376800                                        NYPON-ART-PRARTBES                
376900                                        NYPON-ART-DABASL                  
377000                                        NYPON-ART-TILEVBEG                
377100                                        NYPON-ART-TINEDBRY                
377200                                        NYPON-ART-TIPLAKOP                
377300                                        NYPON-ART-TIREGDAT                
377400                                        NYPON-ART-TIANSKREG               
377500                                        NYPON-ART-TIRITB                  
377600                                        NYPON-ART-TIRITC                  
377700                                        NYPON-ART-TIRITP                  
377800                                        NYPON-ART-TISERLEV(1)             
377900                                        NYPON-ART-TISERLEV(2)             
378000                                        NYPON-ART-TISERLEV(3)             
378100                                        NYPON-ART-TISERLEV(4)             
378200                                        NYPON-ART-TISERLEV(5)             
378300                                        NYPON-ART-TISLUBER                
378400                                        NYPON-ART-TISTABER                
378500                                        NYPON-ART-TISTOMREG               
378600                                        NYPON-ART-TIUPPDAT                
378700                                        NYPON-ART-TIUPB                   
378800                                        NYPON-ART-TIUPG                   
378900                                        NYPON-ART-IDARTNR-MOTSV           
379000                                        NYPON-ART-IDBERED                 
379100                                        NYPON-ART-KVARTVAGN               
379200                                        NYPON-ART-DAFINLEV                
379300                                        NYPON-ART-TITPD                   
379400                                        NYPON-ART-TIMOTSI                 
379500                                                                          
379600     MOVE W-IDARTNR                 TO  NYPON-ART-IDARTNR                 
379700     MOVE SPAR-IDLEVNR              TO  NYPON-ART-IDLEVNR                 
379800     MOVE SPAR-IDANSK               TO  NYPON-ART-IDANSK                  
379900     MOVE SPAR-IDINK                TO  NYPON-ART-IDINK                   
380000     MOVE SPAR-KDSORT               TO  NYPON-ART-KDSORT                  
380100     MOVE SPAR-IDFKNGRP             TO  NYPON-ART-IDFKNGRP                
380200     MOVE SPAR-TILEVBEG             TO  NYPON-ART-TILEVBEG                
380300     MOVE SPAR-DAGENS-DATUM         TO  NYPON-ART-TIINKOP                 
380400     MOVE SPAR-KDPRODSL-ARTC        TO  NYPON-ART-KDPRODSL                
380500     MOVE MID-KVLEVBEG              TO  NYPON-ART-KVLEVBEG                
380600     MOVE MID-KVPROG                TO  NYPON-ART-KVPROG                  
380700     MOVE MOD-BEART                 TO  NYPON-ART-BEART-SVE               
380800     MOVE 'N'                       TO  NYPON-ART-FLPISK                  
380900                                                                          
381000     IF SPAR-KDSORT = 'SW'                                                
381100        MOVE 2 TO NYPON-ART-KDANSKQ                                       
381200     ELSE                                                                 
381300       IF (SPAR-KDEMBKOD  = 20 OR                                         
381400       SPAR-KDEMBKOD  = 25 OR                                             
381500       SPAR-KDEMBKOD  = 50 OR                                             
381600       SPAR-KDEMBKOD  = 80 OR                                             
381700       SPAR-KDEMBKOD  = 30 OR                                             
381800       SPAR-KDEMBKOD  = 35 OR                                             
381900       SPAR-KDEMBKOD  = 40 OR                                             
382000       SPAR-KDEMBKOD  = 45)                                               
382100       OR (SPAR-BEFT = 93 OR 95 OR 98 OR 99)                              
382110       OR KDPRODSL-LYNK                                                   
382200          MOVE 2 TO NYPON-ART-KDANSKQ                                     
382300       ELSE                                                               
382400          MOVE 4 TO NYPON-ART-KDANSKQ                                     
382500       END-IF                                                             
382600     END-IF                                                               
382700                                                                          
382800     IF SPAR-IDINK (1:3) NUMERIC                                          
382900        MOVE SPAR-IDINK (1:3)    TO TEST-IDINK                            
383000     ELSE                                                                 
383100        IF SPAR-IDINK (2:3) NUMERIC                                       
383200           MOVE SPAR-IDINK (2:3) TO TEST-IDINK                            
383300        ELSE                                                              
383400           MOVE ZERO             TO TEST-IDINK                            
383500        END-IF                                                            
383600     END-IF                                                               
383700                                                                          
383800     IF SPAR-KDPRODSL-ARTC = 25 OR 18                                     
383900        MOVE 2 TO NYPON-ART-KDANSKQ                                       
384000     ELSE                                                                 
384100       IF (SPAR-KDPRODSL-ARTC = 19 OR 29 OR 39 OR 59) AND                 
384200          (TEST-IDINK > 399 AND TEST-IDINK < 500)                         
384300          MOVE 2 TO NYPON-ART-KDANSKQ                                     
384400       END-IF                                                             
384500     END-IF                                                               
384600                                                                          
384700     MOVE 'R'                       TO  NYPON-ART-KDKOPTYP                
384800     IF MID-TEANSINK = ALL '+'                                            
384900       MOVE SPACE                   TO NYPON-ART-TEANSINK                 
385000     ELSE                                                                 
385100       MOVE MID-TEANSINK            TO NYPON-ART-TEANSINK                 
385200     END-IF                                                               
385300                                                                          
385400     PERFORM IMS-ISRT-ARTG01                                              
385500     .                                                                    
385600     EJECT                                                                
385700 FG-LOGGA-ART-PRISANDRING SECTION.                                        
385800                                                                          
385900*WDH801                                                                   
386000     MOVE W-IDARTNR                  TO PRI-IDARTNR                       
386100     MOVE FUNCTION CURRENT-DATE(1:8) TO PRI-DAREGDAT                      
386200     MOVE FUNCTION CURRENT-DATE(9:7) TO PRI-TIREGTID                      
386300     MOVE 'J'                        TO PRI-FLKLAR                        
386400     MOVE 'N'                        TO PRI-FLPRFIL                       
386500     MOVE 'N'                        TO PRI-FLPRIBES                      
386600     MOVE 'N'                        TO PRI-FLPRIGO                       
386700     MOVE MSG-SIGNON-USERID          TO PRI-IDUSER                        
386800     MOVE 'A'                        TO PRI-KDPRIBEH                      
386900     MOVE ZERO                       TO PRI-REDIRLEV                      
387000     MOVE SPACE                      TO PRI-O-IDLEVNR-PR                  
387100                                        PRI-O-KDPRURSP                    
387200                                        PRI-O-KDVALISO                    
387300     MOVE ZERO                       TO PRI-O-KDSTATUS-PR                 
387400                                        PRI-O-PRARTBEL-PR                 
387500                                        PRI-O-PRARTBES-PR                 
387600                                        PRI-O-SUINLEV-PR                  
387700                                        PRI-O-TIPRLIST                    
387800     MOVE SPAR-IDLEVNR               TO PRI-N-IDLEVNR-PR                  
387900     MOVE SPACE                      TO PRI-N-KDPRURSP                    
388000                                        PRI-N-KDVALISO                    
388100     MOVE ZERO                       TO PRI-N-KDSTATUS-PR                 
388200                                        PRI-N-PRARTBEL-PR                 
388300                                        PRI-N-PRARTBES-PR                 
388400                                        PRI-N-SUINLEV-PR                  
388500                                        PRI-N-TIPRLIST                    
388600     MOVE ' '                        TO PRI-O-KDCMD                       
388700     MOVE ZERO                       TO PRI-O-PRARTBES                    
388800                                        PRI-O-PRARTSJK                    
388900                                        PRI-O-PRARTSTD                    
389000                                        PRI-O-PRDIRLON                    
389100                                        PRI-O-PRDMTRL                     
389200                                        PRI-O-PRINK                       
389300                                        PRI-O-PRLFKST                     
389400                                        PRI-O-PROVRPAL                    
389500                                        PRI-O-RETULF                      
389600     MOVE SPACE                      TO PRI-O-TEARTNOT                    
389700                                        PRI-N-KDCMD                       
389800     MOVE ZERO                       TO PRI-N-PRARTBES                    
389900     MOVE CLAG-PRARTSJK              TO PRI-N-PRARTSJK                    
390000     MOVE CLAG-PRARTSTD              TO PRI-N-PRARTSTD                    
390100     MOVE CLAG-PRINK                 TO PRI-N-PRINK                       
390200     MOVE ZERO                       TO PRI-N-PRDIRLON                    
390300                                        PRI-N-PRDMTRL                     
390400                                        PRI-N-PRLFKST                     
390500                                        PRI-N-PROVRPAL                    
390600                                        PRI-N-RETULF                      
390700     MOVE SPACE                      TO PRI-N-TEARTNOT                    
390800     PERFORM IMS-ISRT-WDH801                                              
390900     .                                                                    
391000     EJECT                                                                
391100 FH-HTR-1141-GAMLA SECTION.                                               
391200                                                                          
391300     IF SPAR-KDSORT = 'SW'                                                
391400       MOVE '1' TO W-1142KEY-X                                            
391500     ELSE                                                                 
391600       IF (SPAR-KDPRODSL-ARTC = 19 OR 29 OR 39 OR 59) AND                 
391700          (TEST-IDINK > 399 AND TEST-IDINK < 500)                         
391800          MOVE '5' TO W-1142KEY-X                                         
391900       ELSE                                                               
392000         IF SPAR-KDPRODSL-ARTC = 25 OR 18                                 
392100            MOVE '5' TO W-1142KEY-X                                       
392200         ELSE                                                             
392300            MOVE '1' TO W-1142KEY-X                                       
392400         END-IF                                                           
392500       END-IF                                                             
392600     END-IF                                                               
392700                                                                          
392800     PERFORM IMS-GU-WLXXAV11                                              
392900                                                                          
393000     IF SEGMENT-SAKNAS                                                    
393100        MOVE SPACE                   TO XXAV-1142-WDGX1142                
393200        MOVE W-1142KEY-X             TO XXAV-1142-KDSEGKEY                
393300        MOVE W-IDARTNR               TO XXAV-1142-IDARTNR                 
393400        PERFORM IMS-ISRT-XXAV11                                           
393500     END-IF                                                               
393600                                                                          
393700                                                                          
393800*--- TOOLS SKALL SKICKA 2 ST FILER TILL INKÖP,1 NAP + 1 NP-REQ            
393900     IF SPAR-KDPRODSL-ARTC = 18                                           
394000       MOVE '1' TO W-1142KEY-X                                            
394100       PERFORM IMS-GU-WLXXAV11                                            
394200                                                                          
394300       IF SEGMENT-SAKNAS                                                  
394400          MOVE SPACE                   TO XXAV-1142-WDGX1142              
394500          MOVE W-1142KEY-X             TO XXAV-1142-KDSEGKEY              
394600          MOVE W-IDARTNR               TO XXAV-1142-IDARTNR               
394700          PERFORM IMS-ISRT-XXAV11                                         
394800       END-IF                                                             
394900     END-IF                                                               
395000                                                                          
395100                                                                          
395200     IF MSGI-IDLAND-SPR = 'GB'                                            
395300        MOVE MED-112    TO MOD-TEMFSINF                                   
395400     ELSE                                                                 
395500        MOVE MED-12     TO MOD-TEMFSINF                                   
395600     END-IF                                                               
395700     .                                                                    
395800     EJECT                                                                
395900 G-FIXA-MIN-MAX-NYCKLAR SECTION.                                          
396000                                                                          
396100     MOVE SPAR-IDANSK-MIN               TO W-IDANSK-MIN                   
396200     MOVE SPAR-IDPROJ-MIN               TO W-IDPROJ-MIN                   
396300     MOVE SPAR-FLPISK-MIN               TO W-FLPISK-MIN                   
396400     MOVE SPAR-DAFINLEV-MIN             TO W-DAFINLEV-MIN                 
396500     MOVE SPAR-IDAO-MIN                 TO W-IDAO-MIN                     
396600                                                                          
396700     MOVE WS-IDARTNR                    TO W-IDARTNR-MIN                  
396800                                                                          
396900     MOVE HIGH-VALUE                    TO W-FLPISK-MAX                   
397000                                           W-IDAO-MAX                     
397100     MOVE ALL '9'                       TO W-DAFINLEV-MAX                 
397200                                           W-IDARTNR-MAX                  
397300     IF MID-IDANSK-FOM > MID-IDANSK-TOM                                   
397400        MOVE MID-IDANSK-FOM             TO W-IDANSK-MAX                   
397500     ELSE                                                                 
397600        MOVE MID-IDANSK-TOM             TO W-IDANSK-MAX                   
397700     END-IF                                                               
397800                                                                          
397900     IF MID-IDPROJ-VALT = SPACE                                           
398000        MOVE HIGH-VALUE                 TO W-IDPROJ-MAX                   
398100        PERFORM IMS-GU-ARTI01-UTAN-SOK                                    
398200     ELSE                                                                 
398300        MOVE MID-IDPROJ-VALT            TO W-IDPROJ-MAX                   
398400        PERFORM IMS-GU-ARTI01-MED-SOK                                     
398500     END-IF                                                               
398600                                                                          
398700     IF SEGMENT-FINNS                                                     
398800        MOVE ARTI-SEQB-IDARTNR          TO WS-IDARTNR                     
398900                                           MOD-IDARTNR-UT                 
399000        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
399100     ELSE                                                                 
399200        IF MSGI-IDLAND-SPR = 'GB'                                         
399300           MOVE MED-110    TO MOD-TEMFSINF                                
399400        ELSE                                                              
399500           MOVE MED-10     TO MOD-TEMFSINF                                
399600        END-IF                                                            
399700     END-IF                                                               
399800     .                                                                    
399900     EJECT                                                                
400000 H-FIXA-MIN-NYCKLAR SECTION.                                              
400100     SKIP2                                                                
400200     MOVE SPAR-IDANSK-FOM      TO MOD-IDANSK-FOM                          
400300     MOVE SPAR-IDANSK-TOM      TO MOD-IDANSK-TOM                          
400400     MOVE SPAR-IDPROJ-VALT     TO MOD-IDPROJ-VALT                         
400500                                                                          
400600     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
400700        MOVE NYPON-ART-IDANSK     TO MOD-IDANSK-MIN                       
400800        MOVE NYPON-ART-IDPROJ     TO MOD-IDPROJ-MIN                       
400900        MOVE NYPON-ART-FLPISK     TO MOD-FLPISK-MIN                       
401000        MOVE NYPON-ART-DAFINLEV (3:6)  TO MOD-TIFINLEV-MIN                
401100        MOVE NYPON-ART-IDAO       TO MOD-IDAO-MIN                         
401200     ELSE                                                                 
401300        MOVE SPAR-IDANSK-MIN      TO MOD-IDANSK-MIN                       
401400        MOVE SPAR-DAFINLEV-MIN (3:6)  TO MOD-TIFINLEV-MIN                 
401500        MOVE SPAR-IDPROJ-MIN      TO MOD-IDPROJ-MIN                       
401600        MOVE SPAR-FLPISK-MIN      TO MOD-FLPISK-MIN                       
401700        MOVE SPAR-IDAO-MIN        TO MOD-IDAO-MIN                         
401800     END-IF                                                               
401900     .                                                                    
402000     EJECT                                                                
402100 S02-ROER-EJ-FAELT-OVRIGA SECTION.                                        
402200     SKIP3                                                                
402300     MOVE MFS-ROER-EJ-FAELT          TO MOD-BEART                         
402400                                        MOD-TIFINLV                       
402500                                        MOD-IDBERED                       
402600                                        MOD-TEORSAK                       
402700                                        MOD-IDPROJ                        
402800                                        MOD-IDAO                          
402900                                        MOD-IDKAT-1                       
403000                                        MOD-IDKAT-2                       
403100                                        MOD-IDKAT-3                       
403200                                        MOD-IDPROENH                      
403300                                        MOD-VARNOT                        
403400                                        MOD-IDARTNR-ERS1                  
403500                                        MOD-KDERS-1                       
403600                                        MOD-IDLEVNR                       
403700                                        MOD-IDPLANGR-AG                   
403800                                        MOD-IDPLANGR-LEV                  
403900                                        MOD-IDANSK                        
404000                                        MOD-PRARTSTD                      
404100                                        MOD-KDTIPPR                       
404200                                        MOD-IDFTG                         
404300                                        MOD-KVPROG                        
404400                                                                          
404500*FLYTTAS TILL MOD-IDLKTO-POS3-7 PGA AV ATT                                
404600*DE TVÅ FÖRSTA BYTEN AV IDLKTO ENDAST LIGGER                              
404700*I MFS:EN OCH ALLTSÅ INTE FINNS 'PÅ SKÄRMEN'                              
404800                                        MOD-IDLKTO-POS3-7                 
404900                                                                          
405000                                        MOD-KDSORT                        
405100                                        MOD-KDHF                          
405200                                        MOD-TIPBLOCK                      
405300                                        MOD-FLMPB-C1                      
405400                                        MOD-KVPB-C1                       
405500                                        MOD-RESLJUST-C1                   
405600                                        MOD-IDANSK-NOT-IN                 
405700                                        MOD-MFL                           
405800                                        MOD-IDINK                         
405900                                                                          
406000*FÄLT FRÅN NYPON:                                                         
406100                                        MOD-IDARTNR-MOTSV                 
406200                                        MOD-IDPROJK                       
406300                                        MOD-KVARTAR1                      
406400                                        MOD-KVARTAR2                      
406500                                        MOD-KVARTAR3                      
406600                                        MOD-KVARTVAGN                     
406700                                        MOD-FLBYTES                       
406800                                        MOD-KVBASL-KLAR                   
406900                                        MOD-IDANSK-REG                    
407000                                        MOD-TIANSKREG                     
407100                                        MOD-TEORSAK                       
407200                                        MOD-IDMATKTO                      
407300                                        MOD-IDAVD                         
407400                                        MOD-KVBASL                        
407500                                        MOD-KDSTAINK.                     
407600     EJECT                                                                
407700 S03-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
407800     SKIP3                                                                
407900     MOVE MFS-RENSA-FAELT             TO MOD-IDLEVNR-IN                   
408000                                         MOD-IDPLANGR-AG-IN               
408100                                         MOD-IDPLANGR-LEV-IN              
408200                                         MOD-IDANSK-IN                    
408300                                         MOD-PRARTSTD-IN                  
408400                                         MOD-KDTIPPR-IN                   
408500                                         MOD-IDFTG-IN                     
408600                                         MOD-IDLKTO-IN                    
408700                                         MOD-KVPB-C1-IN                   
408800                                         MOD-TIPBLOCK-IN                  
408900                                         MOD-FLMPB-C1-IN                  
409000                                         MOD-RESLJUST-C1-IN               
409100                                         MOD-KDHF-IN                      
409200                                         MOD-IDINK-IN                     
409300                                         MOD-KDKOPTYP-IN                  
409400                                         MOD-TILEVBEG-IN                  
409500                                         MOD-KVLEVBEG-IN                  
409600                                         MOD-KVPROG-IN                    
409700                                         MOD-TEANSINK-IN                  
409800     .                                                                    
409900     EJECT                                                                
410000 S04-RENSA-MOD-SIMULERINGSFAELT SECTION.                                  
410100     SKIP3                                                                
410200     MOVE MFS-RENSA-FAELT             TO MOD-KVSLAGER                     
410300                                         MOD-KVMP.                        
410400     EJECT                                                                
410500 S05-FORMATETS-ATTRIBUT SECTION.                                          
410600     SKIP3                                                                
410700     MOVE MFS-FORMATETS-ATTR         TO MOD-IDLEVNR-IN-ATTR               
410800                                        MOD-IDPLANGR-LEV-IN-ATTR          
410900                                        MOD-IDPLANGR-AG-IN-ATTR           
411000                                        MOD-IDANSK-IN-ATTR                
411100                                        MOD-PRARTSTD-IN-ATTR              
411200                                        MOD-KDTIPPR-IN-ATTR               
411300                                        MOD-IDFTG-IN-ATTR                 
411400                                        MOD-IDLKTO-IN-ATTR                
411500                                        MOD-KVPB-C1-IN-ATTR               
411600                                        MOD-TIPBLOCK-IN-ATTR              
411700                                        MOD-FLMPB-C1-IN-ATTR              
411800                                        MOD-KDHF-IN-ATTR                  
411900                                        MOD-RESLJUST-C1-IN-ATTR           
412000                                        MOD-IDANSK-NOT-IN-ATTR            
412100                                        MOD-IDINK-IN-ATTR                 
412200                                        MOD-KDKOPTYP-IN-ATTR              
412300                                        MOD-TILEVBEG-IN-ATTR              
412400                                        MOD-KVLEVBEG-IN-ATTR              
412500                                        MOD-KVPROG-IN-ATTR                
412600                                        MOD-TEANSINK-IN-ATTR.             
412700     EJECT                                                                
412800 S06-LAS-ERSATTREG-ARTREG SECTION.                                        
412900     SKIP2                                                                
413000*WDD7A1 SEKUNDÄR INDEXINGÅNG                                              
413100                                                                          
413200     MOVE W-IDARTNR               TO W-IDARTNR-MIN7                       
413300                                     W-IDARTNR-MAX7                       
413400     PERFORM IMS-GN-ERSB01                                                
413500     IF SEGMENT-FINNS                                                     
413600        MOVE ERSB01-ERS-IDARTNR   TO MOD-IDARTNR-ERS1                     
413700                                     SPAR-IDARTNR-ERS1                    
413800        PERFORM IMS-GN-ERSB01                                             
413900        IF SEGMENT-FINNS                                                  
414000           MOVE SPAR-MFL        TO MOD-MFL                                
414100        ELSE                                                              
414200           MOVE MFS-RENSA-FAELT TO MOD-MFL                                
414300        END-IF                                                            
414400*WDK601 HÄMTA ERSÄTTNINGSKOD FÖR DEN/DE ERSATTA ARTIKLARNA                
414500        MOVE SPAR-IDARTNR-ERS1       TO W-IDARTNR                         
414600        PERFORM IMS-GHU-ARTC01                                            
414700        IF SEGMENT-FINNS                                                  
414800*WDK611 ERSATT ARTIKEL NR 1                                               
414900          PERFORM IMS-GNP-ARTC11                                          
415000          IF SEGMENT-FINNS                                                
415100            MOVE CLAG-KDERS          TO MOD-KDERS-1                       
415200          ELSE                                                            
415300            MOVE MFS-RENSA-FAELT     TO MOD-KDERS-1                       
415400          END-IF                                                          
415500        ELSE                                                              
415600          MOVE MFS-RENSA-FAELT       TO MOD-KDERS-1                       
415700        END-IF                                                            
415800                                                                          
415900********Å T E R S T Ä L L  N Y C K E L N                                  
416000        MOVE WS-IDARTNR              TO W-IDARTNR                         
416100     ELSE                                                                 
416200        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-ERS1                     
416300                                     MOD-KDERS-1                          
416400     END-IF.                                                              
416500     EJECT                                                                
416600 S07-LAS-BENREG SECTION.                                                  
416700     SKIP2                                                                
416800*WDD311                                                                   
416900     IF MSGI-IDLAND-SPR = 'GB'                                            
417000        MOVE 'GB '                      TO W-IDSKYLT                      
417100     ELSE                                                                 
417200        MOVE 'S  '                      TO W-IDSKYLT                      
417300     END-IF                                                               
417400     PERFORM IMS-GU-BENA11-BSEQ                                           
417500     MOVE BENA11-TEXT-BEART          TO MOD-BEART.                        
417600     EJECT                                                                
417700 S96-W009VADD SECTION.                                                    
417800     SKIP2                                                                
417900     CALL W009VADD USING SPAR-W009VADD-DATUM                              
418000                         SPAR-W009VADD-ANTAL.                             
418100     SKIP3                                                                
418200*S97-WKPSKONV SECTION.                                                    
418300*    SKIP2                                                                
418400*    CALL WKPSKONV USING KPS-WKPSAREA.                                    
418500     EJECT                                                                
418600 S98-WDECEDIT SECTION.                                                    
418700     SKIP2                                                                
418800     CALL WDECEDIT USING DEC-WDECAREA.                                    
418900     EJECT                                                                
419000 S99-WDATKONV SECTION.                                                    
419100     SKIP2                                                                
419200     CALL WDATKONV USING DAT-KDDATFORM                                    
419300                         DAT-I-TIDATUM                                    
419400                         DAT-O-TIDATUM                                    
419500                         DAT-KDSVAR.                                      
419600     EJECT                                                                
419700* IMS SEKTIONER                                                           
419800     SKIP3                                                                
419900 IMS-GET-MSG SECTION.                                                     
420000     SKIP2                                                                
420100     MOVE '  QC' TO GODK-STATUSKODER                                      
420200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
420300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
420400     PERFORM IMS-STATUS-KONTROLL.                                         
420500     SKIP3                                                                
420600 IMS-INSERT-MSG SECTION.                                                  
420700     SKIP2                                                                
420800     IF MSGI-IDLAND-SPR = 'GB'                                            
420900        MOVE 'N' TO MFS-KDHUVOMR                                          
421000     END-IF                                                               
421100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
421200     MOVE SPACE TO GODK-STATUSKODER                                       
421300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
421400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
421500     PERFORM IMS-STATUS-KONTROLL.                                         
421600     EJECT                                                                
421700 IMS-GET-WMSGKOM-MSG SECTION.                                             
421800                                                                          
421900     MOVE '  QD'   TO GODK-STATUSKODER                                    
422000     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
422100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
422200     PERFORM IMS-STATUS-KONTROLL                                          
422300     .                                                                    
422400     SKIP3                                                                
422500 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
422600                                                                          
422700     MOVE '  '  TO GODK-STATUSKODER                                       
422800     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
422900     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
423000     PERFORM IMS-STATUS-KONTROLL                                          
423100     .                                                                    
423200     EJECT                                                                
423300 IMS-GHU-ARTC01 SECTION.                                                  
423400     SKIP2                                                                
423500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
423600             DELIMITED BY SIZE INTO SSA1                                  
423700     MOVE '  GE' TO GODK-STATUSKODER                                      
423800     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA1 SSA1                    
423900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
424000     PERFORM IMS-STATUS-KONTROLL.                                         
424100     SKIP3                                                                
424200 IMS-GU-ARTC01 SECTION.                                                   
424300     SKIP2                                                                
424400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
424500             DELIMITED BY SIZE INTO SSA1                                  
424600     MOVE '  GE' TO GODK-STATUSKODER                                      
424700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
424800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
424900     PERFORM IMS-STATUS-KONTROLL.                                         
425000     SKIP3                                                                
425100 IMS-GNP-ARTC11 SECTION.                                                  
425200     SKIP2                                                                
425300     MOVE 'WLARTC11(KDSEGKEY =1)'    TO SSA1                              
425400     MOVE '  GE' TO GODK-STATUSKODER                                      
425500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1                    
425600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
425700     PERFORM IMS-STATUS-KONTROLL.                                         
425800     SKIP3                                                                
425900 IMS-GHNP-ARTC11 SECTION.                                                 
426000     SKIP2                                                                
426100     MOVE 'WLARTC11(KDSEGKEY =1)'   TO SSA1                               
426200     MOVE '  ' TO GODK-STATUSKODER                                        
426300     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA1 SSA1                   
426400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
426500     PERFORM IMS-STATUS-KONTROLL.                                         
426600     EJECT                                                                
426700 IMS-GNP-ARTC22   SECTION.                                                
426800     SKIP2                                                                
426900     MOVE   'WLARTC11(KDSEGKEY =1)'  TO SSA1                              
427000     MOVE   'WLARTC22 '              TO SSA2                              
427100     MOVE '  GE' TO GODK-STATUSKODER                                      
427200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2               
427300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
427400     PERFORM IMS-STATUS-KONTROLL.                                         
427500     SKIP3                                                                
427600 IMS-GNP-ARTC23   SECTION.                                                
427700     SKIP2                                                                
427800     MOVE   'WLARTC11(KDSEGKEY =1)'  TO SSA1                              
427900     MOVE   'WLARTC23 '              TO SSA2                              
428000     MOVE '  GE' TO GODK-STATUSKODER                                      
428100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2               
428200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
428300     PERFORM IMS-STATUS-KONTROLL.                                         
428400     EJECT                                                                
428500 IMS-GHNP-ARTC25 SECTION.                                                 
428600     SKIP2                                                                
428700     MOVE  'WLARTC11*F(KDSEGKEY =1)'  TO SSA1                             
428800     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
428900            DELIMITED BY SIZE INTO SSA2                                   
429000     MOVE '  GE' TO GODK-STATUSKODER                                      
429100     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2              
429200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
429300     PERFORM IMS-STATUS-KONTROLL.                                         
429400     SKIP3                                                                
429500 IMS-GU-BENA11-BSEQ SECTION.                                              
429600     SKIP2                                                                
429700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
429800             DELIMITED BY SIZE INTO SSA1                                  
429900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
430000             DELIMITED BY SIZE INTO SSA2                                  
430100     MOVE '  ' TO GODK-STATUSKODER                                        
430200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA1 SSA1 SSA2                
430300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
430400     PERFORM IMS-STATUS-KONTROLL.                                         
430500     EJECT                                                                
430600 IMS-GU-LEVA01 SECTION.                                                   
430700     SKIP2                                                                
430800     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
430900             DELIMITED BY SIZE INTO SSA1                                  
431000     MOVE '  GE' TO GODK-STATUSKODER                                      
431100     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA3 SSA1                     
431200     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
431300     PERFORM IMS-STATUS-KONTROLL.                                         
431400     EJECT                                                                
431500 IMS-GN-ERSB01 SECTION.                                                   
431600     SKIP2                                                                
431700     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
431800                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
431900             DELIMITED BY SIZE INTO SSA1                                  
432000     MOVE '  GE' TO GODK-STATUSKODER                                      
432100     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-AREA1 SSA1                     
432200     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
432300     PERFORM IMS-STATUS-KONTROLL.                                         
432400     EJECT                                                                
432500 IMS-GHU-ARTG01-MED-GE SECTION.                                           
432600     SKIP2                                                                
432700     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
432800             DELIMITED BY SIZE INTO SSA1                                  
432900     MOVE '  GE' TO GODK-STATUSKODER                                      
433000     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA2 SSA1                    
433100     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
433200     PERFORM IMS-STATUS-KONTROLL.                                         
433300     EJECT                                                                
433400*2141-KOPPLING:                                                           
433500 IMS-GU-ARTI01-UTAN-SOK SECTION.                                          
433600     SKIP2                                                                
433700     STRING 'WLARTI01(WDD2B1KY >' W-WDD2B1KY-MIN                          
433800                    '&WDD2B1KY=<' W-WDD2B1KY-MAX                          
433900                    '&KDANSKQ  =' W-KDANSKQ-X ')'                         
434000             DELIMITED BY SIZE INTO SSA1                                  
434100     MOVE '  GE' TO GODK-STATUSKODER                                      
434200     CALL CBLTDLI USING GU ARTI-PCB DLI-IO-AREA1 SSA1                     
434300     MOVE ARTI-STATUS-CODE TO STATUS-WS                                   
434400     PERFORM IMS-STATUS-KONTROLL.                                         
434500     EJECT                                                                
434600 IMS-GU-ARTI01-MED-SOK SECTION.                                           
434700     SKIP2                                                                
434800     STRING 'WLARTI01(WDD2B1KY >' W-WDD2B1KY-MIN                          
434900                    '&WDD2B1KY=<' W-WDD2B1KY-MAX                          
435000                    '&IDPROJ   =' W-IDPROJ-MAX                            
435100                    '&KDANSKQ  =' W-KDANSKQ-X ')'                         
435200             DELIMITED BY SIZE INTO SSA1                                  
435300     MOVE '  GE' TO GODK-STATUSKODER                                      
435400     CALL CBLTDLI USING GU ARTI-PCB DLI-IO-AREA1 SSA1                     
435500     MOVE ARTI-STATUS-CODE TO STATUS-WS                                   
435600     PERFORM IMS-STATUS-KONTROLL.                                         
435700     EJECT                                                                
435800 IMS-GU-ARTM01        SECTION.                                            
435900     SKIP2                                                                
436000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
436100             DELIMITED BY SIZE INTO SSA1                                  
436200     MOVE '  GE' TO GODK-STATUSKODER                                      
436300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA1 SSA1                     
436400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
436500     PERFORM IMS-STATUS-KONTROLL.                                         
436600     EJECT                                                                
436700 IMS-GU-WLXXAQ01-UNIK SECTION.                                            
436800     SKIP2                                                                
436900     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
437000             DELIMITED BY SIZE INTO SSA1                                  
437100     MOVE '  GE' TO GODK-STATUSKODER                                      
437200     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA1 SSA1                     
437300     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
437400     PERFORM IMS-STATUS-KONTROLL.                                         
437500     SKIP3                                                                
437600 IMS-GNP-WLXXAQ11-FIRST SECTION.                                          
437700     SKIP2                                                                
437800     STRING 'WLXXAQ11*F(IDPROJOB =' W-IDPROJOBJ                           
437900                      '&IDPROJ   =' W-IDPROJ  ')'                         
438000             DELIMITED BY SIZE INTO SSA1                                  
438100     MOVE '  GE' TO GODK-STATUSKODER                                      
438200     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA1 SSA1                    
438300     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
438400     PERFORM IMS-STATUS-KONTROLL.                                         
438500     EJECT                                                                
438600 IMS-GU-WLXXAT11 SECTION.                                                 
438700     SKIP2                                                                
438800     STRING 'WLXXAT01(WDGXKEY  =' W-1137KEY-X ')'                         
438900             DELIMITED BY SIZE INTO SSA1                                  
439000     STRING 'WLXXAT11(IDUSER   =' W-1138KEY-X ')'                         
439100             DELIMITED BY SIZE INTO SSA2                                  
439200     MOVE '  GE' TO GODK-STATUSKODER                                      
439300     CALL CBLTDLI USING GU XXAT-PCB DLI-IO-AREA1 SSA1 SSA2                
439400     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
439500     PERFORM IMS-STATUS-KONTROLL.                                         
439600     EJECT                                                                
439700 IMS-GU-WLXXAV11      SECTION.                                            
439800     SKIP2                                                                
439900     STRING 'WLXXAV01(WDGXKEY  =' W-1141KEY-X ')'                         
440000             DELIMITED BY SIZE INTO SSA1                                  
440100     STRING 'WLXXAV11(KDSEGKEY =' W-1142KEY-X                             
440200                    '&IDARTNR  =' W-IDARTNR-X ')'                         
440300             DELIMITED BY SIZE INTO SSA2                                  
440400     MOVE '  GE' TO GODK-STATUSKODER                                      
440500     CALL CBLTDLI USING GU XXAV-PCB DLI-IO-AREA1 SSA1 SSA2                
440600     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
440700     PERFORM IMS-STATUS-KONTROLL                                          
440800     .                                                                    
440900     SKIP3                                                                
441000 IMS-GHU-WLXXAV11     SECTION.                                            
441100     SKIP2                                                                
441200     STRING 'WLXXAV01(WDGXKEY  =' W-1141KEY-X ')'                         
441300             DELIMITED BY SIZE INTO SSA1                                  
441400     STRING 'WLXXAV11(KDSEGKEY =' W-1142KEY-X                             
441500                    '&IDARTNR  =' W-IDARTNR-X ')'                         
441600             DELIMITED BY SIZE INTO SSA2                                  
441700     MOVE '    ' TO GODK-STATUSKODER                                      
441800     CALL CBLTDLI USING GHU XXAV-PCB DLI-IO-AREA1 SSA1 SSA2               
441900     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
442000     PERFORM IMS-STATUS-KONTROLL                                          
442100     .                                                                    
442200 IMS-GHU-WLXXAV11-GE  SECTION.                                            
442300     SKIP2                                                                
442400     STRING 'WLXXAV01(WDGXKEY  =' W-1141KEY-X ')'                         
442500             DELIMITED BY SIZE INTO SSA1                                  
442600     STRING 'WLXXAV11(KDSEGKEY =' W-1142KEY-X                             
442700                    '&IDARTNR  =' W-IDARTNR-X ')'                         
442800             DELIMITED BY SIZE INTO SSA2                                  
442900     MOVE '  GE' TO GODK-STATUSKODER                                      
443000     CALL CBLTDLI USING GHU XXAV-PCB DLI-IO-AREA1 SSA1 SSA2               
443100     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
443200     PERFORM IMS-STATUS-KONTROLL                                          
443300     .                                                                    
443400 IMS-DELETE-1142 SECTION.                                                 
443500     SKIP2                                                                
443600     MOVE '  '   TO GODK-STATUSKODER                                      
443700     CALL CBLTDLI USING DLET XXAV-PCB DLI-IO-AREA1                        
443800     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
443900     PERFORM IMS-STATUS-KONTROLL                                          
444000     .                                                                    
444100     EJECT                                                                
444200 IMS-ISRT-ARTC25 SECTION.                                                 
444300     SKIP2                                                                
444400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
444500            DELIMITED BY SIZE INTO SSA1                                   
444600     MOVE 'WLARTC11(KDSEGKEY =1)'  TO SSA2                                
444700     MOVE 'WLARTC25 '     TO SSA3                                         
444800     MOVE '  '   TO GODK-STATUSKODER                                      
444900     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA1 SSA1 SSA2              
445000                                                     SSA3                 
445100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
445200     PERFORM IMS-STATUS-KONTROLL.                                         
445300     SKIP3                                                                
445400 IMS-ISRT-XXAV11 SECTION.                                                 
445500     SKIP2                                                                
445600     MOVE 'WLXXAV11 '     TO SSA1                                         
445700     MOVE '  '   TO GODK-STATUSKODER                                      
445800     CALL CBLTDLI USING ISRT XXAV-PCB DLI-IO-AREA1 SSA1                   
445900     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
446000     PERFORM IMS-STATUS-KONTROLL.                                         
446100     SKIP3                                                                
446200 IMS-ISRT-XXBN SECTION.                                                   
446300     SKIP2                                                                
446400     STRING 'WLXXBN01(WDG3KEY  =' W-WDG3KEY-2221-X ')'                    
446500            DELIMITED BY SIZE INTO SSA1                                   
446600     MOVE 'WLXXBN11 '     TO SSA2                                         
446700     MOVE '  '   TO GODK-STATUSKODER                                      
446800     CALL CBLTDLI USING ISRT XXBN-PCB DLI-IO-AREA1 SSA1 SSA2              
446900     MOVE XXBN-STATUS-CODE TO STATUS-WS                                   
447000     PERFORM IMS-STATUS-KONTROLL.                                         
447100     SKIP3                                                                
447200 IMS-REPL-ARTC SECTION.                                                   
447300     SKIP2                                                                
447400     MOVE '  '   TO GODK-STATUSKODER                                      
447500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA1                        
447600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
447700     PERFORM IMS-STATUS-KONTROLL.                                         
447800     SKIP3                                                                
447900 IMS-DELETE SECTION.                                                      
448000     SKIP2                                                                
448100     MOVE '  '   TO GODK-STATUSKODER                                      
448200     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA1                        
448300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
448400     PERFORM IMS-STATUS-KONTROLL.                                         
448500     SKIP3                                                                
448600 IMS-REPL-NYPON SECTION.                                                  
448700     SKIP2                                                                
448800     MOVE '  '   TO GODK-STATUSKODER                                      
448900     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA2                        
449000     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
449100     PERFORM IMS-STATUS-KONTROLL.                                         
449200     SKIP3                                                                
449300 IMS-ISRT-ARTG01     SECTION.                                             
449400     SKIP2                                                                
449500     STRING 'WLARTG01   '                                                 
449600             DELIMITED BY SIZE INTO SSA1                                  
449700     MOVE '  II' TO GODK-STATUSKODER                                      
449800     CALL CBLTDLI USING ISRT ARTG-PCB DLI-IO-AREA2 SSA1                   
449900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
450000     PERFORM IMS-STATUS-KONTROLL.                                         
450100     SKIP3                                                                
450200 IMS-ISRT-WDH801   SECTION.                                               
450300                                                                          
450400     MOVE 'WLPRIG01 ' TO SSA1                                             
450500     MOVE '  II' TO GODK-STATUSKODER                                      
450600     CALL CBLTDLI USING ISRT PRIG-PCB PRI-WDH801 SSA1                     
450700     MOVE PRIG-STATUS-CODE  TO STATUS-WS                                  
450800     PERFORM IMS-STATUS-KONTROLL                                          
450900     .                                                                    
451000     SKIP3                                                                
451100 IMS-REPL-XXAV11 SECTION.                                                 
451200     MOVE '  ' TO GODK-STATUSKODER                                        
451300     CALL CBLTDLI USING REPL XXAV-PCB DLI-IO-AREA1                        
451400     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
451500     PERFORM IMS-STATUS-KONTROLL                                          
451600     .                                                                    
451700     EJECT                                                                
451800 IMS-STATUS-KONTROLL SECTION.                                             
451900     SET STATUS-IX TO 1                                                   
452000     SEARCH GODK-STATUS AT END CALL FELLOG                                
452100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
452200     END-SEARCH.                                                          
452300     EJECT                                                                
452400*    -COPY WY2000P1                                                       
452500     EJECT                                                                
452600*    -COPY WY2000P3                                                       
